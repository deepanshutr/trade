#!/usr/bin/env python3
"""Offline playback through Main.run and a real pseudo terminal, using the standard library."""

import argparse
import errno
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import select
import signal
import socket
import struct
import subprocess
import sys
import tempfile
import termios
import time

ESC = bytes([27])
START = ESC + b"[H"
END = ESC + b"[J"
CLEAR = ESC + b"[K"
LEAVE = ESC + b"[?2004l" + ESC + b"[?25h" + ESC + b"[?1049l"
RESTORED = b"OFFLINE_TERMINAL_RESTORED=yes"
SGR = re.compile(r"\x1b\[[0-9;]*m")


class Frames:
    def __init__(self):
        self.pending = b""
        self.complete = []

    def feed(self, data, captured_ns):
        self.pending += data
        while START in self.pending:
            start = self.pending.index(START)
            end = self.pending.find(END, start + len(START))
            if end < 0:
                break
            raw = self.pending[start : end + len(END)]
            self.pending = self.pending[end + len(END) :]
            rows = raw[len(START) : -len(END)].split(b"\r\n")
            if not all(row.endswith(CLEAR) for row in rows):
                raise AssertionError("a completed frame lacks row clearing")
            lines = [SGR.sub("", row[: -len(CLEAR)].decode("utf-8")) for row in rows]
            self.complete.append({"captured_ns": captured_ns, "lines": lines})


def market_matches(frame, contract, coin, midpoint):
    if not frame["lines"]:
        return False
    coin, midpoint = re.escape(coin), re.escape(midpoint)
    patterns = {
        # The original live label describes recency, including after a disconnect.
        "original": rf"{coin} {midpoint}(?: mark \S+ orc \S+ 24h \S+ fund \S+ OI \S+ vol \S+)? live (\d+\.\d)s",
        "contextual": rf"connection active (\d+\.\d)s {coin} mid {midpoint}(?: (?:mark|oracle|24h|fund|open interest|volume) \S+)*",
    }
    header = frame["lines"][0]
    if contract == "workspace":
        header = header.split("|", 1)[0]
        contract = "contextual"
    matched = re.fullmatch(patterns[contract], " ".join(header.split()))
    return matched is not None and float(matched[1]) <= 5


def target_matches(frame, contract, coin, side, limit, midpoint, enter="disabled"):
    if len(frame["lines"]) < 5 or not market_matches(frame, contract, coin, midpoint):
        return False
    if contract == "workspace":
        if len(frame["lines"]) < 30:
            return False
        cols = len(frame["lines"][0])
        divider = min(cols * 3 // 5, cols - 47)
        return frame["lines"][-6][divider:] == "|" + f"Target {side} {limit} Up/Down".ljust(cols - divider - 1)
    rows = [row.rstrip() for row in frame["lines"][-4:]]
    if contract == "original":
        selected = rf"Target 9 buy 0 sell \| {re.escape(side)} \${re.escape(limit)}"
        details = r"(?: request \S+ estimate \S+(?: value \$\S+ average (?:\$\S+|unavailable))?| no estimated fill)"
        return rows[0].startswith("Hard ") and rows[1].startswith("Best ") and re.fullmatch(selected + details, rows[2]) is not None
    return rows[0] == f"Target {coin} {side} limit ${limit} | Enter {enter} Escape cancels"


def idle_matches(frame, contract, coin, midpoint):
    if len(frame["lines"]) < 5 or not market_matches(frame, contract, coin, midpoint):
        return False
    if contract == "workspace":
        if len(frame["lines"]) < 30:
            return False
        cols = len(frame["lines"][0])
        divider = min(cols * 3 // 5, cols - 47)
        return frame["lines"][-6][divider:] == "|" + "( ) unavailable. Select one for the reason".ljust(cols - divider - 1)
    controls = {
        "original": "Target 9 buy 0 sell | arrows set price enter sends escape cancels",
        "contextual": "Target 9 buy 0 sell | up/down zoom left/right history m market c chart q quit",
    }
    rows = [row.rstrip() for row in frame["lines"][-4:]]
    return rows[0].startswith("Hard ") and rows[1].startswith("Best ") and rows[2] == controls[contract]


def history_matches(before, after, shift, max_seconds=10, clock_count=3):
    elapsed = after["captured_ns"] - before["captured_ns"]
    if elapsed < 0 or elapsed > max_seconds * 1_000_000_000:
        return False
    minimum_progress = elapsed // 1_000_000_000 - 1
    maximum_progress = (elapsed + 999_999_999) // 1_000_000_000 + 1
    clocks = []
    for frame in (before, after):
        rows = [row for row in frame["lines"] if "c line 1s/col" in row]
        if len(rows) != 1:
            return False
        values = re.findall(r"(?<![\d:])(\d{2}):(\d{2}):(\d{2})(?![\d:])", rows[0])
        if len(values) != clock_count or not 1 <= clock_count <= 3:
            return False
        clocks.append([int(h) * 3600 + int(m) * 60 + int(s) for h, m, s in values])
    return all(minimum_progress <= (new - old + 43200) % 86400 - 43200 - shift <= maximum_progress
               for old, new in zip(*clocks))


def percentile(values, percent):
    ordered = sorted(values)
    return ordered[max(0, (len(ordered) * percent + 99) // 100 - 1)]


def validate_result(result, mode):
    assert result["forbidden"] == [], result["forbidden"]
    assert result["metadata_calls"] > 0, "the offline worker was not exercised"
    samples = result["queue"]["samples"]
    assert any(s["event"] == "key Quit" and s["method"] == "put" for s in samples), "quit did not pass through the fold queue"
    assert not any(s["event"] == "Bad" for s in samples), "recorded input was rejected"
    assert all(int(s["age_ns"]) >= 0 for s in samples), "negative queue age"
    if mode in ("send", "history"):
        assert result["held_work"], "the selected work never entered its hold"
        if result["hold_finished"]:
            ended = int(result.get("hold_finished_ns", "0"))
            quits = [int(s.get("entered_ns", "0")) for s in samples if s["event"] == "key Quit"]
            assert ended > 0 and quits and all(0 < at <= ended for at in quits), "work ended before quit or lacks timing evidence"
        assert not result["hold_released"], "the driver released held work before quit"
        events = [s["event"] for s in samples]
        assert "key PromptSubmit" in events and "key ZoomOut" in events
        assert any(s["event"] == "quote ETH" and s["method"] == "offer" for s in samples), "the selected quote did not enter the real queue"
    if mode == "normal":
        for command in ("key ScrubBack", "key ScrubForward"):
            assert any(s["event"] == command and s["method"] == "put" for s in samples), command + " did not enter the real queue"
    if mode == "queue":
        released = int(result["queue"]["queue_release_ns"])
        zooms = [s for s in samples if s["event"] == "key ZoomOut"]
        assert len(zooms) == 2
        assert len({s["entered_ns"] for s in zooms}) == 2, "equal keys lost their separate arrivals"
        assert all(int(s["entered_ns"]) < released <= int(s["taken_ns"]) for s in zooms)
    return samples


def stop_group(child, pump):
    def signal_group(value):
        child.poll()
        try:
            os.killpg(child.pid, value)
            return True
        except ProcessLookupError:
            return False
        except PermissionError:
            if value != 0:
                raise
            return True

    try:
        if signal_group(signal.SIGTERM):
            deadline = time.monotonic() + 3
            while signal_group(0) and time.monotonic() < deadline:
                pump(0.1)
    finally:
        signal_group(signal.SIGKILL)
        child.wait(timeout=3)
        deadline = time.monotonic() + 3
        while signal_group(0) and time.monotonic() < deadline:
            time.sleep(0.01)
        assert not signal_group(0), "playback process group did not stop"


class Playback:
    def __init__(self, args, mode, directory):
        self.args, self.mode, self.directory = args, mode, directory
        self.frames, self.events, self.actions = Frames(), [], []
        self.raw, self.control_pending = bytearray(), b""
        self.control, self.eof = None, False
        self.temporary = tempfile.TemporaryDirectory(prefix="desk-playback-")
        private = Path(self.temporary.name)
        self.listener = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.listener.bind(str(private / "control"))
        self.listener.listen(1)
        settings = private / "settings.json"
        settings.write_text("{}\n")
        settings.chmod(0o600)
        self.master, self.slave = os.openpty()
        fcntl.ioctl(self.slave, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 100, 0, 0))
        self.stderr = (directory / "stderr.txt").open("wb")

        def terminal_session():
            os.setsid()
            fcntl.ioctl(0, termios.TIOCSCTTY, 0)

        self.started_ns = time.monotonic_ns()
        self.cpu_before = resource.getrusage(resource.RUSAGE_CHILDREN)
        self.child = subprocess.Popen(
            [sys.executable, str(Path(__file__).resolve()), "--terminal-child",
             args.java, "-cp", args.classpath, "desk.OfflinePlayback",
             str(private / "control"), str(Path(args.fixtures).resolve()), str(settings)],
            stdin=self.slave, stdout=self.slave, stderr=self.stderr,
            preexec_fn=terminal_session,
        )

    def pump(self, timeout):
        readers = [self.listener if self.control is None else self.control]
        if not self.eof:
            readers.append(self.master)
        ready, _, _ = select.select(readers, [], [], timeout)
        for source in ready:
            if source is self.listener:
                self.control, _ = self.listener.accept()
            elif source is self.control:
                data = self.control.recv(65536)
                if not data:
                    continue
                self.control_pending += data
                while b"\n" in self.control_pending:
                    line, self.control_pending = self.control_pending.split(b"\n", 1)
                    value = json.loads(line)
                    value["received_ns"] = time.monotonic_ns()
                    self.events.append(value)
            else:
                try:
                    data = os.read(self.master, 65536)
                except OSError as error:
                    if error.errno != errno.EIO:
                        raise
                    data = b""
                if data:
                    self.raw.extend(data)
                    self.frames.feed(data, time.monotonic_ns())
                else:
                    self.eof = True

    def wait(self, predicate, label):
        deadline = time.monotonic() + 10
        while not predicate():
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise AssertionError("watchdog waiting for " + label)
            self.pump(min(remaining, 0.1))
            if self.child.poll() is not None and not predicate():
                self.pump(0)
                if not predicate():
                    raise AssertionError("child exited while waiting for " + label)

    def event(self, kind, count=1):
        self.wait(lambda: sum(e["kind"] == kind for e in self.events) >= count, kind)
        return [e for e in self.events if e["kind"] == kind][count - 1]

    def command(self, command, **values):
        self.wait(lambda: self.control is not None, "control connection")
        self.control.sendall((json.dumps({"command": command, **values}) + "\n").encode())

    def frame(self, predicate, after=0):
        self.wait(lambda: any(predicate(f) for f in self.frames.complete[after:]), "matching full frame")
        return next(f for f in self.frames.complete[after:] if predicate(f))

    def action(self, label, action, predicate):
        after = len(self.frames.complete)
        started = time.monotonic_ns()
        action()
        arrived = time.monotonic_ns()
        frame = self.frame(predicate, after)
        self.actions.append({
            "label": label, "write_started_ns": started, "write_completed_ns": arrived,
            "frame_completed_ns": frame["captured_ns"],
            "arrival_to_frame_ns": frame["captured_ns"] - arrived,
            "write_duration_ns": arrived - started,
        })
        assert frame["captured_ns"] >= arrived
        return frame

    def key(self, label, data, predicate):
        return self.action(label, lambda: self.write(data), predicate)

    def write(self, data):
        while data:
            count = os.write(self.master, data)
            data = data[count:]

    def resize(self, cols, rows):
        fcntl.ioctl(self.slave, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))

    def run(self):
        def text(frame):
            return "\n".join(frame["lines"])

        def market(coin):
            return lambda frame: market_matches(frame, self.args.display_contract, coin, "522.26")

        def contains(value):
            return lambda frame: value in text(frame)

        def target(price):
            return lambda frame: target_matches(frame, self.args.display_contract, "XMR", "buy", price, "522.26")

        self.event("connected")
        self.command("quote", coin="XMR")
        self.frame(market("XMR"))
        assert not any(" bad " in f["lines"][0] for f in self.frames.complete), "recorded input was rejected"
        if self.mode == "normal":
            self.key("zoom out", ESC + b"[B", contains("2s/col"))
            self.key("zoom in", ESC + b"[A", contains("1s/col"))
            self.key("candles", b"c", contains("1m/candle"))
            self.key("line", b"c", contains("1s/col"))
            before = self.frames.complete[-1]
            # Workspace has a 53-column market panel, leaving 28 one-second dots.
            step = 11 if self.args.display_contract == "workspace" else 30
            count = 2 if self.args.display_contract == "workspace" else 3
            history = self.key("history back", ESC + b"[D", lambda f: history_matches(before, f, -step, clock_count=count))
            self.key("history forward", ESC + b"[C",
                     lambda f: history_matches(history, f, step, clock_count=count) and history_matches(before, f, 0, 20, clock_count=count))
            self.key("select public Target", b"9", target("522.28"))
            self.key("move public Target", ESC + b"[A", target("522.29"))
            self.key("cancel Target with lone Escape", ESC,
                     lambda f: idle_matches(f, self.args.display_contract, "XMR", "522.26"))
            pasted = ESC + b"[200~qmETH\r1234567890" + ESC + b"[201~c"
            frame = self.key("discard pasted actions", pasted, contains("1m/candle"))
            assert market("XMR")(frame), "pasted input changed the market"
            self.action("minimum size guard", lambda: self.resize(99, 29),
                        contains("need 100x30, have 99x29"))
            wide = self.action("wide size", lambda: self.resize(120, 40),
                               lambda f: len(f["lines"]) == 40 and market("XMR")(f))
            assert all(len(row) == 120 for row in wide["lines"])
        elif self.mode in ("send", "history"):
            self.command("arm", work=self.mode)
            self.event("armed")
            after = len(self.frames.complete)
            started = time.monotonic_ns()
            self.write(b"mETH\r")
            arrived = time.monotonic_ns()
            self.event("work-held")
            self.command("quote", coin="ETH")
            matched = self.frame(market("ETH"), after)
            self.actions.append({"label": "switch with held " + self.mode,
                                 "write_started_ns": started, "write_completed_ns": arrived,
                                 "frame_completed_ns": matched["captured_ns"],
                                 "arrival_to_frame_ns": matched["captured_ns"] - arrived,
                                 "write_duration_ns": arrived - started})
            self.key("navigate with held " + self.mode, ESC + b"[B",
                     lambda f: market("ETH")(f) and contains("2s/col")(f))
            assert not any(e["kind"] == "work-ended" for e in self.events)
        elif self.mode == "queue":
            self.command("hold-queue")
            self.command("quote", coin="XMR")
            self.event("queue-held")
            after = len(self.frames.complete)
            started = time.monotonic_ns()
            self.write((ESC + b"[B") * 2)
            arrived = time.monotonic_ns()
            self.event("queued-key", 2)
            self.command("release-queue")
            frame = self.frame(contains("5s/col"), after)
            self.actions.append({"label": "drain held queue", "write_started_ns": started,
                                 "write_completed_ns": arrived, "frame_completed_ns": frame["captured_ns"],
                                 "arrival_to_frame_ns": frame["captured_ns"] - arrived,
                                 "write_duration_ns": arrived - started})
        quit_started = time.monotonic_ns()
        self.write(b"q")
        quit_arrived = time.monotonic_ns()
        self.wait(lambda: LEAVE in self.raw, "terminal leave bytes")
        leave_at = time.monotonic_ns()
        result = self.event("result")
        self.wait(lambda: RESTORED in self.raw, "terminal settings restored by child wrapper")
        self.wait(lambda: self.child.poll() is not None, "application exit")
        status = self.child.wait()
        assert status == 0, status
        samples = validate_result(result, self.mode)
        assert any(any(0x2800 <= ord(c) <= 0x28FF for c in line)
                   for f in self.frames.complete for line in f["lines"]), "no recorded braille output"
        cpu_after = resource.getrusage(resource.RUSAGE_CHILDREN)
        ages = [int(s["age_ns"]) for s in samples]
        key_ages = [int(s["age_ns"]) for s in samples if s["event"].startswith("key ")]
        feed_ages = [int(s["age_ns"]) for s in samples if s["method"] == "offer"]
        return {"mode": self.mode, "status": status, "frames": len(self.frames.complete),
                "wall_ns": time.monotonic_ns() - self.started_ns,
                "child_cpu_seconds": cpu_after.ru_utime + cpu_after.ru_stime
                    - self.cpu_before.ru_utime - self.cpu_before.ru_stime,
                "actions": self.actions, "queue_samples": len(samples),
                "queue_age_median_ns": percentile(ages, 50), "queue_age_p99_ns": percentile(ages, 99),
                "key_queue_median_ns": percentile(key_ages, 50), "key_queue_p99_ns": percentile(key_ages, 99),
                "feed_queue_median_ns": percentile(feed_ages, 50), "feed_queue_p99_ns": percentile(feed_ages, 99),
                "quit_write_started_ns": quit_started, "quit_write_completed_ns": quit_arrived,
                "quit_to_leave_ns": leave_at - quit_arrived, "terminal_restored": True}

    def close(self):
        try:
            stop_group(self.child, self.pump)
        finally:
            self.stderr.close()
            (self.directory / "terminal.bin").write_bytes(self.raw)
            (self.directory / "frames.json").write_text(json.dumps(self.frames.complete, indent=2) + "\n")
            (self.directory / "events.json").write_text(json.dumps(self.events, indent=2) + "\n")
            for connection in (self.control, self.listener):
                if connection is not None:
                    connection.close()
            os.close(self.master)
            os.close(self.slave)
            self.temporary.cleanup()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--java", required=True)
    parser.add_argument("--classpath", required=True)
    parser.add_argument("--fixtures", default="fixtures")
    parser.add_argument("--output", required=True)
    parser.add_argument("--mode", choices=("all", "normal", "send", "history", "queue"), default="all")
    parser.add_argument("--display-contract", choices=("original", "contextual", "workspace"), default="workspace")
    parser.add_argument("--repeats", type=int, default=1)
    args = parser.parse_args()
    assert args.repeats > 0
    output = Path(args.output)
    output.mkdir(parents=True, exist_ok=False)
    if args.display_contract == "workspace":
        anchors = [("src/WorkspaceApp.scala", "Thread.sleep(100L)", 100),
                   ("src/WorkspaceApp.scala", "Thread.sleep(10L)", 10),
                   ("src/Main.scala", "val escapeWaitNanos = 40_000_000L", 40),
                   ("src/WorkspaceApp.scala", "if time - sizeAt >= 500L then", 500)]
    else:
        anchors = [("src/Main.scala", fragment, delay) for fragment, delay in
                   [("Thread.sleep(100)", 100), ("Thread.sleep(10)", 10),
                    ("val escapeWaitNanos = 40_000_000L", 40),
                    ("if time - sizeAt >= 1000 then", 1000)]]
    schedule = {}
    for source, fragment, delay in anchors:
        assert Path(source).read_text().count(fragment) == 1, "scheduling premise changed: " + fragment
        schedule[source + ": " + fragment] = delay
    results = []
    modes = ("normal", "send", "history", "queue") if args.mode == "all" else (args.mode,)
    try:
        for repetition in range(args.repeats):
            for mode in modes:
                directory = output / f"{repetition + 1}-{mode}"
                directory.mkdir()
                run = Playback(args, mode, directory)
                try:
                    result = run.run()
                    results.append(result)
                    print(json.dumps(result), flush=True)
                finally:
                    run.close()
    finally:
        manifest = {name: hashlib.sha256((Path(args.fixtures) / name).read_bytes()).hexdigest()
                    for name in ("l2Book.json", "bbo.json", "trades.json", "activeAssetCtx.json")}
        source_hashes = {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(Path("src").glob("*.scala"))}
        (output / "summary.json").write_text(json.dumps({"runs": results, "fixtures_sha256": manifest,
                "source_sha256": source_hashes, "built_in_wait_ms": schedule,
                "display_contract": args.display_contract}, indent=2) + "\n")


if __name__ == "__main__":
    if sys.argv[1:2] == ["--terminal-child"]:
        saved_mode = subprocess.check_output(["/bin/stty", "-g"], text=True).strip()
        subprocess.run(["/bin/stty", "raw", "-echo"], check=True)
        subprocess.run(["/bin/stty", saved_mode], check=True)
        saved = termios.tcgetattr(0)
        status = subprocess.call(sys.argv[2:])
        after = termios.tcgetattr(0)
        restored = after == saved
        if not restored:
            sys.stderr.write("terminal settings before: " + repr(saved) + "\n")
            sys.stderr.write("terminal settings after: " + repr(after) + "\n")
        os.write(1, b"\r\nOFFLINE_TERMINAL_RESTORED=" + (b"yes" if restored else b"no") + b"\r\n")
        sys.exit(status if status else (0 if restored else 3))
    else:
        main()
