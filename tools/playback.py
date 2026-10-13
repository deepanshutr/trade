#!/usr/bin/env python3
"""Offline playback through Main.run and a real pseudo terminal, using the standard library."""

import argparse
from decimal import ROUND_HALF_UP, Decimal, localcontext
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
PAINT_BEGIN = ESC + b"[?2026h"
PAINT_END = ESC + b"[?2026l"
LEAVE = ESC + b"[?2004l" + ESC + b"[?25h" + ESC + b"[?1049l"
RESTORED = b"OFFLINE_TERMINAL_RESTORED=yes"
SGR = re.compile(r"\x1b\[[0-9;]*m")


class Frames:
    """Reconstruct the screen at completed legacy or synchronized paint boundaries."""

    def __init__(self):
        self.pending = b""
        self.complete = []
        self.lines = None

    def feed(self, data, captured_ns):
        self.pending += data
        while True:
            starts = [(self.pending.find(marker), marker) for marker in (PAINT_BEGIN, START)
                      if marker in self.pending]
            if not starts:
                # Preserve a split start marker while discarding unrelated terminal output.
                self.pending = self.pending[-(len(PAINT_BEGIN) - 1):]
                return
            start, marker = min(starts)
            finish = PAINT_END if marker == PAINT_BEGIN else END
            end = self.pending.find(finish, start + len(marker))
            if end < 0:
                self.pending = self.pending[start:]
                return
            body_start = start + len(marker) if marker == PAINT_BEGIN else start
            body_end = end if marker == PAINT_BEGIN else end + len(END)
            lines = self._apply(self.pending[body_start:body_end])
            self.pending = self.pending[end + len(finish):]
            self.lines = lines
            self.complete.append({"captured_ns": captured_ns, "lines": lines.copy()})

    def _apply(self, raw):
        text = raw.decode("utf-8")
        full = raw.startswith(START)
        assert full or self.lines is not None, "a changed-row update has no initial screen"
        lines = [] if full else self.lines.copy()
        row, col, offset = 0, 0, 0
        dirty = set()
        last = None
        while offset < len(text):
            char = text[offset]
            if char == "\x1b":
                command = re.match(r"\x1b\[([0-9;]*)([A-Za-z])", text[offset:])
                assert command, "unrecognized terminal command"
                params, action = command.groups()
                values = [int(v) if v else 0 for v in params.split(";")] if params else []
                if action == "H":
                    assert len(values) <= 2 and all(v > 0 for v in values), "invalid cursor position"
                    row = (values[0] if values else 1) - 1
                    col = (values[1] if len(values) > 1 else 1) - 1
                elif action == "K":
                    assert not values or values == [0], "unsupported row erase"
                    while len(lines) <= row:
                        lines.append("")
                    lines[row] = lines[row][:col]
                    dirty.discard(row)
                elif action == "J":
                    assert not values or values == [0], "unsupported screen erase"
                    lines = lines[:row] + ([lines[row][:col]] if row < len(lines) else [])
                elif action != "m":
                    raise AssertionError("unsupported terminal command " + action)
                if action != "m":
                    last = action
                offset += command.end()
            elif char == "\r":
                col = 0
                offset += 1
            elif char == "\n":
                row += 1
                offset += 1
            else:
                end = offset + 1
                while end < len(text) and text[end] not in "\x1b\r\n":
                    end += 1
                value = text[offset:end]
                while len(lines) <= row:
                    lines.append("")
                lines[row] = lines[row][:col].ljust(col) + value + lines[row][col + len(value):]
                col += len(value)
                dirty.add(row)
                last = None
                offset = end
        assert not dirty, "a completed frame lacks row clearing"
        assert last == ("J" if full else "K"), "a completed frame lacks its final erase"
        return lines


def market_matches(frame, coin, midpoint):
    if not frame["lines"]:
        return False
    coin, midpoint = re.escape(coin), re.escape(midpoint)
    pattern = rf"{coin} mid {midpoint}(?: (?:mark|oracle|24h|fund|open interest|volume) \S+)* ping (\d+\.\d)s"
    header = frame["lines"][0].split("|", 1)[0]
    matched = re.fullmatch(pattern, " ".join(header.split()))
    return matched is not None and float(matched[1]) <= 5


# The band holds the last twelve rows. Its panel is the left 38 columns, with the mode heading on
# its first row, the status on its fourth and the two trade buttons on its seventh.
BAND, PANEL = 12, 38


def panel(frame):
    """The panel's twelve rows, each 38 columns wide."""
    return [row[:PANEL].ljust(PANEL) for row in frame["lines"][-BAND:]]


def prop_ready(frame):
    """Prop on XMR is drawn and its status row is blank, so a click is taken."""
    rows = panel(frame)
    return len(rows) == BAND and rows[0].startswith("PROP XMR") and rows[3].strip() == ""


def trade_clicks(frame):
    """The terminal column and row, counted from one, at the middle of each trade button: the
    buy button first, then the sell button."""
    buttons = list(re.finditer(r"\[[^\]]*\]", panel(frame)[6]))
    assert len(buttons) == 2, "the panel's seventh row lacks its two trade buttons"
    row = len(frame["lines"]) - BAND + 6
    return [((button.start() + button.end()) // 2 + 1, row + 1) for button in buttons]


# The time axis row holds the caption in the left pane of 38 columns, then a tick at a round time
# with its hh:mm:ss label after it, and the outlook of 39 columns closes every row above the band.
def right_edge(frame):
    """The start second of the chart's last cell, read from every labelled tick of the time axis.

    A tick marks a round time inside its cell, so each reading is up to one cell late, and the
    readings of one row must agree within a cell."""
    width = max((len(row) for row in frame["lines"]), default=0)
    caption = r"c line (\d+)s/col(?: history)? $"
    rows = [row.ljust(width) for row in frame["lines"] if re.search(caption, row[:38])]
    if len(rows) != 1:
        return None
    per_cell = int(re.search(caption, rows[0][:38])[1])
    plot = rows[0][38:width - 39]
    edges = [(int(h) * 3600 + int(m) * 60 + int(s) + (len(plot) - 1 - match.start()) * per_cell) % 86400
             for match in re.finditer(r"\|(\d{2}):(\d{2}):(\d{2})(?![\d:])", plot)
             for h, m, s in [match.groups()]]
    if not edges or any(abs((edge - edges[0] + 43200) % 86400 - 43200) >= per_cell for edge in edges):
        return None
    return edges[0], per_cell


def history_matches(before, after, shift, max_seconds=10):
    elapsed = after["captured_ns"] - before["captured_ns"]
    if elapsed < 0 or elapsed > max_seconds * 1_000_000_000:
        return False
    old, new = right_edge(before), right_edge(after)
    if old is None or new is None or old[1] != new[1]:
        return False
    # Each reading may be up to one cell less one second late, so the allowance widens by that.
    slack = new[1] - 1
    minimum_progress = elapsed // 1_000_000_000 - 1 - slack
    maximum_progress = (elapsed + 999_999_999) // 1_000_000_000 + 1 + slack
    return minimum_progress <= (new[0] - old[0] + 43200) % 86400 - 43200 - shift <= maximum_progress


def percentile(values, percent):
    ordered = sorted(values)
    return ordered[max(0, (len(ordered) * percent + 99) // 100 - 1)]


def frame_gaps(frames, began, ended):
    assert frames[0] <= began <= ended <= frames[-1], "frames do not cover the click window"
    return [min(b, ended) - max(a, began) for a, b in zip(frames, frames[1:])
            if a < ended and b > began]


def validate_speed(speed):
    assert speed["clicks"] == speed["recorded_results"] == 600, speed
    assert 59800 <= speed["duration_ms"] <= 599000 / 9.5, "not a sustained ten-click-per-second run"
    assert speed["maximum_click_gap_ms"] <= 200, "click delivery stalled"
    assert speed["frame_gap_p95_ms"] <= 100, speed
    assert speed["frame_gap_max_ms"] <= 250, "a long freeze hid behind the frame percentile"
    assert 0 <= speed["maximum_sampled_result_backlog"] <= 30, "input results stalled"


# The prop margin wallet inside the state folder.
WALLET = Path("prop") / "margin-mainnet.json"


def same_wallet(after, before):
    """Two saves agree apart from an hourly funding charge, which lowers cash and raises funding
    by the same sum and changes nothing else."""
    def parts(wallet):
        rest = {key: value for key, value in wallet.items() if key not in ("cash", "funding")}
        return rest, Decimal(wallet["cash"]) + Decimal(wallet.get("funding", "0"))

    with localcontext() as context:
        context.prec = 50
        (rest_after, total_after), (rest_before, total_before) = parts(after), parts(before)
        return rest_after == rest_before and abs(total_after - total_before) < Decimal("1e-20")


def validate_wallet(saved, results, fixture_book, fixture_context):
    assert len(results) == 600 and all(r["kind"] == "order" for r in results)
    # WorkspaceApp serializes view capture and outcome collection with key handling.
    # A pending refusal therefore precedes the outstanding fill in this result log.
    # Restore those later refused clicks behind that fill before checking sides.
    ordered, pending = [], []
    for result in results:
        if result["text"] == "Order pending":
            pending.append(result)
        else:
            if pending:
                assert " filled " in result["text"], "pending refusals lack their outstanding fill"
            ordered.append(result)
            ordered.extend(pending)
            pending.clear()
    assert not pending, "pending refusals lack their outstanding fill"
    paused = "Opening paused for 2 s after closing"
    with localcontext() as context:
        context.prec = 50
        cash, held, basis, realized, fees = Decimal(10000), Decimal(0), Decimal(0), Decimal(0), Decimal(0)
        filled, refused, holdings = [], [], set()
        for index, result in enumerate(ordered):
            expected_side = "buy" if index % 2 == 0 else "sell"
            direction = 1 if expected_side == "buy" else -1
            # The click opens or grows when nothing is held or the holding is on the click's side.
            opening = held * direction >= 0
            receipt = result["text"]
            if " filled " not in receipt:
                assert receipt in ("Order pending", "Not sent: controls changed", paused), receipt
                # The pause after a close holds back opening presses only. A reducing press passes it.
                assert receipt != paused or opening, "the opening pause refused a reducing click"
                refused.append(receipt)
                continue
            # A fill names the money it put up at five times leverage, a fifth of its value in cents.
            match = re.fullmatch(r"PROP XMR (buy|sell) filled (\$[0-9.]+) at ([^\n]+)\nFee \$([0-9.]+) \| #(\d+) .+", receipt)
            assert match, "invalid fill receipt"
            side, dollars, shown, fee, count = match.groups()
            assert side == expected_side, "fill side differs from clicked button"
            fee = Decimal(fee)
            levels = fixture_book["levels"][1 if side == "buy" else 0]
            touch = Decimal(levels[0]["px"])
            requested = (Decimal(2500) / touch // Decimal("0.001")) * Decimal("0.001")
            if opening:
                quantity = requested
            else:
                # A reduce that would leave under ten dollars at the touch closes the whole position.
                quantity = abs(held) if (abs(held) - requested) * touch < 10 else requested
            assert quantity > 0, "wrong filled quantity"
            remaining, value = quantity, Decimal(0)
            for level in levels:
                take = min(remaining, Decimal(level["sz"]))
                value += take * Decimal(level["px"])
                remaining -= take
            assert remaining == 0, "fill exceeded known depth"
            # The fixture's fills name under a thousand dollars, so the text keeps both decimals.
            assert dollars == "$" + str((value / 5).quantize(Decimal("0.01"), rounding=ROUND_HALF_UP)), "dollars differ from the filled value at five times leverage"
            assert fee == value * Decimal("0.00045"), "fee differs from fixed-book filled value"
            approximate = shown.startswith("~")
            price = Decimal(shown.lstrip("~"))
            assert price > 0, "nonpositive displayed fill price"
            if approximate:
                unit = Decimal(10) ** price.as_tuple().exponent
                assert price * quantity <= value < (price + unit) * quantity, "wrong rounded fill price"
            else:
                assert price * quantity == value, "wrong exact fill price"
            if opening:
                basis += value
                cash -= fee
            else:
                # The reduced part takes its share of the sum paid for the open quantity.
                share = quantity * basis / abs(held)
                gain = (1 if held > 0 else -1) * (value - share)
                basis -= share
                realized += gain
                cash += gain - fee
            held += direction * quantity
            holdings.add(held)
            fees += fee
            filled.append(receipt)
            assert int(count) == len(filled), "fill sequence differs from saved count"
        assert len(filled) >= 40, "fewer than 40 clicks filled"
        assert int(saved["count"]) == len(filled) and saved["orders"] == filled, "saved count or history differs from the fills"
        deposit = Decimal(saved["deposit"])
        assert deposit == 10000, "starting deposit changed"
        # An hour boundary during the run may charge one hour on a position held after some fill:
        # the signed quantity times the served oracle times the served hourly rate, so a long pays
        # at a positive rate.
        funding = Decimal(saved.get("funding", "0"))
        oracle, rate = Decimal(fixture_context["ctx"]["oraclePx"]), Decimal(fixture_context["ctx"]["funding"])
        assert funding == 0 or funding in {position * oracle * rate for position in holdings}, "funding is neither zero nor one hourly payment on a position the run held"
        assert abs(Decimal(saved["cash"]) - (cash - funding)) < Decimal("1e-20"), "cash differs from independent fills, fees and funding"
        assert abs(Decimal(saved["realized"]) - realized) < Decimal("1e-20"), "realized differs from independent fills"
        assert Decimal(saved["fees"]) == fees, "fees differ from independent filled value"
        positions = saved["positions"]
        assert {coin: Decimal(p["quantity"]) for coin, p in positions.items()} == ({"XMR": held} if held else {}), "positions differ from independent fills"
        if held:
            assert abs(Decimal(positions["XMR"]["entry"]) - basis / abs(held)) < Decimal("1e-20"), "entry differs from independent fills"
        assert abs(Decimal(saved["cash"]) - (deposit + Decimal(saved["realized"]) - Decimal(saved["fees"]) - funding)) < Decimal("1e-20"), "cash differs from deposit, realized, fees and funding"
        assert Decimal(saved["cash"]) >= 0, "saved cash is below zero"
    return filled, refused


def validate_result(result, mode):
    assert result["forbidden"] == [], result["forbidden"]
    assert result["metadata_calls"] > 0, "the offline worker was not exercised"
    assert result["polymarket_calls"] > 0, "the Polymarket reader did not run"
    assert result["polymarket_calls"] >= 2, "the Polymarket reader read no midpoints"
    assert result["polymarket_failures"] == 0, "a Polymarket reply was refused"
    assert not result["polymarket_reader_alive"], "the Polymarket reader outlived the quit"
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
        self.clicks = []
        self.receipts, self.progress = {}, []
        self.receipts_at = 0
        self.raw, self.control_pending = bytearray(), b""
        self.control, self.eof = None, False
        self.temporary = tempfile.TemporaryDirectory(prefix="desk-playback-")
        private = Path(self.temporary.name)
        self.state_directory = private / "state"
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
             str(private / "control"), str(Path(args.fixtures).resolve()), str(settings)]
             + (["speed"] if mode == "speed" else []),
            stdin=self.slave, stdout=self.slave, stderr=self.stderr,
            preexec_fn=terminal_session,
        )

    def pump(self, timeout):
        if self.mode == "speed" and time.monotonic() >= self.receipts_at:
            self.read_receipts()
            self.receipts_at = time.monotonic() + 1
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

    def read_receipts(self):
        path = self.state_directory / "order-results.jsonl"
        if path.exists():
            for line in path.read_text(errors="replace").splitlines(keepends=True):
                if not line.endswith("\n"):
                    break  # A concurrent append has not completed yet.
                try:
                    value = json.loads(line)
                except json.JSONDecodeError:
                    continue  # A previous append may have torn before its newline.
                key = value["id"]
                assert key not in self.receipts or self.receipts[key] == value, "receipt changed"
                self.receipts[key] = value
        if self.clicks:
            count = len(self.receipts)
            self.progress.append({"at_ns": time.monotonic_ns(), "clicks": len(self.clicks),
                                  "results": count, "backlog": len(self.clicks) - count})

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

    def settle(self):
        settled_at = time.monotonic_ns() + 1_000_000_000
        next_quote = 0
        while time.monotonic_ns() < settled_at:
            if time.monotonic_ns() >= next_quote:
                self.command("quote", coin="XMR")
                next_quote = time.monotonic_ns() + 100_000_000
            self.pump(max(0, min(next_quote, settled_at) - time.monotonic_ns()) / 1e9)

    def run(self):
        def text(frame):
            return "\n".join(frame["lines"])

        def market(coin):
            return lambda frame: market_matches(frame, coin, "522.26")

        def contains(value):
            return lambda frame: value in text(frame)

        self.event("connected")
        self.command("quote", coin="XMR")
        self.frame(market("XMR"))
        assert not any(" bad " in f["lines"][0] for f in self.frames.complete), "recorded input was rejected"
        if self.mode == "normal":
            # The synthetic Polymarket replies make the reader reachable, so the band names its coin.
            self.frame(contains("Polymarket BTC up%  100"))
            self.key("zoom out", ESC + b"[B", contains("4s/col"))
            self.key("zoom in", ESC + b"[A", contains("2s/col"))
            self.key("candles", b"c", contains("1m/candle"))
            self.key("line", b"c", contains("2s/col"))
            before = self.frames.complete[-1]
            # The scrub step is a quarter of the 46 one second dots in the plot of 23 cells at 100
            # columns, rounded down.
            step = 11
            history = self.key("history back", ESC + b"[D", lambda f: history_matches(before, f, -step))
            self.key("history forward", ESC + b"[C",
                     lambda f: history_matches(history, f, step) and history_matches(before, f, 0, 20))
            self.key("open market prompt", b"m", contains("market:"))
            self.key("cancel market prompt with lone Escape", ESC, market("XMR"))
            pasted = ESC + b"[200~qmETH\r1234567890" + ESC + b"[201~c"
            frame = self.key("discard pasted actions", pasted, contains("1m/candle"))
            assert market("XMR")(frame), "pasted input changed the market"
            self.action("minimum size guard", lambda: self.resize(99, 29),
                        contains("need 100x30, have 99x29"))
            wide = self.action("wide size", lambda: self.resize(120, 40),
                               lambda f: len(f["lines"]) == 40 and market("XMR")(f))
            assert all(len(row) == 120 for row in wide["lines"])
        elif self.mode == "speed":
            places = trade_clicks(self.frame(prop_ready))
            clicks = self.clicks
            began = time.monotonic_ns()
            for index in range(600):
                deadline = began + index * 100_000_000
                while time.monotonic_ns() < deadline:
                    self.pump(max(0, min(0.01, (deadline - time.monotonic_ns()) / 1e9)))
                self.command("quote", coin="XMR")
                x, y = places[index % 2]
                clicks.append(time.monotonic_ns())
                self.write(ESC + f"[<0;{x};{y}M".encode() + ESC + f"[<0;{x};{y}m".encode())
                self.pump(0)
            def recorded():
                self.read_receipts()
                return len(self.receipts) >= 600
            self.wait(recorded, "all six hundred results saved")
            assert len(self.receipts) == 600, "missing or duplicate result"
            results = list(self.receipts.values())
            saved = json.loads((self.state_directory / WALLET).read_text())
            fixture_book = json.loads((Path(self.args.fixtures) / "l2Book.json").read_text())["data"]
            fixture_context = json.loads((Path(self.args.fixtures) / "activeAssetCtx.json").read_text())["data"]
            filled, refused = validate_wallet(saved, results, fixture_book, fixture_context)
            results_saved_ms = (time.monotonic_ns() - clicks[-1]) / 1e6
            self.settle()
            self.read_receipts()
            assert len(self.receipts) == 600, "unexpected later result"
            settled = json.loads((self.state_directory / WALLET).read_text())
            assert same_wallet(settled, saved), "a refused click executed later"
            gaps = frame_gaps([f["captured_ns"] for f in self.frames.complete], clicks[0], clicks[-1])
            backlog = max(p["backlog"] for p in self.progress)
            self.speed = {"clicks": 600, "recorded_results": len(results),
                          "saved_orders": int(saved["count"]), "refused": len(refused),
                          "filled_percent": len(filled) * 100 / 600,
                          "refused_percent": len(refused) * 100 / 600,
                          "duration_ms": (clicks[-1] - clicks[0]) / 1e6,
                          "maximum_click_gap_ms": max(b - a for a, b in zip(clicks, clicks[1:])) / 1e6,
                          "frame_gap_p50_ms": percentile(gaps, 50) / 1e6,
                          "frame_gap_p95_ms": percentile(gaps, 95) / 1e6,
                          "frame_gap_p99_ms": percentile(gaps, 99) / 1e6,
                          "frame_gap_max_ms": max(gaps) / 1e6,
                          "time_in_gaps_over_100ms": sum(g for g in gaps if g > 100_000_000) / 1e6,
                          "maximum_sampled_result_backlog": backlog,
                          "final_results_saved_ms": results_saved_ms,
                          "settle_check_ms": 1000}
            validate_speed(self.speed)
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
                     lambda f: market("ETH")(f) and contains("4s/col")(f))
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
            frame = self.frame(contains("10s/col"), after)
            self.actions.append({"label": "drain held queue", "write_started_ns": started,
                                 "write_completed_ns": arrived, "frame_completed_ns": frame["captured_ns"],
                                 "arrival_to_frame_ns": frame["captured_ns"] - arrived,
                                 "write_duration_ns": arrived - started})
        elif self.mode == "selection":
            rejected = self.key("unknown market keeps chart", b"mmcchhhhaa1pp\r",
                                contains("Unknown market"))
            assert "XMR" in rejected["lines"][0]
            assert any("VIEW XMR" in row for row in rejected["lines"])
            self.key("open another selection", b"m", contains("market:"))
            self.key("cancel selection", ESC, market("XMR"))
            self.write(b"mETH\r")
            self.frame(contains("VIEW ETH"))
            self.command("quote", coin="ETH")
            self.frame(market("ETH"))
            self.command("arm", work="selection")
            self.event("armed")
            self.write(b"mBTC\r")
            self.event("work-held")
            self.frame(contains("Checking BTC"))
            self.key("cancel held selection", ESC, market("ETH"))
            self.key("navigate after cancellation", ESC + b"[B",
                     lambda f: market("ETH")(f) and contains("4s/col")(f))
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
        if self.mode == "speed":
            self.read_receipts()
            assert len(self.receipts) == 600, "unexpected result during shutdown"
            closed = json.loads((self.state_directory / WALLET).read_text())
            assert same_wallet(closed, saved), "ledger changed during shutdown"
        samples = validate_result(result, self.mode)
        assert any(any(0x2800 <= ord(c) <= 0x28FF for c in line)
                   for f in self.frames.complete for line in f["lines"]), "no recorded braille output"
        assert not any("PAXG (token tied to gold)" in text(f) for f in self.frames.complete), "a reference row was drawn"
        cpu_after = resource.getrusage(resource.RUSAGE_CHILDREN)
        ages = [int(s["age_ns"]) for s in samples]
        key_ages = [int(s["age_ns"]) for s in samples if s["event"].startswith("key ")]
        feed_ages = [int(s["age_ns"]) for s in samples if s["method"] == "offer"]
        return {"mode": self.mode, "status": status, "frames": len(self.frames.complete),
                "speed": getattr(self, "speed", None),
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
            if self.mode == "speed":
                self.read_receipts()
                (self.directory / "receipts.json").write_text(json.dumps(self.receipts, indent=2) + "\n")
                (self.directory / "progress.json").write_text(json.dumps(self.progress, indent=2) + "\n")
                (self.directory / "clicks.json").write_text(json.dumps(self.clicks) + "\n")
                saved = self.state_directory / WALLET
                if saved.exists():
                    (self.directory / "simulated-ledger.json").write_bytes(saved.read_bytes())
                records = self.state_directory / "diagnostics" / "events-0.jsonl"
                if records.exists():
                    (self.directory / "diagnostics.jsonl").write_bytes(records.read_bytes())
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
    parser.add_argument("--mode", choices=("all", "normal", "send", "history", "queue", "selection", "speed"), default="all")
    parser.add_argument("--repeats", type=int, default=1)
    args = parser.parse_args()
    assert args.repeats > 0
    output = Path(args.output)
    output.mkdir(parents=True, exist_ok=False)
    results = []
    modes = ("normal", "send", "history", "queue", "selection") if args.mode == "all" else (args.mode,)
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
                "source_sha256": source_hashes}, indent=2) + "\n")


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
