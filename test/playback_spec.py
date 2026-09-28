import importlib.util
import os
import json
import tempfile
import re
import signal
import subprocess
import sys
import time
from pathlib import Path
import unittest
from unittest import mock

spec = importlib.util.spec_from_file_location("playback", Path("tools/playback.py"))
playback = importlib.util.module_from_spec(spec)
spec.loader.exec_module(playback)


class PlaybackSpec(unittest.TestCase):
    def test_changed_sleep_text_does_not_block_playback_setup(self):
        repository = Path.cwd()
        with tempfile.TemporaryDirectory() as root:
            private = Path(root)
            (private / "src").mkdir()
            for name in ("WorkspaceApp.scala", "Main.scala"):
                source = (repository / "src" / name).read_text()
                (private / "src" / name).write_text(source.replace("Thread.sleep(25L)", "Thread.sleep(20L)"))
            output = private / "capture"
            argv = ["playback.py", "--java", "unused", "--classpath", "unused",
                    "--fixtures", str(repository / "fixtures"), "--output", str(output), "--mode", "normal"]
            try:
                os.chdir(private)
                with mock.patch.object(sys, "argv", argv), mock.patch.object(playback, "Playback") as driver:
                    driver.return_value.run.return_value = {}
                    playback.main()
                    driver.return_value.run.assert_called_once()
                    driver.return_value.close.assert_called_once()
            finally:
                os.chdir(repository)
            summary = json.loads((output / "summary.json").read_text())
            self.assertNotIn("built_in_wait_ms", summary)

    def test_removed_display_contract_is_rejected_by_argument_parser(self):
        with tempfile.TemporaryDirectory() as root:
            result = subprocess.run([sys.executable, "tools/playback.py", "--java", "unused",
                                     "--classpath", "unused", "--output", str(Path(root) / "capture"),
                                     "--display-contract", "original"], capture_output=True, text=True)
            self.assertEqual(result.returncode, 2, result.stderr)
            self.assertIn("unrecognized arguments: --display-contract original", result.stderr)

    def test_append_only_receipts_keep_all_600_and_ignore_an_incomplete_tail(self):
        with tempfile.TemporaryDirectory() as root:
            run = object.__new__(playback.Playback)
            run.state_directory = Path(root)
            run.receipts, run.clicks = {}, []
            path = Path(root) / "order-results.jsonl"
            records = [{"id": str(i), "kind": "order", "text": "Order pending"} for i in range(600)]
            path.write_text("".join(json.dumps(r) + "\n" for r in records) + '{"id":')
            run.read_receipts()
            self.assertEqual(list(run.receipts.values()), records)
            run.read_receipts()
            self.assertEqual(len(run.receipts), 600)

    def test_receipts_skip_torn_lines_and_keep_later_records(self):
        with tempfile.TemporaryDirectory() as root:
            run = object.__new__(playback.Playback)
            run.state_directory = Path(root)
            run.receipts, run.clicks = {}, []
            record = {"id": "saved", "kind": "order", "text": "Order pending"}
            path = Path(root) / "order-results.jsonl"
            path.write_bytes(b'{"id":"torn\n' + json.dumps(record).encode() + b'\n{"id":')
            run.read_receipts()
            self.assertEqual(list(run.receipts.values()), [record])
            path.write_bytes(b'{"text":"\xe2\n' + json.dumps(record).encode() + b'\n')
            run.read_receipts()
            self.assertEqual(list(run.receipts.values()), [record])

    def test_settling_keeps_quotes_paced_even_when_output_is_always_ready(self):
        run = object.__new__(playback.Playback)
        clock = [0.0]
        quotes = []
        pumps = []

        def command(name, **values):
            self.assertEqual((name, values), ("quote", {"coin": "XMR"}))
            quotes.append(clock[0])

        def pump(timeout):
            self.assertGreater(timeout, 0)
            pumps.append(timeout)
            clock[0] += min(timeout, 0.001)

        run.command, run.pump = command, pump
        with mock.patch.object(playback.time, "monotonic", side_effect=lambda: clock[0]), \
                mock.patch.object(playback.time, "monotonic_ns", side_effect=lambda: round(clock[0] * 1e9)):
            run.settle()
        self.assertAlmostEqual(clock[0], 1.0)
        self.assertEqual(len(quotes), 10)
        self.assertGreater(len(pumps), 900, "output must continue draining between quotes")
        self.assertTrue(all(b - a >= 0.099 for a, b in zip(quotes, quotes[1:])))

    def test_workspace_target_and_cancel_are_exact_below_the_price_columns(self):
        for width, height in ((100, 30), (120, 40), (160, 45)):
            divider = min(width * 3 // 5, width - 47)
            rows = [" " * width for _ in range(height)]
            rows[0] = "connection active 0.1s XMR mid 522.26".ljust(divider) + "|" + " " * (width - divider - 1)
            row = height - 6
            def shown(text):
                frame = rows.copy()
                frame[row] = " " * divider + "|" + text.ljust(width - divider - 1)
                return {"lines": frame}
            selected = shown("Target buy 522.28 Up/Down")
            self.assertTrue(playback.target_matches(selected, "XMR", "buy", "522.28", "522.26"))
            self.assertFalse(playback.target_matches(selected, "XMR", "buy", "522.29", "522.26"))
            moved = {"lines": selected["lines"].copy()}
            moved["lines"][row - 1], moved["lines"][row] = moved["lines"][row], moved["lines"][row - 1]
            self.assertFalse(playback.target_matches(moved, "XMR", "buy", "522.28", "522.26"))
            self.assertFalse(playback.idle_matches(selected, "XMR", "522.26"))
            idle = shown("")
            idle["lines"][height - 8] = " " * divider + "|" + "| 9 BUY target | | 0 SELL target |".ljust(width - divider - 1)
            self.assertTrue(playback.idle_matches(idle, "XMR", "522.26"))
            self.assertFalse(playback.idle_matches(shown(""), "XMR", "522.26"))

    def test_frame_is_complete_only_after_the_final_clear(self):
        stream = playback.Frames()
        payload = playback.START + "XMR \u2801".encode() + playback.CLEAR + playback.END
        for byte in payload[:-1]:
            stream.feed(bytes([byte]), 10)
            self.assertEqual(stream.complete, [])
        stream.feed(payload[-1:], 11)
        self.assertEqual(stream.complete, [{"captured_ns": 11, "lines": ["XMR \u2801"]}])

    def test_repeated_frames_and_style_bytes_are_separated(self):
        frame = playback.START + playback.ESC + b"[1mXMR" + playback.ESC + b"[0m" + playback.CLEAR + playback.END
        stream = playback.Frames()
        stream.feed(b"before" + frame + frame, 12)
        self.assertEqual([f["lines"] for f in stream.complete], [["XMR"], ["XMR"]])

    def test_bad_encoding_and_missing_row_clear_are_failures(self):
        for body in (b"XMR", b"\xff" + playback.CLEAR):
            with self.subTest(body=body):
                with self.assertRaises((AssertionError, UnicodeDecodeError)):
                    playback.Frames().feed(playback.START + body + playback.END, 10)

    def test_cleanup_stops_a_child_after_its_wrapper_has_exited(self):
        grandchild = "import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); print('ready', flush=True); time.sleep(60)"
        wrapper = "import subprocess,sys; child=subprocess.Popen([sys.executable, '-c', sys.argv[1]], stdout=subprocess.PIPE); child.stdout.readline(); print(child.pid, flush=True)"
        child = subprocess.Popen([sys.executable, "-c", wrapper, grandchild],
                                 stdout=subprocess.PIPE, start_new_session=True, text=True)
        try:
            descendant = int(child.stdout.readline())
            child.wait(timeout=3)
            os.kill(descendant, 0)
            playback.stop_group(child, time.sleep)
            with self.assertRaises(ProcessLookupError):
                os.kill(descendant, 0)
        finally:
            try:
                os.killpg(child.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            child.wait(timeout=3)
            child.stdout.close()

    def cleanup_denial(self, denied, expected=None):
        code = "import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); print('ready', flush=True); time.sleep(60)"
        child = subprocess.Popen([sys.executable, "-c", code], stdout=subprocess.PIPE,
                                 start_new_session=True, text=True)
        real_signal = os.killpg
        calls = []

        def controlled_signal(group, value):
            self.assertEqual(group, child.pid)
            deny = (value == 0 and denied == "persistent") or value == denied
            if value == 0 and denied == "transient":
                deny = not any(v == 0 and result == "denied" for v, result in calls)
            if deny:
                calls.append((value, "denied"))
                raise PermissionError("controlled signal denial")
            try:
                result = real_signal(group, value)
            except ProcessLookupError:
                calls.append((value, "absent"))
                raise
            calls.append((value, "present" if value == 0 else "sent"))
            return result

        try:
            self.assertEqual(child.stdout.readline().strip(), "ready")
            self.assertEqual(os.getpgid(child.pid), child.pid)
            with mock.patch.object(playback.os, "killpg", side_effect=controlled_signal):
                if expected is None:
                    playback.stop_group(child, time.sleep)
                else:
                    with self.assertRaises(expected):
                        playback.stop_group(child, time.sleep)
            if denied == signal.SIGKILL:
                real_signal(child.pid, 0)
            else:
                with self.assertRaises(ProcessLookupError):
                    real_signal(child.pid, 0)
            return calls
        finally:
            try:
                real_signal(child.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            child.wait(timeout=3)
            child.stdout.close()
            with self.assertRaises(ProcessLookupError):
                real_signal(child.pid, 0)

    def test_cleanup_retries_a_transient_denied_probe_until_absence_is_observed(self):
        calls = self.cleanup_denial("transient")
        self.assertEqual(calls.count((0, "denied")), 1)
        self.assertIn((signal.SIGKILL, "sent"), calls)
        self.assertEqual(calls[-1], (0, "absent"))

    def test_cleanup_cannot_claim_absence_when_every_probe_is_denied(self):
        calls = self.cleanup_denial("persistent", AssertionError)
        self.assertGreater(calls.count((0, "denied")), 1)
        self.assertIn((signal.SIGKILL, "sent"), calls)
        self.assertNotIn((0, "absent"), calls)

    def test_cleanup_does_not_hide_denied_termination_signals(self):
        for denied in (signal.SIGTERM, signal.SIGKILL):
            with self.subTest(signal=denied):
                calls = self.cleanup_denial(denied, PermissionError)
                self.assertIn((denied, "denied"), calls)

    @staticmethod
    def clock_frame(first, captured_ns):
        values = [(first + offset) % 86400 for offset in (0, 61, 121)]
        row = "c line 1s/col " + " ".join(f"{v // 3600:02}:{v // 60 % 60:02}:{v % 60:02}" for v in values)
        return {"captured_ns": captured_ns, "lines": [row]}

    def test_history_moves_back_and_returns_across_midnight(self):
        before = self.clock_frame(5, 0)
        history = self.clock_frame(-24, 100_000_000)
        returned = self.clock_frame(6, 200_000_000)
        self.assertTrue(playback.history_matches(before, history, -30))
        self.assertTrue(playback.history_matches(history, returned, 30))
        self.assertTrue(playback.history_matches(before, returned, 0, 20))
        self.assertFalse(playback.history_matches(before, history, 0, 20))

    def test_ordinary_clock_progress_cannot_replace_either_history_key(self):
        before = self.clock_frame(86398, 0)
        for elapsed in range(11):
            after = self.clock_frame(86398 + elapsed, elapsed * 1_000_000_000)
            for shift in (-30, 30):
                with self.subTest(elapsed=elapsed, shift=shift):
                    self.assertFalse(playback.history_matches(before, after, shift))

    def test_history_requires_three_clocks_and_bounds_the_elapsed_allowance(self):
        before = self.clock_frame(100, 0)
        good = self.clock_frame(70, 100_000_000)
        wrong = {**good, "lines": [good["lines"][0].replace("00:02:11", "00:02:41")]}
        self.assertFalse(playback.history_matches(before, wrong, -30))
        self.assertFalse(playback.history_matches(before, {**good, "lines": ["c line 1s/col 00:01:10"]}, -30))
        self.assertFalse(playback.history_matches(before, self.clock_frame(70, 11_000_000_000), -30))

    def test_nonblocking_claim_needs_held_work_at_quit_and_a_real_queue_trace(self):
        events = ("key Quit", "key PromptSubmit", "key ZoomOut", "quote ETH")
        good = {"forbidden": [], "metadata_calls": 1, "held_work": True,
                "hold_finished": False,
                "queue": {"samples": [{"event": e, "age_ns": "1",
                    "method": "offer" if e.startswith("quote ") else "put"} for e in events]}}
        playback.validate_result(good, "send")
        for field, value in (("forbidden", ["submission"]), ("metadata_calls", 0),
                             ("held_work", False), ("hold_finished", True)):
            with self.subTest(field=field):
                with self.assertRaises(AssertionError):
                    playback.validate_result({**good, field: value}, "send")
        with self.assertRaises(AssertionError):
            playback.validate_result({**good, "queue": {"samples": []}}, "send")


    def test_shutdown_can_interrupt_held_work_only_after_quit(self):
        events = ("key Quit", "key PromptSubmit", "key ZoomOut", "quote ETH")
        result = {"forbidden": [], "metadata_calls": 1, "held_work": True,
                  "hold_finished": True, "hold_finished_ns": "11",
                  "queue": {"samples": [{"event": event, "age_ns": "1", "entered_ns": "10",
                      "method": "offer" if event.startswith("quote ") else "put"} for event in events]}}
        playback.validate_result(result, "send")
        for ended in ("0", "9"):
            with self.assertRaises(AssertionError):
                playback.validate_result({**result, "hold_finished_ns": ended}, "send")

    def test_normal_result_requires_both_scrub_commands(self):
        keys = ("key Quit", "key ScrubBack", "key ScrubForward")
        good = {"forbidden": [], "metadata_calls": 1,
                "queue": {"samples": [{"event": key, "age_ns": "1", "method": "put"} for key in keys]}}
        playback.validate_result(good, "normal")
        for omitted in keys[1:]:
            rows = [sample for sample in good["queue"]["samples"] if sample["event"] != omitted]
            with self.assertRaises(AssertionError):
                playback.validate_result({**good, "queue": {"samples": rows}}, "normal")

if __name__ == "__main__":
    unittest.main()
