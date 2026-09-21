import importlib.util
import os
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
            self.assertTrue(playback.target_matches(selected, "workspace", "XMR", "buy", "522.28", "522.26"))
            self.assertFalse(playback.target_matches(selected, "workspace", "XMR", "buy", "522.29", "522.26"))
            moved = {"lines": selected["lines"].copy()}
            moved["lines"][row - 1], moved["lines"][row] = moved["lines"][row], moved["lines"][row - 1]
            self.assertFalse(playback.target_matches(moved, "workspace", "XMR", "buy", "522.28", "522.26"))
            self.assertFalse(playback.idle_matches(selected, "workspace", "XMR", "522.26"))
            self.assertTrue(playback.idle_matches(shown("Trade buttons open a review"), "workspace", "XMR", "522.26"))

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
                "hold_finished": False, "hold_released": False,
                "queue": {"samples": [{"event": e, "age_ns": "1",
                    "method": "offer" if e.startswith("quote ") else "put"} for e in events]}}
        playback.validate_result(good, "send")
        for field, value in (("forbidden", ["submission"]), ("metadata_calls", 0),
                             ("held_work", False), ("hold_finished", True), ("hold_released", True)):
            with self.subTest(field=field):
                with self.assertRaises(AssertionError):
                    playback.validate_result({**good, field: value}, "send")
        with self.assertRaises(AssertionError):
            playback.validate_result({**good, "queue": {"samples": []}}, "send")


    def test_shutdown_can_interrupt_held_work_only_after_quit(self):
        events = ("key Quit", "key PromptSubmit", "key ZoomOut", "quote ETH")
        result = {"forbidden": [], "metadata_calls": 1, "held_work": True,
                  "hold_finished": True, "hold_finished_ns": "11", "hold_released": False,
                  "queue": {"samples": [{"event": event, "age_ns": "1", "entered_ns": "10",
                      "method": "offer" if event.startswith("quote ") else "put"} for event in events]}}
        playback.validate_result(result, "send")
        for ended in ("0", "9"):
            with self.assertRaises(AssertionError):
                playback.validate_result({**result, "hold_finished_ns": ended}, "send")

    @staticmethod
    def displays():
        # Header and footer rows extracted from the retained original and contextual frames.
        return (
            ('original', '522.26', '522.28', {"lines": [
                'XMR  522.26  mark 522.28  orc 521.515  24h -1.4%  fund 0.0130%  OI 210.2k  vol 31.2M  live 0.9s',
                'Hard 1 buy $500 2 buy $1000 3 sell $500 4 sell $1000 | position unavailable',
                'Best 5 buy $499.29 6 buy $998.59 7 sell $499.78 8 sell $999.53',
                'Target 9 buy 0 sell | buy $522.28 request 1.914 estimate 1.914 value $~999.6 average $522.28',
                'unavailable | View only',
            ]}),
            ('contextual', '528.045', '528.2', {"lines": [
                'connection active 0.3s  XMR  mid 528.045  mark 527.78  oracle 526.393  24h +3.2%  fund 0.0214%',
                'Target XMR buy limit $528.2 | Enter sends Escape cancels',
                'request 0.946 estimate 0.946 value $~499.6 average $528.2 | up/down price',
                'position long 1 | target estimate long 1.946 | 1-8 and left/right inactive',
                'ready',
            ]}),
        )

    def test_header_pins_the_display_contract_market_and_midpoint_field(self):
        for contract, midpoint, _, frame in self.displays():
            with self.subTest(contract=contract):
                self.assertTrue(playback.market_matches(frame, contract, "XMR", midpoint))
                other = "contextual" if contract == "original" else "original"
                self.assertFalse(playback.market_matches(frame, other, "XMR", midpoint))
                self.assertFalse(playback.market_matches(frame, contract, "ETH", midpoint))
                self.assertFalse(playback.market_matches(frame, contract, "XMR", midpoint + "0"))
                for price in (midpoint + "1", midpoint.replace(".", "x")):
                    changed = frame["lines"][0].replace(midpoint, price, 1)
                    self.assertFalse(playback.market_matches({"lines": [changed]}, contract, "XMR", midpoint))
                wrong = frame["lines"][0].replace(midpoint, "999.99", 1)
                wrong = re.sub(r"mark \S+", "mark " + midpoint, wrong)
                self.assertFalse(playback.market_matches({"lines": [wrong]}, contract, "XMR", midpoint))

    def test_displayed_recency_requires_finite_active_age_and_no_bad_field(self):
        for contract, midpoint, _, frame in self.displays():
            header = frame["lines"][0]
            for age, expected in (("0.0", True), ("5.0", True), ("5.1", False),
                                  ("-0.1", False), ("NaN", False), ("1", False), ("1.00", False)):
                changed = re.sub(r"\d+\.\ds", age + "s", header)
                with self.subTest(contract=contract, age=age):
                    self.assertEqual(playback.market_matches({"lines": [changed]}, contract, "XMR", midpoint), expected)
            label = "live" if contract == "original" else "active"
            for changed in (header.replace(label, "stale"), header.replace(label, "disconnected"),
                            header + " bad 1", re.sub(r"(\d+\.\ds)", r"\1 bad 1", header, count=1),
                            header.replace(label, "")):
                self.assertFalse(playback.market_matches({"lines": [changed]}, contract, "XMR", midpoint))
            self.assertFalse(playback.market_matches({"lines": []}, contract, "XMR", midpoint))

    def test_target_pins_market_side_exact_limit_and_footer_location(self):
        for contract, midpoint, limit, frame in self.displays():
            with self.subTest(contract=contract):
                self.assertTrue(playback.target_matches(frame, contract, "XMR", "buy", limit, midpoint, "sends"))
                for coin, side, price in (("ETH", "buy", limit), ("XMR", "sell", limit),
                                           ("XMR", "buy", limit + "0"), ("XMR", "buy", limit.replace(".", "x"))):
                    self.assertFalse(playback.target_matches(frame, contract, coin, side, price, midpoint, "sends"))
                row = -2 if contract == "original" else -4
                for price in (limit + "1", limit.replace(".", "x")):
                    changed = {"lines": [line.replace("$" + limit, "$" + price) for line in frame["lines"]]}
                    self.assertFalse(playback.target_matches(changed, contract, "XMR", "buy", limit, midpoint, "sends"))
                wrong = frame["lines"].copy()
                wrong[row] = wrong[row].replace("$" + limit, "$999.99", 1)
                self.assertIn("$" + limit, " ".join(wrong))
                self.assertFalse(playback.target_matches({"lines": wrong}, contract, "XMR", "buy", limit, midpoint, "sends"))
                moved = frame["lines"].copy()
                moved[row], moved[-1] = moved[-1], moved[row]
                self.assertFalse(playback.target_matches({"lines": moved}, contract, "XMR", "buy", limit, midpoint, "sends"))

    def test_ready_and_viewing_contextual_enter_fields_remain_distinct(self):
        contract, midpoint, limit, frame = self.displays()[1]
        self.assertFalse(playback.target_matches(frame, contract, "XMR", "buy", limit, midpoint))
        viewing = {"lines": [row.replace("Enter sends", "Enter disabled") for row in frame["lines"]]}
        viewing["lines"][-1] = "view only"
        self.assertTrue(playback.target_matches(viewing, contract, "XMR", "buy", limit, midpoint))
        self.assertFalse(playback.target_matches(viewing, contract, "XMR", "buy", limit, midpoint, "sends"))
        viewing["lines"][-4] = viewing["lines"][-4].replace(" Escape cancels", "")
        self.assertFalse(playback.target_matches(viewing, contract, "XMR", "buy", limit, midpoint))

    def test_cancellation_requires_the_ordinary_controls_and_action_rows(self):
        controls = {
            "original": "Target 9 buy 0 sell | arrows set price enter sends escape cancels",
            "contextual": "Target 9 buy 0 sell | up/down zoom left/right history m market c chart q quit",
        }
        for contract, midpoint, limit, frame in self.displays():
            self.assertFalse(playback.idle_matches(frame, contract, "XMR", midpoint))
            still_selected = {"lines": [row.replace("$" + limit, "$999.99") for row in frame["lines"]]}
            self.assertFalse(playback.idle_matches(still_selected, contract, "XMR", midpoint))
            idle = {"lines": [frame["lines"][0], "Hard 1 buy $500", "Best 5 buy --", controls[contract], "view only"]}
            self.assertTrue(playback.idle_matches(idle, contract, "XMR", midpoint))
            self.assertFalse(playback.idle_matches(idle, contract, "ETH", midpoint))
            idle["lines"][-4] = "Target XMR buy limit $" + limit
            self.assertFalse(playback.idle_matches(idle, contract, "XMR", midpoint))
            idle["lines"][-2] = ""
            self.assertFalse(playback.idle_matches(idle, contract, "XMR", midpoint))

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
