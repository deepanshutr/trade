import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
import playback


class WorkspaceCaptureSpec(unittest.TestCase):
    def clock(self, second, elapsed):
        second %= 86400
        value = f"{second // 3600:02}:{second // 60 % 60:02}:{second % 60:02}"
        return {"captured_ns": elapsed * 1_000_000_000, "lines": ["c line 1s/col       " + value + " |"]}

    def test_narrow_history_requires_a_whole_clock_and_real_shift(self):
        before = self.clock(2, 0)
        for elapsed in range(11):
            for shift in (-7, 7):
                with self.subTest(elapsed=elapsed, shift=shift):
                    self.assertTrue(playback.history_matches(before, self.clock(2 + elapsed + shift, elapsed), shift, clock_count=1))
                    self.assertFalse(playback.history_matches(before, self.clock(2 + elapsed, elapsed), shift, clock_count=1))
        corrupt = {"captured_ns": 0, "lines": ["c line 1s/col 00:00:00:00:02"]}
        self.assertFalse(playback.history_matches(before, corrupt, 0, clock_count=1))

    def frame(self):
        lines = [" " * 53 + "|" + " " * 46 for _ in range(30)]
        lines[0] = "connection active 0.3s  XMR  mid 522.26".ljust(53) + "|" + "VIEW XMR".ljust(46)
        lines[24] = " " * 53 + "|" + "Target buy 522.28 Up/Down".ljust(46)
        return {"lines": lines}

    def test_workspace_pins_exact_market_and_midpoint(self):
        frame = self.frame()
        self.assertTrue(playback.market_matches(frame, "XMR", "522.26"))
        self.assertFalse(playback.market_matches(frame, "XMR", "522.260"))

    def test_workspace_header_requires_a_fresh_exact_market(self):
        frame = self.frame()
        header = frame["lines"][0]
        for age, expected in (("0.0", True), ("5.0", True), ("5.1", False),
                              ("-0.1", False), ("NaN", False), ("1", False), ("1.00", False)):
            frame["lines"][0] = header.replace("0.3s", age + "s")
            self.assertEqual(playback.market_matches(frame, "XMR", "522.26"), expected)
        for changed in (header.replace("active", "disconnected"), header.replace("XMR", "ETH"),
                        header.replace("522.26", "522x26"), header.replace(" |", " bad 1 |")):
            frame["lines"][0] = changed
            self.assertFalse(playback.market_matches(frame, "XMR", "522.26"))
        self.assertFalse(playback.market_matches({"lines": []}, "XMR", "522.26"))

if __name__ == "__main__":
    unittest.main()
