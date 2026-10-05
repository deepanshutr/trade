import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
import playback


class WorkspaceCaptureSpec(unittest.TestCase):
    # The time axis of a 100 column frame: the caption in 38 columns, 23 cells at two seconds a
    # cell with one labelled tick at cell 0, and the outlook. The last cell starts at second.
    def axis(self, plot):
        return "c line 2s/col".rjust(37) + " " + plot.ljust(23) + "|" + " " * 38

    def clock(self, second, elapsed):
        value = (second - 44) % 86400
        label = f"|{value // 3600:02}:{value // 60 % 60:02}:{value % 60:02}"
        return {"captured_ns": elapsed * 1_000_000_000, "lines": [self.axis(label)]}

    def test_narrow_history_requires_a_whole_clock_and_real_shift(self):
        before = self.clock(2, 0)
        for elapsed in range(11):
            for shift in (-7, 7):
                with self.subTest(elapsed=elapsed, shift=shift):
                    self.assertTrue(playback.history_matches(before, self.clock(2 + elapsed + shift, elapsed), shift))
                    self.assertFalse(playback.history_matches(before, self.clock(2 + elapsed, elapsed), shift))
        corrupt = {"captured_ns": 0, "lines": [self.axis("|00:00:00:00:02")]}
        self.assertFalse(playback.history_matches(before, corrupt, 0))

    # A frame as Desk draws it: above the band the chart area of cols - 39 columns, a bar and the
    # outlook of 38. The band is the last twelve rows: the panel of 38 columns, a bar and the
    # gauges. The panel's rows are given from its first row down.
    def frame(self, panel, cols=100, rows=30):
        top = rows - 12
        lines = [" " * (cols - 39) + "|" + "SHORT 1h -- |MID 4h --   |LONG 1d --  " for _ in range(top)]
        lines[0] = "XMR  mid 522.26".ljust(cols - 49) + "ping 0.3s " + lines[0][cols - 39:]
        gauges = "BOOK now .⣿.|... D-0.55 Q-0.73 BTC 15m unavailable 0/15".ljust(cols - 39)
        band = [row.ljust(38) + "|" + gauges for row in (panel + [""] * 12)[:12]]
        return {"lines": lines + band}

    def prop(self, status="", sell="[3 SHORT $500 x1]"):
        return ["PROP XMR 1x FLAT", "Val    $0.00 Open   $0.00 Net   $0.00",
                "Prop   $10000.00  Usable   $10000", status, "[$500 *] [$1000] [x Close]",
                "[t Market] [o Orders] v Muted", "[1 LONG $500 x1] " + sell, "", "", "", "",
                "[p Prop] [l Live] m Market c Chart"]

    def test_workspace_pins_exact_market_and_midpoint(self):
        frame = self.frame(["VIEW XMR --"])
        self.assertTrue(playback.market_matches(frame, "XMR", "522.26"))
        self.assertFalse(playback.market_matches(frame, "XMR", "522.260"))

    def test_workspace_header_requires_a_fresh_exact_market(self):
        frame = self.frame(["VIEW XMR --"])
        header = frame["lines"][0]
        for age, expected in (("0.0", True), ("5.0", True), ("5.1", False),
                              ("-0.1", False), ("NaN", False), ("1", False), ("1.00", False)):
            frame["lines"][0] = header.replace("0.3s", age + "s")
            self.assertEqual(playback.market_matches(frame, "XMR", "522.26"), expected)
        for changed in (header.replace("ping 0.3s", "ping down"), header.replace("XMR", "ETH"),
                        header.replace("522.26", "522x26"), header.replace(" |", " bad 1 |"),
                        header.replace("XMR  mid 522.26", "mid 522.26  XMR")):
            frame["lines"][0] = changed
            self.assertFalse(playback.market_matches(frame, "XMR", "522.26"))
        self.assertFalse(playback.market_matches({"lines": []}, "XMR", "522.26"))

    def test_the_panel_is_the_left_38_columns_of_the_last_twelve_rows(self):
        frame = self.frame(self.prop(status="Bought #1"))
        panel = playback.panel(frame)
        self.assertEqual(len(panel), 12)
        self.assertTrue(all(len(row) == 38 for row in panel))
        self.assertEqual(panel[0].rstrip(), "PROP XMR 1x FLAT")
        self.assertEqual(panel[3].rstrip(), "Bought #1")
        self.assertEqual(panel[11].rstrip(), "[p Prop] [l Live] m Market c Chart")

    def test_prop_is_ready_only_with_its_heading_and_a_blank_status_row(self):
        self.assertTrue(playback.prop_ready(self.frame(self.prop())))
        # The gauges right of the bar carry text on the status row, so a reader past the bar fails.
        self.assertTrue(playback.prop_ready(self.frame(self.prop(), cols=120, rows=40)))
        self.assertFalse(playback.prop_ready(self.frame(self.prop(status="Waiting for prices"))))
        view = self.prop()
        view[0] = "VIEW XMR --"
        self.assertFalse(playback.prop_ready(self.frame(view)))
        # The words in another region are not the panel's heading.
        elsewhere = self.frame([""] + self.prop()[1:])
        elsewhere["lines"][1] = elsewhere["lines"][1][:62] + "PROP XMR 1x FLAT"
        self.assertFalse(playback.prop_ready(elsewhere))

    # The bracketed button under a terminal column counted from one, or None.
    def button(self, row, x):
        start, end = row.rfind("[", 0, x), row.find("]", x - 1)
        return row[start:end + 1] if 0 <= start and "]" not in row[start:x - 1] and end >= 0 else None

    def test_clicks_land_inside_the_two_trade_buttons_on_the_panel_seventh_row(self):
        frame = self.frame(self.prop())
        clicks = playback.trade_clicks(frame)
        self.assertEqual(clicks, [(9, 25), (26, 25)])
        row = frame["lines"][24]
        self.assertEqual([self.button(row, x) for x, _ in clicks], ["[1 LONG $500 x1]", "[3 SHORT $500 x1]"])
        # A long position relabels the sell button REDUCE, and the same click still lands on it.
        held = self.frame(self.prop(sell="[3 REDUCE $500 x1]"))["lines"][24]
        self.assertEqual(self.button(held, clicks[1][0]), "[3 REDUCE $500 x1]")
        # At 120 by 40 the band starts lower and the buttons keep their columns.
        self.assertEqual(playback.trade_clicks(self.frame(self.prop(), cols=120, rows=40)), [(9, 35), (26, 35)])
        broken = self.prop()
        broken[6] = "[1 LONG $500 x1]"
        with self.assertRaises(AssertionError):
            playback.trade_clicks(self.frame(broken))


if __name__ == "__main__":
    unittest.main()
