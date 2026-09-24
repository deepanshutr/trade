import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location("playback", Path("tools/playback.py"))
playback = importlib.util.module_from_spec(spec)
spec.loader.exec_module(playback)

ESC = b"\x1b"
BEGIN = ESC + b"[?2026h"
FINISH = ESC + b"[?2026l"
RESET = ESC + b"[0m"


def row(text):
    return RESET + text.encode("utf-8") + RESET + playback.CLEAR


def full(lines):
    return BEGIN + playback.START + b"\r\n".join(map(row, lines)) + playback.END + FINISH


def delta(*changes):
    return BEGIN + b"".join(ESC + f"[{index + 1};1H".encode() + row(text)
                            for index, text in changes) + FINISH


class TerminalPlaybackSpec(unittest.TestCase):
    def test_synchronized_full_frame_waits_for_complete_paint_end(self):
        stream = playback.Frames()
        data = full(["XMR \u28ff", "BUY SELL"])
        for byte in data[:-1]:
            stream.feed(bytes([byte]), 10)
            self.assertEqual(stream.complete, [])
        stream.feed(data[-1:], 20)
        self.assertEqual(stream.complete, [{"captured_ns": 20, "lines": ["XMR \u28ff", "BUY SELL"]}])

    def test_arbitrary_chunks_retain_unchanged_rows_and_clear_shortened_text(self):
        first = ["connection active", "\x1b[31mSELL\x1b[0m", "BUY SELL 500", "last"]
        expected = ["connection active", "\u28ff", "", "last"]
        data = b"startup noise" + full(first) + delta((1, "\x1b[32m\u28ff"), (2, ""))
        for size in range(1, len(data) + 1):
            with self.subTest(chunk=size):
                stream = playback.Frames()
                for offset in range(0, len(data), size):
                    stream.feed(data[offset:offset + size], offset)
                self.assertEqual(len(stream.complete), 2)
                self.assertEqual(stream.complete[0]["lines"], ["connection active", "SELL", "BUY SELL 500", "last"])
                self.assertEqual(stream.complete[1]["lines"], expected)

    def test_full_resize_erases_old_width_and_rows_then_accepts_deltas(self):
        stream = playback.Frames()
        stream.feed(full(["long original", "second", "old third"]), 1)
        stream.feed(full(["short"]), 2)
        self.assertEqual(stream.complete[-1]["lines"], ["short"])
        stream.feed(full(["a", "b", "c", "d"]), 3)
        stream.feed(delta((3, "done")), 4)
        self.assertEqual(stream.complete[-1]["lines"], ["a", "b", "c", "done"])
        stream.feed(full([]), 5)
        self.assertEqual(stream.complete[-1]["lines"], [])

    def test_control_positions_remain_fixed_across_changed_market_rows(self):
        for width, height in ((100, 30), (120, 40), (360, 99)):
            with self.subTest(width=width, height=height):
                lines = [f"row {i}".ljust(width) for i in range(height)]
                lines[-8] = "BUY SELL".ljust(width)
                lines[-6] = "Target buy 522.28 Up/Down".ljust(width)
                stream = playback.Frames()
                stream.feed(full(lines), 1)
                stream.feed(delta((0, "new quote".ljust(width)), (height - 1, "receipt".ljust(width))), 2)
                actual = stream.complete[-1]["lines"]
                self.assertEqual(len(actual), height)
                self.assertEqual(actual[1:-1], lines[1:-1])
                self.assertEqual(actual[-8], lines[-8])
                self.assertEqual(actual[-6], lines[-6])
                self.assertEqual(actual[-1], "receipt".ljust(width))

    def test_legacy_full_frames_and_synchronized_deltas_can_share_one_chunk(self):
        stream = playback.Frames()
        legacy = playback.START + b"one" + playback.CLEAR + playback.END
        stream.feed(legacy + delta((0, "two")) + legacy, 1)
        self.assertEqual([f["lines"] for f in stream.complete], [["one"], ["two"], ["one"]])

    def test_malformed_delta_is_rejected_without_publishing_a_partial_screen(self):
        for broken in (BEGIN + ESC + b"[1;1Hnew" + FINISH,
                       BEGIN + ESC + b"[0;1Hbad" + playback.CLEAR + FINISH,
                       BEGIN + ESC + b"[1;1H\xff" + playback.CLEAR + FINISH,
                       BEGIN + ESC + b"[2A" + FINISH):
            with self.subTest(broken=broken):
                stream = playback.Frames()
                stream.feed(full(["before"]), 1)
                with self.assertRaises((AssertionError, UnicodeDecodeError)):
                    stream.feed(broken, 2)
                self.assertEqual(stream.complete, [{"captured_ns": 1, "lines": ["before"]}])

    def test_delta_requires_a_completed_initial_full_frame(self):
        with self.assertRaises(AssertionError):
            playback.Frames().feed(delta((0, "unknown screen")), 1)


if __name__ == "__main__":
    unittest.main()
