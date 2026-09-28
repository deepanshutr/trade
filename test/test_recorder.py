import unittest

from tools.recorder import definition


class RecorderTests(unittest.TestCase):
    def test_colon_market_is_refused(self):
        with self.assertRaises(ValueError):
            definition("/java", "/desk.jar", "/state", "xyz:TSLA")

    def test_supported_markets_keep_recording_arguments(self):
        for coin in ("XMR", "BTC", "abc.def_1-2"):
            with self.subTest(coin=coin):
                service = definition("/java", "/desk.jar", "/state", coin)
                self.assertEqual(
                    service["ProgramArguments"][-5:],
                    [coin, "--record", "--no-analysis", "--state", "/state"],
                )


if __name__ == "__main__":
    unittest.main()
