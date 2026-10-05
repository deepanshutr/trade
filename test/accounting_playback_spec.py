import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location("playback", Path("tools/playback.py"))
playback = importlib.util.module_from_spec(spec)
spec.loader.exec_module(playback)
validate_speed = playback.validate_speed
validate_wallet = playback.validate_wallet


def speed():
    return {'clicks': 600, 'recorded_results': 600, 'saved_orders': 600,
            'refused': 0, 'filled_percent': 100, 'duration_ms': 59900,
            'frame_gap_p95_ms': 100, 'frame_gap_max_ms': 250,
            'maximum_click_gap_ms': 200, 'maximum_sampled_result_backlog': 1}


def wallet_case():
    results = [{'id': index, 'kind': 'order', 'at': index,
                'text': f'PROP XMR {"buy" if index % 2 else "sell"} filled $500.00 at 500\nFee $0.25 | #{index} offline'}
               for index in range(1, 601)]
    saved = {'deposit': '10000', 'cash': '9850', 'fees': '150', 'realized': '0',
             'positions': {}, 'count': 600, 'orders': [result['text'] for result in results]}
    book = {'coin': 'XMR', 'time': 1000,
            'levels': [[{'px': '500', 'sz': '10', 'n': 1}], [{'px': '500', 'sz': '10', 'n': 1}]]}
    return saved, results, book


class IndependentOracleReview(unittest.TestCase):
    def test_pending_refusal_before_earlier_fill_retains_input_side_order(self):
        saved, _, book = wallet_case()
        filled = []
        for click in range(1, 601):
            if click == 2:
                continue
            side = 'buy' if click % 2 else 'sell'
            filled.append(f'PROP XMR {side} filled $500.00 at 500\nFee $0.25 | #{len(filled) + 1} offline')
        completion = ['Order pending', filled[0]] + filled[1:]
        results = [{'kind': 'order', 'text': text} for text in completion]
        saved.update(cash='9350.25', fees='149.75', count=599,
                     positions={'XMR': {'quantity': '1', 'entry': '500'}}, orders=filled)
        accepted, refused = validate_wallet(saved, results, book)
        self.assertEqual((len(accepted), len(refused)), (599, 1))
        results[1]['text'] = results[1]['text'].replace(' buy ', ' sell ')
        with self.assertRaises(AssertionError):
            validate_wallet(saved, results, book)

    def test_pending_refusal_needs_its_outstanding_completion(self):
        saved, results, book = wallet_case()
        for replacement in ('Order pending', 'Not sent: controls changed'):
            candidate = [dict(result) for result in results]
            candidate[-2]['text'] = 'Order pending'
            candidate[-1]['text'] = replacement
            with self.assertRaises(AssertionError):
                validate_wallet(saved, candidate, book)

    def test_hard_timing_bounds_include_the_stated_boundary(self):
        for duration in (59800, 599000 / 9.5):
            sample = speed()
            sample['duration_ms'] = duration
            validate_speed(sample)

    def test_timing_limits_reject_values_just_outside_each_bound(self):
        invalid = (('duration_ms', 59799.999),
                   ('duration_ms', 599000 / 9.5 + 0.001),
                   ('frame_gap_p95_ms', 100.001),
                   ('frame_gap_max_ms', 250.001),
                   ('maximum_click_gap_ms', 200.001))
        for key, value in invalid:
            with self.subTest(key=key, value=value):
                sample = speed()
                sample[key] = value
                with self.assertRaises(AssertionError):
                    validate_speed(sample)

    def test_a_fifteen_second_freeze_cannot_hide_behind_a_good_percentile(self):
        sample = speed()
        sample.update(frame_gap_p95_ms=36.414166, frame_gap_max_ms=15037.647334)
        with self.assertRaises(AssertionError):
            validate_speed(sample)

    def test_a_five_per_second_run_does_not_prove_ten_per_second(self):
        sample = speed()
        sample.update(duration_ms=119801.938666, frame_gap_p95_ms=72.828332, frame_gap_max_ms=159.481582)
        with self.assertRaises(AssertionError):
            validate_speed(sample)

    def test_a_compressed_burst_does_not_prove_a_sustained_minute(self):
        sample = speed()
        sample['duration_ms'] = 30000
        with self.assertRaises(AssertionError):
            validate_speed(sample)

    def test_a_late_click_followed_by_catchup_does_not_hide_in_total_duration(self):
        sample = speed()
        sample['maximum_click_gap_ms'] = 200.001
        with self.assertRaises(AssertionError):
            validate_speed(sample)

    def test_simple_exact_wallet_trace_is_valid(self):
        validate_wallet(*wallet_case())

    def test_reversed_button_results_do_not_pass_as_healthy_fills(self):
        saved, results, book = wallet_case()
        results[0]['text'] = 'No XMR to sell'
        for index in range(2, 601):
            results[index - 1]['text'] = f'PROP XMR {"sell" if index % 2 else "buy"} filled $500.00 at 500\nFee $0.25 | #{index} offline'
        saved.update(cash='9350.25', fees='149.75', count=599,
                     positions={'XMR': {'quantity': '1', 'entry': '500'}},
                     orders=[result['text'] for result in results[1:]])
        with self.assertRaises(AssertionError):
            validate_wallet(saved, results, book)

    def test_zero_displayed_prices_cannot_agree_only_through_fees(self):
        saved, results, book = wallet_case()
        for result in results:
            result['text'] = result['text'].replace(' at 500\n', ' at 0\n')
        saved['orders'] = [result['text'] for result in results]
        with self.assertRaises(AssertionError):
            validate_wallet(saved, results, book)

    def test_fill_sides_are_bound_to_click_ids_even_without_refusals(self):
        saved, results, book = wallet_case()
        results[3]['text'] = results[3]['text'].replace(' sell ', ' buy ')
        results[4]['text'] = results[4]['text'].replace(' buy ', ' sell ')
        saved['orders'] = [result['text'] for result in results]
        with self.assertRaises(AssertionError):
            validate_wallet(saved, results, book)

    def test_consistently_doubled_fees_cannot_define_a_doubled_fill_value(self):
        saved, results, book = wallet_case()
        for result in results:
            result['text'] = result['text'].replace('Fee $0.25', 'Fee $0.50')
        saved.update(cash='9700', fees='300', orders=[result['text'] for result in results])
        with self.assertRaises(AssertionError):
            validate_wallet(saved, results, book)

    def test_an_empty_wallet_cannot_explain_refusing_a_buy_as_a_sell(self):
        saved, results, book = wallet_case()
        results[0]['text'] = 'No XMR to sell'
        results[1]['text'] = 'No XMR to sell'
        for index in range(3, 601):
            results[index - 1]['text'] = results[index - 1]['text'].replace(f'#{index} ', f'#{index - 2} ')
        saved.update(cash='9850.50', fees='149.50', count=598,
                     orders=[result['text'] for result in results[2:]])
        with self.assertRaises(AssertionError):
            validate_wallet(saved, results, book)

    def test_zero_quantity_cannot_be_counted_as_six_hundred_fills(self):
        saved, results, book = wallet_case()
        for result in results:
            result['text'] = result['text'].replace('filled $500.00 ', 'filled $0.00 ').replace('Fee $0.25', 'Fee $0')
        saved.update(cash='10000', fees='0', orders=[result['text'] for result in results])
        with self.assertRaises(AssertionError):
            validate_wallet(saved, results, book)

    def test_tiny_fills_cannot_stand_in_for_the_selected_five_hundred(self):
        saved, results, book = wallet_case()
        for result in results:
            result['text'] = result['text'].replace('filled $500.00 ', 'filled $0.50 ').replace('Fee $0.25', 'Fee $0.00025')
        saved.update(cash='9999.85', fees='0.15', orders=[result['text'] for result in results])
        with self.assertRaises(AssertionError):
            validate_wallet(saved, results, book)

    def test_a_fill_must_name_the_dollars_of_the_selected_amount(self):
        saved, results, book = wallet_case()
        for wrong in ('filled $499.99 ', 'filled 1 ', 'filled $500 '):
            with self.subTest(wrong=wrong):
                candidate = [dict(result) for result in results]
                candidate[0]['text'] = candidate[0]['text'].replace('filled $500.00 ', wrong)
                with self.assertRaises(AssertionError):
                    validate_wallet(dict(saved, orders=[r['text'] for r in candidate]), candidate, book)
