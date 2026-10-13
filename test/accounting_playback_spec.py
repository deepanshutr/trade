import importlib.util
import json
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location("playback", Path("tools/playback.py"))
playback = importlib.util.module_from_spec(spec)
spec.loader.exec_module(playback)
validate_speed = playback.validate_speed

# The context the offline playback serves: oracle 521.515 and hourly rate 0.0001302411. A long of
# 5 pays 5 * 521.515 * 0.0001302411 for an hour.
CONTEXT = json.loads(Path('fixtures/activeAssetCtx.json').read_text())['data']
HOUR_ON_5 = '0.3396134363325'


def validate_wallet(saved, results, fixture_book, fixture_context=CONTEXT):
    return playback.validate_wallet(saved, results, fixture_book, fixture_context)


PAUSED = 'Opening paused for 2 s after closing'
LONG = {'XMR': {'quantity': '5', 'entry': '500'}}
SHORT = {'XMR': {'quantity': '-5', 'entry': '500'}}


def speed():
    return {'clicks': 600, 'recorded_results': 600, 'saved_orders': 600,
            'refused': 0, 'filled_percent': 100, 'duration_ms': 59900,
            'frame_gap_p95_ms': 100, 'frame_gap_max_ms': 250,
            'maximum_click_gap_ms': 200, 'maximum_sampled_result_backlog': 1}


def receipt(index, side, dollars='$500.00', price='500', fee='1.125'):
    return f'PROP XMR {side} filled {dollars} at {price}\nFee ${fee} | #{index} offline'


def trade_log(refusals=None):
    """600 results numbered from 1, odd results buy. A result named in refusals carries that
    text, and every other result is a fill of 5 at 500, numbered in order."""
    refusals = refusals or {}
    results, orders = [], []
    for index in range(1, 601):
        if index in refusals:
            text = refusals[index]
        else:
            text = receipt(len(orders) + 1, 'buy' if index % 2 else 'sell')
            orders.append(text)
        results.append({'id': index, 'kind': 'order', 'at': index, 'text': text})
    return results, orders


def wallet(orders, cash, fees, realized='0', positions=None):
    return {'deposit': '10000', 'cash': cash, 'fees': fees, 'realized': realized,
            'positions': positions or {}, 'count': len(orders), 'orders': orders}


def book(bid='500', ask='500'):
    return {'coin': 'XMR', 'time': 1000,
            'levels': [[{'px': bid, 'sz': '10', 'n': 1}], [{'px': ask, 'sz': '10', 'n': 1}]]}


def wallet_case():
    results, orders = trade_log()
    return wallet(orders, '9325', '675'), results, book()


def counts(accepted):
    return tuple(len(part) for part in accepted)


def fills(texts):
    """The result log of the given texts in log order, numbered from 1."""
    return [{'id': index, 'kind': 'order', 'at': index, 'text': text}
            for index, text in enumerate(texts, start=1)]


def ten_dollar_case():
    """Bid 498, ask 500. Result 1 is paused with nothing held. Each sell opens or grows a short by
    5.02 at 498, and each buy asks for 5 at 500. The first buy leaves (5.02 - 5) * 500, exactly
    $10, so it reduces 5 and does not close. Every later buy leaves more, and the last sell leaves
    a short of 11 that the run held at no earlier point."""
    texts, orders = [PAUSED], []
    for index in range(2, 601):
        if index % 2 == 0:
            text = receipt(len(orders) + 1, 'sell', dollars='$499.99', price='498', fee='1.124982')
        else:
            text = receipt(len(orders) + 1, 'buy')
        orders.append(text)
        texts.append(text)
    saved = wallet(orders, '6336.1304', '673.8696', realized='-2990',
                   positions={'XMR': {'quantity': '-11', 'entry': '498'}})
    return saved, fills(texts), book(bid='498')


class IndependentOracleReview(unittest.TestCase):
    def test_pending_refusal_before_earlier_fill_retains_input_side_order(self):
        # Click 2 is refused while click 1 is pending, so click 3 grows the long to 10. From then
        # on every sell reduces 5 and every buy grows 5.
        filled = []
        for click in range(1, 601):
            if click == 2:
                continue
            filled.append(receipt(len(filled) + 1, 'buy' if click % 2 else 'sell'))
        completion = ['Order pending', filled[0]] + filled[1:]
        results = [{'kind': 'order', 'text': text} for text in completion]
        saved = wallet(filled, '9326.125', '673.875', positions=LONG)
        self.assertEqual(counts(validate_wallet(saved, results, book())), (599, 1))
        results[1]['text'] = results[1]['text'].replace(' buy ', ' sell ')
        with self.assertRaisesRegex(AssertionError, 'fill side differs'):
            validate_wallet(saved, results, book())

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
        # Every buy opens 5 at 500 and every sell closes it, each at a fee of 1.125.
        self.assertEqual(counts(validate_wallet(*wallet_case())), (600, 0))

    def test_opening_pause_after_a_close_is_accepted_and_shorts_follow(self):
        # Result 3 is a buy right after a close. From then on every sell opens a short of 5 and
        # every buy closes it.
        results, orders = trade_log({3: PAUSED})
        saved = wallet(orders, '9326.125', '673.875', positions=SHORT)
        self.assertEqual(counts(validate_wallet(saved, results, book())), (599, 1))

    def test_opening_pause_cannot_refuse_a_reducing_click(self):
        # Result 2 is a sell while long. The rest of the log is what follows if it were refused.
        results, orders = trade_log({2: PAUSED})
        saved = wallet(orders, '9326.125', '673.875', positions=LONG)
        with self.assertRaisesRegex(AssertionError, 'reducing click'):
            validate_wallet(saved, results, book())

    def test_no_xmr_to_sell_is_no_longer_a_refusal(self):
        # Once while long, and once at a sell with nothing held, which the spot rule accepted.
        cases = (({2: 'No XMR to sell'}, '9326.125', '673.875', LONG),
                 ({1: PAUSED, 2: 'No XMR to sell'}, '9327.25', '672.75', None))
        for refusals, cash, fees, positions in cases:
            with self.subTest(refusals=refusals):
                results, orders = trade_log(refusals)
                with self.assertRaisesRegex(AssertionError, 'No XMR to sell'):
                    validate_wallet(wallet(orders, cash, fees, positions=positions), results, book())
        # The same log with the pause in its place passes, so only the text is refused.
        results, orders = trade_log({1: PAUSED, 2: PAUSED})
        self.assertEqual(counts(validate_wallet(wallet(orders, '9327.25', '672.75'), results, book())),
                         (598, 2))

    def test_dollars_at_one_times_leverage_are_refused(self):
        results, _ = trade_log()
        for result in results:
            result['text'] = result['text'].replace('filled $500.00 ', 'filled $2500.00 ')
        saved = wallet([result['text'] for result in results], '9325', '675')
        with self.assertRaisesRegex(AssertionError, 'dollars differ'):
            validate_wallet(saved, results, book())

    def test_fee_at_the_old_rate_is_refused(self):
        results, _ = trade_log()
        for result in results:
            result['text'] = result['text'].replace('Fee $1.125 ', 'Fee $1.25 ')
        saved = wallet([result['text'] for result in results], '9250', '750')
        with self.assertRaisesRegex(AssertionError, 'fee differs'):
            validate_wallet(saved, results, book())

    def test_funding_is_zero_or_one_hourly_payment_on_a_held_position(self):
        # The log of test 1 holds a long of 5 or nothing, so it pays one hour on 5 or nothing.
        saved, results, book = wallet_case()
        for funding, cash in (('0', '9325'), (HOUR_ON_5, '9324.6603865636675')):
            with self.subTest(funding=funding):
                self.assertEqual(counts(validate_wallet(dict(saved, funding=funding, cash=cash), results, book)),
                                 (600, 0))
        # The sign flipped, eight hours, a charge on no held position, and the old bound's figures.
        # Cash moves with each, so only the funding rule refuses them.
        for funding, cash in (('-' + HOUR_ON_5, '9325.3396134363325'), ('2.71690749066', '9322.28309250934'),
                              ('1', '9324'), ('3', '9322'), ('-3', '9328')):
            with self.subTest(funding=funding):
                with self.assertRaisesRegex(AssertionError, 'funding is neither zero nor one hourly payment'):
                    validate_wallet(dict(saved, funding=funding, cash=cash), results, book)
        # The log of test 2 ends short 5, which receives one hour at the positive rate.
        results, orders = trade_log({3: PAUSED})
        saved = wallet(orders, '9326.4646134363325', '673.875', positions=SHORT)
        self.assertEqual(counts(validate_wallet(dict(saved, funding='-' + HOUR_ON_5), results, book)), (599, 1))

    def test_the_open_position_keeps_its_entry(self):
        # Test 9's log ends long 5, all bought at 500.
        filled = []
        for click in range(1, 601):
            if click != 2:
                filled.append(receipt(len(filled) + 1, 'buy' if click % 2 else 'sell'))
        results = fills(['Order pending', filled[0]] + filled[1:])
        for entry in ('400', '0', '500.001'):
            with self.subTest(entry=entry):
                saved = wallet(filled, '9326.125', '673.875', positions={'XMR': {'quantity': '5', 'entry': entry}})
                with self.assertRaisesRegex(AssertionError, 'entry differs'):
                    validate_wallet(saved, results, book())

    def test_saved_cash_never_below_zero(self):
        # Bid 100 and ask 500. Each buy opens 5 at 500 and each sell closes it at 100, realized
        # -2000. Twenty round trips, then pauses, leave cash at 10000 - 40000 - 27.
        texts = []
        for index in range(1, 601):
            if index > 40:
                texts.append(PAUSED)
            elif index % 2:
                texts.append(receipt(index, 'buy'))
            else:
                texts.append(receipt(index, 'sell', dollars='$100.00', price='100', fee='0.225'))
        saved = wallet(texts[:40], '-30027', '27', realized='-40000')
        with self.assertRaisesRegex(AssertionError, 'saved cash is below zero'):
            validate_wallet(saved, fills(texts), book(bid='100'))
        # Test 9's log ends long 5 with cash 9326.125. A context whose hour on 5 is exactly that cash
        # takes it to zero, which is allowed.
        filled = []
        for click in range(1, 601):
            if click != 2:
                filled.append(receipt(len(filled) + 1, 'buy' if click % 2 else 'sell'))
        saved = dict(wallet(filled, '0', '673.875', positions=LONG), funding='9326.125')
        context = {'coin': 'XMR', 'ctx': {'oraclePx': '1865.225', 'funding': '1'}}
        self.assertEqual(counts(validate_wallet(saved, fills(['Order pending', filled[0]] + filled[1:]), book(),
                                                context)), (599, 1))

    def test_a_wrong_saved_position_is_refused(self):
        results, orders = trade_log({3: PAUSED})
        for positions in ({}, LONG, {'XMR': {'quantity': '-10', 'entry': '500'}}):
            with self.subTest(positions=positions):
                with self.assertRaisesRegex(AssertionError, 'positions differ'):
                    validate_wallet(wallet(orders, '9326.125', '673.875', positions=positions), results, book())

    def test_saved_count_and_history_must_equal_the_fills(self):
        saved, results, book = wallet_case()
        for changed in (dict(saved, count=601), dict(saved, count=599),
                        dict(saved, orders=saved['orders'][:-1] + [saved['orders'][0]])):
            with self.subTest(count=changed['count']):
                with self.assertRaisesRegex(AssertionError, 'saved count or history differs'):
                    validate_wallet(changed, results, book)

    def test_a_reduce_leaving_exactly_ten_dollars_is_a_part_reduce(self):
        saved, results, fixture = ten_dollar_case()
        self.assertEqual(counts(validate_wallet(saved, results, fixture)), (599, 1))

    def test_funding_may_charge_the_position_the_last_fill_left(self):
        # The short of 11 exists only after the last fill, and pays -11 * 521.515 * 0.0001302411.
        saved, results, fixture = ten_dollar_case()
        saved.update(funding='-0.7471495599315', cash='6336.8775495599315')
        self.assertEqual(counts(validate_wallet(saved, results, fixture)), (599, 1))

    def test_funding_is_one_hour_and_exact(self):
        # Two hours on the long of 5, and one hour rounded to cents, each with cash moved to match.
        saved, results, book = wallet_case()
        for funding, cash in (('0.679226872665', '9324.320773127335'), ('0.34', '9324.66')):
            with self.subTest(funding=funding):
                with self.assertRaisesRegex(AssertionError, 'funding is neither zero nor one hourly payment'):
                    validate_wallet(dict(saved, funding=funding, cash=cash), results, book)

    def test_funding_may_charge_a_position_a_reduce_left(self):
        # The first buy of the ten dollar case reduces the short of 5.02 to 0.02, a position no
        # opening fill leaves. One hour on it is -0.02 * 521.515 * 0.0001302411.
        saved, results, book = ten_dollar_case()
        saved.update(funding='-0.00135845374533', cash='6336.13175845374533')
        self.assertEqual(counts(validate_wallet(saved, results, book)), (599, 1))

    def test_a_short_keeps_its_entry(self):
        saved, results, book = ten_dollar_case()
        for entry in ('400', '-498', '498.001'):
            with self.subTest(entry=entry):
                changed = dict(saved, positions={'XMR': {'quantity': '-11', 'entry': entry}})
                with self.assertRaisesRegex(AssertionError, 'entry differs'):
                    validate_wallet(changed, results, book)

    def test_a_press_is_sized_in_lots_of_a_thousandth(self):
        # 2500 / 522.28 is 4.7867..., so a press is 4.786, value 2499.63208.
        results, _ = trade_log()
        for result in results:
            result['text'] = result['text'].replace('filled $500.00 at 500\nFee $1.125 ',
                                                    'filled $499.93 at 522.28\nFee $1.124834436 ')
        saved = wallet([result['text'] for result in results], '9325.0993384', '674.9006616')
        self.assertEqual(counts(validate_wallet(saved, results, book('522.28', '522.28'))), (600, 0))

    def test_dollars_round_half_up_at_the_cent(self):
        # At 475 a press is 5.263, value 2499.925, a fifth 499.985: half up gives $499.99.
        def at(dollars):
            results, _ = trade_log()
            for result in results:
                result['text'] = result['text'].replace(
                    'filled $500.00 at 500\nFee $1.125 ', f'filled {dollars} at 475\nFee $1.12496625 ')
            return wallet([r['text'] for r in results], '9325.02025', '674.97975'), results
        saved, results = at('$499.99')
        self.assertEqual(counts(validate_wallet(saved, results, book('475', '475'))), (600, 0))
        saved, results = at('$499.98')
        with self.assertRaisesRegex(AssertionError, 'dollars differ'):
            validate_wallet(saved, results, book('475', '475'))

    def test_the_pause_may_refuse_a_click_that_grows_the_position(self):
        # Click 2 is refused while click 1 is pending, so click 3 grows the long to 10 and click 4
        # reduces it to 5. Click 5 would grow it again and meets the pause. Click 6 closes.
        texts, orders = ['Order pending'], []

        def fill(side):
            orders.append(receipt(len(orders) + 1, side))
            texts.append(orders[-1])
        fill('buy')
        fill('buy')
        fill('sell')
        texts.append(PAUSED)
        fill('sell')
        for click in range(7, 601):
            fill('buy' if click % 2 else 'sell')
        saved = wallet(orders, '9327.25', '672.75')
        self.assertEqual(counts(validate_wallet(saved, fills(texts), book())), (598, 2))

    def test_a_part_reduce_share_is_kept_to_fifty_digits(self):
        # Asks 1 at 499 then 10 at 500, bid 498. A buy asks for 5.01 and pays 2504 (shown ~499.8).
        # A sell asks for 5.02 at 498. Click 2 is pending, so click 3 grows the long to 10.02 and
        # every sell from then on is a part reduce with the entry 2504 / 5.01, which never ends.
        # Gain per reduce 2499.96 - 5.02 * 2504 / 5.01, times 299 reduces.
        fixture = {'coin': 'XMR', 'time': 1000,
                   'levels': [[{'px': '498', 'sz': '10', 'n': 1}],
                              [{'px': '499', 'sz': '1', 'n': 1}, {'px': '500', 'sz': '10', 'n': 1}]]}
        texts, orders = ['Order pending'], []

        def fill(side):
            if side == 'buy':
                orders.append(receipt(len(orders) + 1, 'buy', dollars='$500.80', price='~499.8', fee='1.1268'))
            else:
                orders.append(receipt(len(orders) + 1, 'sell', dollars='$499.99', price='498', fee='1.124982'))
            texts.append(orders[-1])
        fill('buy')
        for click in range(3, 601):
            fill('buy' if click % 2 else 'sell')
        saved = wallet(orders, '6623.2271883872255489021956087824351297405190',
                       '674.409618', realized='-2702.3631936127744510978043912175648702594810',
                       positions={'XMR': {'quantity': '2.02', 'entry': '499.8003992015968063872255489021956087824351'}})
        self.assertEqual(counts(validate_wallet(saved, fills(texts), fixture)), (599, 1))

    def test_same_wallet_refuses_a_tiny_cash_change(self):
        saved, _, _ = wallet_case()
        self.assertFalse(playback.same_wallet(dict(saved, cash='9324.99999999999999999'), saved))

    def test_fewer_than_forty_fills_are_refused(self):
        results, orders = trade_log({index: PAUSED for index in range(39, 601)})
        with self.assertRaisesRegex(AssertionError, 'fewer than 40'):
            validate_wallet(wallet(orders, '9957.25', '42.75'), results, book())
        results, orders = trade_log({index: 'Not sent: controls changed' for index in range(40, 601)})
        with self.assertRaisesRegex(AssertionError, 'fewer than 40'):
            validate_wallet(wallet(orders, '9956.125', '43.875', positions=LONG), results, book())
        results, orders = trade_log({index: PAUSED for index in range(41, 601)})
        self.assertEqual(counts(validate_wallet(wallet(orders, '9955', '45'), results, book())), (40, 560))

    def test_realized_carries_the_sign_of_the_held_position(self):
        # Bid 500 and ask 501. Result 1 buys 4.99 at 501 and result 2 sells it at 500, realized
        # -4.99. After the pause at result 3, each sell opens a short of 5 at 500 and each buy
        # closes it at 501, realized -5. That is 299 shorts opened and 298 closed.
        results, orders = [], []
        for index in range(1, 601):
            if index == 3:
                text = PAUSED
            else:
                number = len(orders) + 1
                if index == 1:
                    text = receipt(number, 'buy', price='501', fee='1.1249955')
                elif index == 2:
                    text = receipt(number, 'sell', dollars='$499.00', fee='1.12275')
                elif index % 2:
                    text = receipt(number, 'buy', dollars='$501.00', price='501', fee='1.12725')
                else:
                    text = receipt(number, 'sell')
                orders.append(text)
            results.append({'id': index, 'kind': 'order', 'at': index, 'text': text})
        saved = wallet(orders, '7830.4667545', '674.5432455', realized='-1494.99', positions=SHORT)
        self.assertEqual(counts(validate_wallet(saved, results, book(ask='501'))), (599, 1))
        # The short results saved without their sign, beside the right cash.
        with self.assertRaisesRegex(AssertionError, 'realized differs|cash differs from deposit'):
            validate_wallet(dict(saved, realized='1485.01'), results, book(ask='501'))

    def test_post_run_comparison_allows_only_a_funding_charge(self):
        saved, _, _ = wallet_case()
        charged = dict(saved, cash='9324.9875', funding='0.0125')
        self.assertTrue(playback.same_wallet(saved, saved))
        self.assertTrue(playback.same_wallet(charged, saved))
        for changed in (dict(saved, cash='9324.9875'), dict(charged, funding='0.02'),
                        dict(saved, count=601), dict(saved, realized='1')):
            with self.subTest(changed=changed):
                self.assertFalse(playback.same_wallet(changed, saved))

    def test_playback_reads_the_margin_wallet(self):
        self.assertEqual(playback.WALLET, Path('prop/margin-mainnet.json'))

    def test_reversed_button_results_do_not_pass_as_healthy_fills(self):
        results, _ = trade_log({1: PAUSED})
        for index in range(2, 601):
            results[index - 1]['text'] = receipt(index - 1, 'sell' if index % 2 else 'buy')
        saved = wallet([result['text'] for result in results[1:]], '9326.125', '673.875', positions=LONG)
        with self.assertRaisesRegex(AssertionError, 'fill side differs'):
            validate_wallet(saved, results, book())

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
            result['text'] = result['text'].replace('Fee $1.125', 'Fee $2.25')
        saved.update(cash='8650', fees='1350', orders=[result['text'] for result in results])
        with self.assertRaises(AssertionError):
            validate_wallet(saved, results, book)

    def test_zero_quantity_cannot_be_counted_as_six_hundred_fills(self):
        saved, results, book = wallet_case()
        for result in results:
            result['text'] = result['text'].replace('filled $500.00 ', 'filled $0.00 ').replace('Fee $1.125', 'Fee $0')
        saved.update(cash='10000', fees='0', orders=[result['text'] for result in results])
        with self.assertRaises(AssertionError):
            validate_wallet(saved, results, book)

    def test_tiny_fills_cannot_stand_in_for_the_selected_five_hundred(self):
        saved, results, book = wallet_case()
        for result in results:
            result['text'] = result['text'].replace('filled $500.00 ', 'filled $0.50 ').replace('Fee $1.125', 'Fee $0.001125')
        saved.update(cash='9999.325', fees='0.675', orders=[result['text'] for result in results])
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
