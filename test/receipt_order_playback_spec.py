"""Checks under the production invariant that a pending refusal precedes its fill."""
from decimal import Decimal
import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location('tested_playback', Path('tools/playback.py'))
playback = importlib.util.module_from_spec(spec)
spec.loader.exec_module(playback)


def trace(start, count):
    results, filled = [], []
    cash, held, fees = Decimal(10000), Decimal(0), Decimal(0)
    delayed = None
    delayed_result = None
    for click in range(1, 601):
        if start < click <= start + count:
            results.append({'kind': 'order', 'text': 'Order pending'})
            continue
        if delayed is not None:
            delayed_result = len(results)
            results.append(delayed)
            delayed = None
        side = 'buy' if click % 2 else 'sell'
        if side == 'sell' and held == 0:
            result = {'kind': 'order', 'text': 'No XMR to sell'}
        else:
            direction = 1 if side == 'buy' else -1
            held += direction
            cash -= direction * 500 + Decimal('0.25')
            fees += Decimal('0.25')
            text = f'PROP XMR {side} filled $500.00 at 500\nFee $0.25 | #{len(filled)+1} offline'
            filled.append(text)
            result = {'kind': 'order', 'text': text}
        if click == start:
            assert ' filled ' in result['text']
            delayed = result
        else:
            results.append(result)
    assert delayed is None and len(results) == 600
    saved = {'deposit': '10000', 'cash': str(cash), 'fees': str(fees), 'realized': '0',
             'positions': {'XMR': {'quantity': str(held), 'entry': '500'}} if held else {},
             'count': len(filled), 'orders': filled}
    book = {'levels': [[{'px': '500', 'sz': '10'}], [{'px': '500', 'sz': '10'}]]}
    return saved, results, book, delayed_result


class ReceiptOrderFinder(unittest.TestCase):
    def test_multiple_refusals_keep_each_delayed_fill_bound_to_its_original_click(self):
        for start in (1, 2, 15, 30, 299, 560):
            for count in (1, 2, 3, 4, 7, 20):
                with self.subTest(start=start, count=count):
                    saved, results, book, delayed = trace(start, count)
                    accepted, refused = playback.validate_wallet(saved, results, book)
                    self.assertEqual(len(accepted), saved['count'])
                    self.assertEqual(len(accepted) + len(refused), 600)
                    text = results[delayed]['text']
                    old, new = (' buy ', ' sell ') if ' buy ' in text else (' sell ', ' buy ')
                    results[delayed]['text'] = text.replace(old, new)
                    saved['orders'] = [r['text'] for r in results if ' filled ' in r['text']]
                    with self.assertRaisesRegex(AssertionError, 'fill side differs'):
                        playback.validate_wallet(saved, results, book)
