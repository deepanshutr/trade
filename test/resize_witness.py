import argparse
import hashlib
import json
from pathlib import Path
import re
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'tools'))
import playback

parser = argparse.ArgumentParser()
parser.add_argument('--java', required=True)
parser.add_argument('--classpath', required=True)
parser.add_argument('--output', required=True)
args = parser.parse_args()
args.fixtures = str(Path('fixtures').resolve())
args.display_contract = 'workspace'
output = Path(args.output)
output.mkdir(exist_ok=True)

def axis(frame):
    rows = [row.split('|', 1)[0] for row in frame['lines'] if 'c line 1s/col' in row]
    assert len(rows) == 1, rows
    values = re.findall(r'(?<![\d:])(\d{2}):(\d{2}):(\d{2})(?![\d:])', rows[0])
    assert values, rows
    h, m, s = map(int, values[-1])
    return h * 3600 + m * 60 + s

records = []
failures = []
for number in range(3):
    directory = output / ('trial-' + str(number + 1))
    directory.mkdir()
    run = playback.Playback(args, 'normal', directory)
    try:
        run.event('connected')
        run.command('quote', coin='XMR')
        run.frame(lambda f: playback.market_matches(f, 'workspace', 'XMR', '522.26'))
        run.command('hold-queue')
        run.command('quote', coin='XMR')
        run.event('queue-held')
        before = run.frames.complete[-1]
        run.resize(120, 40)
        run.write(playback.ESC + b'[D')
        run.event('queued-key')
        run.frame(lambda f: len(f['lines']) == 40 and len(f['lines'][0]) == 120)
        after_index = len(run.frames.complete)
        run.command('release-queue')
        after = run.frame(lambda f: len(f['lines'][0]) == 120 and
                          (axis(f) - axis(before) + 43200) % 86400 - 43200 -
                          (f['captured_ns'] - before['captured_ns']) / 1_000_000_000 <= -5,
                          after_index)
        run.write(b'q')
        result = run.event('result')
        run.wait(lambda: playback.RESTORED in run.raw, 'terminal restored')
        run.wait(lambda: run.child.poll() is not None, 'application exit')
        assert run.child.wait() == 0
        assert result['forbidden'] == []
        elapsed = after['captured_ns'] - before['captured_ns']
        difference = (axis(after) - axis(before) + 43200) % 86400 - 43200
        samples = [s for s in result['queue']['samples'] if s['event'].startswith('resize ') or s['event'] == 'key ScrubBack']
        pan = next(s for s in samples if s['event'] == 'key ScrubBack')
        resized = next(s for s in samples if s['event'] == 'resize 120 40')
        ordered = int(resized['entered_ns']) < int(pan['entered_ns'])
        minimum = elapsed // 1_000_000_000 - 1
        maximum = (elapsed + 999_999_999) // 1_000_000_000 + 1
        correct_step = minimum <= difference + 16 <= maximum
        record = {'trial': number + 1, 'elapsed_ns': elapsed, 'clock_difference_seconds': difference,
                  'expected_pan_seconds': 16, 'resize_before_pan': ordered, 'correct_step': correct_step,
                  'samples': samples, 'before_axis': axis(before), 'after_axis': axis(after)}
        records.append(record)
        print(json.dumps(record), flush=True)
        if not ordered or not correct_step:
            failures.append(record)
    finally:
        run.close()
    if failures:
        break
result = {'records': records, 'failures': failures,
          'source_sha256': {str(path): hashlib.sha256(path.read_bytes()).hexdigest()
                            for path in sorted(Path('src').glob('*.scala'))}}
(output / 'result.json').write_text(json.dumps(result, indent=2) + '\n')
assert not failures, 'resize input ordering used the previous viewport; see ' + str(output / 'result.json')
