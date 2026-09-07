#!/usr/bin/env python3
"""Run the driver against independent local healthy and failure controls."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

root = Path(__file__).resolve().parent
cases = {'healthy': 0, 'stale': 3, 'disconnected': 3, 'restoration_failure': 3, 'child_failure': 7}
records = []
evidence = Path(os.environ.get('DESK_CHECK_EVIDENCE', str(root.parent / 'driver-evidence')))
evidence.mkdir(exist_ok=True, parents=True)
with tempfile.TemporaryDirectory(prefix='desk-driver-') as temporary:
    home = Path(temporary)
    binary = home / '.local/bin/desk'
    binary.parent.mkdir(parents=True)
    shutil.copyfile(root / 'liverun_child.py', binary)
    binary.chmod(0o700)
    for case, expected in cases.items():
        log = evidence / f'{case}.log'
        env = dict(os.environ, HOME=str(home), DESK_CHECK_CASE=case)
        result = subprocess.run(['expect', str(root / 'liverun.exp'), str(log), '1'],
                                env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                timeout=45)
        (evidence / f'{case}.output').write_bytes(result.stdout)
        records.append({'case': case, 'expected': expected, 'actual': result.returncode})
        assert result.returncode == expected, (case, expected, result.returncode, result.stdout[-1000:])
        text = result.stdout.decode(errors='replace')
        if case == 'healthy':
            assert 'last stats line after soak: connection active 0.1s  BTC' in text
            assert '\x1b[?2004l\x1b[?25h\x1b[?1049l' in text
            assert 'terminal settings restored\r\n' in text
        elif case in ('stale', 'disconnected'):
            assert f'BLOCKED: {case} connection during the soak window' in text
        elif case == 'restoration_failure':
            assert 'BLOCKED: terminal settings were not restored' in text
(evidence / 'results.json').write_text(json.dumps(records, indent=2) + '\n')
print(json.dumps(records))
