#!/usr/bin/env python3
"""Render the public market recorder's Mac user-service definition.

This command does not install or start a service. Keep the rendered definition
reviewable before placing it in ~/Library/LaunchAgents.
"""

import argparse
import pathlib
import plistlib
import re
import sys


LABEL = "local.desk.recorder"


def definition(java, jar, state, coin="XMR", config=None):
    paths = [java, jar, state] + ([config] if config else [])
    if not all(pathlib.Path(path).is_absolute() for path in paths):
        raise ValueError("Service paths must be absolute")
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]{0,39}", coin):
        raise ValueError("Invalid market")
    command = [str(java), "-XX:ActiveProcessorCount=2", "-Xmx512m", "-jar", str(jar),
               coin, "--record", "--no-analysis", "--state", str(state)]
    if config:
        command += ["--config", str(config)]
    return {
        "Label": LABEL,
        "ProgramArguments": command,
        "RunAtLoad": True,
        "KeepAlive": True,
        "ThrottleInterval": 30,
        "ExitTimeOut": 15,
        "ProcessType": "Background",
        "LowPriorityIO": True,
        "Nice": 10,
        "Umask": 0o077,
    }


def main():
    home = pathlib.Path.home()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--java", default="/opt/homebrew/opt/openjdk/bin/java")
    parser.add_argument("--jar", default=str(home / ".local/lib/desk.jar"))
    parser.add_argument("--state", default=str(home / ".local/state/desk"))
    parser.add_argument("--coin", default="XMR")
    parser.add_argument("--config")
    args = parser.parse_args()
    try:
        value = definition(args.java, args.jar, args.state, args.coin, args.config)
    except ValueError as error:
        parser.error(str(error))
    sys.stdout.buffer.write(plistlib.dumps(value, sort_keys=False))


if __name__ == "__main__":
    main()
