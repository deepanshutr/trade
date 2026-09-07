#!/usr/bin/env python3
"""Count committed Scala source twice and report each recorded task delta."""

import argparse
import io
import json
from pathlib import Path
import subprocess
import tarfile
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("repository")
    parser.add_argument("--base", default="a02c11b")
    parser.add_argument("--task", action="append", default=[], help="label=commit")
    args = parser.parse_args()

    def git(*command):
        return subprocess.check_output(["git", *command], cwd=args.repository)

    def count(revision):
        names = git("ls-tree", "-r", "--name-only", revision, "--", "src").decode().splitlines()
        names = [name for name in names if name.endswith(".scala")]
        rows = []
        for name in names:
            raw = git("show", revision + ":" + name)
            lines = raw.decode().splitlines()
            assert raw.endswith(b"\n"), name
            rows.append({"path": name, "physical": len(lines),
                         "nonempty": sum(bool(line.strip()) for line in lines)})
        primary = {key: sum(row[key] for row in rows) for key in ("physical", "nonempty")}
        with tempfile.TemporaryDirectory(prefix="desk-source-count-") as directory:
            paths = []
            with tarfile.open(fileobj=io.BytesIO(git("archive", revision, "src"))) as archive:
                for member in archive:
                    if member.isfile() and member.name.endswith(".scala"):
                        path = Path(directory) / member.name
                        path.parent.mkdir(parents=True, exist_ok=True)
                        path.write_bytes(archive.extractfile(member).read())
                        paths.append(path)
            assert sorted(str(p.relative_to(directory)) for p in paths) == sorted(names)
            physical = sum(int(subprocess.check_output(["wc", "-l", str(p)]).split()[0]) for p in paths)
            nonempty = int(subprocess.check_output(["awk", "NF {n++} END {print n}", *map(str, paths)]))
            assert primary == {"physical": physical, "nonempty": nonempty}
        return {"revision": git("rev-parse", revision).decode().strip(), "files": len(names),
                **primary, "by_file": rows, "independent_count_agrees": True}

    baseline = count(args.base)
    assert baseline["physical"] == 3513 and baseline["nonempty"] == 3248, "unexpected cleanup baseline"
    result = {"baseline": baseline, "tasks": []}
    previous, previous_count = args.base, baseline
    for task in args.task:
        label, revision = task.split("=", 1)
        current = count(revision)
        diff = git("diff", "--no-renames", "--numstat", previous, revision, "--", "src").decode().splitlines()
        changes = [line.split("\t", 2) for line in diff if line.endswith(".scala")]
        added = sum(int(parts[0]) for parts in changes)
        removed = sum(int(parts[1]) for parts in changes)
        assert current["physical"] - previous_count["physical"] == added - removed
        result["tasks"].append({"task": label, "added": added, "removed": removed,
                                "physical_delta": added - removed,
                                "nonempty_delta": current["nonempty"] - previous_count["nonempty"],
                                "count": current})
        previous, previous_count = revision, current
    result["final"] = previous_count
    result["net_physical"] = previous_count["physical"] - baseline["physical"]
    result["net_nonempty"] = previous_count["nonempty"] - baseline["nonempty"]
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
