#!/usr/bin/env python3
# Copyright  :  (C) 2026, QBayLogic B.V.
# License    :  BSD2 (see the file LICENSE)
"""Do two VCD files show the same waveform?

usage: same-waveform.py A.vcd B.vcd

Compares by meaning, not text: each var by its full scope path, as the
changes of its value over time in ps (identifier codes, timescales and
declaration order may differ). Exits 0 when they are the same; otherwise
prints the first differences and exits 1. check.sh uses it to compare an FST,
converted by GTKWave's fst2vcd, with the VCD of the same recording.
"""
import re
import sys
from collections import defaultdict

UNITS = {"s": 10**12, "ms": 10**9, "us": 10**6, "ns": 10**3, "ps": 1}


def parse(path):
    scale = 1
    scopes = []
    ids = defaultdict(list)
    widths = {}
    changes = defaultdict(list)
    current = {}
    time = 0
    with open(path) as f:
        tokens = iter(f.read().split())
    definitions = True
    for tok in tokens:
        if definitions:
            if tok == "$timescale":
                text = "".join(iter(lambda: next(tokens), "$end"))
                m = re.match(r"(\d+)(\w+)", text)
                scale = int(m.group(1)) * UNITS[m.group(2)]
            elif tok == "$scope":
                next(tokens)
                scopes.append(next(tokens))
                next(tokens)
            elif tok == "$upscope":
                scopes.pop()
                next(tokens)
            elif tok == "$var":
                next(tokens)
                width, code, name = int(next(tokens)), next(tokens), next(tokens)
                for t in iter(lambda: next(tokens), "$end"):
                    pass
                full = ".".join(scopes + [name])
                ids[code].append(full)
                widths[full] = width
            elif tok == "$enddefinitions":
                next(tokens)
                definitions = False
            elif tok in ("$date", "$version", "$comment"):
                for t in iter(lambda: next(tokens), "$end"):
                    pass
            continue
        if tok in ("$dumpvars", "$end", "$dumpall", "$dumpon", "$dumpoff"):
            continue
        if tok.startswith("#"):
            time = int(tok[1:]) * scale
            continue
        if tok[0] in "bB":
            value, code = tok[1:], next(tokens)
        else:
            value, code = tok[0], tok[1:]
        for full in ids[code]:
            width = widths[full]
            if len(value) < width:
                value = value.rjust(width, "0" if value[0] == "1" else value[0])
            if current.get(full) != value:
                changes[full].append((time, value))
                current[full] = value
    return changes


def main():
    a, b = parse(sys.argv[1]), parse(sys.argv[2])
    problems = []
    for path in sorted(set(a) | set(b)):
        if path not in a or path not in b:
            problems.append(f"{path}: only in {sys.argv[1] if path in a else sys.argv[2]}")
        elif a[path] != b[path]:
            first = next((i for i, (x, y) in enumerate(zip(a[path], b[path])) if x != y), None)
            problems.append(f"{path}: differs at change {first}")
    for p in problems[:10]:
        print(p)
    sys.exit(1 if problems else 0)


if __name__ == "__main__":
    main()
