# tools/ledger.py
# Firewall gate for the PUBLIC STS course repo (ported from the sister course's
# ledger; only the firewall half is kept, see bottom of file for what was dropped).
#
#   python tools/ledger.py firewall          # path + branch check, then tests/test_firewall.py
#   python tools/ledger.py firewall --paths-only
#   python tools/ledger.py forbidden         # print the forbidden path tokens and frozen branches
#
# Exit 0 = clean. Any refusal prints "LEDGER REFUSED: ..." and exits 1.
# There is no --force.

import argparse
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

# Substrings that must never appear in a path added to this PUBLIC repo.
# Content (not just paths) under docs-v3/ is scanned by tests/test_firewall.py,
# which also bans enrolled-only links (meeting passcodes, forms, LMS URLs, ...).
FORBIDDEN = ("sysen_instructors", "courses/sts/", "solution", "answer_key",
             "answer-key", "answerkey", "rubric")

# Paths that look forbidden but are explicitly allowed. Checked before FORBIDDEN.
# Empty on purpose: add an entry only with a ruling, never to make a check pass.
FIREWALL_ALLOW: tuple = ()

# Frozen / legacy branches. Course work never happens on these.
FROZEN_BRANCHES = ("v2025", "v2026", "3week")


def die(msg, code=1):
    print(f"LEDGER REFUSED: {msg}")
    sys.exit(code)


def git(*args):
    r = subprocess.run(["git", *args], capture_output=True, text=True, cwd=ROOT)
    if r.returncode != 0:
        die(f"git {' '.join(args)} failed: {r.stderr.strip()}")  # fail closed
    return r.stdout


def path_violations(paths):
    bad = []
    for raw in paths:
        p = raw.strip().strip('"').replace("\\", "/").lower()
        if not p or any(a in p for a in FIREWALL_ALLOW):
            continue
        hits = [t for t in FORBIDDEN if t in p]
        if hits:
            bad.append(f"{raw.strip()}  <- {hits}")
    return bad


def firewall_check(paths_only=False):
    # 1. every changed/untracked path, plus every tracked path under docs-v3/
    changed = [line[3:] for line in git("status", "--porcelain", "--untracked-files=all").splitlines()]
    published = git("ls-files", "docs-v3").splitlines()
    bad = path_violations(changed + published)
    if bad:
        die("firewall: forbidden paths:\n  " + "\n  ".join(sorted(set(bad))))
    # 2. branch
    branch = git("branch", "--show-current").strip()
    if branch in FROZEN_BRANCHES:
        die(f"firewall: you are on {branch!r}, a frozen branch. Course work never happens there.")
    print(f"firewall paths: OK ({len(changed)} changed, {len(published)} published; branch {branch or '(detached)'})")
    if paths_only:
        return
    # 3. content scan of the published site
    r = subprocess.run([sys.executable, "-X", "utf8", "-P", "-m", "pytest", "tests/test_firewall.py", "-q"],
                       capture_output=True, text=True, cwd=ROOT)
    tail = (r.stdout + r.stderr).strip().splitlines()[-25:]
    if r.returncode != 0:
        die("firewall: tests/test_firewall.py failed:\n" + "\n".join(tail))
    print("firewall content: OK (" + (tail[-1] if tail else "no output") + ")")


def cmd_firewall(args):
    firewall_check(paths_only=args.paths_only)


def cmd_forbidden(_):
    print("forbidden path tokens: " + ", ".join(FORBIDDEN))
    print("allowlist:             " + (", ".join(FIREWALL_ALLOW) or "(empty)"))
    print("frozen branches:       " + ", ".join(FROZEN_BRANCHES))


def main():
    ap = argparse.ArgumentParser(description="Firewall gate for the public STS course repo.")
    sub = ap.add_subparsers(dest="cmd", required=True)
    s = sub.add_parser("firewall", help="refuse forbidden paths, frozen branches, and forbidden site content")
    s.add_argument("--paths-only", action="store_true", help="skip the pytest content scan")
    sub.add_parser("forbidden", help="print the forbidden tokens and frozen branches")
    args = ap.parse_args()
    {"firewall": cmd_firewall, "forbidden": cmd_forbidden}[args.cmd](args)


if __name__ == "__main__":
    main()

# Dropped from the donor on purpose: the chapter checkout/checkin/release/note
# state machine and its gate commands (verify_code.py, validate_contract.py,
# check_links.py, extract_page_code.py do not exist in this repo yet). Re-add
# them as a ledger task when those gates land; keep firewall_check() as the
# first step of checkin.
