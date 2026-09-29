"""The firewall, checked on what actually gets published.

This repo publishes docs-v3/ as a public website with no login. Nothing from
the private instructor repo may appear there (not its name, not its private
course paths, not solutions, answer keys or rubrics), and no link that only
works for, or should only be seen by, enrolled students (meeting passcodes,
booking pages, surveys, forms, forum, cloud projects, lecture recordings,
shared docs, LMS course-internal URLs, the retired legacy branch).

Adversarial first: every rule has a fixture page under
tests/fixtures/firewall/ that MUST fail the scanner (and trip only that rule);
clean-prose.html MUST pass, proving words like "solution" and "rubric" in
ordinary prose are not false positives. Then every text file under docs-v3/
must pass, and no published file NAME may carry a forbidden token.

The patterns here are generic by design. Never add a specific id, passcode,
URL path or person's name to this file.
"""
from __future__ import annotations

import pathlib
import re

import pytest

ROOT = pathlib.Path(__file__).resolve().parents[1]
SITE = ROOT / "docs-v3"
FIXTURES = ROOT / "tests" / "fixtures" / "firewall"

I = re.IGNORECASE


def _file_word(word: str) -> re.Pattern:
    """`word` used as a path / file / identifier, not as an English word.

    Matches hw1_solution.html, solutions/, answers-solution.Rmd, and any
    href/src/data-* attribute value containing the word. Does NOT match
    "a solution to the join problem" or "the rubric rewards reasoning".
    """
    w = word + "s?"
    return re.compile(
        rf"(?<![A-Za-z]){w}(?=[_\-./][A-Za-z0-9])"          # solution_x, solution.html, solutions/
        rf"|(?<=[A-Za-z0-9][_\-/.]){w}(?![A-Za-z])"          # hw1_solution, grading/rubric
        rf"|(?:href|src|action|data-[\w-]+)\s*=\s*[\"'][^\"']*{w}",  # inside a link/attribute
        I,
    )


# rule name -> compiled pattern. One fixture per rule (fNN-*.html, see RULE_FIXTURE).
RULES: dict[str, re.Pattern] = {
    # --- the private instructor repo -------------------------------------
    "instructor_repo": re.compile(r"sysen_instructors", I),
    "private_course_path": re.compile(r"courses/sts/", I),
    "solution": _file_word("solution"),
    "answer_key": re.compile(r"answer[_\-]?key", I),
    "rubric": _file_word("rubric"),
    # --- enrolled-students-only links (public-site hazards) --------------
    "zoom_passcode": re.compile(r"zoom\.us/(?:j|w|s|my)/[^\s\"'<>]*pwd=", I),
    "google_booking": re.compile(
        r"calendar\.app\.google/|calendar\.google\.com/calendar/(?:u/\d+/)?(?:appointments|selfsched)", I),
    "qualtrics": re.compile(r"\.qualtrics\.com/", I),
    "google_forms": re.compile(r"docs\.google\.com/forms/|forms\.gle/", I),
    "ed_discussion": re.compile(r"edstem\.org/[a-z]{2}/courses/", I),
    "posit_cloud": re.compile(r"posit\.cloud/content/", I),
    "panopto": re.compile(r"panopto\.(?:com|eu)/", I),
    "google_docs_drive": re.compile(
        r"docs\.google\.com/(?:document|spreadsheets|presentation|file|drawings)/|drive\.google\.com/", I),
    "canvas_course_url": re.compile(
        r"(?:canvas\.[\w.-]+|[\w-]+\.instructure\.com)/(?:api/v1/)?courses/\d+", I),
    "canvas_secure_params": re.compile(r"secure_params", I),
    "legacy_branch": re.compile(r"/(?:tree|blob|raw)/3week\b", I),
}

RULE_FIXTURE = {
    "instructor_repo": "f01-instructor-repo.html",
    "private_course_path": "f02-private-course-path.html",
    "solution": "f03-sol-file.html",
    "answer_key": "f04-key-file.html",
    "rubric": "f05-grading-file.html",
    "zoom_passcode": "f06-video-passcode.html",
    "google_booking": "f07-booking-link.html",
    "qualtrics": "f08-survey-link.html",
    "google_forms": "f09-form-link.html",
    "ed_discussion": "f10-forum-link.html",
    "posit_cloud": "f11-cloud-project.html",
    "panopto": "f12-lecture-video.html",
    "google_docs_drive": "f13-shared-doc.html",
    "canvas_course_url": "f14-lms-course-url.html",
    "canvas_secure_params": "f15-lms-secure-params.html",
    "legacy_branch": "f16-legacy-branch.html",
}

TEXT_SUFFIXES = {".html", ".htm", ".js", ".mjs", ".css", ".json", ".md",
                 ".txt", ".csv", ".tsv", ".py", ".r", ".R", ".Rmd", ".qmd",
                 ".svg", ".xml", ".yaml", ".yml"}


def scan(text: str) -> dict[str, list[str]]:
    """Return {rule: [matched snippets]} for every rule the text trips."""
    hits: dict[str, list[str]] = {}
    for name, pat in RULES.items():
        found = sorted({m.group(0) for m in pat.finditer(text)})
        if found:
            hits[name] = found
    return hits


def site_files():
    for p in sorted(SITE.rglob("*")):
        if p.is_file() and p.suffix in TEXT_SUFFIXES:
            yield p


# ---------------------------------------------------------------- adversarial
def test_every_rule_has_a_fixture():
    assert set(RULE_FIXTURE) == set(RULES), (
        f"FIREWALL: rules without a fixture {sorted(set(RULES) - set(RULE_FIXTURE))}, "
        f"fixtures without a rule {sorted(set(RULE_FIXTURE) - set(RULES))}")
    for rule, name in RULE_FIXTURE.items():
        assert (FIXTURES / name).is_file(), f"FIREWALL: fixture for {rule!r} missing: {FIXTURES / name}"


@pytest.mark.parametrize("rule", sorted(RULE_FIXTURE))
def test_fixture_trips_its_rule(rule):
    path = FIXTURES / RULE_FIXTURE[rule]
    hits = scan(path.read_text(encoding="utf-8"))
    assert hits, f"FIREWALL SCANNER BLIND: {path.name} names a {rule!r} string but the scanner passed it"
    assert set(hits) == {rule}, (
        f"FIREWALL: {path.name} should trip only {rule!r}, tripped {sorted(hits)}: {hits}")


def test_clean_prose_fixture_passes():
    path = FIXTURES / "clean-prose.html"
    hits = scan(path.read_text(encoding="utf-8"))
    assert not hits, f"FIREWALL FALSE POSITIVE: {path.name} is legitimate prose but tripped {hits}"


# ---------------------------------------------------------------- the real site
def test_site_exists():
    assert SITE.is_dir(), f"published site folder missing: {SITE}"


@pytest.mark.parametrize("path", list(site_files()), ids=lambda p: str(p.relative_to(ROOT)))
def test_published_file_carries_nothing_private(path):
    text = path.read_text(encoding="utf-8", errors="replace")
    hits = scan(text)
    assert not hits, (
        f"FIREWALL: {path.relative_to(ROOT)} contains forbidden content {hits}. "
        "Nothing private and no enrolled-only link may be published; remove it.")


def test_published_file_names_carry_nothing_private():
    bad = {}
    for p in SITE.rglob("*"):
        rel = str(p.relative_to(SITE)).replace("\\", "/")
        hits = scan(rel)
        if hits:
            bad[rel] = hits
    assert not bad, f"FIREWALL: forbidden tokens in published file names: {bad}"
