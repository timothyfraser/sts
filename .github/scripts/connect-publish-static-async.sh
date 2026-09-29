#!/usr/bin/env bash
# connect-publish-static-async.sh — upload a STATIC site bundle to Posit Connect,
# start the deploy, watch it only BRIEFLY, then detach.
#
# WHY THIS EXISTS (2026-08-21):
# deploy-connect.yml used to call `rsconnect deploy html`, which does
# upload -> deploy -> and then BLOCKS in wait_for_task() until Connect finishes
# unpacking and activating the bundle, and then makes a verification request
# against the deployed URL. Every second of that is a GitHub Actions minute
# spent watching a server do work it will do whether we watch or not. The
# render belongs on Connect; the WAITING does not.
#
# Ending the poll does NOT cancel anything: the deploy task is server-side and
# GET /v1/tasks/{id} is read-only. We poll for POLL_BUDGET_SECONDS only to
# catch IMMEDIATE failures (bad manifest, rejected bundle, missing entrypoint),
# then detach with the task id logged for follow-up in Connect.
#
# WHY NOT rsconnect-python (checked against rsconnect-python 1.30.0, the
# version this workflow used to `pip install`):
#   * `rsconnect deploy html --help` exposes no --no-wait / --detach / --async
#     flag. Its only nearby option, `--no-verify`, skips the post-deploy HTTP
#     check of the deployed URL, NOT the wait — rsconnect/api.py still calls
#     client.wait_for_task(...) on the deploy task either way.
#   * `rsconnect write-manifest` has subcommands for quarto/notebook/api/...
#     but NOT for html, so there is no supported way to make rsconnect emit a
#     static manifest.json and stop.
# So the CLI cannot detach, and the bundle+API route below is the port of
# cpportal's .github/scripts/connect-publish-async.sh.
#
# The manifest this script writes is byte-for-byte the shape rsconnect itself
# produces for `deploy html` (rsconnect/bundle.py: create_html_manifest ->
# make_html_manifest, appmode "static", per-file md5 checksums):
#   {"version":1,
#    "metadata":{"appmode":"static","primary_html":"<e>","entrypoint":"<e>"},
#    "files":{"<rel path>":{"checksum":"<md5 hex>"}, ...}}
#
# API sequence (Connect Server API v1):
#   1. POST /v1/content/{guid}/bundles   body: bundle tar.gz (application/gzip)
#      -> { "id": "<bundle_id>", ... }
#   2. POST /v1/content/{guid}/deploy    body: {"bundle_id": "<bundle_id>"}
#      -> { "task_id": "<task_id>" }
#   3. GET  /v1/tasks/{task_id}?wait=N&first=M   (bounded loop)
#      -> { "output": [...], "last": int, "finished": bool, "code": int, "error": "..." }
#
# Required env:
#   CONNECT_SERVER      e.g. https://connect.systems-apps.com (trailing slash ok)
#   CONNECT_API_KEY     Connect API key (never echoed)
#   CONTENT_GUID        target content GUID — a wrong GUID here overwrites
#                       whatever content it names.
#   BUNDLE_DIR          directory of static files to ship (e.g. docs-v3)
# Optional env:
#   ENTRYPOINT          landing page, relative to BUNDLE_DIR (default index.html)
#   POLL_BUDGET_SECONDS how long to watch the deploy task before detaching
#                       (default 120; the task keeps running either way)
#   POLL_WAIT_SECONDS   long-poll window per GET /tasks call (default 10) —
#                       this is the poll interval; Connect returns early when
#                       there is new output or the task ends.
#
# Outputs (GITHUB_OUTPUT): deploy_status=succeeded|detached, task_id, bundle_id
#
# Exit codes:
#   0 = deploy finished successfully within budget (deploy_status=succeeded),
#       OR detached cleanly with the task still running on Connect
#       (deploy_status=detached). The log says which, in words.
#   1 = definite failure (missing entrypoint, upload rejected, deploy rejected,
#       task finished with a nonzero code, bad config)
set -euo pipefail

: "${CONNECT_SERVER:?CONNECT_SERVER is required}"
: "${CONNECT_API_KEY:?CONNECT_API_KEY is required}"
: "${CONTENT_GUID:?CONTENT_GUID is required}"
: "${BUNDLE_DIR:?BUNDLE_DIR is required}"
ENTRYPOINT="${ENTRYPOINT:-index.html}"
POLL_BUDGET_SECONDS="${POLL_BUDGET_SECONDS:-120}"
POLL_WAIT_SECONDS="${POLL_WAIT_SECONDS:-10}"

SERVER="${CONNECT_SERVER%/}"
AUTH=(-H "Authorization: Key ${CONNECT_API_KEY}")
WORKDIR="$(mktemp -d)"
# The manifest is written INTO BUNDLE_DIR (that is where a Connect bundle
# expects it, at the archive root) and removed again on exit so the checkout
# is left as we found it.
trap 'rm -rf "$WORKDIR"; rm -f "${BUNDLE_DIR%/}/manifest.json"' EXIT

fail() { echo "❌ $*" >&2; exit 1; }

# set -e safe: `[ -n "$X" ] && echo ...` as a statement exits the script when
# the test is false. Use a function with an explicit if instead.
emit() {
  if [ -n "${GITHUB_OUTPUT:-}" ]; then
    printf '%s\n' "$1" >> "$GITHUB_OUTPUT"
  fi
}

command -v jq >/dev/null || fail "jq is required"
command -v python3 >/dev/null || fail "python3 is required"
[ -d "$BUNDLE_DIR" ] || fail "BUNDLE_DIR '${BUNDLE_DIR}' is not a directory"
[ -f "${BUNDLE_DIR%/}/${ENTRYPOINT}" ] \
  || fail "entrypoint '${ENTRYPOINT}' not found in ${BUNDLE_DIR}"

# ------------------------------------------------- 1. manifest + bundle tar --
# One python3 pass: walk BUNDLE_DIR, md5 every file, write manifest.json into
# BUNDLE_DIR and a NUL-separated file list for tar (NUL-separated so spaces and
# other odd characters in filenames cannot split a path, and so 1000+ files
# never approach ARG_MAX).
LIST_PATH="${WORKDIR}/files.nul"
BUNDLE_DIR="$BUNDLE_DIR" ENTRYPOINT="$ENTRYPOINT" LIST_PATH="$LIST_PATH" \
python3 - <<'PY'
import hashlib, json, os, sys

bundle_dir = os.environ["BUNDLE_DIR"].rstrip("/\\")
entrypoint = os.environ["ENTRYPOINT"]
list_path = os.environ["LIST_PATH"]

# Mirrors rsconnect's own exclusions: manifest.json at the bundle root is
# regenerated, never shipped from disk; VCS/cache dirs are not content.
# Verified 2026-08-21 against rsconnect 1.30.0's create_html_manifest() on
# docs-v3: identical metadata and identical md5 for all 1203 files. The only
# difference is that rsconnect also bundled
# data/functions/__pycache__/functions_models.cpython-312.pyc, which SKIP_DIRS
# drops — a compiled cache file has no business in a static site bundle.
SKIP_DIRS = {".git", ".github", "__pycache__", ".ipynb_checkpoints", ".quarto"}

def md5(path):
    h = hashlib.md5()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()

files = {}
for root, dirs, names in os.walk(bundle_dir):
    dirs[:] = sorted(d for d in dirs if d not in SKIP_DIRS)
    for name in sorted(names):
        abs_path = os.path.join(root, name)
        rel = os.path.relpath(abs_path, bundle_dir).replace(os.sep, "/")
        if rel == "manifest.json":
            continue
        files[rel] = {"checksum": md5(abs_path)}

if not files:
    sys.exit("no files found under %s" % bundle_dir)
if entrypoint not in files:
    sys.exit("entrypoint %r is not among the bundled files" % entrypoint)

manifest = {
    "version": 1,
    "metadata": {
        "appmode": "static",
        "primary_html": entrypoint,
        "entrypoint": entrypoint,
    },
    "files": files,
}
with open(os.path.join(bundle_dir, "manifest.json"), "w", encoding="utf-8") as fh:
    json.dump(manifest, fh, indent=2)

# manifest.json first, then every content file, NUL-separated for `tar -T -`.
with open(list_path, "wb") as fh:
    for rel in ["manifest.json"] + sorted(files):
        fh.write(rel.encode("utf-8") + b"\0")

# ASCII only: python's stdout encoding is not guaranteed to be UTF-8.
print("manifest: %d files, entrypoint %s" % (len(files), entrypoint))
PY

TARBALL="${WORKDIR}/bundle.tar.gz"
# -h dereferences symlinked files so what ships matches the checksum we hashed.
tar -czhf "$TARBALL" -C "${BUNDLE_DIR%/}" --null --files-from "$LIST_PATH"
echo "📦 bundle: $(wc -c < "$TARBALL") bytes"

# ---------------------------------------------------------------- 2. upload --
UP_BODY="${WORKDIR}/upload.json"
UP_CODE=$(curl -sS -o "$UP_BODY" -w '%{http_code}' \
  -X POST "${SERVER}/__api__/v1/content/${CONTENT_GUID}/bundles" \
  "${AUTH[@]}" -H "Content-Type: application/gzip" \
  --data-binary @"$TARBALL") || fail "curl could not reach ${SERVER} to upload the bundle"
[ "$UP_CODE" = "200" ] || [ "$UP_CODE" = "201" ] \
  || fail "bundle upload failed (HTTP ${UP_CODE}): $(cat "$UP_BODY")"
BUNDLE_ID=$(jq -r '.id' "$UP_BODY")
[ -n "$BUNDLE_ID" ] && [ "$BUNDLE_ID" != "null" ] || fail "no bundle id in upload response"
echo "⬆️  uploaded bundle id=${BUNDLE_ID}"
emit "bundle_id=${BUNDLE_ID}"

# ---------------------------------------------------------------- 3. deploy --
DEP_BODY="${WORKDIR}/deploy.json"
DEP_CODE=$(curl -sS -o "$DEP_BODY" -w '%{http_code}' \
  -X POST "${SERVER}/__api__/v1/content/${CONTENT_GUID}/deploy" \
  "${AUTH[@]}" -H "Content-Type: application/json" \
  -d "{\"bundle_id\": \"${BUNDLE_ID}\"}") || fail "curl could not reach ${SERVER} to start the deploy"
[ "$DEP_CODE" = "200" ] || [ "$DEP_CODE" = "202" ] \
  || fail "deploy request failed (HTTP ${DEP_CODE}): $(cat "$DEP_BODY")"
TASK_ID=$(jq -r '.task_id' "$DEP_BODY")
[ -n "$TASK_ID" ] && [ "$TASK_ID" != "null" ] || fail "no task_id in deploy response"
echo "🚀 deploy task started: ${TASK_ID}"
emit "task_id=${TASK_ID}"

# ----------------------------------------------- 4. bounded watch, then detach
DEADLINE=$(( $(date +%s) + POLL_BUDGET_SECONDS ))
FIRST=0
while [ "$(date +%s)" -lt "$DEADLINE" ]; do
  T_BODY="${WORKDIR}/task.json"
  T_CODE=$(curl -sS -o "$T_BODY" -w '%{http_code}' \
    "${SERVER}/__api__/v1/tasks/${TASK_ID}?wait=${POLL_WAIT_SECONDS}&first=${FIRST}" \
    "${AUTH[@]}")
  if [ "$T_CODE" != "200" ]; then
    echo "⚠️  task poll HTTP ${T_CODE} (transient?) — continuing" >&2
    sleep 5
    continue
  fi
  jq -r '.output[]?' "$T_BODY" | sed 's/^/   connect> /'
  NEXT=$(jq -r '.last // 0' "$T_BODY")
  if [ "$(jq -r '.finished' "$T_BODY")" = "true" ]; then
    CODE=$(jq -r '.code' "$T_BODY")
    if [ "$CODE" = "0" ]; then
      echo "✅ deploy task ${TASK_ID} finished successfully within the watch window."
      emit "deploy_status=succeeded"
      exit 0
    fi
    fail "deploy task ${TASK_ID} FAILED (code=${CODE}): $(jq -r '.error // "see output above"' "$T_BODY")"
  fi
  # Guard against a hot loop if Connect returns instantly with no new output.
  if [ "$NEXT" = "$FIRST" ]; then sleep 1; fi
  FIRST="$NEXT"
done

echo "⏱️  DETACHING after ${POLL_BUDGET_SECONDS}s: task ${TASK_ID} is still running on Connect."
echo "   This is the designed fire-and-forget behavior, NOT a failure — Connect"
echo "   finishes unpacking and activating the bundle server-side whether or not"
echo "   this runner is watching, and watching costs Actions minutes."
echo "   Follow it in Connect: ${SERVER}/connect/#/apps/${CONTENT_GUID}"
emit "deploy_status=detached"
exit 0
