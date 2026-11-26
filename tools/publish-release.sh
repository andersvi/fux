#!/usr/bin/env bash
# Publish a numbered FUX release on GitHub.
# Usage: tools/publish-release.sh v0.1.1 (increment version if necessary...)
set -euo pipefail
cd "$(dirname "$0")/.."

die() { printf '%s\n' "$*" >&2; exit 1; }

version=${1:-}
[[ $# -eq 1 && $version =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]] ||
    die "Usage: $0 v0.1.1"

[[ $(git branch --show-current) == master ]] || die 'Switch to master first.'
[[ -z $(git status --porcelain) ]] || die 'Commit pending changes first.'
command -v gh >/dev/null || die 'Install GitHub CLI (gh) first.'
gh auth status

git fetch origin master --tags
[[ $(git rev-parse HEAD) == $(git rev-parse origin/master) ]] ||
    die 'Local master differs from origin/master. Synchronize first.'
if git show-ref --verify --quiet "refs/tags/$version"; then
    die "Tag $version already exists. Choose a new version."
fi

python3 tools/build.py
for file in dist/fux.lisp dist/fux-public.zip; do
    [[ -s $file ]] || die "Missing or empty file: $file"
done
if [[ -n $(git status --porcelain) ]]; then
    git status --short
    die 'Build changed tracked or untracked files. Review, commit and push, then rerun.'
fi

commit=$(git rev-parse HEAD)
printf '\nRelease: %s\nCommit:  %s\nAssets:\n' "$version" "$commit"
ls -lh dist/fux.lisp dist/fux-public.zip
read -r -p 'Publish release? [y/N] ' answer
[[ $answer == y || $answer == Y ]] || die 'Cancelled.'

gh release create "$version" \
    dist/fux.lisp dist/fux-public.zip \
    --repo andersvi/fux \
    --target "$commit" \
    --title "FUX $version" \
    --generate-notes \
    --latest
