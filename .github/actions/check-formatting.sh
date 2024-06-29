#!/usr/bin/env bash
set -euo pipefail

treefmt --version

treefmt

if [[ -n "$(git diff --stat)" ]]; then
  git status
  echo "FAIL: found code changes" 
  git diff
  exit 1
fi
