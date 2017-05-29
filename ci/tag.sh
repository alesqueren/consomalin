#!/bin/bash
set -e

if [ $# -ne 0 ]; then
  tag_name=$1
else
  tag_name="latest"
fi

git tag -fa $tag_name -m $tag_name
git push origin :refs/tags/$tag_name
git push --tags
