#!/bin/bash
set -e

tag_name="latest"

git tag -fa $tag_name -m $tag_name
git push origin :refs/tags/$tag_name
git push --tags
