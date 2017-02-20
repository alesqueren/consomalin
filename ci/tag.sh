#!/bin/bash
set -e

git tag -fa cicd -m "cicd ok"
git push origin :refs/tags/cicd
git push --tags
