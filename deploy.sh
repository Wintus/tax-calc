#!/usr/bin/env bash

set -eu

grep homepage elmapp.config.js

elm-app build
gh-pages -d build
