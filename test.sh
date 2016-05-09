#!/usr/bin/env bash

if ! [[ -d deps ]]; then
    mkdir deps;
fi
if ! [[ -d deps/dash ]]; then
    git clone https://github.com/magnars/dash.el deps/dash;
fi
if ! [[ -d deps/powerline ]]; then
    git clone https://github.com/milkypostman/powerline deps/powerline;
fi

emacs -batch \
      -L deps/dash -L deps/powerline \
      -l ert \
      -l spaceline.el \
      -l tests.el \
      -f ert-run-tests-batch-and-exit
