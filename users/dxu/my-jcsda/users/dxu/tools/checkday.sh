#!/bin/bash

grep segmentation $@
grep abort $@
grep unexpected $@
grep 'not found' $@
grep missing $@

