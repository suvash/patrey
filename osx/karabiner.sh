#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set remap.controlL2controlL_escape 1
/bin/echo -n .
$cli set repeat.initial_wait 30
/bin/echo -n .
$cli set repeat.wait 10
/bin/echo -n .
/bin/echo
