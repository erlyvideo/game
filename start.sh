#!/bin/sh
erl -sname game -pa ebin -pa deps/*/ebin \
	-boot start_sasl -s game
