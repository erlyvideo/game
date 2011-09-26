#!/bin/sh
erl -name game -pa ebin -pa deps/*/ebin \
	-boot start_sasl -s game
