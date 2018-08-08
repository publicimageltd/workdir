CASK ?= cask

all: test compile

test: clean-elc
	${CASK} exec buttercup -L .

clean-elc:
	${CASK} clean-elc

compile:
	${CASK} build


.PHONY: all test clean-elc compile
