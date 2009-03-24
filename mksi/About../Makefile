#!/bin/make

.SUFFIXES:
.SUFFIXES: .fig .eps .ps

.fig.ps:
	fig2dev -L ps $< $@

.fig.eps:
	fig2dev -L eps $< $@

all: mksi.eps mksi.ps

clean:
	rm -f mksi.eps mksi.ps

#mksi.eps: mksi.fig
#	fig2dev -L eps mksi.fig $@

#.
