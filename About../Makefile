#!/bin/make

SHELL	= /bin/sh

all: pdf
pdf: gsisolver.pdf

gsisolver.ps: gsisolver.dvi
	dvips gsisolver.dvi

gsisolver.pdf: gsisolver.dvi
	dvipdf gsisolver.dvi

GSISOLVER_TEX	= gsisolver.tex		\
		  symbols.tex

ALGO_TEX	= algo-glbsoi-code.tex	\
		  algo-pcgsoi-code.tex	\
		  algo-pcgsoi-math.tex

ALGO_EPS	= $(ALGO_TEX:.tex=.eps)

.SUFFIXES:
.SUFFIXES: .tex .eps .pdf .ps

.tex.eps:
	latex $*.tex
	dvips -E -o $*.eps $*.dvi
	rm $*.dvi $*.aux $*.log

gsisolver.dvi: ${GSISOLVER_TEX} $(EPS)
	latex gsisolver.tex
	latex gsisolver.tex

eps: $(ALGO_EPS)

clean:
	rm -f *.ps *.pdf *.xdvi *.aux *.log
