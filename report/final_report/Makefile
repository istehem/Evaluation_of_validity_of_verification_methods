#!/bin/bash

ifeq ($(LATEX_CMD),)
LATEX_CMD=pdflatex
endif


all: pdf clean

pdf:
	"$(LATEX_CMD)" report
	-makeglossaries report
	-bibtex report
	"$(LATEX_CMD)" report
	"$(LATEX_CMD)" report

clean:
	rm -f *.synctex.gz *.dvi *.log *.out *.gli *.glo *.gls *.ist

cleanAll: clean
	rm -f report.pdf
