SVGAnnotate.pdf:

JSS_DIR=$(HOME)/Papers/JSSstyle

%.pdf: %.tex 
	(export TEXINPUTS=.:$(JSS_DIR):: ; pdflatex $*) 
	(export BSTINPUTS=.:$(JSS_DIR):: ; bibtex $*)
	(export TEXINPUTS=.:$(JSS_DIR):: ; pdflatex $*) 
	(export TEXINPUTS=.:$(JSS_DIR):: ; pdflatex $*) 
