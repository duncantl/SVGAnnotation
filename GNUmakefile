PKG_NAME=SVGAnnotation
R=$(R_HOME)/bin/R

build: configure
	$(R) CMD build ../$(PKG_NAME)

install: configure
	$(R) CMD INSTALL .

check: configure
	$(R) CMD check .


configure: configure.in
	autoconf

ifndef XDYN_DOCS
XDYN_DOCS=$(HOME)/Classes/StatComputing/XDynDocs/inst
endif

FAQ_XSL=http://www.omegahat.org/XSL/html/faq.xsl
FAQ_CSS=$(XDYN_DOCS)/CSS/faq.css
FAQ_CSS=../faq.css
FAQ_XSL_ARGS=--nonet --stringparam cssFile $(FAQ_CSS)

FAQ.html: FAQ.xml GNUmakefile
	xsltproc -o $@ $(FAQ_XSL_ARGS) $(FAQ_XSL) $< 


