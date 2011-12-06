PACKAGE=SVGAnnotation

include ../../AdminTools/GNUmakefile

TARGET_DIR=www.omegahat.org:OmegaWeb/$(PACKAGE)

SVG_FILES=pairs_link axes_tips axes_hrefs occ_anim

R=$(R_HOME)/bin/R

SVG_FILES=$(shell echo "library(XML); z = htmlParse('examples.html', error = function(...){}); cat(unlist(getNodeSet(z, '//embed/@src')))" | ${R} --slave --vanilla) 

#SVG_FILES=../tests/twopi-highlight.svg ../tests/knn.svg

e:
	@echo "$(SVG_FILES)"

shipExample: 
	scp examples.html $(TARGET_DIR)/tests/

ship:
	scp $(SVG_FILES) examples.html $(TARGET_DIR)/tests
	scp ../inst/CSS/RSVGPlot.css $(TARGET_DIR)

update:
	scp examples.html $(TARGET_DIR)/tests

fix fixExamples:
	for i in $(SVG_FILES) ; do \
	   echo "$$i" ; \
	   ssh eeyore ./OmegaWeb/SVGAnnotation/tests/fix OmegaWeb/SVGAnnotation/tests/$$i ; \
	done

#


