infofiles=ocml.info

TEXI = ocml.texi

all: ocml.ps ocml.info ocml.html ocml.pdf

ocml.dvi: $(TEXI)
	texi2dvi ocml.texi

ocml.ps: ocml.dvi
	dvips -o $@ $<

ocml.info: $(TEXI)
	makeinfo $<

ocml.html: $(TEXI)
	texi2html --split=section $<

ocml.pdf: $(TEXI)
	texi2pdf $<

clean:
	rm -f ocml.aux
	rm -f ocml.cp
	rm -f ocml.cps
	rm -f ocml.dvi
	rm -f ocml.fn
	rm -f ocml.fns
	rm -f ocml.ky
	rm -f ocml.log
	rm -f ocml.pg
	rm -f ocml.tmp
	rm -f ocml.toc
	rm -f ocml.tp
	rm -f ocml.vr

really-clean: clean
	rm -f ocml.info
	rm -f ocml.pdf
	rm -f ocml.ps
	rm -rf ocml
