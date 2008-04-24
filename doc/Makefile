all: ocml.pdf ocml.html

ocml.pdf: ocml.texi
	texi2pdf ocml.texi

ocml.html: ocml.texi
	texi2html ocml.texi

clean:
	rm -f ocml.aux ocml.cp ocml.fn ocml.fns ocml.fo ocml.ky ocml.log
	rm -f ocml.pg ocml.tp ocml.toc ocml.vr
	rm -f ocml.html
	rm -f ocml.pdf
