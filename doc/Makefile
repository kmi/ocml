all: ocml.pdf ocml.html

ocml.pdf: ocml.texi
	texi2pdf ocml.texi

ocml.html: ocml.texi
	texi2html ocml.texi

clean:
	rm -f ocml.fo
	rm -f ocml.html
	rm -f ocml.pdf
