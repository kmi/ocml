source = ocml.xml
fo-stylesheet = /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl

html-stylesheet = /usr/share/xml/docbook/stylesheet/nwalsh/html/docbook.xsl 

docbook-flags = --stringparam  chapter.autolabel 1 \
		 --stringparam  section.autolabel 1 \
		 --stringparam  section.autolabel.max.depth 3 \
		 --stringparam  appendix.autolabel 1 \
		 --stringparam  section.label.includes.component.label 1

fo-docbook-flags = --stringparam paper.type A4 \
			$(docbook-flags)

html-docbook-flags =  \
			$(docbook-flags)



all: ocml.pdf ocml.html

ocml.pdf: ocml.fo
	fop ocml.fo ocml.pdf

ocml.fo: ocml.xml
	xsltproc $(fo-docbook-flags) $(fo-stylesheet)  ocml.xml  > ocml.fo

ocml.html: ocml.xml
	xsltproc $(html-docbook-flags) $(html-stylesheet)  ocml.xml  > ocml.html

clean:
	rm -f ocml.fo
	rm -f ocml.html
	rm -f ocml.pdf
