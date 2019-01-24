bib.satyh: bib.xml ../dblp2satysfi/dblp2satysfi.xsl Makefile
	xsltproc ../dblp2satysfi/dblp2satysfi.xsl $< | nkf --numchar-input -w | sed '2s/bibliography/maswag-bibliography/' | sed '1i @import: local' > $@
