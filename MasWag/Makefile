../template.pdf: introduction.satyh timedAutomata.satyh membership.satyh conclusion.satyh bib.satyh local.satyh
	make -C ..

bib.satyh: bib.xml ../dblp2satysfi/dblp2satysfi.xsl Makefile
	xsltproc ../dblp2satysfi/dblp2satysfi.xsl $< | nkf --numchar-input -w | sed '2s/bibliography/maswag-bibliography/' | sed '1i @import: local' > $@

deploy: ~/Library/Mobile\ Documents/com~apple~CloudDocs/template.pdf

~/Library/Mobile\ Documents/com~apple~CloudDocs/template.pdf: ../template.pdf
	cp "$<" "$@"
