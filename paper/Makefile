
DVI = dpa.dvi

all: $(DVI)

$(DVI) : dpa.tex plots.tex srccode.tex
	latex $<

bib:
	: dpa.bib
	bibtex dpa

force:
	touch *.tex; make all

clean:
	$(RM) *.dvi *.log *.aux *.toc *.lof *.lot *.ps *.blg *.bbl

ps: $(DVI)
	dvips -f dpa.dvi > dpa.ps

dpa.ps: ps

print: dpa.ps
	lpr -Ppsrich $<
