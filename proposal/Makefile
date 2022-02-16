
DVI = proposal.dvi

all: $(DVI)

$(DVI) : proposal.tex
	latex $<

force:
	touch *.tex; make all

clean:
	$(RM) *.dvi *.log *.aux *.toc *.lof *.lot *.ps

ps: $(DVI)
	dvips -f proposal.dvi > proposal.ps

proposal.ps: ps

print: proposal.ps
	lpr -Ppsrich $<
