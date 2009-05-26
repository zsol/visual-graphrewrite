vpath %.dot figs
vpath %.eps eps

DOTFLAGS := -Gcharset=latin1

eps = eltecimer.eps
figs = arch.dot

all: aux/diplomamunka.dvi
	latex -file-line-error -halt-on-error -output-directory=aux diplomamunka.tex
	mkdir -p out
	dvipdfm -o out/diplomamunka.pdf aux/diplomamunka.dvi

aux/diplomamunka.dvi: eps aux *.tex *.sty
	latex -file-line-error -halt-on-error -output-directory=aux diplomamunka.tex

aux:
	mkdir -p aux

%.eps : %.dot
	dot $(DOTFLAGS) -Teps $< -o eps/$@

eps: $(eps) $(patsubst %.dot,%.eps, $(figs)) $(figs)

clean:
	rm -rf aux/

distclean: clean
	rm -rf out/

.PHONY: all clean distclean
