all: out aux/diplomamunka.dvi
	latex -file-line-error -halt-on-error -output-directory=aux diplomamunka.tex
	dvipdfm -o out/diplomamunka.pdf aux/diplomamunka.dvi

aux/diplomamunka.dvi: aux *.tex *.sty
	latex -file-line-error -halt-on-error -output-directory=aux diplomamunka.tex

aux:
	mkdir -p aux

out:
	mkdir -p out

clean:
	rm -rf aux/

distclean: clean
	rm -rf out/

.PHONY: all clean distclean
