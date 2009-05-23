out/diplomamunka.pdf: *.tex *.sty
	latex -file-line-error -halt-on-error -output-directory=aux diplomamunka.tex
	dvipdfm -o out/diplomamunka.pdf aux/diplomamunka.dvi

all: *.tex *.sty
	latex -file-line-error -halt-on-error -output-directory=aux diplomamunka.tex
	echo -en '\n\n\n'
	latex -file-line-error -halt-on-error -output-directory=aux diplomamunka.tex
	dvipdfm -o out/diplomamunka.pdf aux/diplomamunka.dvi

pub:
	mv out/diplomamunka.pdf out/diplomamunka_nezheto.pdf

clean:
	rm -f aux/*
