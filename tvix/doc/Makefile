all: build

puml:
	plantuml *.puml -tlatex:nopreamble

pdf:
	pandoc *.md -f markdown -t latex -o tvix.pdf --csl=${CSL}

build: puml pdf

clean:
	rm -f *.tex *.pdf *.png *.svg
