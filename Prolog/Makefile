sources=$(wildcard *.txt *.md)

print.ps: $(sources)
	a2ps -o $@ --font-size=10 -R --columns=1 $^

print.pdf: print.ps
	ps2pdf $^

format: $(sources)
	cp ../../convert.class .
	java convert $^
	rm convert.class

