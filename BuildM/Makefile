# Build Makefile

SOURCES = *.m

INT = $(SOURCES:.m=.int)
 

.int.m: test:
	mmc --make-short-int utils.m
	mmc --make-short-int lexer.m
	mmc --make-priv-int utils.m
	mmc --make-priv-int lexer.m
	mmc --make-int utils.m
	mmc --make-int lexer.m
	mmc utils.m lexer.m build.m -o build


all : source.pdf $(SOURCES) $(PROJFILE) $(SLNFILE)
	#xbuild $(SLNFILE)

print: source.pdf

source.pdf: source.ps
	ps2pdf source.ps

source.ps: $(SOURCES) format
	a2ps -r --columns=1 $(SOURCES)  -f 8 -o source.ps

format: convert.class
	java convert $(SOURCES)

convert.class: convert.java
	javac convert.java

clean:
	rm -rf *.c
	rm -rf Bin


