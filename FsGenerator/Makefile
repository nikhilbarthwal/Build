# Build Makefile

#SOURCES = Generator/*.fs
SOURCES = Notes/*.txt
PROJFILE = Generator/generator.fsproj
SLNFILE = build.sln

all : source.pdf target

target: Build.exe
	mkdir Target/
	mono ./Build.exe > Build.log

Build.exe: $(SOURCES) $(PROJFILE) $(SLNFILE)
	xbuild $(SLNFILE)

print: source.pdf

source.pdf: source.ps
	ps2pdf source.ps

source.ps: $(SOURCES) format
	a2ps -r --columns=1 $(SOURCES)  -f 8 -o source.ps

format: $(SOURCES) convert.class
	java convert $(SOURCES)

convert.class: convert.java
	javac convert.java

clean:
	rm -rf Generator/obj
	rm -f Build.log Build.exe* *.suo
	rm -f source.p* *.xml convert.class *.pdb *.dll
	rm -rf Target

