# Build Makefile

SOURCES = Build/*.fs
PROJFILE = Build/build.fsproj
SLNFILE = build.sln

all : source.pdf $(SOURCES) $(PROJFILE) $(SLNFILE)
	#xbuild $(SLNFILE)

print: source.pdf

source.pdf: source.ps
	ps2pdf source.ps

source.ps: $(SOURCES) format
	a2ps -r --columns=1 $(SOURCES)  -f 8 -o source.ps

format: convert.class
	java convert $(SOURCES)
	java convert $(PROJFILE)
	#java convert $(SLNFILE)

convert.class: convert.java
	javac convert.java

clean:
	rm -rf Build/obj Build/bin
	rm -f *.suo source.p* convert.class
	rm -rf Bin


