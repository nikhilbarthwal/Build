# ============================================================================
# Project Name   : Builder
# Author         : Nikhil Barthwal
# Description    : Source Code generator for MVP
# ============================================================================

CXX = g++
CXXFLAGS =  -c -O0 -g -Wall 
SOURCES = $(wildcard *.cpp)
HEADERS = $(wildcard *.h)
OBJS = $(SOURCES:cpp=o)
LIBS =
EXE = build
TARGET = Target
TEMP = Temp

.SUFFIXES: .cpp .o

.cpp.o:
	$(CXX) $(CXXFLAGS) $*.cpp

all: $(EXE)
	./$(EXE)
	# mkdir $(TARGET)/
	# cd $(TARGET)
	#../$(EXE)
	# cd .:wq

$(EXE): $(OBJS)
	$(CXX) -o $(EXE) $(OBJS) $(LIBS)

print: source.pdf

source.pdf: source.ps
	ps2pdf source.ps

source.ps: $(SOURCES) $(HEADERS)
	a2ps $(SOURCES) $(HEADERS) -o source.ps 

clean:
	rm -f $(OBJS) $(EXE) *.out source.ps source.pdf *.exe
	rm -rf $(TARGET) $(TEMP)

