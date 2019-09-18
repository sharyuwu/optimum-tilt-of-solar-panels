OBJECTS = Service.o Coarsening.o Refining.o Output.o Input.o Mesh.o
	
SOURCES = Service.cpp Coarsening.cpp Refining.cpp Output.cpp Input.cpp Mesh.cpp

HEADERS = Service.h Coarsening.h Refining.h Input.h Input.h Mesh.h Cell.h Edge.h Vertex.h

# all testing excutable
all: $(OBJECTS)
	
# all object files of PMGT
$(OBJECTS): $(SOURCES) $(HEADERS)
	g++ -c $(SOURCES)
	
# remove generated files
clean:
	rm -f *.o