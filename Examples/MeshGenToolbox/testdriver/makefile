OBJECTS = ../mgt/Service.o ../mgt/Coarsening.o ../mgt/Refining.o ../mgt/Output.o ../mgt/Input.o ../mgt/Mesh.o
TEST_OBJ = 5_test_sr_arc.o
	
TEST_SRC = 5_test_sr_arc.cpp

ALL_TEST = 5_test_sr_arc	

# all testing executable
all: $(TEST_OBJ)
	@cd ../mgt; $(MAKE) $@
	g++ -o 5_test_sr_arc $(OBJECTS) 5_test_sr_arc.o

$(TEST_OBJ): $(TEST_SRC)
	g++ -c $(TEST_SRC) -I../mgt

# remove generated testing and PMGT files
clean:
	@cd ../mgt; $(MAKE) $@
	rm -f *.o $(ALL_TEST)

# remove generated testing files.
# keep the PMGT files
cleanTest:
	rm -f $(TEST_OBJ) $(ALL_TEST)

# remove generated output files
cleanOutput:
	rm newVertices* newCells*
