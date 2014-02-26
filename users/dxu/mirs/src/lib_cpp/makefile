SHELL = /bin/sh

include ../../setup/paths

EXTRA_CXX_FLAGS = -I$(HDF5INC)

all:
	$(CXX) $(CXX_FLAGS) $(EXTRA_CXX_FLAGS) -c mt.cc


clean:
	-rm -f mt.o

