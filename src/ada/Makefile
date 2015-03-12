
all: nml_test_write_cpp
	gnatmake nmltest_server_ada -L../../lib -cargs -g
	gnatmake nmltest_write_ada  -L../../lib -cargs -g
	gnatmake nmltest_read_ada -L../../lib -cargs -g

nml_test_write_cpp: nml_test_write_cpp.cc nml_test_format_n.cc otherheader_n.cc
	gcc -g -I../../include $^ -L../../lib -lrcs -o $@


.PHONY: all


