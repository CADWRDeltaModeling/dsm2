
local_dir := $(SOURCE_DIR)/test

gensourcetest := $(addprefix $(SOURCE_DIR)/$(subdirectory)/,input_storage_channel.cpp input_storage_xsect.cpp input_storage_envvar.cpp input_state_map.cpp buffer_actions.cpp text_parser.cpp)
genheadertest : = $(call source-to-object,$(gensourcetest))

$(gensourcetest) $(genheadertest): $(SOURCE_DIR)/test/generate.py $(SOURCE_DIR)/src/generate_input_storage.py
	python $(SOURCE_DIR)/test/generate.py

local_lib := $(subdirectory)/testgeneration.a

local_src := $(addprefix $(subdirectory)/,$(gensourcetest))

$(eval $(call make-library,testgeneration.a,$(local_src)))

unit_test_src :=  $(SOURCE_DIR)/$(subdirectory)/test_input_storage.cpp

programs += test_parser_cpp.exe test_parser_fortran.exe


generated += $(gensourcetest) $(genheadertest)


all : $(SOURCE_DIR)/test/test.txt $(SOURCE_DIR)/test/test_include.txt


test_input := test_include.txt test.txt
test_input_src := $(addprefix $(SOURCE_DIR)/test/,$(test_input))
test_input_local := $(addprefix $(SOURCE_DIR)/$(subdirectory)/,$(test_input))
$(test_input) : $(test_input_src)
	$(shell cp -u -f $^ ./$@)

#all:$(SOURCE_DIR)/test/test.txt $(SOURCE_DIR)/test/test_include.txt
#	$(shell cp -u -f $(SOURCE_DIR)/test/test*.txt .	)


test_parser_cpp.exe: $(unit_test_src) testgeneration.a input_storage.a
	$(CC) ${CPPFLAGS} $^ -o$@ ${TESTLIBS}



FLIB := -L/lib -L/cygdrive/c/devtools/hdf5_cygwin/lib -lboost_regex-gcc-mt -lhdf5_fortran -lhdf5_hl -lhdf5 -L/cygdrive/c/cygwin/szip/lib -lsz -lz

local_fortran := ../test/input_storage_fortran.f90 ../test/test_parser.f90

test_parser_fortran.exe:  $(local_fortran) ../test/text_parser.o testgeneration.a input_storage.a
	g95 -g $^ -L /usr/lib/gcc/i686-pc-cygwin/3.4.4 -lstdc++ $(LIBS) $(CPPFLAGS) -fno-second-underscore -fno-underscoring -o test_parser_fortran.exe
