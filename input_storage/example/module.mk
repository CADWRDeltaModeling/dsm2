
local_dir := $(SOURCE_DIR)/example

gensource := $(addprefix $(SOURCE_DIR)/$(subdirectory)/,input_storage_channel.cpp input_storage_xsect.cpp input_storage_envvar.cpp input_state_map.cpp buffer_actions.cpp text_parser.cpp)
genheader : = $(call source-to-object,$(gensource))


local_lib := $(subdirectory)/examplegeneration.a

local_src := $(addprefix $(subdirectory)/,$(gensource))

$(eval $(call make-library,examplegeneration.a,$(local_src)))


generated += $(gensource) $(genheadertest)


programs += fortran_example.exe 
#cpp_example.exe





example_input := example_include.txt example.txt
example_input_src := $(addprefix $(SOURCE_DIR)/example/,$(example_input))
example_input_local  := $(addprefix $(SOURCE_DIR)/$(subdirectory)/,$(example_input))
$(example_input) : $(example_input_src)
	$(shell cp -u -f $^ .)

all: $(example_input)




cpp_example.exe: $(local_cpp) examplegeneration.a input_storage.a
	$(CC) ${CPPFLAGS} $^ -o$@ 



FLIB := -L/lib -L/cygdrive/c/devtools/hdf5_cygwin/lib -lboost_regex-gcc-mt -lhdf5_fortran -lhdf5_hl -lhdf5 -L/cygdrive/c/cygwin/szip/lib -lsz -lz 

local_fortran := ../example/input_storage_fortran.f90 ../example/example_parser.f90

fortran_example.exe:  $(local_fortran) ../example/text_parser.o examplegeneration.a input_storage.a
	g95 -g $^ -L /usr/lib/gcc/i686-pc-cygwin/3.4.4 -lstdc++ $(LIBS) $(CPPFLAGS) -fno-second-underscore -fno-underscoring -o example_parser.exe
