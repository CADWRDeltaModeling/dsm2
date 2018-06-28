SOURCE_DIR := ..

# $(call source-to-object, source-file-list)
source-to-object = $(subst .cpp,.o,$(filter %.cpp,$1)) \
                   $(subst .f90,.o,$(filter %.f90,$1)) \

source-to-header = $(subst .cpp,.h,$(filter %.cpp,$1))


# $(subdirectory)
subdirectory = $(patsubst $(SOURCE_DIR)/%/module.mk,%, \
                 $(word                                \
                   $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST)))

vpath %.cpp $(SOURCE_DIR)



# $(call make-library, library-name, source-file-list)
define make-library
libraries += $1
sources += $2
$1: $(call source-to-object,$2)
	$(AR) $(ARFLAGS) $$@ $$^
endef

# Collect information from each module in these four variables.
# Initialize them here as simple variables.
modules := src test example
programs :=
libraries :=
sources :=
generated :=

objects = $(call source-to-object,$(sources))
dependencies = $(subst .o,.d,$(objects))

include_dirs := $(SOURCE_DIR)/src /cygdrive/c/devtools/hdf5_cygwin/include /usr/include/boost-1_33_1
CPPFLAGS += $(addprefix -I,$(include_dirs))

LIBS := -L/lib -L/cygdrive/c/devtools/hdf5_cygwin/lib  -lboost_filesystem-gcc-mt -lboost_regex-gcc-mt -lhdf5_fortran -lhdf5_hl -lhdf5 -L/cygdrive/c/cygwin/szip/lib -lsz -lz 
TESTLIBS := $(LIBS) -lboost_unit_test_framework-gcc-mt


vpath %.h $(include_dirs)


CC := g++
AR := ar
MV := mv -f
RM := rm -f
SED := sed
TEST := test

create-output-directories :=                 \
	$(shell for f in $(modules);         \
           do                                \
              echo f; \
              $(TEST) -d $$f || $(MDKIR) $$f; \
           done)                             \

all:

include $(patsubst %, $(SOURCE_DIR)/%/module.mk,$(modules))

.PHONY: all
all: $(programs)


.PHONY: gen
gen: $(generated)
	python $(SOURCE_DIR)/test/generate.py
	python $(SOURCE_DIR)/example/generate.py



.PHONY: libraries
libraries: $(libraries)

.PHONY: clean

clean:
	$(RM) $(objects) $(programs) $(libraries) $(dependencies)


ifneq "$(MAKECMDGOALS)" "clean"
 -include $(dependencies)
endif

# $(call make-depend,source-file,object-file,depend-file)
define make-depend
  gcc    -MM            \
	 -MF $3         \
         -MP            \
         -MT $2         \
         $(CFLAGS)      \
         $(CPPFLAGS)    \
         $(TARGET_ARCH) \
         $1
endef

#%.o: %.cpp
#	$(warning *******modules $(modules))
#	$(warning ********compiling: $<)
#	#$(call make-depend,$<,$@,$(subst .o,.d,$@))
#	$(COMPILE.cpp) $<



