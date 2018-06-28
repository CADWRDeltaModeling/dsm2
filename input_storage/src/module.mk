local_dir := $(SOURCE_DIR)/src

local_lib := $(subdirectory)/input_storage.a

srcs :=  hdf_storage.cpp InputState.cpp FileInputState.cpp InsertFileState.cpp EnvSubstitution.cpp ApplicationTextReader.cpp

local_src := $(addprefix $(subdirectory)/,$(srcs))


$(eval $(call make-library,input_storage.a,$(local_src)))


