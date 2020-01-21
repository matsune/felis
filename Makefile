CXX := g++
SRC_DIR := ./src
BUILD_DIR := ./build
TARGET := $(BUILD_DIR)/a.out
SRCS = $(shell find $(SRC_DIR) -name *.cpp )
OBJS = $(SRCS:%=$(BUILD_DIR)/%.o)
DEPS = $(OBJS:.o=.d)
INC_DIRS := $(shell find $(SRC_DIR) -type d)
INC_FLAGS := $(addprefix -I,$(INC_DIRS))
CPPFLAGS := $(INC_FLAGS) -MMD -MP
CXXFLAGS := -std=c++14

LLVM_MODULES := core 
LLVM_FLAGS := `llvm-config --cppflags --ldflags --libs $(LLVM_MODULES)`

$(TARGET): gen $(OBJS)
	$(CXX) $(OBJS) -o $@ $(LLVM_FLAGS) $(LDFLAGS)

$(BUILD_DIR)/%.cpp.o:	%.cpp
	mkdir -p $(dir $@)
	$(CXX) -c $< -o $@ $(LLVM_FLAGS) $(CPPFLAGS) $(CXXFLAGS) 

gen:
	cd $(SRC_DIR)/parse; $(MAKE)
 
.PHONY: clean
clean:
	$(RM) -r $(BUILD_DIR)
	cd $(SRC_DIR)/parse; $(MAKE) clean
 
-include $(DEPS)
