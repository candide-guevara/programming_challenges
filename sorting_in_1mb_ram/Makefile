.ONESHELL:
.SECONDARY:
.PHONY: all clean opt dbg

BIN_DIR := /tmp
INC_DIR := .
SRC_DIR := .

CC       := clang++
CPPFLAGS := 
opt: CPPFLAGS := $(CPPFLAGS) -DNDEBUG
dbg: CPPFLAGS := $(CPPFLAGS) 
CFLAGS   := -std=c++11 -I $(INC_DIR) -Wall -Werror
opt: CFLAGS := $(CFLAGS) -mtune=native -march=native -O3
dbg: CFLAGS := $(CFLAGS) -ggdb -O0
LDFLAGS  :=
LDLIBS   :=

all: opt dbg
opt: $(addprefix $(BIN_DIR)/,bucket_sorting_opt integer_sorting_opt radix_sorting_opt)
dbg: $(addprefix $(BIN_DIR)/,bucket_sorting_dbg integer_sorting_dbg radix_sorting_dbg)

clean:
	rm $(addprefix $(BIN_DIR)/,bucket_sorting_opt integer_sorting_opt radix_sorting_opt)
	rm $(addprefix $(BIN_DIR)/,bucket_sorting_dbg integer_sorting_dbg radix_sorting_dbg)

$(BIN_DIR)/bucket_sorting_opt: $(wildcard $(SRC_DIR)/bucket_sorting*)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

$(BIN_DIR)/bucket_sorting_dbg: $(wildcard $(SRC_DIR)/bucket_sorting*)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

$(BIN_DIR)/integer_sorting_opt: $(wildcard $(SRC_DIR)/integer_sorting*)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

$(BIN_DIR)/integer_sorting_dbg: $(wildcard $(SRC_DIR)/integer_sorting*)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

$(BIN_DIR)/radix_sorting_opt: $(wildcard $(SRC_DIR)/radix_sorting*)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

$(BIN_DIR)/radix_sorting_dbg: $(wildcard $(SRC_DIR)/radix_sorting*)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

