.ONESHELL:
.SECONDARY:
.PHONY: all clean opt dbg

BIN_DIR := /tmp
INC_DIR := .
SRC_DIR := .

CC       := g++
CPPFLAGS := 
opt: CPPFLAGS := $(CPPFLAGS) -DNDEBUG
dbg: CPPFLAGS := $(CPPFLAGS) 
CFLAGS   := -std=c++17 -I $(INC_DIR) -Wall -Werror
opt: CFLAGS := $(CFLAGS) -mtune=native -march=native -O3
dbg: CFLAGS := $(CFLAGS) -ggdb -O0
LDFLAGS  :=
LDLIBS   :=
HEADERS  :=$(wildcard $(SRC_DIR)/*.hpp) 
CPPUNITS :=$(wildcard $(SRC_DIR)/*.cpp)
DBG_TGTS :=$(addprefix $(BIN_DIR)/,xrates_dp_dbg)
OPT_TGTS :=$(addprefix $(BIN_DIR)/,xrates_dp_opt)

all: opt dbg
dbg: $(DBG_TGTS)
opt: $(OPT_TGTS)

clean:
	rm $(DBG_TGTS) $(OPT_TGTS)

$(OPT_TGTS) $(DBG_TGTS): $(HEADERS) $(CPPUNITS)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(CPPUNITS)

#$(BIN_DIR)/xrates_opt: $(wildcard $(SRC_DIR)/*.h) $(wildcard $(SRC_DIR)/*.cpp)
#	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

