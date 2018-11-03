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
CFLAGS   := -std=c++17 -I $(INC_DIR) -Wall -Werror
opt: CFLAGS := $(CFLAGS) -mtune=native -march=native -O3
dbg: CFLAGS := $(CFLAGS) -ggdb -O0
LDFLAGS  :=
LDLIBS   :=
DBG_TGTS :=$(addprefix $(BIN_DIR)/,xrates_dj_dbg xrates_dp_dbg)
OPT_TGTS :=$(addprefix $(BIN_DIR)/,xrates_dj_opt xrates_dp_opt)

all: opt dbg
dbg: $(DBG_TGTS)
opt: $(OPT_TGTS)

clean:
	rm $(DBG_TGTS) $(OPT_TGTS)
	true

$(BIN_DIR)/xrates_dp_dbg $(BIN_DIR)/xrates_dp_opt: $(wildcard $(SRC_DIR)/xrates_dp*)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

$(BIN_DIR)/xrates_dj_dbg $(BIN_DIR)/xrates_dj_opt: $(wildcard $(SRC_DIR)/xrates_dj*)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

