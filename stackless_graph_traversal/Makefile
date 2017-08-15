.ONESHELL:
.SECONDARY:
.PHONY: all clean images

BIN_DIR := bin
SRC_DIR := src
INC_DIR := include

FLAME_ROOT := $(HOME)/Programation/Perl/FlameGraph
CC       := gcc
CPPFLAGS := 
opt: CPPFLAGS := $(CPPFLAGS) -D__LEVEL_LOG__=2 -D__LEVEL_ASSERT__=0
dbg: CPPFLAGS := $(CPPFLAGS) -D__LEVEL_LOG__=4 -D__LEVEL_ASSERT__=1
CFLAGS   := -std=c99 -I $(INC_DIR) -Wall -Werror
opt: CFLAGS := $(CFLAGS) -ggdb -mtune=native -march=native -O3
dbg: CFLAGS := $(CFLAGS) -ggdb -fsanitize=address
LDFLAGS  :=
dbg: LDFLAGS := $(LDFLAGS) -fsanitize=address
LDLIBS   := 

flavors = opt dbg
headers = $(wildcard $(INC_DIR)/*.h)
c_files = $(wildcard $(SRC_DIR)/*.c)
objects = $(addsuffix .o, $(basename $(notdir $(c_files))))

all: $(flavors);
%: init_% $(BIN_DIR)/%/project ;
init_% :
	mkdir -p "$(BIN_DIR)/$*"

images : 
	cd $(BIN_DIR)
	echo building images for *.dot
	for f in *.dot; do cat $$f | neato -Tsvg > "$${f}.svg"; done

%_prof : %
	cd $(BIN_DIR)
	sudo perf record -F 997 --call-graph dwarf -- $*/project
	sudo chown $$USER:$$USER perf.data
	perf script | perl $(FLAME_ROOT)/stackcollapse-perf.pl | perl $(FLAME_ROOT)/flamegraph.pl --hash --minwidth=5 > flame_prof_$*.svg

package :
	tar -hzcf package_$(shell date +%d_%m_%y).tar $(BIN_DIR) $(SRC_DIR) $(INC_DIR)

clean:
	rm -rf $(BIN_DIR)/*

%/project : $(addprefix %/, $(objects))
	$(CC) $(LDFLAGS) -o $@ $^ $(LOADLIBES) $(LDLIBS)

$(BIN_DIR)/opt/%.o : $(SRC_DIR)/%.c $(headers)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(BIN_DIR)/dbg/%.o : $(SRC_DIR)/%.c $(headers)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

