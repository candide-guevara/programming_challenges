.ONESHELL:
.SECONDARY:
.PHONY: all .tags sanitize clean package hw_counters

BIN_DIR := bin
SRC_DIR := src
INC_DIR := include

CC       := gcc
CPPFLAGS := 
opt: CPPFLAGS := $(CPPFLAGS) -D_GNU_SOURCE -D__LEVEL_LOG__=2 -D__LEVEL_ASSERT__=0
dbg: CPPFLAGS := $(CPPFLAGS) -D_GNU_SOURCE -D__LEVEL_LOG__=4 -D__LEVEL_ASSERT__=1

CFLAGS   := -std=c11 -I $(INC_DIR) -Wall -Wno-missing-braces -Wno-unused-function -Werror $(user_cflags)
opt: CFLAGS := $(CFLAGS) -mtune=native -march=native -O3
dbg: CFLAGS := $(CFLAGS) -ggdb $(compile_sanitize)

LDFLAGS  := $(user_ldflags)
dbg: LDFLAGS := $(LDFLAGS) $(link_sanitize)
LDLIBS   := -pthread

flavors = opt dbg
headers = $(wildcard $(INC_DIR)/*.h)
c_files = $(wildcard $(SRC_DIR)/*.c)
objects = $(addsuffix .o, $(basename $(notdir $(c_files))))

all: $(flavors);
%: init_% $(BIN_DIR)/%/project ;
init_% :
	mkdir -p "$(BIN_DIR)/$*"

sanitize:
	sanitizers=( -fsanitize=address -fsanitize=leak -fsanitize=thread -fsanitize=undefined )
	for sanitizer in $${sanitizers[@]}; do
		rm -rf $(BIN_DIR)/dbg
		make dbg "compile_sanitize=-O1 $$sanitizer" "link_sanitize=$$sanitizer" || exit 1
		$(BIN_DIR)/dbg/project || exit 2
	done
	rm -rf $(BIN_DIR)/dbg

hw_counters : $(flavors)
	cd $(BIN_DIR)
	events=(
		"cycles,instructions,branch-misses,branches,cycle_activity.cycles_no_execute,uops_issued.any,uops_retired.all"
		"L1-dcache-load-misses,L1-dcache-loads,LLC-load-misses,LLC-loads,dTLB-load-misses,dTLB-loads"
	)
	for flavor in $(flavors); do
		echo -e "\n### Running $@ for flavor $$flavor ###\n"
		report_name="hw_counters_$${flavor}.csv"
		: > "$$report_name"
		for event in "$${events[@]}"; do
			sudo perf stat -x, --append -o "$$report_name" -e "$$event" -- $$flavor/$(EXE_NAME) 
		done
	done
	python3 $(CURDIR)/etc/plot_stats.py hw_counters_*.csv

package :
	tar -hzcf package_$(shell date +%d_%m_%y).tar $(BIN_DIR) $(SRC_DIR) $(INC_DIR)

.tags:
	# we use tac to invert the sorting order of tags to put definitions first
	# useful when jump around with vim
	find src include -iname '*.h' -o -iname '*.c' \
		| ctags --extra=+f --c-kinds=+p+d \
			--exclude='.git' --exclude='.hg' --exclude='bin' -L - -f - \
	  | tac > .tags

clean:
	rm -rf $(BIN_DIR)/*

%/project : $(addprefix %/, $(objects))
	$(CC) $(LDFLAGS) -o $@ $^ $(LOADLIBES) $(LDLIBS)

$(BIN_DIR)/opt/%.o : $(SRC_DIR)/%.c $(headers)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(BIN_DIR)/dbg/%.o : $(SRC_DIR)/%.c $(headers)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

