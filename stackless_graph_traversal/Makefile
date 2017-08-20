.ONESHELL:
.SECONDARY:
.PHONY: all clean images test complexity hw_counters flamegraph usr_dyn_probe usr_static_probe

BIN_DIR := bin
SRC_DIR := src
INC_DIR := include
EXE_NAME := project

FLAME_ROOT := $(HOME)/Programation/Perl/FlameGraph
CC       := gcc
CPPFLAGS := 
opt: CPPFLAGS := $(CPPFLAGS) -D__LEVEL_LOG__=2 -D__LEVEL_ASSERT__=0
dbg: CPPFLAGS := $(CPPFLAGS) -D__LEVEL_LOG__=4 -D__LEVEL_ASSERT__=1
CFLAGS   := -std=c99 -I $(INC_DIR) -Wall -Werror
opt: CFLAGS := $(CFLAGS) -ggdb -mtune=native -march=native -O3
dbg: CFLAGS := $(CFLAGS) -ggdb -fsanitize=address
LDFLAGS  := -rdynamic
dbg: LDFLAGS := $(LDFLAGS) -fsanitize=address
LDLIBS   := -ldl

flavors = dbg opt
headers = $(wildcard $(INC_DIR)/*.h)
c_files = $(wildcard $(SRC_DIR)/*.c)
objects = $(addsuffix .o, $(basename $(notdir $(c_files))))

all: $(flavors);
%: init_% $(BIN_DIR)/%/$(EXE_NAME) ;
init_% :
	mkdir -p "$(BIN_DIR)/$*"

test : $(flavors)
	cd $(BIN_DIR)
	for flavor in $(flavors); do
		echo -e "\n### Running $@ for flavor $$flavor ###\n"
		"$$flavor/$(EXE_NAME)" test 128 || exit 2
	done

images : 
	cd $(BIN_DIR)
	echo building images for *.dot
	for f in *.dot; do cat $$f | neato -Tsvg > "$${f}.svg"; done

complexity: $(flavors)
	cd $(BIN_DIR)
	for flavor in $(flavors); do
		echo -e "\n### Running $@ for flavor $$flavor ###\n"
		graph_size=`python3 $(CURDIR)/etc/graph_size_for_flavor.py $@ $$flavor`
		$$flavor/$(EXE_NAME) stress_runtime_complexity_all_algo $$graph_size "complexity_results_$${flavor}.csv"
	done
	python3 $(CURDIR)/etc/plot_complexity.py complexity_results_*.csv

hw_counters : $(flavors)
	cd $(BIN_DIR)
	events=(
		"cycles,instructions,branch-misses,branches,cycle_activity.cycles_no_execute,uops_issued.any,uops_retired.all"
		"L1-dcache-load-misses,L1-dcache-loads,LLC-load-misses,LLC-loads,dTLB-load-misses,dTLB-loads"
	)
	for flavor in $(flavors); do
		graph_size=`python3 $(CURDIR)/etc/graph_size_for_flavor.py $@ $$flavor`
		algos=( `$$flavor/$(EXE_NAME) list_algo` )
		
		for algo in "$${algos[@]}"; do
			echo -e "\n### Running $@ for flavor $$flavor, algo $$algo ###\n"
			report_name="hw_counters_$${flavor}_$${algo}.csv"
			: > "$$report_name"
			for event in "$${events[@]}"; do
				sudo perf stat -x, --append -o "$$report_name" -e "$$event" -- \
					$$flavor/$(EXE_NAME) stress_profile_algo_on_graph_type $$graph_size "" $$algo "$(mk_graph_type)"
			done
		done
	done
	python3 $(CURDIR)/etc/plot_stats.py hw_counters_*.csv

flamegraph : $(flavors)
	cd $(BIN_DIR)
	for flavor in $(flavors); do
		graph_size=`python3 $(CURDIR)/etc/graph_size_for_flavor.py $@ $$flavor`
		algos=( `$$flavor/$(EXE_NAME) list_algo` )
		
		for algo in "$${algos[@]}"; do
			echo -e "\n### Running $@ for flavor $$flavor, algo $$algo ###\n"
			report_name="flame_$${flavor}_$${algo}.svg"
			sudo perf record -F 997 --call-graph dwarf -- \
				$$flavor/$(EXE_NAME) stress_profile_algo_on_graph_type $$graph_size "" $$algo
			sudo chown $(USER):$(USER) perf.data
			perf script | perl $(FLAME_ROOT)/stackcollapse-perf.pl | perl $(FLAME_ROOT)/flamegraph.pl --hash --minwidth=5 > $$report_name
		done
	done

# Dynamic probes are not available in opt flavor
usr_dyn_probe : dbg
	cd $(BIN_DIR)
	sudo perf probe -qx $</$(EXE_NAME) -a "$(EXE_NAME):prune=destructive_std_depth_first_traversal_helper:5 stack_depth"
	sudo perf probe -qx $</$(EXE_NAME) -a "$(EXE_NAME):patho=bf_pathological_branch_loop_back:7 node"
	sudo perf stat -e "$(EXE_NAME):*" -- \
		$</$(EXE_NAME) stress_profile_algo_on_graph_type 256K "" destructive_std_depth_first_traversal build_graph_with_undirected_cycles
	sudo perf stat -e "$(EXE_NAME):*" -- \
		$</$(EXE_NAME) stress_profile_algo_on_graph_type 256K "" destructive_pointer_back_and_forth_traversal build_graph_with_undirected_cycles
	sudo perf probe -qd "$(EXE_NAME):*"

# TRAP : run the buildid as root otherwise it will not find the probes to add with `perf probe`
usr_static_probe : opt
	cd $(BIN_DIR)
	sudo perf probe -qd "sdt_traversal:*"
	sudo perf buildid-cache -p $</$(EXE_NAME) > /dev/null
	sudo perf buildid-cache -a $</$(EXE_NAME) | tail
	sudo perf probe -qa "sdt_traversal:*"
	sudo perf stat -e "%sdt_traversal:*" -- \
		$</$(EXE_NAME) stress_profile_algo_on_graph_type 256K "" destructive_std_depth_first_traversal build_graph_with_undirected_cycles
	sudo perf stat -e "%sdt_traversal:*" -- \
		$</$(EXE_NAME) stress_profile_algo_on_graph_type 256K "" destructive_pointer_back_and_forth_traversal build_graph_with_undirected_cycles

package :
	tar -hzcf package_$(shell date +%d_%m_%y).tar $(BIN_DIR) $(SRC_DIR) $(INC_DIR)

clean:
	rm -rf $(BIN_DIR)/*

%/$(EXE_NAME) : $(addprefix %/, $(objects))
	$(CC) $(LDFLAGS) -o $@ $^ $(LOADLIBES) $(LDLIBS)

$(BIN_DIR)/opt/%.o : $(SRC_DIR)/%.c $(headers)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(BIN_DIR)/dbg/%.o : $(SRC_DIR)/%.c $(headers)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

