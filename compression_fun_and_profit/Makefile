.ONESHELL:
.SECONDARY:
.PHONY: all clean opt dbg

BIN_DIR := /tmp/compression_fun_and_profit
PYTHON_EXE := python3
CPP_DIR := compressor

CC       := g++
CPPFLAGS := 
opt: CPPFLAGS := $(CPPFLAGS) -DNDEBUG
dbg: CPPFLAGS := $(CPPFLAGS) 
CFLAGS   := -std=c++17 -Wall -Werror -I $(CPP_DIR)
opt: CFLAGS := $(CFLAGS) -mtune=native -march=native -O3
dbg: CFLAGS := $(CFLAGS) -ggdb -O0
LDFLAGS  :=
LDLIBS   :=

$(BIN_DIR):
	mkdir -p $(BIN_DIR)/stage

clean:
	rm -r $(BIN_DIR)

py_prep: $(BIN_DIR)
	cp stats_calc/*.py $(BIN_DIR)
	rm $(BIN_DIR)/stage/*
	true

python_test: py_prep
	cp etc/test_*.gz $(BIN_DIR)
	pushd $(BIN_DIR)
	#$(PYTHON_EXE) -m pdb test.py --stage-dir stage
	$(PYTHON_EXE) test.py --stage-dir stage
	#cp $(BIN_DIR)/test_*.gz etc
	
python_calc: py_prep
	cp etc/secout_* $(BIN_DIR)
	pushd $(BIN_DIR)
	$(PYTHON_EXE) main.py --raw-input secout_bics_1_tech.gz --prob-output $(BIN_DIR)/prob_dstrb_bics_1_tech.txt
	$(PYTHON_EXE) main.py --raw-input secout_xchng_us.gz --prob-output $(BIN_DIR)/prob_dstrb_xchng_us.txt
	popd
	cp $(BIN_DIR)/prob_dstr* etc/

opt: $(BIN_DIR) $(BIN_DIR)/compressor_opt
dbg: $(BIN_DIR) $(BIN_DIR)/compressor_dbg

compressor_test: dbg
	cp etc/prob_dstr* $(BIN_DIR)
	cp etc/*.bin $(BIN_DIR)
	pushd $(BIN_DIR)
	./compressor_dbg	

$(BIN_DIR)/compressor_dbg: $(wildcard $(CPP_DIR)/*)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

$(BIN_DIR)/compressor_opt: $(wildcard $(CPP_DIR)/*)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(filter %.cpp,$^)

