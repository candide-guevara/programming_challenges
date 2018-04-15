.ONESHELL:
.SECONDARY:
.PHONY: all clean opt dbg

BIN_DIR := /tmp/compression_fun_and_profit
PYTHON_EXE := python3

$(BIN_DIR):
	mkdir -p $(BIN_DIR)/stage
	cp etc/*.gz $(BIN_DIR)

clean:
	rm -r $(BIN_DIR)

py_prep: $(BIN_DIR)
	cp stats_calc/*.py $(BIN_DIR)
	pushd $(BIN_DIR)
	rm stage/*
	true

python_test: py_prep
	pushd $(BIN_DIR)
	#$(PYTHON_EXE) -m pdb test.py --stage-dir stage
	$(PYTHON_EXE) test.py --stage-dir stage
	#cp $(BIN_DIR)/test_*.gz etc
	
calc: py_prep
	pushd $(BIN_DIR)
	$(PYTHON_EXE) main.py --raw-input secout_bics_1_tech.gz --prob-output $(BIN_DIR)/prob_dstrb_bics_1_tech.txt
	$(PYTHON_EXE) main.py --raw-input secout_xchng_us.gz --prob-output $(BIN_DIR)/prob_dstrb_xchng_us.txt

