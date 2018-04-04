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

python_test: $(BIN_DIR)
	cp stats_calc/*.py $(BIN_DIR)
	pushd $(BIN_DIR)
	rm stage/*
	$(PYTHON_EXE) test.py --stage-dir stage
	popd
	cp $(BIN_DIR)/test_*.gz etc
	
