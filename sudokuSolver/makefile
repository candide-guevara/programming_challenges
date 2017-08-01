.PHONY: clean all backup init

SRC_DIR = src
BIN_DIR = bin
OPT_DIR = $(BIN_DIR)/opt
DBG_DIR = $(BIN_DIR)/dbg
PYT_DIR = $(BIN_DIR)/python
PRL_DIR = $(BIN_DIR)/perl
INC_DIR = include

headers = $(wildcard $(INC_DIR)/*.h)
implemFiles = $(wildcard $(SRC_DIR)/*.c)
objects = $(addsuffix .o, $(basename $(notdir $(implemFiles))))
dbgObj = $(addprefix $(DBG_DIR)/, $(objects))
optObj = $(addprefix $(OPT_DIR)/, $(objects))
pyFiles = $(wildcard $(SRC_DIR)/*.py)

COMP_FLAGS = -I $(INC_DIR) -Wall -Werror -std=c99
DBG_FLAGS = -fsanitize=address

$(OPT_DIR)/%.o : $(SRC_DIR)/%.c $(headers)
	gcc -c -mtune=native -march=native $(COMP_FLAGS) -O3 -o $@ $<

$(DBG_DIR)/%.o : $(SRC_DIR)/%.c $(headers)
	gcc $(DBG_FLAGS) -c $(COMP_FLAGS) -ggdb -o $@ $<

$(PYT_DIR)/%.pyc : $(SRC_DIR)/%.py
	python3 -m py_compile $<
	mv $(SRC_DIR)/$*.pyc $(PYT_DIR)

all: init $(DBG_DIR)/solver $(OPT_DIR)/solver $(PYT_DIR)/solver.py $(PRL_DIR)/solver.pl

init:
	test -d $(OPT_DIR) || mkdir -p $(OPT_DIR)
	test -d $(DBG_DIR) || mkdir -p $(DBG_DIR)
	test -d $(PYT_DIR) || mkdir -p $(PYT_DIR)
	test -d $(PRL_DIR) || mkdir -p $(PRL_DIR)

$(OPT_DIR)/solver: $(optObj)
	gcc -o $@ $^ 

$(DBG_DIR)/solver: $(dbgObj)
	gcc $(DBG_FLAGS) -o $@ $^ 

$(PYT_DIR)/solver.py: $(pyFiles)
	cp $^ $(PYT_DIR)
	cd $(PYT_DIR) && python3 -m py_compile $(notdir $@)
	chmod a+x $@

$(PRL_DIR)/solver.pl: $(SRC_DIR)/solver.pl
	cp $< $@
	chmod a+x $@

clean :
	rm -rf $(BIN_DIR)/*

backup :
	tar -zcf ../sudokuSolver_backup$(shell date +%d_%m_%y).tar *

