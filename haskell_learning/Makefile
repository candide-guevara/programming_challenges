.ONESHELL:
.SECONDARY:
.PHONY: all clean

BIN_DIR  := /tmp/haskell_learning
GHC      := ghc
GHCFLAGS := -dynamic -outputdir $(BIN_DIR)
SOURCES  := $(wildcard *.hs)
TARGETS  := $(addprefix $(BIN_DIR)/, $(basename $(SOURCES)) ) 

all: bin $(BIN_DIR) $(TARGETS)

clean:
	rm -r $(BIN_DIR)

bin:
	ln -s "$(BIN_DIR)" bin

$(BIN_DIR):
	mkdir $(BIN_DIR)
	echo -e "choco\nlat\nradar\nmonkey" > "$(BIN_DIR)/testinput"

$(BIN_DIR)/% : %.hs
	$(GHC) $(GHCFLAGS) -o "$@" "$<"
	# silly but if the object files are left behind we will just relink the old program ...
	rm "$(BIN_DIR)"/Main.*

