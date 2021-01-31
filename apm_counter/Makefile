.ONESHELL:
.SECONDARY:
.PHONY: all clean fs_init c_code go_code go_unittest test sysinfo

STAGE_PATH    := /tmp/bin_apm_counter
GOENV    := $(STAGE_PATH)/go_env
MYGOSRC  := golang

CC       := gcc
CPPFLAGS :=
CFLAGS   :=
LDFLAGS  :=
LDLIBS   :=

all: go_code

go_code test: | bin

clean:
	if [[ -f "$(GOENV)" ]]; then
		GOPATH="`GOENV="$(GOENV)" go env GOPATH`"
		chmod --recursive 'a+wx' "$$GOPATH"
	fi
	rm -rf bin/*

test: all go_unittest;

go_code: c_code $(GOENV) $(go_files) $(GO_PROTO_GEN_SRCS)
	pushd "$(MYGOSRC)"
	GOENV="$(GOENV)" go install ./...

go_unittest: go_code
	pushd "$(MYGOSRC)"
	# add --test.v to get verbose tests
	GOENV="$(GOENV)" go test ./...

$(GOENV): bin
	GOENV="$(GOENV)" go env -w CC="$(CC)" \
														 CGO_CFLAGS="$(CFLAGS)" \
														 CGO_LDFLAGS="" \
														 GOPATH="$(STAGE_PATH)/gopath" \
														 GOBIN="$(STAGE_PATH)/gobin" \
														 GOCACHE="$(STAGE_PATH)/go-build"
	#GOENV="$(GOENV)" go env

bin: | $(STAGE_PATH)
	[[ -L bin ]] || ln -s $(STAGE_PATH) bin

$(STAGE_PATH):
	[[ -d $(STAGE_PATH) ]] || mkdir $(STAGE_PATH)

