.ONESHELL:
.SECONDARY:
.PHONY: all clean fs_init c_code go_code proto go_unittest test

STAGE_PATH    := /tmp/bin_apm_counter
GOENV    := $(STAGE_PATH)/go_env
GO_PROTOC_INSTALL := $(STAGE_PATH)/gobin/protoc-gen-go
SRC_GO  := golang
SRC_PROTO := proto

CC       := gcc
CPPFLAGS :=
CFLAGS   :=
LDFLAGS  :=
LDLIBS   :=

all: go_code

go_code test: proto | bin

clean:
	if [[ -f "$(GOENV)" ]]; then
		GOPATH="`GOENV="$(GOENV)" go env GOPATH`"
		chmod --recursive 'a+wx' "$$GOPATH"
	fi
	rm -rf bin/*

test: all go_unittest;

go_code: c_code $(GOENV) $(go_files) $(GO_PROTO_GEN_SRCS)
	pushd "$(SRC_GO)"
	GOENV="$(GOENV)" go install ./...

go_unittest: go_code
	pushd "$(SRC_GO)"
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

proto: $(SRC_GO)/messages/timeserie.pb.go;
$(SRC_GO)/messages/timeserie.pb.go: $(SRC_PROTO)/timeserie.proto $(GOENV) | $(GO_PROTOC_INSTALL)
	export PATH="$(PATH):`GOENV="$(GOENV)" go env GOBIN`"
	protoc '-I=$(SRC_PROTO)' '--go_out=$(SRC_GO)' "$<"

bin: | $(STAGE_PATH)
	[[ -L bin ]] || ln -s $(STAGE_PATH) bin

$(STAGE_PATH):
	[[ -d $(STAGE_PATH) ]] || mkdir $(STAGE_PATH)

$(GO_PROTOC_INSTALL): $(GOENV)
	GOENV="$(GOENV)" go get google.golang.org/protobuf/cmd/protoc-gen-go

