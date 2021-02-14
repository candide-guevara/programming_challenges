.ONESHELL:
.SECONDARY:
.PHONY: all install clean fs_init go_code py_code sh_code proto sh_unittest go_unittest py_unittest test

STAGE_PATH := /tmp/bin_apm_counter
STAGE_PY   := $(STAGE_PATH)/python
GOENV      := $(STAGE_PATH)/go_env
PY_VENV    := $(STAGE_PATH)/py_venv
SRC_GO     := golang
SRC_PY     := python
SRC_SH     := etc
SRC_PROTO  := proto
GO_PROTOC_INSTALL := $(STAGE_PATH)/gobin/protoc-gen-go
PREFIX     := /home/cguevara/Temp

CC       := gcc
CPPFLAGS :=
CFLAGS   :=
LDFLAGS  :=
LDLIBS   :=

all: py_code go_code sh_code

install: all
	rsync --archive --exclude="*/gopath" --exclude="*/go-build" "$(STAGE_PATH)" "$(PREFIX)"

sh_code py_code go_code: proto | $(STAGE_PATH)

clean:
	if [[ -f "$(GOENV)" ]]; then
		GOPATH="`GOENV="$(GOENV)" go env GOPATH`"
		chmod --recursive 'a+wx' "$$GOPATH"
	fi
	[[ -d "$(STAGE_PATH)" ]] && rm -rf "$(STAGE_PATH)"/*

test: all go_unittest py_unittest sh_unittest;

go_code: $(GOENV)
	pushd "$(SRC_GO)"
	GOENV="$(GOENV)" go install ./...

py_code: | $(PY_VENV)
	cp --recursive --update "$(SRC_PY)" "$(STAGE_PATH)"

sh_code:
	cp --recursive --update "$(SRC_SH)" "$(STAGE_PATH)"

sh_unittest: sh_code
	pushd "$(SRC_SH)"
	sudo bash sandbox_test.sh

go_unittest: go_code
	pushd "$(SRC_GO)"
	# add --test.v to get verbose tests
	GOENV="$(GOENV)" go test ./...

py_unittest: py_code
	source "$(PY_VENV)/bin/activate"
	pushd "$(STAGE_PY)"
	python -m unittest discover --failfast --pattern '*_test.py'

$(GOENV): | $(STAGE_PATH)
	GOENV="$(GOENV)" go env -w CC="$(CC)" \
														 CGO_CFLAGS="$(CFLAGS)" \
														 CGO_LDFLAGS="" \
														 GOPATH="$(STAGE_PATH)/gopath" \
														 GOBIN="$(STAGE_PATH)/gobin" \
														 GOCACHE="$(STAGE_PATH)/go-build"
	#GOENV="$(GOENV)" go env

proto: $(STAGE_PY)/replay_pb2.py $(SRC_GO)/messages/replay.pb.go $(STAGE_PY)/timeserie_pb2.py $(SRC_GO)/messages/timeserie.pb.go;
$(STAGE_PY)/replay_pb2.py $(SRC_GO)/messages/replay.pb.go $(STAGE_PY)/timeserie_pb2.py $(SRC_GO)/messages/timeserie.pb.go &: \
                                                                 $(SRC_PROTO)/replay.proto $(SRC_PROTO)/timeserie.proto $(GOENV) \
                                                                 | $(GO_PROTOC_INSTALL) \
																																   $(PY_VENV)
	export PATH="$(PATH):`GOENV="$(GOENV)" go env GOBIN`"
	protoc '-I=$(SRC_PROTO)' '--go_out=$(SRC_GO)' $(SRC_PROTO)/replay.proto $(SRC_PROTO)/timeserie.proto
	protoc '-I=$(SRC_PROTO)' '--python_out=$(STAGE_PY)' $(SRC_PROTO)/replay.proto $(SRC_PROTO)/timeserie.proto
	cp --recursive --update "$(SRC_PROTO)" "$(STAGE_PATH)"

bin: | $(STAGE_PATH)
	[[ -L bin ]] || ln -s $(STAGE_PATH) bin

$(STAGE_PATH):
	[[ -d $(STAGE_PATH) ]] || mkdir $(STAGE_PATH)
	[[ -d $(STAGE_PY) ]] || mkdir $(STAGE_PY)

$(PY_VENV): | $(STAGE_PATH)
	[[ -d $(PY_VENV) ]] || mkdir $(PY_VENV)
	pushd "$(PY_VENV)"
	python -m venv .
	source bin/activate
	pip install --upgrade pip
	pip install --quiet pandas
	pip install --quiet matplotlib
	pip install --quiet pyqt5
	pip install --quiet protobuf
	pip install --quiet mgz
	pip list

$(GO_PROTOC_INSTALL): $(GOENV)
	GOENV="$(GOENV)" go get google.golang.org/protobuf/cmd/protoc-gen-go

