#! /bin/bash
source "`dirname "$0"`/config.sh"

descriptor="$1"
filepath="`readlink -f "$2"`"

case "$descriptor" in
  GameDetails) proto_fp='proto/replay.proto' ;;
  ReplayRepo) proto_fp='proto/replay.proto' ;;
  Timeserie) proto_fp='proto/timeserie.proto' ;;
  TimeserieRepo) proto_fp='proto/timeserie.proto' ;;
  *) proto_fp='no_proto_file_for_$descriptor' ;;
esac

pushd_to_root
zcat "$filepath" | protoc -I. --decode="messages.$descriptor" "$proto_fp"

