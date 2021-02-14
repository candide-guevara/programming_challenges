descriptor="$1"
filepath="$2"

case "$descriptor" in
  GameDetails) proto_fp='proto/replay.proto' ;;
  ReplayRepo) proto_fp='proto/replay.proto' ;;
  Timeserie) proto_fp='proto/timeserie.proto' ;;
  TimeserieRepo) proto_fp='proto/timeserie.proto' ;;
  *) proto_fp='no_proto_file_for_$descriptor' ;;
esac

if [[ ! -e "$proto_fp" ]]; then
  echo "[ERROR] the script must be run at the install base"
  exit 1
fi

zcat "$filepath" | protoc --decode="messages.$descriptor" "$proto_fp"

