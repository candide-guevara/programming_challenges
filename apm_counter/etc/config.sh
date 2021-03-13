CFG_ROOT_DIR="`dirname "$0"`"
if [[ "$CFG_ROOT_DIR" = *etc ]]; then
  CFG_ROOT_DIR="`readlink -f "$CFG_ROOT_DIR/.."`"
else
  CFG_ROOT_DIR="`readlink -f "$CFG_ROOT_DIR"`"
fi

if [[ ! -d "$CFG_ROOT_DIR" ]]; then
  echo "[ERROR] CFG_ROOT_DIR='$CFG_ROOT_DIR' is not a dir"
  exit 1
fi

CFG_STEAM_DIR="/media/llewelyn_data_b/SteamLibrary"
CFG_AOE2_USR_DIR="steamapps/compatdata/813780/pfx/drive_c/users/steamuser/Games/Age of Empires 2 DE"
CFG_AOE2_RPL_DIR="$CFG_STEAM_DIR/$CFG_AOE2_USR_DIR"
CFG_TIMESERIES_DIR="$CFG_ROOT_DIR/data"
[[ -d "$CFG_TIMESERIES_DIR" ]] || mkdir "$CFG_TIMESERIES_DIR"
CFG_TIMESERIES_REPO="$CFG_TIMESERIES_DIR/timeserie_repo.pb.gz"
CFG_REPLAY_PARSE_REPO="$CFG_TIMESERIES_DIR/replay_repo.pb.gz"
CFG_INPUT_DEVS=(
  "/dev/input/by-id/usb-Logitech_Gaming_Mouse_G502_1393375E3137-event-mouse"
  "/dev/input/by-id/usb-046a_0023-event-kbd"
)

# To make sure both golang and python share same flag names
OPT_STEAM_DIR="--steam_dir=$CFG_STEAM_DIR"
OPT_AOE2_USR_DIR="--aoe2_usr_dir=$CFG_AOE2_USR_DIR"
OPT_TIMESERIES_DIR="--ts_root=$CFG_TIMESERIES_DIR"
OPT_INPUT_DEVS="--dev_files=`printf '%s,' "${CFG_INPUT_DEVS[@]}"`"
OPT_LOG_LVL="--log_lvl=debug"
OPT_PLAYER="--player=le_maire_dessomes_sur_marne"

get_bin_path() {
  local relpath="$1"
  local retvar="$2"
  local abspath="$CFG_ROOT_DIR/$relpath"
  if [[ ! -e "$abspath" ]]; then
    echo "[ERROR] Could not find bin '$abspath'"
    exit 1
  fi
  echo "$abspath"
}

active_py_venv() {
  local activate_sh="$CFG_ROOT_DIR/py_venv/bin/activate"
  if [[ ! -e "$activate_sh" ]]; then
    echo "[ERROR] Could not find bin '$activate_sh'"
    exit 1
  fi
  source "$activate_sh"
}

pushd_to_root() {
  pushd "$CFG_ROOT_DIR" > /dev/null
}

