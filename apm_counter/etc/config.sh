CFG_STEAM_DIR="/media/llewelyn_data_b/SteamLibrary"
CFG_AOE2_USR_DIR="steamapps/compatdata/813780/pfx/drive_c/users/steamuser/Games/Age of Empires 2 DE"
CFG_AOE2_RPL_DIR="$CFG_STEAM_DIR/$CFG_AOE2_USR_DIR"
CFG_TIMESERIES_DIR="/tmp/bin_apm_counter"
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

