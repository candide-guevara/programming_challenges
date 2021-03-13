#! /bin/bash

source "`dirname "$0"`/config.sh"

python_script="`get_bin_path "python/aoe2_apm_show.py"`"
active_py_venv
python "$python_script" --do_parse "$OPT_PLAYER" "$OPT_STEAM_DIR" "$OPT_AOE2_USR_DIR" "$OPT_TIMESERIES_DIR" "$OPT_LOG_LVL" "$@"

