#! /bin/bash

source "`dirname "$0"`/config.sh"

python_script="`get_bin_path "python/aoe2_replay_repo_builder.py"`"
if [[ -z "$RUNNING_INSIDE_SANDBOX" ]]; then
  source "`dirname "$0"`/sandbox.sh"
  nested_script="`readlink -f "$0"`"
  pushd_to_root
  run_in_sandbox bash "$nested_script"
else
  echo "RUNNING_INSIDE_SANDBOX='$RUNNING_INSIDE_SANDBOX'"
  active_py_venv
  python "$python_script" --do_parse "$OPT_STEAM_DIR" "$OPT_AOE2_USR_DIR" "$OPT_TIMESERIES_DIR" "$OPT_LOG_LVL"
fi

