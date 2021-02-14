source "`dirname "$0"`/config.sh"
source "`dirname "$0"`/sandbox.sh"

python_script="python/aoe2_apm_show.py"
if [[ ! -e "$python_script" ]]; then
  echo "[ERROR] the script must be run at the install base"
  exit 1
fi
run_in_sandbox bash -c "
  source py_venv/bin/activate
  python '$python_script' --do_parse '$OPT_STEAM_DIR' '$OPT_AOE2_USR_DIR' '$OPT_TIMESERIES_DIR' '$OPT_LOG_LVL' $@
"


