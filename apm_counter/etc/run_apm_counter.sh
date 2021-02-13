source "`dirname "$0"`/config.sh"

apm_bin="gobin/apm_counter"
if [[ ! -e "$apm_bin" ]]; then
  echo "[ERROR] the script must be run at the install base"
  exit 1
fi
sudo --preserve-env=TEMP -g input \
  "$apm_bin" "$OPT_INPUT_DEVS" "$OPT_TIMESERIES_DIR" "$OPT_LOG_LVL"

