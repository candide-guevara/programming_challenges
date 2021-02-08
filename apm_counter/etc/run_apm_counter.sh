source "`dirname "$0"`/config.sh"

apm_bin=apm_counter
[[ -e "$apm_bin" ]] || apm_bin="bin/gobin/apm_counter"
[[ -e "$apm_bin" ]] || apm_bin="../bin/gobin/apm_counter"
[[ -e "$apm_bin" ]] || exit 1
sudo --preserve-env=TEMP -g input \
  "$apm_bin" "$OPT_INPUT_DEVS" "$OPT_TIMESERIES_DIR" "$OPT_LOG_LVL"

