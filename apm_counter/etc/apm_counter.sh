#! /bin/bash

source "`dirname "$0"`/config.sh"

apm_bin="`get_bin_path "gobin/apm_counter"`"
sudo --preserve-env=TEMP -g input \
  "$apm_bin" "$OPT_INPUT_DEVS" "$OPT_TIMESERIES_DIR" "$OPT_LOG_LVL"

