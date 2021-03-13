# This is meant to be sourced

shell_in_sandbox() {
  __preamble__=(
    systemd-run
    --unit "sandboxed_shell"
    --shell
  )
  __run_in_sandbox__
}

run_in_sandbox() {
  __preamble__=(
    systemd-run
    --unit "sandboxed_$1"
    --quiet --wait --collect --pty
  )
  __run_in_sandbox__ "$@"
}

__run_in_sandbox__() {
  local cur_wd="`readlink -f .`"
  local user="${SUDO_USER:-$USER}"
  if [[ ! -z "$RUNNING_INSIDE_SANDBOX" ]]; then
    echo "[ERROR] Cannot recursively invoke sandboxes"
    return 1
  fi
  if [[ "`id -u "$user"`" == "0" ]]; then
    echo "[ERROR] user cannot be root SUDO_USER='$SUDO_USER', USER='$USER'"
    return 1
  fi
  echo "[SANDBOXED] $@"
  # Will request root privileges to set the sandbox unless the function is run via sudo
  # Note ProcSubset=pid will prevent reading files like /proc/cpuinfo
  "${__preamble__[@]}" \
    -p User="$user" \
    -p Group="`id -g "$user"`" \
    -p Environment="RUNNING_INSIDE_SANDBOX=$RANDOM" \
    -p NoNewPrivileges=true \
    -p ProtectSystem=strict \
    -p ProtectHome=tmpfs \
    -p ProtectKernelTunables=true \
    -p ProtectKernelModules=true \
    -p ProtectProc=noaccess \
    -p WorkingDirectory="$cur_wd" \
    -p BindPaths="$cur_wd" \
    -p ReadWritePaths="$cur_wd" \
    -p InaccessiblePaths="/opt/aur /boot /sys /root" \
    -p PrivateNetwork=true \
    -p PrivateTmp=true \
    -p PrivateDevices=true \
    "$@"
  # Note the usage of BindPaths, it is needed if the working directory is within a protected path
  # ProtectProc+ProcSubset will most of the files inside /proc not accessible (but pid dirs can be listed)
}

