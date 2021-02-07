set -e
source "sandbox.sh"

fail_if_cmd_ok() {
  if "$@"; then false; fi
}

# better run this as sudo otherwise every invocation of systemd-run will ask for pw
run_in_sandbox_test_cases() {
  local workdir="${TEMP:-/tmp}/run_in_sandbox_test_cases"
  [[ -d "$workdir" ]] || mkdir "$workdir"
  local user="${SUDO_USER:-$USER}"
  chown --recursive "${user}:${user}" "$workdir"
  pushd "$workdir" > /dev/null

  # The whole damn thing actually works
  run_in_sandbox pwd
  # The commands args are taken into account
  run_in_sandbox echo 1 2 3 4 | grep '1 2 3 4'
  # Can change write files inside the working dir
  rm "chocoloco" || true
  run_in_sandbox touch "./chocoloco"
  test -e "chocoloco"
  # Is not invoked as root
  [[ "$user" != "root" ]] || false
  run_in_sandbox whoami | grep "$user"
  # Cannot write certain paths, but they are readable
  for target_dir in "/etc" "/usr"; do
    test -d "$target_dir"
    fail_if_cmd_ok run_in_sandbox touch "$target_dir/zzz_sandbox_failure"
    test ! -e "$target_dir/zzz_sandbox_failure"
    run_in_sandbox test -d "$target_dir"
  done
  # My home is protected
  test -d "/home/$user"
  fail_if_cmd_ok run_in_sandbox test -d "/home/$user"
  # Cannot read certain paths
  for target_dir in "/proc/$$" "/boot"; do
    test -d "$target_dir"
    fail_if_cmd_ok run_in_sandbox ls "$target_dir"
  done
  # Cannot connect to the internet
  fail_if_cmd_ok run_in_sandbox ping -c 3 -i 0.5 "8.8.8.8"
  fail_if_cmd_ok run_in_sandbox dig "+timeout=1" "+tries=1" "+short" google.com
  # Can write to /tmp but it is gone after
  rm "/tmp/chocoloco" || true
  run_in_sandbox touch "/tmp/chocoloco"
  test ! -e "/tmp/chocoloco"
  # Cannot read input devices
  fail_if_cmd_ok run_in_sandbox ls /dev/input

  popd > /dev/null
  echo "ALL tests OK"
}

run_in_sandbox_test_cases

