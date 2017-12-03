#! /bin/bash

PARAMS_TYPE=$1
REPORT_PATH=$2
EXECUTABLE=$3
declare -a PARAM_SPACE

define_no_overwrite() {
  local var_name="$1"
  shift
  if [[ -z "${!var_name}" ]]; then
    eval "$var_name=( $@ )"
  fi
  local aref="${var_name}[@]"
  PARAM_SPACE+=( "$var_name = ${!aref}" )
}

adjust_parameter_space() {
  define_no_overwrite BENCH_RERUN_COUNT 20
  define_no_overwrite BENCH_THREAD_ITERATIONS 200000
  define_no_overwrite BENCH_ALL_THR_WAIT 0
  define_no_overwrite MAX_ACQUIRE_SWAP_ATTEMPTS 32
  define_no_overwrite MAX_RELEASE_SWAP_ATTEMPTS 64
  define_no_overwrite MORE_ATO_LESS_CONTENTION 1
  define_no_overwrite BENCH_FUNCTION_RANGE ALL

  if [[ "$PARAMS_TYPE" == '--explore' ]]; then
    define_no_overwrite BENCH_ALL_THR_COUNT_RANGE 4 6 16
    define_no_overwrite MEMORY_ORDER_REL_LOCK_RANGE memory_order_acq_rel memory_order_release
    define_no_overwrite MEMORY_ORDER_ACQ_LOCK_RANGE memory_order_acq_rel memory_order_acquire
    define_no_overwrite WAIT_QUANTUM_RANGE 0 127 511
    define_no_overwrite BENCH_FANCY_ALIGN_RANGE 0 1
  elif [[ "$PARAMS_TYPE" == '--scale' ]]; then
    define_no_overwrite BENCH_ALL_THR_COUNT_RANGE 2 3 4 5 6 7 8 9 10 11 12 13 14 15 18 21 24
    define_no_overwrite MEMORY_ORDER_REL_LOCK_RANGE memory_order_acq_rel
    define_no_overwrite MEMORY_ORDER_ACQ_LOCK_RANGE memory_order_acq_rel
    define_no_overwrite WAIT_QUANTUM_RANGE 127
    define_no_overwrite BENCH_FANCY_ALIGN_RANGE 0
  else
    define_no_overwrite BENCH_ALL_THR_COUNT_RANGE 4 6
    define_no_overwrite MEMORY_ORDER_REL_LOCK_RANGE memory_order_acq_rel
    define_no_overwrite MEMORY_ORDER_ACQ_LOCK_RANGE memory_order_acq_rel
    define_no_overwrite WAIT_QUANTUM_RANGE 127
    define_no_overwrite BENCH_FANCY_ALIGN_RANGE 1
  fi
  printf "%s, " "${PARAM_SPACE[@]}"
  echo
}

do_parametric_benchmarking() {
  [[ -e "$REPORT_PATH" ]] && rm "$REPORT_PATH"
  touch "$REPORT_PATH"

  for bench_func in ${BENCH_FUNCTION_RANGE[@]}; do
  for thread_cnt in ${BENCH_ALL_THR_COUNT_RANGE[@]}; do
  for memory_rel in ${MEMORY_ORDER_REL_LOCK_RANGE[@]}; do
  for memory_acq in ${MEMORY_ORDER_ACQ_LOCK_RANGE[@]}; do
  for wait_quant in ${WAIT_QUANTUM_RANGE[@]}; do
  for fancy_algn in ${BENCH_FANCY_ALIGN_RANGE[@]}; do

    echo "$bench_func $thread_cnt $memory_rel $memory_acq $wait_quant $fancy_algn"
    user_cflags="\
    -DBENCH_RERUN_COUNT=$BENCH_RERUN_COUNT\
    -DBENCH_FUNCTION=$bench_func\
    -DBENCH_FANCY_ALIGN=$fancy_algn\
    -DBENCH_ALL_THR_COUNT=$thread_cnt\
    -DBENCH_ALL_THR_WAIT=$BENCH_ALL_THR_WAIT\
    -DBENCH_THREAD_ITERATIONS=$BENCH_THREAD_ITERATIONS\
    -DMAX_ACQUIRE_SWAP_ATTEMPTS=$MAX_ACQUIRE_SWAP_ATTEMPTS\
    -DMAX_RELEASE_SWAP_ATTEMPTS=$MAX_RELEASE_SWAP_ATTEMPTS\
    -DWAIT_QUANTUM=$wait_quant\
    -DMORE_ATO_LESS_CONTENTION=$MORE_ATO_LESS_CONTENTION\
    -DMEMORY_ORDER_REL_LOCK=$memory_rel\
    -DMEMORY_ORDER_ACQ_LOCK=$memory_acq\
    "
    make user_cflags="$user_cflags" clean_opt opt > /dev/null
    if [[ $? != 0 ]]; then
      echo -e "Failed compilation with config : \n$user_cflags"
      exit 1
    fi

    "$EXECUTABLE" >> "$REPORT_PATH"
    if [[ $? != 0 ]]; then
      echo -e "Failed bench run with config : \n$user_cflags"
      exit 2
    fi

  done
  done
  done
  done
  done
  done
}
adjust_parameter_space
do_parametric_benchmarking

