source "`dirname "$0"`/config.sh"
source "`dirname "$0"`/sandbox.sh"

run_in_sandbox python aoe2_replay_parser.py "$OPT_AOE2_RPL_DIR" "$OPT_REPLAY_PARSE_REPO"

