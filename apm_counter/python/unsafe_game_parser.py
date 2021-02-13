# This is random code from the internet so it should only be run within a sandbox
import logging
import os
import datetime as dt
# https://www.freedesktop.org/software/systemd/man/systemd.exec#Environment%20Variables%20in%20Spawned%20Processes
if 'INVOCATION_ID' not in os.environ:
  raise Exception('Not invoked inside a systemd sandbox')

# https://github.com/happyleavesaoc/aoc-mgz
import mgz
import mgz.body
from datetime import timedelta

import io_util
import replay_pb2
import util

def add_game_information(game_details, header):
  # or header.scenario.game_settings.map_id
  map_id = header.de.selected_map_id
  game_details.map_name = mgz.const.DE_MAP_NAMES.get(map_id, '')
  game_details.map_size = header.map_info.size_x
  game_details.game_speed = header.de.speed

def add_player_information(game_details, header):
  idx_map = [ None for _ in range(len(header.de.players)) ]
  for idx,player in enumerate(header.de.players):
    if player.type != 'human': continue
    idx_map[idx] = len(game_details.players)
    pb_player = game_details.players.add()
    pb_player.name = player.name.value.decode()
    pb_player.civ_id = player.civ_id
  return idx_map

def add_action_information(game_details, idx_map, replay_file):
  size = os.path.getsize(replay_file.name)
  cur_millis = 0
  while replay_file.tell() < size:
    op = mgz.body.operation.parse_stream(replay_file)
    if op.type == 'sync':
      cur_millis += op.time_increment
      continue
    if op.type == 'action':
      idx = op.action.search('player_id')
      if idx == None or idx >= len(idx_map) or idx_map[idx] == None: continue
      action = game_details.players[idx_map[idx]].actions.add()
      action.offset_millis = cur_millis
      action.type = op.action.type_int
      action.tech_id = op.action.search('technology_type') or 0
      action.unit_type = op.action.search('unit_type') or 0
      action.building_type = op.action.search('building_type') or 0
      action.amount = int(op.action.search('queue_amount') or op.action.search('amount') or 0)

def write_game_details_internal(conf, filepath):
  logging.info("Parsing file: %r", filepath)
  game_details = replay_pb2.GameDetails()
  with open(filepath, 'rb') as replay_file:
    header = mgz.header.parse_stream(replay_file)
    if header.version != mgz.util.Version.DE:
      raise Exception('Not AOE2_DE replay version: %r', filepath)
    mgz.body.meta.parse_stream(replay_file) # discarted
    add_game_information(game_details, header)
    idx_map = add_player_information(game_details, header)
    if not idx_map:
      logging.warning("No players found ? %r", filepath)
    else:
      add_action_information(game_details, idx_map, replay_file)
  return game_details

def write_game_details(parser_input):
  try:
    game_details = write_game_details_internal(parser_input.conf, parser_input.filepath)
    str_date = dt.datetime.fromtimestamp(parser_input.start_secs).strftime("%Y-%m-%d-%H%M")
    filepath = os.path.join(parser_input.conf.ts_root, "game_details_%s_%s.pb.gz"
                            % (game_details.map_name.replace(' ', ''), str_date))
    io_util.serialize_to_gz_file(filepath, game_details)
    duration = int(game_details.duration_millis / (game_details.game_speed*1000))
    return util.ParserResult(True, filepath, duration)
  except:
    logging.exception("Failed parsing %r", parser_input.filepath)
    return util.ParserResult(False, '', 0)

def main():
  conf = util.init_conf()
  util.init_logging(conf)
  game_details = write_game_details_internal(conf, conf.replay_file)
  
if __name__ == '__main__':
  main()

