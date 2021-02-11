header_de_example = '''
  version = 1029.0999755859375
  interval_version = 1000
  game_options_version = 1
  dlc_count = 6
  dlc_ids = ListContainer: ...
  dataset_ref = 2
  difficulty = standard (total 8)
  selected_map_id = 116
  resolved_map_id = 116
  reveal_map = 0
  victory_type_id = 9
  victory_type = standard2 (total 9)
  starting_resources_id = 0
  starting_resources = standard (total 8)
  starting_age_id = 0
  starting_age = standard (total 8)
  ending_age_id = 0
  ending_age = standard (total 8)
  game_type = 0
  speed = 2.0
  treaty_length = 0
  population_limit = 200
  num_players = 5
  unused_player_color = 0
  victory_amount = 4294967295
  trade_enabled = True
  team_bonus_disabled = False
  random_positions = False
  all_techs = False
  num_starting_units = 0
  lock_teams = True
  lock_speed = False
  multiplayer = True
  cheats = False
  record_game = True
  animals_enabled = True
  predators_enabled = True
  turbo_enabled = False
  shared_exploration = False
  team_positions = True
  players = ListContainer:
'''

header_de_player_example = '''
  dlc_id = 8
  color_id = 0
  selected_color = 255
  selected_team_id = 1
  resolved_team_id = 1
  dat_crc = |\xc9\xe6$H\x01\x00\x00 (total 8)
  mp_game_version = 0
  civ_id = 10
  ai_type = Container:
      length = 0
      value =  (total 0)
  ai_civ_name_index = 0
  ai_name = Container:
      length = 0
      value =  (total 0)
  name = Container:
      length = 27
      value = le_maire_dessomes_sur_marne (total 27)
  type = human (total 5)
  profile_id = 2905460
  player_number = 1
  hd_rm_elo = 1600
  hd_dm_elo = 1600
  animated_destruction_enabled = False
  custom_ai = False
'''

header_map_info_example = '''
  size_x = 200
  size_y = 200
  tile_num = 40000
  zone_num = 8
  all_visible = False
  fog_of_war = True
  check = Container:
      val = 4294901814
  tile = ListContainer:
      Container:
          terrain_type = 58
          elevation = 0
          unk0 = -1
          unk1 = -1
      ...
'''

header_scenario_example = '''
  scenario_header = Container:
      next_uid = 0
      constant = 33\xb3? (total 4)
      names = ListContainer: (byte strings ...)
      player_ids = ListContainer: (ints ...)
      player_data = ListContainer: (not sure what this means, since there are more than 8)
          Container:
              active = 0
              human = 1
              civilization = 36
              constant = 36
      elapsed_time = 0.0
      scenario_filename =  (total 0)
  messages = Container:
      instruction_id = 4294967294
      hints_id = 4294967294
      victory_id = 4294967294
      defeat_id = 4294967294
      history_id = 4294967294
      scouts_id = 4294967294
      instructions_length = 496
      instructions = Standard Game: Win this game by constructing a Wonder, by captur... (truncated, total 496)
      hints =  (total 0)
      victory =  (total 0)
      defeat =  (total 0)
      history =  (total 0)
      scouts =  (total 0)
      pg_cin =  (total 0)
      vict_cin =  (total 0)
      loss_cin =  (total 0)
      background =  (total 0)
      bitmap_included = 3
      bitmap_x = 0
      bitmap_y = 0
  players = Container:
      ai_names = ListContainer: (byte strings ...)
      ai = ListContainer: ...
      resources = ListContainer:
          Container:
              gold = 16843009
              wood = 16843009
              food = 16843009
              stone = 4294967197
              unk0 = 0
              unk1 = 0
  victory = Container:
      is_conquest = 1
      relics = 0
      explored = 0
      all = 0
      mode = 0
      score = 900
      time = 9000
  disables = Container:
  game_settings = Container:
      starting_ages = ListContainer:
      map_id = 116
      difficulty_id = 0
      difficulty = hardest (total 7)
      lock_teams = 0
      player_info = ListContainer:
          Container:
              data_ref = 0
              type = closed (total 6)
              name = Gaia\x00 (total 5)
          Container:
              data_ref = 1
              type = human (total 5)
              name = Player1\x00 (total 8)
          Container:
              data_ref = 4294967295
              type = closed (total 6)
              name = Closed\x00 (total 7)
  triggers = Container:
      num_triggers = 0
'''
body_op_sync_example = '''
  type = sync (total 4)
  start = 885599
  op = 2
  time_increment = 40
  next = 3
  checksum = None
  end = 885607
'''
body_op_action_research_example = '''
  type = action (total 6)
  start = 887223
  op = 1
  length = 28
  action = Container: 
      type_int = 101
      type = research (total 8)
      building_id = 4920
      player_id = 4
      next = Container: 
          check = -1
      selected = 1
      technology_type = 101
      selected_ids = ListContainer: 
          -1
  end = 887263
'''
body_op_chat_example = '''
  type = chat (total 4)
  start = 342461
  op = 4
  length = 55
  text = {"player":1,"channel":1,"message":"gg","messageAGP":""} (total 55)
  end = 342528
'''
body_op_action_build_example = '''
  type = action (total 6)
  start = 353489
  op = 1
  length = 32
  action = Container:
      type_int = 102
      type = build (total 5)
      selected = 1
      player_id = 3
      x = 34.0
      y = 113.0
      building_type = 70
      sprite_id = 65538
      unit_ids = ListContainer:
          65535
  end = 353533
'''
body_op_action_de_queue_example = '''
  type = action (total 6)
  start = 440889
  op = 1
  length = 14
  action = Container:
      type_int = 129
      type = de_queue (total 8)
      player_id = 1
      building_type = 109
      selected = 1
      unit_type = 83
      queue_amount = 4
      building_ids = ListContainer: (int list ...)
  end = 440915
'''


