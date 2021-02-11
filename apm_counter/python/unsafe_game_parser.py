# This is random code from the internet so it should only be run within a sandbox
import os
#https://www.freedesktop.org/software/systemd/man/systemd.exec#Environment%20Variables%20in%20Spawned%20Processes
if 'INVOCATION_ID' not in os.environ:
  raise Exception('Not invoked inside a systemd sandbox')

from mgz import header, body
from mgz.summary import Summary
from datetime import timedelta

comment='''
dir_root = '/media/llewelyn_data_b/SteamLibrary/steamapps/compatdata/813780/pfx/drive_c/users/steamuser/Games/Age of Empires 2 DE/76561198367239985/savegame'
filepath = dir_root + '/MP Replay v101.101.43210.0 @2021.01.25 235247 (1).aoe2record'

cur_time = 0
with open(filepath, 'rb') as data:
  s = Summary(data)
  print(s.get_map()['name']) # can also get map size
  #eof = os.fstat(data.fileno()).st_size
  #h = header.parse_stream(data)
  #b = body.meta.parse_stream(data)
  #while data.tell() < eof:
  #  o = body.operation.parse_stream(data)
  #  if o.type == 'sync': cur_time += o.time_increment
    #if o.type == 'action' and o.action.type == 'create': # gets nothing
    #if o.type == 'action' and o.action.type == 'order': # not super interesting
    #if o.type == 'action' and o.action.type == 'add_attribute': # nothing
    #  td = timedelta(seconds=int(cur_time/1000))
    #  print(str(td), 'player', o.action.player_id, 'attribute', o.action.attribute, 'amount', o.action.amount)
    #if o.type == 'action' and o.action.type == 'de_queue':
    #  td = timedelta(seconds=int(cur_time/1000))
    #  print(str(td), 'player', o.action.player_id, 'building', list(o.action.building_ids), 'unit', o.action.unit_type, 'amount', o.action.queue_amount, 'selected', o.action.selected)
    #if o.type == 'action' and o.action.type == 'research' and o.action.technology_type in (101, 102, 103, 104):
    #  td = timedelta(seconds=int(cur_time/1000))
    #  print(str(td), 'player', o.action.player_id, 'building', o.action.building_id, 'tech', o.action.technology_type)

#td = timedelta(seconds=int(cur_time/1000))
#print('last time', str(td))
#print('last time (1.3)', str(td/1.3))
#print('last time (1.5)', str(td/1.5))
#print('last time (1.7)', str(td/1.7))
#print('last time (2.0)', str(td/2.0))
#print(h.de)
#print(h.replay)

#Container: 
#    type = sync (total 4)
#    start = 885599
#    op = 2
#    time_increment = 40
#    next = 3
#    checksum = None
#    end = 885607
#Container: 
#    type = action (total 6)
#    start = 887223
#    op = 1
#    length = 28
#    action = Container: 
#        type_int = 101
#        type = research (total 8)
#        building_id = 4920
#        player_id = 4
#        next = Container: 
#            check = -1
#        selected = 1
#        technology_type = 101
#        selected_ids = ListContainer: 
#            -1
#    end = 887263
'''
