# Enumerating Polycubes

See https://github.com/noelle-crawfish/Enumerating-Polycubes

## Results from hand written code

### Python (single-threaded)

The goal is to use numpy to remove as many for loops in python as possible.

```
python3 count_cubes.py
[INFO] root::main  size=3, count=2
[INFO] root::print_time  : 0 secs
[INFO] root::main  size=4, count=8
[INFO] root::print_time  : 0 secs
[INFO] root::main  size=5, count=29
[INFO] root::print_time  : 0 secs
[INFO] root::main  size=6, count=166
[INFO] root::print_time  : 0 secs
[INFO] root::main  size=7, count=1023
[INFO] root::print_time  : 0 secs
[INFO] root::main  size=8, count=6922
[INFO] root::print_time  : 3 secs
[INFO] root::main  size=9, count=48311
[INFO] root::print_time  : 26 secs
[INFO] root::main  size=10, count=346543
[INFO] root::print_time  : 209 secs
```

### Rust (multi-threaded)

```
size=3  cubes=2, secs=0.000164961
size=4  cubes=8, secs=0.000150477
size=5  cubes=29, secs=0.000370131
size=6  cubes=166, secs=0.001448043
size=7  cubes=1023, secs=0.010167798
size=8  cubes=6922, secs=0.06545488
size=9  cubes=48311, secs=0.55243635
size=10 cubes=346543, secs=4.7072964
size=11 cubes=2522522, secs=40.469322
```

## Results from AI code (only correct implementations)

### Python

```
PYTHONPATH=. python3 ai_benchmark/chat_gpt_4_version.py
[INFO] root::main  size=3, count=2
[INFO] root::print_time  : 0 secs
[INFO] root::main  size=4, count=8
[INFO] root::print_time  : 0 secs
[INFO] root::main  size=5, count=29
[INFO] root::print_time  : 0 secs
[INFO] root::main  size=6, count=166
[INFO] root::print_time  : 1 secs
[INFO] root::main  size=7, count=1023
[INFO] root::print_time  : 11 secs
[INFO] root::main  size=8, count=6922
[INFO] root::print_time  : 86 secs
```

