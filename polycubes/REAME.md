# Enumerating Polycubes

See https://github.com/noelle-crawfish/Enumerating-Polycubes

The goal is to use numpy to remove as many for loops in python as possible.

## Results from hand written code

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

## Results from AI code (only correct implementations)

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

