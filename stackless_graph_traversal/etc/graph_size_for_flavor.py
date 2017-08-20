import sys

size_per_flavor = {
	'opt' : '2M',
	'dbg' : '256K',
}

target = sys.argv[1]
flavor = sys.argv[2]
sys.stdout.write(size_per_flavor[flavor])

