import sys
from collections import defaultdict

INVALID_CHARS = ',:;.()[]{}'
def clear(w):
	return w.strip(INVALID_CHARS)

m = defaultdict(int)
for line in sys.stdin:
	for word in line.split():
		m[clear(word)] += 1

for item in sorted(m.items(), key=lambda item: item[1], reverse=True):
	print(item[0], item[1])
