import re

f = open("input/level1_5.in", "r")
f.readline()
for x in f:
  y = [int(s) for s in re.findall(r'\d+', x)]
  print(int(y[0] / 3 * y[1]))