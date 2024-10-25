import re

f = open("input/level2_5.in", "r")
f.readline()

for line in f:
  curr_table = 1
  x,y,rooms = [int(s) for s in re.findall(r'\d+', line)]

  roomsPerRow = int(x / 3)

  for i in range(y):
    for j in range(roomsPerRow):
      print(curr_table, end=" ")
      print(curr_table, end=" ")
      print(curr_table, end=" ")
      curr_table += 1
    print()
  print()
