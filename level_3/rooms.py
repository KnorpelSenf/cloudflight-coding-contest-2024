import re

f = open("input/level3_5.in", "r")
f.readline()

for line in f:
  curr_table = 1
  x,y,rooms = [int(s) for s in re.findall(r'\d+', line)]

  matrix = [[0 for x_dim in range(x)] for y_dim in range(y)] 

  for i in range(y):
    remaining_rows = y - i

    for j in range(x):
      if matrix[i][j] != 0:
        continue

      remaining_cells = x - j

      if remaining_cells >= 3:
        matrix[i][j] = curr_table
        matrix[i][j+1] = curr_table
        matrix[i][j+2] = curr_table
        curr_table += 1
        continue

      if remaining_rows >= 3:
        matrix[i][j] = curr_table
        matrix[i+1][j] = curr_table
        matrix[i+2][j] = curr_table
        curr_table += 1
        continue

  
  for i in range(y):
    for j in range(x):
      print(matrix[i][j], end=" ")
    print()

  print()
