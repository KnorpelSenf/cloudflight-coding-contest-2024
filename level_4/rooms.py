import re
import sys

def safe_set(matrix, x, y, value):
  if x >= 0 and x < len(matrix) and y >= 0 and y < len(matrix[0]):
    matrix[x][y] = value

f = open(f"{sys.argv[1]}", "r")
f.readline()

for line in f:
  x,y,tables = [int(s) for s in re.findall(r'\d+', line)]

  matrix = [[0 for x_dim in range(x)] for y_dim in range(y)] 

  for i in range(y):
    remaining_rows = y - i

    for j in range(x):
      if matrix[i][j] != 0:
        continue

      remaining_cells = x - j

      if remaining_cells >= 3:
        # place dots above
        safe_set(matrix, i-1, j-1, ".")
        safe_set(matrix, i-1, j,   ".")
        safe_set(matrix, i-1, j+1, ".")
        safe_set(matrix, i-1, j+2, ".")
        safe_set(matrix, i-1, j+3, ".")
        # place desk
        safe_set(matrix, i, j-1,  ".")
        safe_set(matrix, i, j,    "X")
        safe_set(matrix, i, j+1,  "X")
        safe_set(matrix, i, j+2,  "X")
        safe_set(matrix, i, j+3,  ".")
        # place dots below
        safe_set(matrix, i+1, j-1, ".")
        safe_set(matrix, i+1, j,   ".")
        safe_set(matrix, i+1, j+1, ".")
        safe_set(matrix, i+1, j+2, ".")
        safe_set(matrix, i+1, j+3, ".")
        
        continue

      if remaining_rows >= 3:
        # place dots above
        safe_set(matrix, i-1, j-1, ".")
        safe_set(matrix, i  , j-1, ".")
        safe_set(matrix, i+1, j-1, ".")
        safe_set(matrix, i+2, j-1, ".")
        safe_set(matrix, i+3, j-1, ".")
        # place desk
        safe_set(matrix, i-1, j,  ".")
        safe_set(matrix, i  , j,  "X")
        safe_set(matrix, i+1, j,  "X")
        safe_set(matrix, i+2, j,  "X")
        safe_set(matrix, i+3, j,  ".")
        # place dots below
        safe_set(matrix, i-1, j+1, ".")
        safe_set(matrix, i  , j+1, ".")
        safe_set(matrix, i+1, j+1, ".")
        safe_set(matrix, i+2, j+1, ".")
        safe_set(matrix, i+3, j+1, ".")
        continue

  xes_placed = 0
  for i in range(y):
    for j in range(x):
      if matrix[i][j] == 0:
        matrix[i][j] = "."
      if matrix[i][j] == "X":
        xes_placed += 1
      print(matrix[i][j], end="")
    print()

  if xes_placed != tables * 3:
    raise ValueError(f"Not enough tables placed for problem: {line}, placed: {xes_placed}, expected: {tables * 3}")

  print()

