import re
import sys

def safe_set(matrix, x, y, value):
  if x >= 0 and x < len(matrix) and y >= 0 and y < len(matrix[0]):
    matrix[x][y] = value

f = open(f"{sys.argv[1]}", "r")
f.readline()

def place_submatrix(matrix, x, y, submatrix):
  for i in range(len(submatrix)):
    for j in range(len(submatrix[0])):
      safe_set(matrix, x + i, y + j, submatrix[i][j])

def solve(x, y):
  matrix = [["." for x_dim in range(x)] for y_dim in range(y)] 

  if (x <= 0 or y <= 0):
    print("1")
    if (x < 0 or y < 0):
      print(f"error ?? {x},{y}")
    return matrix
  if (x == 1 and y == 1):
    print("2")
    return matrix
  elif (x == 2):
    print("3")
    place_submatrix(matrix, 0, 0, [
      ["X", "X"]
    ])
    place_submatrix(matrix, 0, 2, solve(x, y-2))
    print(matrix)
  elif (y == 2):
    print("4")
    place_submatrix(matrix, 0, 0, [
      ["X"],
      ["X"]
    ])
    place_submatrix(matrix, 2, 0, solve(x-2, y))
  elif (x == 1):
    print("5")
    place_submatrix(matrix, 0, 0, [
      ["X"],
      ["X"]
    ])
    place_submatrix(matrix, 0, 3, solve(x, y-3))
  elif (y == 1):
    print("6")
    place_submatrix(matrix, 0, 0, [
      ["X", "X"]
    ])
    place_submatrix(matrix, 3, 0, solve(x-3, y))
  elif (x == 3):
    place_submatrix(matrix, 0, 0, [
      ["X", ".", "X"],
      ["X", ".", "X"]
    ])
    place_submatrix(matrix, 0, 3, solve(x, y-3))
  elif (y == 3):
    place_submatrix(matrix, 0, 0, [
      ["X", "X"],
      [".", "."],
      ["X", "X"]
    ])
    place_submatrix(matrix, 3, 0, solve(x-3, y))
  elif (x % 2 == 1):
    place_submatrix(matrix, 0, 0, [
      ["X" if i % 2 == 0 else "." for i in range(x)],
      ["X" if i % 2 == 0 else "." for i in range(x)],
    ])
    place_submatrix(matrix, 0, 3, solve(x, y-3))
  elif (y % 2 == 1):
    place_submatrix(matrix, 0, 0, [["X", "X"] if i % 2 == 0 else [".", "."] for i in range(y)])
    place_submatrix(matrix, 3, 0, solve(x-3, y))
  else:
    # assert x % 2 == 0 and y % 2 == 0
    if (x % 2 != 0):
      print(f"error x not even!: {x},{y}")

    horizontal = solve(x - 3, 2)
    print('hor')
    print(horizontal)
    vertical = solve(2, y - 3)
    print('vert')
    print(vertical)
    # 1: place top row
    place_submatrix(matrix, 0, 0, horizontal)
    # 2: place right row
    place_submatrix(matrix, x - 2, 0, vertical)
    # 3: place bottom row
    place_submatrix(matrix, 3, y - 2, horizontal)
    # 4: place left row
    place_submatrix(matrix, 0, 3, vertical)
    print('border done')
    print(matrix)
    # 5: solve center
    place_submatrix(matrix, 3, 3, solve(x - 6, y - 6))
    print('center done')
    print(matrix)


  return matrix

for line in f:
  print(solve(2,3))
  x,y,tables = [int(s) for s in re.findall(r'\d+', line)]
  matrix = solve(x, y)

  xes_placed = 0
  for i in range(y):
    for j in range(x):
      if matrix[i][j] == 0:
        matrix[i][j] = "."
      if matrix[i][j] == "X":
        xes_placed += 1
      print(matrix[i][j], end="")
    print()
  print()
  
  if xes_placed != tables * 2:
    raise ValueError(f"Not enough tables placed for problem: {line}, placed: {xes_placed}, expected: {tables * 2}")