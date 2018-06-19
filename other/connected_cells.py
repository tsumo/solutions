#!/usr/bin/env python3

"""
For a two-dimensional board finds the biggest region
of "connected" cells.
"""

board_1 = [ [1, 1, 2, 3],
            [1, 2, 2, 3],
            [4, 3, 3, 3] ]

board_2 = [ [1, 1, 2, 3, 3],
            [1, 2, 2, 3, 3],
            [4, 3, 3, 3, 3] ]

board_3 = [ [1, 1, 2, 3, 3],
            [1, 2, 2, 2, 1],
            [4, 3, 3, 1, 3] ]

def get_neighbors(row, col, board):
    rows = len(board) - 1
    cols = len(board[0]) - 1
    neighbors = []
    if row > 0:
        neighbors.append([row - 1, col])
    if col > 0:
        neighbors.append([row, col - 1])
    if row < rows:
        neighbors.append([row + 1, col])
    if col < cols:
        neighbors.append([row, col + 1])
    return neighbors

def print_board(board):
    for row in board:
        for elem in row:
            print(' ', elem, end='')
        print()

visited = []

def count_connected(row, col, n, board):
    visited.append([row, col])
    neighbors = get_neighbors(row, col, board)
    for neighbor in neighbors:
        if neighbor not in visited:
            if board[row][col] == board[neighbor[0]][neighbor[1]]:
                n += count_connected(*neighbor, 1, board)
    return n

def find_max(board):
    global visited
    visited = []
    print_board(board)
    maximum = 0
    for row in range(len(board)):
        for col in range(len(board[0])):
            count = count_connected(row, col, 1, board)
            if count > maximum:
                maximum = count
    return maximum

print(find_max(board_1))
print(find_max(board_2))
print(find_max(board_3))

