#!/usr/bin/env python3

"""
For a two-dimensional board finds the biggest region
of "connected" cells, i.e. those that have the same values.
"""

board_1 = [[1, 1, 2, 3],
           [1, 2, 2, 3],
           [4, 3, 3, 3]]

board_2 = [[1, 1, 2, 3, 3],
           [1, 2, 2, 3, 3],
           [4, 3, 3, 3, 3]]

board_3 = [[1, 1, 2, 3, 3],
           [1, 2, 2, 2, 1],
           [4, 3, 3, 1, 3]]

def get_neighbors(row, col, board):
    """
    Returns up, down, left and right neighbors of the given cell.
    Checks for board boundaries.
    """
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

# Holds already checked cells
visited = []

def count_connected(row, col, n, board):
    """
    For a given cell recursively finds and counts all same-value cells.
    """
    visited.append([row, col])
    neighbors = get_neighbors(row, col, board)
    for neighbor in neighbors:
        if neighbor not in visited:
            if board[row][col] == board[neighbor[0]][neighbor[1]]:
                n += count_connected(*neighbor, 1, board)
    return n

def find_max(board):
    """
    Finds the biggest region of same-value cells.
    """
    global visited
    visited = []
    [print(row) for row in board]
    maximum = 0
    for row in range(len(board)):
        for col in range(len(board[0])):
            if [row, col] not in visited:
                count = count_connected(row, col, 1, board)
                if count > maximum:
                    maximum = count
    print(maximum)
    return maximum

assert find_max(board_1) == 5
assert find_max(board_2) == 8
assert find_max(board_3) == 4

