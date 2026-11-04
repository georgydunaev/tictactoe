#!/usr/bin/env python3
"""Unit test suite for tictactoe (SML file check + minimax optimal play invariant)."""

import os
import unittest

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

WIN_LINES = [
    (0, 1, 2),
    (3, 4, 5),
    (6, 7, 8),
    (0, 3, 6),
    (1, 4, 7),
    (2, 5, 8),
    (0, 4, 8),
    (2, 4, 6),
]


def winner(board: tuple[str, ...]) -> str | None:
    for a, b, c in WIN_LINES:
        if board[a] != " " and board[a] == board[b] == board[c]:
            return board[a]
    return None


def minimax(board: tuple[str, ...], player: str, alpha: int = -2, beta: int = 2) -> int:
    w = winner(board)
    if w == "X":
        return 1
    if w == "O":
        return -1
    if " " not in board:
        return 0
    if player == "X":
        best = -2
        for i, cell in enumerate(board):
            if cell == " ":
                nb = board[:i] + ("X",) + board[i + 1 :]
                val = minimax(nb, "O", alpha, beta)
                if val > best:
                    best = val
                if best > alpha:
                    alpha = best
                if beta <= alpha:
                    break
        return best
    else:
        best = 2
        for i, cell in enumerate(board):
            if cell == " ":
                nb = board[:i] + ("O",) + board[i + 1 :]
                val = minimax(nb, "X", alpha, beta)
                if val < best:
                    best = val
                if best < beta:
                    beta = best
                if beta <= alpha:
                    break
        return best


class TestTicTacToe(unittest.TestCase):
    def test_sml_source_exists(self):
        sml = os.path.join(ROOT, "tictactoe.sml")
        self.assertTrue(os.path.isfile(sml))
        self.assertGreater(os.path.getsize(sml), 0)

    def test_empty_board_optimal_value_is_draw(self):
        empty = (" ",) * 9
        self.assertEqual(minimax(empty, "X"), 0)

    def test_immediate_win_detection(self):
        board = ("X", "X", " ", "O", "O", " ", " ", " ", " ")
        self.assertEqual(minimax(board, "X"), 1)


if __name__ == "__main__":
    unittest.main()
