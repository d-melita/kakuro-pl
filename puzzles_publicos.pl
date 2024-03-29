
%-----------------------------------------------------------------------------
%                              Puzzles publicos
%-----------------------------------------------------------------------------
%-----------------------------------------------------------------------------
%                              puzzle_publico 1
%-----------------------------------------------------------------------------
puzzle_publico(1, [[[0, 0], [9, 0], [7, 0]],
           [[0, 9], _, _],
       [[0, 7], _, _]]).
%----------------
%|0\0 |12\0|3\0 |
%----------------
%|0\9 | 7  | 2  |
%----------------
%|0\6 | 5  | 1  |
%----------------
%
%----------------
%|0\0 |12\0|3\0 |
%----------------
%|0\9 | 8  | 1  |
%----------------
%|0\6 | 4  | 2  |
%----------------

%-----------------------------------------------------------------------------
%                              puzzle_publico 2
%-----------------------------------------------------------------------------
puzzle_publico(2, [[[0, 0], [0, 0], [0, 0], [17, 0], [10, 0]],
    [[0, 0], [24, 0], [11, 3], _, _],
    [[0, 16], _, _, _, _],
    [[0, 26], _, _, _, _],
    [[0, 17], _, _, [0, 0], [0, 0]]]).
%--------------------------
%|0\0 |0\0 |0\0 |17\0|10\0|
%--------------------------
%|0\0 |24\0|11\3| 2  | 1  |
%--------------------------
%|0\16| 7  | 1  | 6  | 2  |
%--------------------------
%|0\26| 8  | 2  | 9  | 7  |
%--------------------------
%|0\17| 9  | 8  |0\0 |0\0 |
%--------------------------

%-----------------------------------------------------------------------------
%                              puzzle_publico 3
%-----------------------------------------------------------------------------
puzzle_publico(3,
       [[[0, 0], [3, 0], [18, 0], [29, 0], [0, 0]],
       [[0, 11], _, _, _, [0, 0]],
       [[0, 19], _, _, _, [16, 0]],
       [[0, 0], [0, 22], _, _, _],
       [[0, 0], [0, 14], _, _, _]]).
%--------------------------
%|0\0 |3\0 |18\0|29\0|0\0 |
%--------------------------
%|0\11| 1  | 3  | 7  |0\0 |
%--------------------------
%|0\19| 2  | 8  | 9  |16\0|
%--------------------------
%|0\0 |0\22| 5  | 8  | 9  |
%--------------------------
%|0\0 |0\14| 2  | 5  | 7  |
%--------------------------

%-----------------------------------------------------------------------------
%                              puzzle_publico 4
%-----------------------------------------------------------------------------
puzzle_publico(4,
       [[[0, 0], [0, 0], [22, 0], [3, 0], [15, 0], [29, 0], [0, 0]],
       [[0, 0], [0, 21], _, _, _, _, [7, 0]],
       [[0, 0], [22, 16], _, _, _, _, _],
       [[0, 13], _, _, [0, 0], [0, 9], _, _],
       [[0, 15], _, _, [16, 0], [11, 10], _, _],
       [[0, 27], _, _, _, _, _, [0, 0]],
       [[0, 0], [0, 14],  _, _, _, _, [0, 0]]]).

%-------------------------------------------
%| 0\0 | 0\0 |22\0 | 3\0 |15\0 |29\0 | 0\0 |
%-------------------------------------------
%| 0\0 |0\21 |  5  |  1  |  9  |  6  | 7\0 |
%-------------------------------------------
%| 0\0 |22\16|  3  |  2  |  6  |  1  |  4  |
%-------------------------------------------
%|0\13 |  9  |  4  | 0\0 | 0\9 |  7  |  2  |
%-------------------------------------------
%|0\15 |  8  |  7  |16\0 |11\10|  9  |  1  |
%-------------------------------------------
%|0\27 |  5  |  2  |  9  |  7  |  4  | 0\0 |
%-------------------------------------------
%| 0\0 |0\14 |  1  |  7  |  4  |  2  | 0\0 |
%-------------------------------------------