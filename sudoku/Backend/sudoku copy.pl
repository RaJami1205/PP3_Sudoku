:- use_module(library(clpfd)).
:- use_module(library(random)).

% Referencias:
% Resolución de sudoku 9x9: https://www.swi-prolog.org/pldoc/man?section=clpfd-sudoku
% Ejemplo de implementación de sudoku en prolog: https://github.com/triska/clpz
% Ejemplo de creación de un sudoku 9x9 en prolog: https://github.com/KentGrigo/Sudoku/blob/main/README.md

% =========================================
% sudoku(+Rows)
% Entradas: Rows - Lista de 9 listas (filas del Sudoku) con variables o valores entre 1 y 9
% Salidas: Rows - Mismo tablero con todas las celdas resueltas
% Restricciones: Cada fila, columna y subcuadro 3x3 debe tener valores distintos entre 1 y 9
% Objetivo: Resolver un tablero completo de Sudoku
% =========================================
sudoku(Rows) :-
    length(Rows, 9), maplist(same_length(Rows), Rows),
    append(Rows, Vs), Vs ins 1..9,

    maplist(all_distinct, Rows),

    transpose(Rows, Columns),
    maplist(all_distinct, Columns),

    Rows = [A,B,C,D,E,F,G,H,I],
    blocks(A,B,C), blocks(D,E,F), blocks(G,H,I),

    maplist(labeling([ff]), Rows).

% =========================================
% blocks(+Row1, +Row2, +Row3)
% Entradas: Row1, Row2, Row3 - tres listas representando filas contiguas del Sudoku
% Objetivo: Verificar que cada bloque 3x3 tenga valores distintos
% =========================================
blocks([], [], []).
blocks([A1,A2,A3|As1], [B1,B2,B3|Bs1], [C1,C2,C3|Cs1]) :-
    all_distinct([A1,A2,A3,B1,B2,B3,C1,C2,C3]),
    blocks(As1, Bs1, Cs1).

% =========================================
% generate_full_board(-Board)
% Salidas: Board - Un tablero completamente resuelto (9x9)
% Objetivo: Generar una solución completa válida de Sudoku
% =========================================
generate_full_board(Board) :-
    length(Board, 9),
    maplist(length_(9), Board),
    sudoku(Board).

% =========================================
% length_(+L, -List)
% Entradas: L - longitud deseada
% Salidas: List - lista de longitud L
% Objetivo: Ayuda a construir listas de longitud L
% =========================================
length_(L, List) :- length(List, L).

% =========================================
% remove_cells(+Board, +NumToRemove, -Puzzle)
% Entradas: Board - tablero completo de Sudoku
%           NumToRemove - número de celdas a borrar
% Salidas: Puzzle - tablero con algunas celdas vacías
% Restricciones: NumToRemove debe estar entre 17 y 64
% Objetivo: Remover celdas aleatoriamente para generar un puzzle
% =========================================
remove_cells(Board, NumToRemove, Puzzle) :-
    flatten(Board, Flat),
    positions(81, Positions),
    random_permutation(Positions, Shuffled),
    length(ToRemove, NumToRemove),
    append(ToRemove, _, Shuffled),
    remove_at_indices(Flat, ToRemove, FlatRemoved),
    reshape(FlatRemoved, 9, Puzzle).

% =========================================
% positions(+N, -Positions)
% Entradas: N - número de posiciones (usualmente 81)
% Salidas: Positions - lista [0, 1, ..., N-1]
% Objetivo: Generar índices para recorrer el tablero como lista plana
% =========================================
positions(N, Positions) :-
    N1 is N - 1,
    findall(I, between(0, N1, I), Positions).

% =========================================
% remove_at_indices(+List, +Indices, -Result)
% Entradas: List - lista original
%           Indices - índices a reemplazar con variables
% Salidas: Result - nueva lista con variables en los índices dados
% Objetivo: Reemplazar ciertos elementos por variables anónimas
% =========================================
remove_at_indices(List, Indices, Result) :-
    remove_at_indices(List, 0, Indices, Result).

remove_at_indices([], _, _, []).
remove_at_indices([X|Xs], Index, Indices, [Y|Ys]) :-
    (   member(Index, Indices)
    ->  Y = _
    ;   Y = X
    ),
    Index1 is Index + 1,
    remove_at_indices(Xs, Index1, Indices, Ys).

% =========================================
% reshape(+List, +N, -Matrix)
% Entradas: List - lista plana
%           N - número de elementos por fila
% Salidas: Matrix - matriz NxN
% Objetivo: Convertir una lista en una matriz de NxN
% =========================================
reshape([], _, []).
reshape(List, N, [Row|Rows]) :-
    length(Row, N),
    append(Row, Rest, List),
    reshape(Rest, N, Rows).

% =========================================
% unique_solution(+Puzzle)
% Entradas: Puzzle - tablero parcial de Sudoku
% Objetivo: Verificar que el tablero tenga una única solución
% =========================================
unique_solution(Puzzle) :-
    findnsols(2, Puzzle, sudoku(Puzzle), Solutions),
    length(Solutions, 1).

% =========================================
% generate_random_puzzle(-Puzzle, -Solution)
% Salidas: Puzzle - tablero con espacios vacíos
%          Solution - tablero completamente resuelto
% Objetivo: Generar un puzzle aleatorio con solución única
% =========================================
generate_random_puzzle(Puzzle, Solution) :-
    generate_full_board(Solution),
    between(56, 64, NumToRemove),
    %remove_cells(Solution, NumToRemove, Puzzle),
    unique_solution(Puzzle), !.

% =========================================
% Generar e imprimir tablero con solución
% para verificar la funcionalidad del backend
% =========================================
print_board([]).
print_board([Row|Rows]) :-
    print_row(Row),
    print_board(Rows).

print_row([]) :- nl.
print_row([Cell|Cells]) :-
    (var(Cell) -> write('. ') ; format('~w ', [Cell])),
    print_row(Cells).

generate_and_show :-
    generate_random_puzzle(Puzzle, Solution),
    write('Puzzle:'), nl,
    print_board(Puzzle), nl,
    write('Solution:'), nl,
    print_board(Solution).
