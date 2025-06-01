:- use_module(library(clpfd)).
:- use_module(library(random)).

% =========================================
% random_labeling(+Vars)
% 
% Entradas:
%   +Vars: Lista de variables finitas a etiquetar.
%
% Salida:
%   Asigna valores a Vars usando un orden aleatorio.
%
% Objetivo:
%   Etiquetar las variables en orden aleatorio para evitar generar
%   siempre la misma solución del Sudoku.
% =========================================
random_labeling(Vars) :-
    random_permutation(Vars, ShuffledVars),
    labeling([], ShuffledVars).

% =========================================
% sudoku(+Rows)
%
% Entradas:
%   +Rows: Lista de listas de 9 filas, cada una con 9 elementos.
%
% Salida:
%   Las filas son completadas con números del 1 al 9 cumpliendo
%   las reglas del Sudoku: filas, columnas y bloques 3x3 distintos.
%
% Restricción:
%   Rows debe tener dimensión 9x9.
%
% Objetivo:
%   Resolver un Sudoku o generar una solución válida.
% =========================================
sudoku(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs), Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [A,B,C,D,E,F,G,H,I],
    blocks(A,B,C), blocks(D,E,F), blocks(G,H,I),
    random_labeling(Vs).

% =========================================
% blocks(+Row1, +Row2, +Row3)
%
% Entradas:
%   +Row1, +Row2, +Row3: Tres filas consecutivas del tablero.
%
% Objetivo:
%   Verificar que los bloques 3x3 correspondientes tengan valores distintos.
% =========================================
blocks([], [], []).
blocks([A1,A2,A3|As1], [B1,B2,B3|Bs1], [C1,C2,C3|Cs1]) :-
    all_distinct([A1,A2,A3,B1,B2,B3,C1,C2,C3]),
    blocks(As1, Bs1, Cs1).

% =========================================
% generate_full_board(-Board)
%
% Salida:
%   -Board: Tablero 9x9 completamente solucionado.
%
% Objetivo:
%   Generar una solución completa de Sudoku.
% =========================================
generate_full_board(Board) :-
    length(Board, 9),
    maplist(length_(9), Board),
    sudoku(Board).

% =========================================
% length_(+L, -List)
%
% Entradas:
%   +L: Longitud deseada.
%   -List: Lista de longitud L.
%
% Objetivo:
%   Crear una lista de longitud L.
% =========================================
length_(L, List) :- length(List, L).

% =========================================
% remove_cells(+Board, +NumToRemove, -Puzzle)
%
% Entradas:
%   +Board: Tablero 9x9 completo.
%   +NumToRemove: Cantidad de celdas a vaciar.
%
% Salida:
%   -Puzzle: Tablero con NumToRemove celdas vacías.
%
% Objetivo:
%   Generar un puzzle removiendo aleatoriamente celdas del tablero completo.
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
%
% Entradas:
%   +N: Número total de posiciones.
%
% Salida:
%   -Positions: Lista [0,1,...,N-1].
%
% Objetivo:
%   Generar índices para las posiciones del tablero.
% =========================================
positions(N, Positions) :-
    N1 is N - 1,
    findall(I, between(0, N1, I), Positions).

% =========================================
% remove_at_indices(+List, +Indices, -Result)
%
% Entradas:
%   +List: Lista de elementos.
%   +Indices: Posiciones a vaciar.
%
% Salida:
%   -Result: Lista con variables en lugar de los elementos removidos.
%
% Objetivo:
%   Reemplazar ciertas posiciones por variables no instanciadas (vacías).
% =========================================
remove_at_indices(List, Indices, Result) :-
    remove_at_indices(List, 0, Indices, Result).

remove_at_indices([], _, _, []).
remove_at_indices([X|Xs], Index, Indices, [Y|Ys]) :-
    (   member(Index, Indices) -> Y = _ ; Y = X ),
    Index1 is Index + 1,
    remove_at_indices(Xs, Index1, Indices, Ys).

% =========================================
% reshape(+FlatList, +N, -Matrix)
%
% Entradas:
%   +FlatList: Lista plana de tamaño N*N.
%   +N: Número de columnas por fila.
%
% Salida:
%   -Matrix: Lista de listas (matriz) N x N.
%
% Objetivo:
%   Convertir una lista plana en una matriz de NxN.
% =========================================
reshape([], _, []).
reshape(List, N, [Row|Rows]) :-
    length(Row, N),
    append(Row, Rest, List),
    reshape(Rest, N, Rows).

% =========================================
% unique_solution(+Puzzle)
%
% Entradas:
%   +Puzzle: Sudoku parcialmente lleno.
%
% Salida:
%   true si solo se puede encontrar una forma de resolverlo.
%
% Objetivo:
%   Determinar si el puzzle tiene una única solución (estructuralmente única).
% =========================================
unique_solution(Puzzle) :-
    copy_term(Puzzle, Copy),
    count_distinct_solutions(Copy, [], 0, Count),
    Count =:= 1.

% =========================================
% count_distinct_solutions(+Puzzle, +PrevSolutions, +Acc, -Count)
%
% Entradas:
%   +Puzzle: Puzzle a resolver.
%   +PrevSolutions: Lista de soluciones ya encontradas.
%   +Acc: Contador acumulador.
%
% Salida:
%   -Count: Número de soluciones distintas encontradas (máximo 2).
%
% Objetivo:
%   Contar soluciones *estructuralmente diferentes* del Sudoku.
% =========================================
count_distinct_solutions(_, _, 2, 2) :- !.  % Corta si ya hay 2 distintas
count_distinct_solutions(Puzzle, PrevSolutions, Acc, Count) :-
    Acc < 2,
    (   sudoku(Puzzle),
        \+ memberchk(Puzzle, PrevSolutions)
    ->  Acc1 is Acc + 1,
        count_distinct_solutions(Puzzle, [Puzzle|PrevSolutions], Acc1, Count)
    ;   Count = Acc
    ).

% =========================================
% generate_random_puzzle(-Puzzle, -Solution)
%
% Salidas:
%   -Puzzle: Sudoku parcialmente lleno con solución única.
%   -Solution: Solución completa del puzzle.
%
% Objetivo:
%   Generar un puzzle válido con entre 17 y 29 pistas, y solución única.
% =========================================
generate_random_puzzle(Puzzle, Solution) :-
    repeat,
        generate_full_board(Solution),
        RandomPistas is 17 + random(13),  % 17 a 29 pistas
        NumToRemove is 81 - RandomPistas,
        remove_cells(Solution, NumToRemove, Puzzle),
        unique_solution(Puzzle), !.

% =========================================
% replace_vars_with_null(+BoardWithVars, -BoardWithNulls)
%
% Entradas:
%   +BoardWithVars: Lista de listas que representa el tablero, 
%                   con posibles variables no instanciadas (espacios vacíos).
%
% Salidas:
%   -BoardWithNulls: Mismo tablero donde las variables son reemplazadas por 'null'.
%
% Objetivo:
%   Convertir un tablero que contiene variables de Prolog en una representación
%   adecuada para JSON, usando 'null' donde hay celdas vacías.
% =========================================
replace_vars_with_null([], []).
replace_vars_with_null([Row|Rest], [RowConverted|RestConverted]) :-
    maplist(var_to_null, Row, RowConverted),
    replace_vars_with_null(Rest, RestConverted).

% =========================================
% var_to_null(+Cell, -Converted)
%
% Entradas:
%   +Cell: Celda individual del tablero, que puede ser una variable o un número.
%
% Salidas:
%   -Converted: 'null' si Cell es una variable; el mismo valor si no lo es.
%
% Objetivo:
%   Convertir una celda del tablero en 'null' si está vacía.
% =========================================
var_to_null(Cell, null) :- var(Cell), !.
var_to_null(Cell, Cell).


% =========================================
% generate_board_json/0
% =========================================
% Imprime el puzzle generado en formato JSON para ser leído desde C#
generate_board_json :-
    generate_random_puzzle(Puzzle, Solution),
    replace_vars_with_null(Puzzle, ConvertedPuzzle),
    format('~q', [json{ 'puzzle': ConvertedPuzzle, 'solution': Solution }]),
    halt.

% =========================================
% print_board(+Board)
%
% Entradas:
%   +Board: Tablero 9x9 (completo o con variables).
%
% Objetivo:
%   Imprimir el tablero de forma legible, usando '.' para celdas vacías.
% =========================================
print_board([]).
print_board([Row|Rows]) :-
    print_row(Row),
    print_board(Rows).

print_row([]) :- nl.
print_row([Cell|Cells]) :-
    (var(Cell) -> write('. ') ; format('~w ', [Cell])),
    print_row(Cells).

% =========================================
% count_clues(+Puzzle, -Count)
%
% Entradas:
%   +Puzzle: Sudoku con algunas celdas llenas.
%
% Salida:
%   -Count: Cantidad de pistas visibles (celdas no vacías).
%
% Objetivo:
%   Contar cuántas celdas del puzzle están ya definidas.
% =========================================
count_clues(Board, Count) :-
    flatten(Board, Flat),
    include(nonvar, Flat, Filled),
    length(Filled, Count).

% =========================================
% generate_and_show/0
%
% Objetivo:
%   Genera un puzzle aleatorio, lo imprime junto con la cantidad de pistas,
%   y muestra su solución.
% =========================================
generate_and_show :-
    generate_random_puzzle(Puzzle, Solution),
    count_clues(Puzzle, Count),
    format('Puzzle (~w pistas visibles):~n', [Count]),
    print_board(Puzzle), nl,
    write('Solution:'), nl,
    print_board(Solution).
