% -*- prolog -*-
% 2013, Bjoern Hoefling
%
% Just some tests for learning Prolog/ECLIPSE CLP
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lib(ic).
:- import alldifferent/1 from ic_global.

unary :-
  writeln("Hello, World!").

%some facts:
likes(alice, icecream).
likes(bob, pizza).
%likes(ben, X).

%from Sudoku example

print_board(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
	    	X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.

problem(1, [](
    [](_, _, 2, _, _, 5, _, 7, 9),
    [](1, _, 5, _, _, 3, _, _, _),
    [](_, _, _, _, _, _, 6, _, _),
    [](_, 1, _, 4, _, _, 9, _, _),
    [](_, 9, _, _, _, _, _, 8, _),
    [](_, _, 4, _, _, 9, _, 1, _),
    [](_, _, 9, _, _, _, _, _, _),
    [](_, _, _, 1, _, _, 3, _, 6),
    [](6, 8, _, 3, _, _, 4, _, _))).

solve(NumProblem) :-
 problem(NumProblem, Board),
 dim(Board, [N, N]),
% --print_board(Board).
% dim(Board, [N,N]),
 writeln("Hello, World!"),
% (for(I,1,10) do
%  writeln(I)).
 writeln(N),
 (for(I,1,N) do
   writeln(I)).

solve(2) :-
  dim(NewBoard, [1,2]),
  writeln(NewBoard).  

doIt(N, X) :-
 writeln(X).

%solve(1).

main :-
 solve(1).

%prime :-
% a(2, 3, 5, 7, 11).

% play around with alldifferent:
% call for example:
% mkdiff(1,A).
% mkdiff(7, A).
% mkdiff(1, [](1,2,3)).
% mkdiff(1,a(1,2,3)).

mkdiff(X, A) :-
 A =  [](X,_,_),
 A :: 1..5,
 alldifferent(A),
 writeln(A).

% Play around with term_variables and labeling.
% Yes, looks good!
termfoo(Board) :-
	Board[1..5] :: 1..5,
	term_variables(Board, Vars),
	labeling(Vars),
	writeln(Vars).

termbar :-
termfoo([](_,2,3,4,5)).