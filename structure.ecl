% -*- prolog -*-
%
% Playing with "struct"s
% See here for official documentation of struct:
% http://eclipseclp.org/doc/userman/umsroot022.html#@default148
%

:- lib(ic).

% define a struct that represets a match (as in match-box) positioned on the floor.
:- local struct( match(xstart, ystart, xend, yend, direction)).

% Test weather it is this direction or not:
isDirection(D, Match) :-
	arg(direction of match, Match, Direction),
	D #= Direction.

% YES:
% isYear(23, match{direction:23}).
% NO:
% isYear(42, match{direction:23}).


%extract elements and compare:
setStartEnd(M0, M1) :-
	M0 = match(XStart0, YStart0, XEnd0, Yend0, Direction0),
	M1 = match(XStart1, YStart1, XEnd1, Yend1, Direction1),
	XStart1 = XEnd0.

doIt(Dir) :-
					
	isDirection(Dir,match{direction:north}).
%	setStartEnd(match{xend:23}, match{xstart:23}).



%    book{}
%    book{title:'tom sawyer'}
%    book{title:'tom sawyer', year:1886, author:twain}
%which translate to the corresponding forms%
%
%    book(_, _, _, _)
%    book(_, 'tom sawyer', _, _)
%    book(twain, 'tom sawyer', 1886, _)

%This transformation is done by the parser, therefore it can be used in any context and is as efficient as using the structures directly.

%The argument index of a field in a structure can be obtained using a term of the form

%    FieldName of StructName

%For example, to access (i.e., unify) a single argument of a structure use arg/3 like this:

%    ..., arg(year of book, B, Y), ...
