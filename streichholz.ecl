% -*- prolog -*-
:- lib(ic).

% (C) 2014, Andreas Petter and Björn Höfling
% Licence: TBD
%
% Idea: Lay out matches on the floor in a rectangled grid. 
% Given n matches, how many figures can you draw?
%
% n=4:
%     +--+
%     |  |
%     0->+
%
% n=6:
%     +--+--+
%     |     |
%     0--+--+
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% define a struct that represets a match (as in match-box) positioned on the floor.
:- local struct( match(xstart, ystart, xend, yend, direction)).

% Set constraint betweeten matches M0 and M1: 
% End coordinates of M0 shall be the same as start coordinates of M1,
% i.e. M1 "follows" M0.
% 
% Furthermore, the direction of Match1 must not be the opposite of Match0
%
setStartEndNew(M1, M0) :-
	M0 = match(_      , _      , XEnd0, Yend0, Direction0),
	M1 = match(XStart1, YStart1, _    , _    , Direction1),

	XStart1 #= XEnd0,
	YStart1 #= YEnd0,
        Direction0 #= 0 => Direction1 #\= 2,
        Direction0 #= 1 => Direction1 #\= 3,
        Direction0 #= 2 => Direction1 #\= 0,
        Direction0 #= 3 => Direction1 #\= 1.


generateVarConstraintsNew(1, [Z], Z, Z) :-
	writeln("generateVarConstraintsNew"),
	writeln(Z),
	Z=match(XStartPos, YStartPos, XEndPos, YEndPos, Direction),
	XStartPos #= 0,
	YStartPos #= 0,
	XEndPos   :: -1..1, %-10000..10000,
	YEndPos   :: -1..1, %-10000..10000,
	Direction  :: 0..3,
	Direction #= 0 => XEndPos #= XStartPos + 1,
	Direction #= 1 => XEndPos #= XStartPos,
	Direction #= 2 => XEndPos #= XStartPos - 1,
	Direction #= 3 => XEndPos #= XStartPos,

	Direction #= 0 => YEndPos #= YStartPos,
	Direction #= 1 => YEndPos #= YStartPos + 1,
	Direction #= 2 => YEndPos #= YStartPos,
	Direction #= 3 => YEndPos #= YStartPos - 1,
	nl
	.

generateVarConstraintsNew(N, [Z | [X | Results]], Last, First) :-
	Z = First,
	Z=match(XStartPos, YStartPos, XEndPos, YEndPos, Direction),
	XStartPos :: -10000..10000,
	YStartPos :: -10000..10000,
	XEndPos   :: -10000..10000,
	YEndPos   :: -10000..10000,
	Direction :: 0..3,

	Direction #= 0 => XEndPos #= XStartPos + 1,
	Direction #= 1 => XEndPos #= XStartPos,
	Direction #= 2 => XEndPos #= XStartPos - 1,
	Direction #= 3 => XEndPos #= XStartPos,

	Direction #= 0 => YEndPos #= YStartPos,
	Direction #= 1 => YEndPos #= YStartPos + 1,
	Direction #= 2 => YEndPos #= YStartPos,
	Direction #= 3 => YEndPos #= YStartPos - 1,

	%setStartEndNew(First,Last),
	Npred is N - 1,
	setStartEndNew(Z, X),
	generateVarConstraintsNew(Npred, [X | Results], Last, _).


%runNew(N) :-
doit(N) :- 
	%%M=match{xstart:0, ystart:0, xend:1, yend:0, direction:0},
	%%generateVarConstraintsNew(1, [M], M, M)
	generateVarConstraintsNew(N, ResultSet, Last, First),
	%setStartEndNew(Last, First),
	flatten(ResultSet, VarSet),
	writeln("Resultset:"),
	writeln(ResultSet),
	writeln("VarSet:"),
	writeln(VarSet),

	term_variables(VarSet, Vars),
	writeln("Vars:"),
	writeln(Vars),
	labeling(Vars),
	%%search(Vars, 0, input_order, indomain, complete, []),
	writeln(ResultSet)
	.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% BELOW THIS LINE: OLD IMPLEMENTATION.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setStartEnd(XStartPos, XEndPos, YStartPos, YEndPos, Richtung, NMinusOne) :-
        NMinusOne=[XStartPos1, XEndPos1, YStartPos1, YEndPos1, Richtung1],
        XStartPos #= XEndPos1,
        YStartPos #= YEndPos1,
        XStartPos #= XEndPos1,
        XStartPos #= XEndPos1,

        Richtung1 #= 0 => Richtung #\= 2,
        Richtung1 #= 1 => Richtung #\= 3,
        Richtung1 #= 2 => Richtung #\= 0,
        Richtung1 #= 3 => Richtung #\= 1

%	writeln(NMinusOne)
	.

setStartEnd2(A, B) :- 
	A = [XS1, XE1, YS1, YE1, Ri],
	setStartEnd(XS1, XE1, YS1, YE1, Ri, B).

generateVarConstraints(1, [Z], Z, Z) :-
	XStartPos #= 0,
	XEndPos   :: -10000..10000,
	YStartPos #= 0,
	YEndPos   :: -10000..10000,
	Richtung  :: 0..3,
	or(or(and(Richtung #= 0, XEndPos #= XStartPos + 1), and(Richtung #= 1, XEndPos #= XStartPos)), or(and(Richtung #= 2, XEndPos #= XStartPos - 1), and(Richtung #= 3, XEndPos #= XStartPos))), 
	or(or(and(Richtung #= 0, YEndPos #= YStartPos), and(Richtung #= 1, YEndPos #= YStartPos + 1)), or(and(Richtung #= 2, YEndPos #= YStartPos), and(Richtung #= 3, YEndPos #= YStartPos - 1))), 
	Z = [XStartPos, XEndPos, YStartPos, YEndPos, Richtung].

generateVarConstraints(N, [Z | [X | Results]], Last, Z) :-
	XStartPos :: -10000..10000,
	XEndPos   :: -10000..10000,
	YStartPos :: -10000..10000,
	YEndPos   :: -10000..10000,
	Richtung  :: 0..3,
	or(or(and(Richtung #= 0, XEndPos #= XStartPos + 1), and(Richtung #= 1, XEndPos #= XStartPos)), or(and(Richtung #= 2, XEndPos #= XStartPos - 1), and(Richtung #= 3, XEndPos #= XStartPos))), 
	or(or(and(Richtung #= 0, YEndPos #= YStartPos), and(Richtung #= 1, YEndPos #= YStartPos + 1)), or(and(Richtung #= 2, YEndPos #= YStartPos), and(Richtung #= 3, YEndPos #= YStartPos - 1))), 
	Z = [XStartPos, XEndPos, YStartPos, YEndPos, Richtung],
	F is N - 1,
	setStartEnd(XStartPos, XEndPos, YStartPos, YEndPos, Richtung, X),
	generateVarConstraints(F, [X | Results], Last, _).

run(N) :- 
	generateVarConstraints(N, ResultSet, Last, First),
	setStartEnd2(Last, First),
	flatten(ResultSet, VarSet),
	search(VarSet, 0, input_order, indomain, complete, []),
	writeln(ResultSet)
	.

setVarConstraints :-
	XStartPos0 #= 0,
	YStartPos0 #= 0,
	XStartPos1 :: -10000..10000,
	YStartPos1 :: -10000..10000,
	XStartPos2 :: -10000..10000,
	YStartPos2 :: -10000..10000,
	XStartPos3 :: -10000..10000,
	YStartPos3 :: -10000..10000,
	XStartPos0 #= XEndPos3,
	YStartPos0 #= YEndPos3,
	XStartPos1 #= XEndPos0,
	YStartPos1 #= YEndPos0,
	XStartPos2 #= XEndPos1,
	YStartPos2 #= YEndPos1,
	XStartPos3 #= XEndPos2,
	YStartPos3 #= YEndPos2,
	Richtung0 #>= 0,
	Richtung0 #=< 3,
	Richtung1 #>= 0,
	Richtung1 #=< 3,
	Richtung2 #>= 0,
	Richtung2 #=< 3,
	Richtung3 #>= 0,
	Richtung3 #=< 3,

	% X
	or(or(and(Richtung0 #= 0, XEndPos0 #= XStartPos0 + 1), and(Richtung0 #= 1, XEndPos0 #= XStartPos0)), or(and(Richtung0 #= 2, XEndPos0 #= XStartPos0 - 1), and(Richtung0 #= 3, XEndPos0 #= XStartPos0))), 
	or(or(and(Richtung1 #= 0, XEndPos1 #= XStartPos1 + 1), and(Richtung1 #= 1, XEndPos1 #= XStartPos1)), or(and(Richtung1 #= 2, XEndPos1 #= XStartPos1 - 1), and(Richtung1 #= 3, XEndPos1 #= XStartPos1))), 
	or(or(and(Richtung2 #= 0, XEndPos2 #= XStartPos2 + 1), and(Richtung2 #= 1, XEndPos2 #= XStartPos2)), or(and(Richtung2 #= 2, XEndPos2 #= XStartPos2 - 1), and(Richtung2 #= 3, XEndPos2 #= XStartPos2))),
	or(or(and(Richtung3 #= 0, XEndPos3 #= XStartPos3 + 1), and(Richtung3 #= 1, XEndPos3 #= XStartPos3)), or(and(Richtung3 #= 2, XEndPos3 #= XStartPos3 - 1), and(Richtung3 #= 3, XEndPos3 #= XStartPos3))),  
	
	% Y
	or(or(and(Richtung0 #= 0, YEndPos0 #= YStartPos0), and(Richtung0 #= 1, YEndPos0 #= YStartPos0 + 1)), or(and(Richtung0 #= 2, YEndPos0 #= YStartPos0), and(Richtung0 #= 3, YEndPos0 #= YStartPos0 - 1))), 
	or(or(and(Richtung1 #= 0, YEndPos1 #= YStartPos1), and(Richtung1 #= 1, YEndPos1 #= YStartPos1 + 1)), or(and(Richtung1 #= 2, YEndPos1 #= YStartPos1), and(Richtung1 #= 3, YEndPos1 #= YStartPos1 - 1))), 
	or(or(and(Richtung2 #= 0, YEndPos2 #= YStartPos2), and(Richtung2 #= 1, YEndPos2 #= YStartPos2 + 1)), or(and(Richtung2 #= 2, YEndPos2 #= YStartPos2), and(Richtung2 #= 3, YEndPos2 #= YStartPos2 - 1))), 
	or(or(and(Richtung3 #= 0, YEndPos3 #= YStartPos3), and(Richtung3 #= 1, YEndPos3 #= YStartPos3 + 1)), or(and(Richtung3 #= 2, YEndPos3 #= YStartPos3), and(Richtung3 #= 3, YEndPos3 #= YStartPos3 - 1))), 
	
	% Richtung
%        0 #\= 4 mod 2,
        Richtung0 #= 0 => Richtung1 #\= 2,
        Richtung0 #= 1 => Richtung1 #\= 3,
        Richtung0 #= 2 => Richtung1 #\= 0,
        Richtung0 #= 3 => Richtung1 #\= 1,

        Richtung1 #= 0 => Richtung2 #\= 2,
        Richtung1 #= 1 => Richtung2 #\= 3,
        Richtung1 #= 2 => Richtung2 #\= 0,
        Richtung1 #= 3 => Richtung2 #\= 1,

        Richtung2 #= 0 => Richtung3 #\= 2,
        Richtung2 #= 1 => Richtung3 #\= 3,
        Richtung2 #= 2 => Richtung3 #\= 0,
        Richtung2 #= 3 => Richtung3 #\= 1,

        Richtung3 #= 0 => Richtung0 #\= 2,
        Richtung3 #= 1 => Richtung0 #\= 3,
        Richtung3 #= 2 => Richtung0 #\= 0,
        Richtung3 #= 3 => Richtung0 #\= 1,



%	TmpR1 #= (Richtung0 + 2),
%	Richtung1 #\= TmpR1 mod 4,
%	Richtung2 #\= (Richtung1 + 2) mod 4,
%	Richtung3 #\= TmpR3, 
%	mod(Richtung2 + 2, 4, TmpR3),
%	Richtung0 #\= TmpR0,

%	mod(Richtung3 + 2, 4, TmpR0),

	search([Richtung0, Richtung1, Richtung2, Richtung3, XStartPos0, XStartPos1, XStartPos2, XStartPos3, YStartPos0, YStartPos1, YStartPos2, YStartPos3, XEndPos0, XEndPos1, XEndPos2, XEndPos3, YEndPos0, YEndPos1, YEndPos2, YEndPos3], 0, input_order, indomain, complete, []),
	writeln([Richtung0, Richtung1, Richtung2, Richtung3, XStartPos0, XStartPos1, XStartPos2, XStartPos3, YStartPos0, YStartPos1, YStartPos2, YStartPos3, XEndPos0, XEndPos1, XEndPos2, XEndPos3, YEndPos0, YEndPos1, YEndPos2, YEndPos3]),
	writeln("Richtung0, Richtung1, Richtung2, Richtung3, XStartPos0, XStartPos1, XStartPos2, XStartPos3, YStartPos0, YStartPos1, YStartPos2, YStartPos3, XEndPos0, XEndPos1, XEndPos2, XEndPos3, YEndPos0, YEndPos1, YEndPos2, YEndPos3")
	.  
	
main :- 
	setVarConstraints.
	