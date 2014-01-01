% -*- prolog -*-
:- lib(ic).

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
	