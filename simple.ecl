

%:- lib(ic).

verheiratet(bjoern, cecilia).
verheiratet(andreas, steffi).


istVerheiratet(Mann, Frau) :- verheiratet(Mann, Frau).

  