mortal(X) :- person(X).

person(socrates).
person(plato).
person(aristotle).

mortal_report:- write('Known mortals are: '), nl, 
mortal(X),
write(X), nl,
fail.
