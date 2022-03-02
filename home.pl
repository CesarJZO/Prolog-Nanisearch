% home.pl file %

:- dynamic here/1.
:- dynamic location/2.
:- dynamic have/1.
:- dynamic turned_off/1.
:- dynamic turned_on/1.
:- dynamic turn_on/1.
:- dynamic turn_off/1.

room(kitchen).
room(my_room).
room(office).
room(mothers_room).
room(bathroom).
room(corridor).
room('living room').
room(garage).

location(table, kitchen).
location(tv, kitchen).
location('jug of water', kitchen).
location(console, my_room).
location(bed, my_room).
location(controller, my_room).
location(desk, office).
location(laptop, desk).
location(lamp, office).
location(mirror, mothers_room).
location(frame, mothers_room).
location(plant, mothers_room).
location(toilet, bathroom).
location(towel, bathroom).
location(soap, bathroom).
location(sofa, 'living room').
location(cat_bowl, 'living room').
location(car, garage).
location(pizza, kitchen).
location(apple, kitchen).
location(soda, kitchen).
location(brocolli, kitchen).

edible(pizza).
edible(apple).
tastes_yucky(brocolli).

drinkable(soda).

breakable(console).
breakable(frame).
breakable(cat_bowl).

door(my_room, corridor).
door(office, corridor).
door(mothers_room, corridor).
door(bathroom, corridor).
door(kitchen, corridor).
door(corridor, garage).
door('living room', corridor).

turned_off(laptop).
turned_off(console).
turned_off(tv).
turned_off(lamp).

here(corridor).

report :- location(X, kitchen), edible(X), write(X), nl, fail.

where_food(X, Y) :- location(X, Y), edible(X).
where_food(X, Y) :- location(X, Y), tastes_yucky(X).

connect(X, Y) :- door(X, Y).
connect(X, Y) :- door(Y, X).

% goto(Place) :- here(X), connect(X, Place),
%    retract(here(X)), assert(here(Place)).

% take(Thing) :- here(X), location(Thing, X),
%    retract(location(Thing, X)), assert(have(Thing)),
%    write(taken).

inventory :- write("You have in inventory:"), nl, 
    have(X), tab(2),  write(X), nl.

eat(Thing) :- have(Thing), edible(Thing), retract(have(Thing)), write('yummy').

drink(X) :- have(X), drinkable(X), retract(have(X)).

list_things(Place) :- location(X, Place), tab(2),
    write(X), nl, fail.
list_things(_).

list_connections(Place) :- connect(X, Place), tab(2),
    write(X), nl, fail.
list_connections(_).

look :- here(Place), write(Place), nl, 
    write('You can see: '), nl, list_things(Place),
    write('You can go to: '), nl, list_connections(Place), fail.

look_in(Thing) :- location(X, Thing), list_things(X).

can_go(Place) :- here(X), connect(X, Place).

move(Place) :- retract(here(_)), assert(here(Place)).

goto(Place) :- can_go(Place), move(Place), look.

can_take(Thing) :- here(Place), location(Thing, Place).

take_object(X) :- here(Y), retract(location(X, Y)), 
    assert(have(X)), write('taken'), nl.

take(X) :- can_take(X), take_object(X).


put(X) :- have(X), here(Y), assert(location(X, Y)), retract(have(X)).

drop(X) :- have(X), breakable(X), retract(have(X)), write("broken"), nl.
drop(X) :- put(X).

turn_on :- have(lamp), retract(turned_off(lamp)), assert(turned_on(lamp)).

turn_off :- have(lamp), retract(turned_on(lamp)), assert(turned_off(lamp)).

turn_on(X) :- have(X), retract(turned_off(X)), assert(turned_on(X)). 
turn_on(X) :- here(Y), location(X, Y), retract(turned_off(X)), assert(turned_on(X)).

turn_off(X) :- have(X), retract(turned_on(X)), assert(turned_off(X)). 
turn_off(X) :- here(Y), location(X, Y), retract(turned_on(X)), assert(turned_off(X)).
