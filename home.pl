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
room(living_room).
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
location(sofa, living_room).
location(cat_bowl, living_room).
location(car, garage).
location(pizza, kitchen).
location(apple, kitchen).
location(soda, kitchen).
location(brocolli, kitchen).
location(fridge, kitchen).

is_contained_in(T1, T2) :- location(T1, T2).
is_contained_in(T1, T2) :- location(X, T2), is_contained_in(T1, X).

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
door(living_room, corridor).

is_opened(my_room, corridor).
is_opened(office, corridor).
is_opened(mothers_room, corridor).
is_opened(bathroom, corridor).

opened_door(From, To) :- opened_door(From, To).
opened_door(From, To) :- opened_door(To, From).

turned_off(laptop).
turned_off(console).
turned_off(tv).
turned_off(lamp).

here(kitchen).

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
    have(X), tab(2), write(X), nl, is_contained_in(Y, X), tab(2), write(Y), nl.


eat(Thing) :- have(Thing), edible(Thing), retract(have(Thing)), write('yummy').

drink(X) :- have(X), drinkable(X), retract(have(X)).

list_things(Place) :- is_contained_in(X, Place), tab(2),
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
take(Thing, In) :- here(Place), location(In, Place), 
    is_contained_in(Thing, In), retract(location(Thing, In)),
    assert(have(Thing)), write('taken'), nl.

take_in(Thing) :- is_contained_in(Thing, In), 
    retract(location(Thing, In)), assert(have(Thing)), write('taken'), nl, fail.
% take_in(Thing) :- here(Place), is_contained_in(Thing, Place), retract(location()).
put(X) :- have(X), here(Y), assert(location(X, Y)), retract(have(X)).

put(Thing, In) :- have(Thing), here(Place), location(In, Place),
    assert(location(Thing, In)), retract(have(Thing)).

drop(X) :- have(X), breakable(X), retract(have(X)), write("broken"), nl.
drop(X) :- put(X), write("dropped"), nl.

turn_on :- have(lamp), retract(turned_off(lamp)), assert(turned_on(lamp)).

turn_off :- have(lamp), retract(turned_on(lamp)), assert(turned_off(lamp)).

turn_on(X) :- have(X), retract(turned_off(X)), assert(turned_on(X)). 
turn_on(X) :- here(Y), location(X, Y), retract(turned_off(X)), assert(turned_on(X)).

turn_off(X) :- have(X), retract(turned_on(X)), assert(turned_off(X)). 
turn_off(X) :- here(Y), location(X, Y), retract(turned_on(X)), assert(turned_off(X)).

% is_opened always_opened check_door
