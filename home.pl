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

object(table, brown, big, 50).
object(tv, black, big, 30).
object(jug_of_water, beige, big, 15).
object(console, white, small, 5).
object(bed, blue, big, 50).
object(controller, gray, small, 1).
object(desk, gray, big, 40).
object(laptop, silver, small, 3).
object(lamp, beige, small, 4).
object(mirror, reflecitve, small, 1).
object(frame, begie, big, 20).
object(plant, green, small, 6).
object(toilet, white, big, 30).
object(towel, brown, small, 2).
object(soap, green, small, 1).
object(sofa, brown, big, 40).
object(cat_bowl, silver, small, 1).
object(car, red, big, 1000).
object(pizza, red, small, 4).
object(apple, green, small, 2).
object(soda, black, small, 4).
object(brocolli, green, small, 1).
object(fridge, black, big, 60).

location(object(table, brown, big, 50), kitchen).
location(object(tv, black, big, 30), kitchen).
location(object(jug_of_water, beige, big, 15), kitchen).
location(object(console, white, small, 5), my_room).
location(object(bed, blue, big, 50), my_room).
location(object(controller, gray, small, 1), my_room).
location(object(desk, gray, big, 40), office).
location(object(laptop, silver, small, 3), object(desk, gray, big, 40)).
location(object(lamp, beige, small, 4), office).
location(object(mirror, reflecitve, small, 1), mothers_room).
location(object(frame, begie, big, 20), mothers_room).
location(object(plant, green, small, 6), mothers_room).
location(object(toilet, white, big, 30), bathroom).
location(object(towel, brown, small, 2), bathroom).
location(object(soap, green, small, 1), bathroom).
location(object(sofa, brown, big, 40), living_room).
location(object(cat_bowl, silver, small, 1), living_room).
location(object(car, red, big, 1000), garage).
location(object(pizza, red, small, 4), kitchen).
location(object(apple, green, small, 2), kitchen).
location(object(soda, black, small, 4), kitchen).
location(object(brocolli, green, small, 1), kitchen).
location(object(fridge, black, big, 60), kitchen).

is_contained_in(T1, T2) :-
    location(object(T1, _, _, _), T2).
is_contained_in(T1, T2) :-
    location(object(X, _, _, _), T2),
    is_contained_in(T1, X).

edible(object(pizza, red, small, 4)).
edible(object(apple, green, small, 2)).
tastes_yucky(object(brocolli, _, _, _)).

drinkable(object(soda, _, _, _)).

breakable(object(console, _, _, _)).
breakable(object(frame, _, _, _)).
breakable(object(cat_bowl, _, _, _)).

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

turned_off(object(tv, _, _, _)).
turned_off(object(console, _, _, _)).
turned_off(object(laptop, _, _, _)).
turned_off(object(lamp, _, _, _)).

here(kitchen).

% report :- location(object(X, _, _, _), kitchen), edible(object(X, _, _, _)), write(X), nl, fail.
report :- location(object(X, _, _, _), kitchen), write(X), nl, fail.

where_food(X, Y) :- location(X, Y), edible(X).
where_food(X, Y) :- location(X, Y), tastes_yucky(X).

connect(X, Y) :- door(X, Y).
connect(X, Y) :- door(Y, X).

% % % % % %  - Inventory -  % % % % % %

inventory :- write("You have in inventory:"), nl, 
    have(object(Thing, _, _, _)), tab(2),
    write(Thing), nl, is_contained_in(In, Thing), 
    tab(2), write(object(In, _, _, _)), nl.

% % % % % %  - Eat -  % % % % % %

eat(Thing) :- have(object(Thing, _, _, _)),
    tastes_yucky(object(Thing, _, _, _)),
    write('Gross >m<'), fail.
eat(Thing) :- not(have(object(Thing, _, _, _))),
    write('You don\'t have a '), write(Thing), fail.
eat(Thing) :- have(object(Thing, _, _, _)),
    edible(object(Thing, _, _, _)), 
    retract(have(object(Thing, _, _, _))), write('yummy').

% % % % % %  - Drink -  % % % % % %

drink(Thing) :- not(have(object(Thing, _, _, _))),
    write('You don\'t have a '), write(Thing), fail.
drink(Thing) :- have(object(Thing, _, _, _)),
    not(drinkable(object(Thing, _, _, _))),
    write('You can\'t drink that!'), fail.
drink(Thing) :- have(object(Thing, _, _, _)),
    drinkable(object(Thing, _, _, _)),
    retract(have(object(Thing, _, _, _))), write('Refreshing!'), fail.

list_things(Place) :- 
    is_contained_in(Thing, Place),
    tab(2), write(Thing), nl, fail.
list_things(_).

% % % % % %  - List things -  % % % % % %

list_things_s(Place) :-
    location(object(Thing, Color, Size, Weight), Place),
    write('A '), write(Size), tab(1), write(Color), tab(1),
    write(Thing), write(', weighing '), write(Weight),
    write(' pounds'), nl, fail.
list_things_s(_).

list_connections(Place) :- connect(X, Place), tab(2),
    write(X), nl, fail.
list_connections(_).


% % % % % %  - Look -  % % % % % %

look :- turned_off(object(lamp, _, _, _)), write('You can\'t see anything').
look :- here(Place), write(Place), nl, 
    write('You can see: '), nl, list_things_s(Place),
    write('You can go to: '), nl, list_connections(Place), fail.

look_in(Thing) :- location(X, Thing), list_things_s(X).

can_go(Place) :- here(X), connect(X, Place).

move(Place) :- retract(here(_)), assert(here(Place)).

goto(Place) :- can_go(Place), move(Place), look.

% % % % % %  - Take -  % % % % % %

can_take(Thing) :- here(Place),
    location(object(Thing, _, small, _), Place).
can_take(Thing) :- here(Place), 
    location(object(Thing, _, big, _), Place),
    write('The '), write(Thing), write(' is too big to carry.'),
    nl, fail.
can_take(Thing) :- here(Place),
    not(location(object(Thing, _, _, _), Place)),
    write('There is no '), write(Thing), write(' here.'),
    nl, fail.

take_object(Thing) :- here(Place), retract(location(object(Thing, X, Y, Z), Place)), 
    assert(have(object(Thing, X, Y, Z))), write('taken'), nl.
take_object(_).

take(X) :- can_take(X), take_object(X).
take(Thing, In) :- here(Place), location(In, Place), 
    is_contained_in(Thing, In), retract(location(Thing, In)),
    assert(have(Thing)), write('taken'), nl.

take_in(Thing) :- object(Thing, _, _, _), object(In, _, _, _), is_contained_in(Thing, In), 
    retract(location(object(Thing, _, _, _), object(In, _, _, _))), 
    assert(have(object(Thing, _, _, _))), write('taken'), nl, fail.

% % % % % %  - Put -  % % % % % 

put(Thing) :- not(have(object(Thing, _, _, _))), 
    write('You don\'t have a '), write(Thing), nl, fail.
put(Thing) :- have(object(Thing, X, Y, Z)), here(Place), 
    assert(location(object(Thing, X, Y, Z), Place)), 
    retract(have(object(Thing, X, Y, Z))).

put(Thing, In) :- have(object(Thing, XT, YT, ZT)), here(Place), 
    location(object(In, XI, YI, ZI), Place),
    assert(location(object(Thing, XT, YT, ZT), object(In, XI, YI, ZI))), 
    retract(have(object(Thing, XT, YT, ZT))).

% % % % % %  - Drop -  % % % % % 

drop(X) :- have(object(X, _, _, _)), breakable(object(X, _, _, _)),
    retract(have(object(X, _, _, _))), write("broken"), nl.
drop(X) :- put(X), write("dropped"), nl.

turn_on :- have(object(lamp, _, _, _)),
    retract(turned_off(object(lamp, _, _, _))),
    assert(turned_on(object(lamp, _, _, _))).

turn_off :- have(object(lamp, _, _, _)),
    retract(turned_on(object(lamp, _, _, _))),
    assert(turned_off(object(lamp, _, _, _))).

turn_on(X) :- have(X), retract(turned_off(X)), assert(turned_on(X)). 
turn_on(X) :- here(Y), location(X, Y), retract(turned_off(X)), assert(turned_on(X)).

turn_off(X) :- have(X), retract(turned_on(X)), assert(turned_off(X)). 
turn_off(X) :- here(Y), location(X, Y), retract(turned_on(X)), assert(turned_off(X)).
