% home.pl file %

:- dynamic here/1.
:- dynamic location/2.
:- dynamic have/1.
:- dynamic turned_off/1.
:- dynamic turned_on/1.
:- dynamic turn_on/1.
:- dynamic turn_off/1.
:- dynamic loc_list/2.

room(kitchen).
room(my_room).
room(office).
room(mothers_room).
room(bathroom).
room(corridor).
room(living_room).
room(garage).
room(kid).

loc_list([
    object(apple, green, small, 2),
    object(soda, black, small, 3),
    object(water, trasparent, small, 3),
    object(pizza, red, small, 5),
    object(brocolli, green, small, 1),
    object(fridge, black, big, 50)
    ], kitchen).
loc_list([
    object(console, black, small, 6),
    object(bed, blue, big, 40),
    object(controller, gray, 2)
    ], my_room).
loc_list([
    object(desk, gray, big, 25),
    object(laptop, silver, big, 10),
    object(lashlight, pink, small, 2)
    ], office).
loc_list([
    object(mirror, green, big, 30),
    object(frame, yellow, big, 15),
    object(plant, green, small, 6)
    ], mothers_room).
loc_list([
    object(toilet, white, big, 25),
    object(towel, brown, small, 2),
    object(soap, green, small, 1)
    ], bathroom).
loc_list([
    object(sofa, brown, big, 30),
    object(cat_bowl, silver, small, 3)
    ], living_room).
loc_list([], corridor).
loc_list([], kid).

member(H, [H|_]).
member(X, [_|T]) :- member(X, T).

append([], X, X).
append([H|T1], X, [H|T2]) :- append(T1, X, T2).

location(X, Y) :-
    loc_list(List, Y), 
    member(X, List).

add_thing1(NewThing, Containter, NewList) :- 
    loc_list(OldList, Containter),
    append([NewThing], OldList, NewList).

add_thing2(NewThing, Containter, NewList) :- 
    loc_list(OldList, Containter),
    NewList = [NewThing|OldList].

add_thing3(NewThing, Containter, [NewThing|OldList]) :-
    loc_list(OldList, Containter).
 
add_thing(Thing, Place) :- 
    retract(loc_list(Objects, Place)),
    assert(loc_list([Thing|Objects], Place)).
add_thing(Thing, Place) :-
    assert(loc_list([Thing|[]], Place)).

put_thing(Thing, Place) :-
    retract(loc_list(List, Place)),
    assert(loc_list([Thing|List], Place)).

remove(Object, [H|T], NewList) :-
    Object == H, NewList = T.
remove(Object, [H|T], NewList) :-
    remove(Object, T, Aux), NewList = [H|Aux].

remove_thing(Object, Place) :-
    retract(loc_list(List, Place)), remove(Object, List, NewList),
    assert(loc_list(NewList, Place)).

break_out([]).
break_out([H|T]) :- assertz(stuff(H)), break_out(T).

% respond([H|T], )

is_contained_in(T1, T2) :-
    location(object(T1, _, _, _), T2).
is_contained_in(T1, T2) :-
    location(object(X, _, _, _), T2),
    is_contained_in(T1, X).

edible(object(pizza, red, small, 4)).
edible(object(apple, green, small, 2)).
tastes_yucky(object(brocolli, green, small, 1)).

drinkable(object(soda, _, _, _)).
drinkable(object(water, _, _, _)).

breakable(object(console, black, small, 6)).
breakable(object(frame, yellow, big, 15)).
breakable(object(cat_bowl, silver, small, 3)).
breakable(object(lamp, _, _, _)).

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

where_food(X, Y) :- location(X, Y), edible(X).
where_food(X, Y) :- location(X, Y), tastes_yucky(X).

connect(X, Y) :- door(X, Y).
connect(X, Y) :- door(Y, X).

% % % % % %  - Inventory -  % % % % % %

inventory :- loc_list([], kid), write("You have nothing in your inventory").
inventory :- write("You have in inventory:"), nl, 
    list_things(kid).

% inventory :- write("You have in inventory:"), nl, 
%     have(Thing), tab(2),
%     write(Thing), nl, is_contained_in(In, Thing), 
%     tab(2), write(In), nl.


% % % % % %  - Eat -  % % % % % %

eat(Thing) :- location(object(Thing, X, Y, Z), kid),
    drinkable(object(Thing, X, Y, Z)),
    write('You can\'t eat '), write(Thing), fail.
eat(Thing) :- location(object(Thing, X, Y, Z), kid),
    not(edible(object(Thing, X, Y, Z))),
    write('You can\'t eat a '), write(Thing), fail.
eat(Thing) :- location(object(Thing, X, Y, Z), kid),
    tastes_yucky(object(Thing, X, Y, Z)),
    write('Gross >m<'), fail.
eat(Thing) :- not(location(object(Thing, _, _, _), kid)),
    write('You don\'t have a '), write(Thing), fail.
eat(Thing) :- location(object(Thing, X, Y, Z), kid),
    edible(object(Thing, X, Y, Z)), 
    remove_thing(object(Thing, X, Y, Z), kid), write('yummy').

% % % % % %  - Drink -  % % % % % %

drink(Thing) :- not(location(object(Thing, _, _, _), kid)),
    write('You don\'t have a '), write(Thing), fail.
drink(Thing) :- location(object(Thing, X, Y, Z), kid),
    not(drinkable(object(Thing, X, Y, Z))),
    write('You can\'t drink that!'), fail.
drink(Thing) :- location(object(Thing, X, Y, Z), kid),
    drinkable(object(Thing, X, Y, Z)),
    remove_thing(object(Thing, X, Y, Z), kid),
    write('Refreshing!'), fail.


% % % % % %  - List things -  % % % % % %

list_things(Place) :- 
    location(object(Thing, _, _, _), Place),
    tab(2), write(Thing), nl, fail.
list_things(_).

list_things_s(Place) :- not(room(Place)),
    write(Place), write(' is not a place'), nl, fail.
list_things_s(Place) :- not(location(object(_, _, _, _), Place)),
    write('There\'s nothing in '), write(Place), nl, fail.
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

% look :- turned_off(object(lamp, _, _, _)), write('You can\'t see anything').
look :- here(Place), write(Place), nl, 
    write('You can see: '), nl, list_things_s(Place),
    write('You can go to: '), nl, list_connections(Place), fail.

look_in(Thing) :- location(X, Thing), list_things_s(X).

can_go(Place) :- here(X), is_opened(X, Place), connect(X, Place).

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

take_object(Thing) :- here(Place), location(object(Thing, X, Y, Z), Place),
    remove_thing(object(Thing, X, Y, Z), Place), 
    add_thing(object(Thing, X, Y, Z), kid), write('taken'), nl.
% take_object(_).

take(X) :- can_take(X), take_object(X).

take(Thing, In) :- here(Place), location(In, Place), 
    is_contained_in(Thing, In), retract(location(Thing, In)),
    assert(have(Thing)), write('taken'), nl.

take_in(Thing) :- object(Thing, _, _, _), object(In, _, _, _),
    is_contained_in(Thing, In), 
    retract(location(object(Thing, _, _, _), object(In, _, _, _))), 
    assert(have(object(Thing, _, _, _))), write('taken'), nl, fail.

% % % % % %  - Put -  % % % % % 

put(Thing) :- not(location(object(Thing, _, _, _), kid)),
    write('You don\'t have a '), write(Thing), nl, fail.
put(Thing) :- location(object(Thing, X, Y, Z), kid), here(Place), 
    add_thing(object(Thing, X, Y, Z), Place),
    remove_thing(object(Thing, X, Y, Z), kid).

put(Thing, In) :- location(object(Thing, XT, YT, ZT), kid), here(Place), 
    location(object(In, XI, YI, ZI), Place),
    add_thing(object(Thing, XT, YT, ZT), object(In, XI, YI, ZI)),
    remove_thing(object(Thing, XT, YT, ZT), kid).

% % % % % %  - Drop -  % % % % % 

drop(Thing) :- location(object(Thing, X, Y, Z), kid),
    breakable(object(Thing, X, Y, Z)),
    remove_thing(object(Thing, X, Y, Z), kid),
    write(Thing), write(" broken"), nl.
drop(Thing) :- put(Thing), write("dropped"), nl.

% % % % % %  - Turn on/off -  % % % % % 

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

% % % % % %  - Take -  % % % % % %
% % % % % %  - Put -  % % % % % %
% % % % % %  - Drop -  % % % % % %
% % % % % %  - List Things -  % % % % % %
% % % % % %  - Inventory -  % % % % % %
% % % % % %  - Eat -  % % % % % %
% % % % % %  - Drink -  % % % % % %