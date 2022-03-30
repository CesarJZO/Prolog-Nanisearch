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

loc_list([apple, soda, water, pizza, fridge], kitchen).
loc_list([console, bed, controller], my_room).
loc_list([desk, laptop, lamp], office).
loc_list([mirror, frame, plant], mothers_room).
loc_list([toilet, towel, soap], bathroom).
loc_list([sofa, cat_bowl], living_room).
loc_list([], corridor).

member(H, [H|_]).
member(X, [_|T]) :- member(X, T).

append([], X, X).
append([H|T1], X, [H|T2]) :- append(T1, X, T2).

location(X, Y) :- loc_list(List, Y), member(X, List).

add_thing(NewThing, Containter, NewList) :- 
    loc_list(OldList, Containter),
    append([NewThing], OldList, NewList).

add_thing2(NewThing, Containter, NewList) :- 
    loc_list(OldList, Containter),
    NewList = [NewThing|OldList].

add_thing3(NewThing, Containter, [NewThing|OldList]) :-
    loc_list(OldList, Containter).

put_thing(Thing, Place) :-
    retract(loc_list(List, Place)),
    assert(loc_list([Thing|List], Place)).

break_out([]).
break_out([H|T]) :- assertz(stuff(H)), break_out(T).

respond([H|T], ) :-

list_things_l() :- 

take_l().



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

% report :- location(object(X, _, _, _), kitchen), edible(object(X, _, _, _)), write(X), nl, fail.
report :- location(object(X, _, _, _), kitchen), write(X), nl, fail.

where_food(X, Y) :- location(X, Y), edible(X).
where_food(X, Y) :- location(X, Y), tastes_yucky(X).

connect(X, Y) :- door(X, Y).
connect(X, Y) :- door(Y, X).

% % % % % %  - Inventory -  % % % % % %

inventory :- not(have(_)), write("You have nothing in your inventory").
inventory :- write("You have in inventory:"), nl, 
    have(object(Thing, _, _, _)), tab(2),
    write(Thing), nl, is_contained_in(In, Thing), 
    tab(2), write(object(In, _, _, _)), nl.

% inventory :- write("You have in inventory:"), nl, 
%     have(Thing), tab(2),
%     write(Thing), nl, is_contained_in(In, Thing), 
%     tab(2), write(In), nl.


% % % % % %  - Eat -  % % % % % %

eat(Thing) :- have(object(Thing, _, _, _)),
    drinkable(object(Thing, _, _, _)),
    write('You can\'t eat '), write(Thing), fail.
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


% % % % % %  - List things -  % % % % % %

list_things(Place) :- 
    is_contained_in(Thing, Place),
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

look :- turned_off(object(lamp, _, _, _)), write('You can\'t see anything').
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

have(object(lamp, beige, small, 4)).


