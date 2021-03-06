val string_list = ["Abbie", "Betty", "Clarice"];
all_except_option("Abbie", string_list) = SOME(["Betty", "Clarice"]);
all_except_option("Betty", string_list) = SOME(["Abbie", "Clarice"]);
all_except_option("Clarice", string_list) = SOME(["Abbie", "Betty"]);
all_except_option("Clarice", ["Abbie", "Clarice"]) = SOME(["Abbie"]);
all_except_option("Clarice", ["Clarice"]) = SOME([]);
all_except_option("Clarice", ["Barry"]) = NONE;

get_substitutions([], "Bill") = [];
get_substitutions([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "") = [];
get_substitutions([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") =  ["Fredrick","Freddie","F"];
get_substitutions([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Betty") = ["Elizabeth"];
get_substitutions([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Bill") = [];

get_substitutions2([], "Bill") = [];
get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "") = [];
get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") =  ["Fredrick","Freddie","F"];
get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Betty") = ["Elizabeth"];
get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Bill") = [];

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Betty", middle="W", last="Smith"}) = 
[{first="Betty", last="Smith", middle="W"}, {first="Elizabeth", last="Smith", middle="W"}];

similar_names([["Fred","Fredrick"],["Elizabeth","Betty","Beth"],["Freddie","Fred","F"]],{first="Betty", middle="W", last="Smith"}) = 
[{first="Betty", last="Smith", middle="W"}, {first="Beth", last="Smith", middle="W"}, {first="Elizabeth", last="Smith", middle="W"}];

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"}) = 
[{first="Fred", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}];

similar_names([],{first="Betty", middle="W", last="Smith"}) = [{first="Betty", middle="W", last="Smith"}];

remove_card([(Hearts,Queen)],(Hearts,Queen),IllegalMove)=[];
remove_card([(Hearts,Queen),(Hearts,Queen)],(Hearts,Queen),IllegalMove)=[(Hearts,Queen)];
remove_card([(Hearts,Queen),(Spades,Jack)],(Spades,Jack),IllegalMove)=[(Hearts,Queen)];

(*remove_card([(Hearts,Queen),(Spades,Jack)],(Spades,Queen),IllegalMove) handle IllegalMove => true;*)

all_same_color([]) = true;
all_same_color([(Hearts,Queen)]) = true;
all_same_color([(Hearts,Queen),(Diamonds,Num(3))]) = true;
all_same_color([(Hearts,Queen),(Spades,Num(3))]) = false;
all_same_color([(Hearts,Queen),(Diamonds,Num(3)), (Hearts,Ace)]) = true;
all_same_color([(Hearts,Queen), (Diamonds,King),(Spades,Num(3)), (Clubs,Ace)]) = false;

sum_cards([]) = 0;
sum_cards([(Hearts,Ace)]) = 11;
sum_cards([(Hearts,Ace),(Spades,Num(2))]) = 13;
sum_cards([(Hearts,Ace),(Spades,Num(2)),(Diamonds,Queen)]) = 23;

(* Scoring works as follows: Let sum be the sum
of the values of the held-cards. If sum is greater than goal, the preliminary score is three times sum - goal,
else the preliminary score is goal - sum. The score is the preliminary score unless all the held-cards are the
same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual with
integer division; use ML's div operator).*)
score([],0)=0;
score([],28)=14; 
score([(Spades,Num(2))],28)=13; 
score([(Spades,Num(2)),(Clubs,Num(2))],28)=12; 
score([(Spades,Num(2)),(Hearts,Num(2))],28)=24; 
score([(Spades,Queen),(Spades,Ace)],28)=3; 
score([(Spades,Queen),(Hearts,Ace)],28)=7; 
score([(Spades,Queen),(Hearts,Ace)],21)=0; 
score([(Spades,Queen),(Hearts,Ace)],20)=3; 
score([(Spades,Num(9)),(Clubs,Num(9)),(Clubs,Num(3))],20)=1; 
score([(Spades,Num(9)),(Clubs,Num(9)),(Hearts,Num(3))],20)=3; 

(*
rite a function officiate, which \runs a game." It takes a card list (the card-list) a move list
(what the player \does" at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally dened recursive
helper function that takes several arguments that together represent the current state of the game. As
described above:
 The game starts with the held-cards being the empty list.
 The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
 If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
 If the player draws and the card-list is empty, the game is over. Else if drawing causes the sum of
the held-cards to exceed the goal, the game is over. Else play continues with a larger held-cards
and a smaller card-list.
*)
officiate([(Spades,Num(2))],[],28)=13; 