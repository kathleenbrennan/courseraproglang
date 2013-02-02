val string_list = ["Abbie", "Betty", "Clarice"];
all_except_option("Abbie", string_list) = SOME(["Betty", "Clarice"]);
all_except_option("Betty", string_list) = SOME(["Abbie", "Clarice"]);
all_except_option("Clarice", string_list) = SOME(["Abbie", "Betty"]);
all_except_option("Clarice", ["Abbie", "Clarice"]) = SOME(["Abbie"]);
all_except_option("Clarice", ["Clarice"]) = NONE;

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