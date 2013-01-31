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

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"}) = 
[{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}];
