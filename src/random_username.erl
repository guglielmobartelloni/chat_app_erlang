-module(random_username).
-export([generate/0]).

adjectives() -> ["Cool", "Fast", "Happy", "Brave", "Mysterious", "Clever", "Witty"].
nouns() -> ["Tiger", "Falcon", "Ninja", "Wizard", "Panda", "Phoenix", "Samurai"].

generate() ->
    Adjective = lists:nth(rand:uniform(length(adjectives())), adjectives()),
    Noun = lists:nth(rand:uniform(length(nouns())), nouns()),
    list_to_binary(lists:concat([Adjective, Noun])).
