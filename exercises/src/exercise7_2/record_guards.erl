%% @author ivershinina
%% @doc @todo Add description to record_guards.


-module(record_guards).

%% ====================================================================
%% API functions
%% ====================================================================
-export([birthday/1, joe/0, showPerson/1, foobar/1]).

-record(person, {name, 
                 age = 0, 
                 phone,
                 address = undefined}).

birthday(#person{age = Age} = P) ->
    P#person{age = Age + 1}.

joe() ->
    #person{name = "Joe", 
            age = 21,
            phone = "999-999",
            address = "Lenina Str., 4-22"}.

showPerson(#person{age = Age, phone = Phone, name = Name, address = Address}) ->
    io:format("name: ~p age: ~p phone: ~p, address: ~p", [Name, Age, Phone, Address]).

foobar(P) when is_record(P, person), P#person.name == "Joe" ->
    true;
foobar(_) ->
    false.
%% ====================================================================
%% Internal functions
%% ====================================================================