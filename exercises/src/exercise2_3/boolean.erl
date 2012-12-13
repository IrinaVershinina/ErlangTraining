-module(boolean).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).

%b_not
b_not(true) ->
	false;
b_not(false) ->
	true.

%b_and
b_and(true, true) ->
	true;
b_and(true, false) ->
	false;
b_and(false, true) ->
	false;
b_and(false, false) ->
	false.

%b_or
b_or(true, true) ->
	true;
b_or(true, false) ->
	true;
b_or(false, true) ->
	true;
b_or(false, false) ->
	false.

%b_nand
b_nand(V1, V2) ->
	b_not(b_and(V1, V2)).




	

