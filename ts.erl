-module(ts).
-export([match/2, in/2,out/2,new/0 ]).

match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS); 
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.

in(Ts,T) -> todo.

out(Ts,F) -> todo.

new() -> todo.
