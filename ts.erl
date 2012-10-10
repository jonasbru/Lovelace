-module(ts).
-export([match/2, in/2,out/2,new/0,tsProcessing/2, findMatching/2 ]).

match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS); 
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.

in(Ts,T) when is_tuple(T) ->
	Ref = make_ref(),
	Ts ! {self(), Ref, inReq, T},
	receive
		{Ts, Ref, inRes, TResult} -> TResult
	end;
in(_,_) -> 
	io:format("~p~n", [error_Tuple_in]).

out(Ts,F) when is_tuple(F) ->
	Ts ! {self(), make_ref(), outReq, F};
out(_,_) ->
	io:format("~p~n", [error_Tuple_out]).

new() -> 
	spawn_link(ts, tsProcessing, [[],[]]).

tsProcessing(Data, Waiting) -> 
	receive
		{From, Ref, inReq, T} -> 
			case findMatching(T,Data) of
				false -> %No matching found
					tsProcessing(Data, Waiting ++ [{From, Ref, inReq, T}]); %Add to the waiting list	
				TData -> 
					From ! {self(), Ref, inRes, TData},
					tsProcessing(Data -- [TData],Waiting)				
			end;
		{From, Ref, outReq, T} -> 
			case processWaiting(Waiting, {From, Ref, outReq, T}) of
				false -> tsProcessing(Data ++ [T],Waiting);
				{From2, Ref2, outReq, T2} ->
					From2 ! {self(), Ref2, inRes, T2},
					tsProcessing(Data, Waiting -- [{From2, Ref2, outReq, T2}])
			end;
		stop ->
			true
	end.

% /1 = tuple
% /2 = Elements a parcourir pr match
findMatching(_,[]) -> false;
findMatching(T,[TM|Q]) ->
	case match(T,TM) of
		true -> TM;
		false -> findMatching(T,Q)
	end.

processWaiting([],_) -> false;
processWaiting([H|Q], P) ->
	{_, _, outReq, T} = P,
	{_, _, inReq, T2} = H,
	case match(T, T2) of
		true -> H;
		false -> processWaiting(Q, P)
	end.

