-module(ts).
-export([match/2, in/2,out/2,new/0,tsProcessing/2, findMatching/2 ]).

% Matching function
match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS); 
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.

% Ts: Table space's PID 
% Format: Format of a tuple
in(Ts,Format) when is_tuple(Format) ->
	Ref = make_ref(),
	Ts ! {self(), Ref, inReq, Format},
	receive
		{Ts, Ref, inResult, TResult} -> TResult
	end;
in(_,_) -> 
	erlang:error(badarg).

% Ts: Table space's PID
% Tuple: Tuple to add in the table space
out(Ts,Tuple) when is_tuple(Tuple) ->
	Ts ! {self(), make_ref(), outReq, Tuple};
out(_,_) ->
	erlang:error(badarg).

% Create a process wich manage a new tuple space.
new() -> 
	spawn_link(ts, tsProcessing, [[],[]]).

% Function which manage one tuple space (only the functions "out" and "in" are managed)
% Data: List where are stored all the tuples
% Waiting: List of all pending "in request"  
tsProcessing(Data, Waiting) -> 
	receive
		{From, Ref, inReq, Format} -> % reception of a "in request", with the format "Format"
			case findMatching(Format,Data) of
				false -> %No matching found
					tsProcessing(Data, Waiting ++ [{From, Ref, inReq, Format}]); %Add to the waiting list	
				TData -> %Matching found
					From ! {self(), Ref, inResult, TData},
					tsProcessing(Data -- [TData],Waiting)				
			end;
		{From, Ref, outReq, Tuple} -> % reception of a "out request"
			case processWaiting(Waiting, {From, Ref, outReq, Tuple}) of
				false ->  %if no pending "in request" match
					tsProcessing(Data ++ [Tuple],Waiting);
				{From2, Ref2, inReq, T2} -> %if one pending "in request" match
					From2 ! {self(), Ref2, inResult, Tuple},
					tsProcessing(Data, Waiting -- [{From2, Ref2, inReq, T2}])
			end;
		stop ->
			true
	end.

% /1 = tuple to find match
% /2 = List of tuple to compare with the first argument
% return a tuple from the list which match the first parameter. I there are no such tuple, return false.
findMatching(_,[]) -> false;
findMatching(Tuple,[TupleInList|Q]) ->
	case match(Tuple,TupleInList) of
		true -> TupleInList;% Match found
		false -> findMatching(Tuple,Q)
	end.
 
% /1 = List of pending "in request"
% /2 = a "out request"
% return a pending "in request" if the "out tuple" matches the "in tuple". Otherwise, return false.
processWaiting([],_) -> false;
processWaiting([H|Q], P) ->
	{_, _, outReq, T} = P,
	{_, _, inReq, T2} = H,
	case match(T2, T) of
		true -> H;
		false -> processWaiting(Q, P)
	end.

