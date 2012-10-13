-module(testOwn).
-export([test/0, slave/2]).
-import(

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ts  % <-- CHANGE THIS TO YOUR TUPLESPACE MODULE NAME 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
,[in/2,out/2,new/0]
). 

%% To compile the test: 
%% c(testOwn).
%% then run:
%% testOwn:test().


test() ->
	io:format("##### TEST 1 #####~n", []),
	test1(),
	
	io:format("~n~n##### TEST 2 #####~n", []),
	test2(),
	
	io:format("~n~n##### TEST 3 #####~n", []),
	test3(),
	
	io:format("~n~n##### TEST 4 #####~n", []),
	test4().
	
%% Tests with multiple TS
test1() ->
    Delay = 500,
    process_flag(trap_exit, true),
    TS1 = new(),
    link(TS1),
    io:format("TEST: new tuplespace TS created : ~w~n", [TS1]),
    
    TS2 = new(),
    link(TS2),
    io:format("TEST: new tuplespace TS created : ~w~n", [TS2]),
	

    Slave1 = spawn_in_test(TS1, {plop,any}),
    sleep(Delay),

    out_test(TS2, {plop,pouf}),
    sleep(Delay),

    Slave2 = spawn_in_test(TS2, {plop,any}),
    sleep(Delay),

    replytest(Slave2, {plop,any}, {plop,pouf}),
    sleep(Delay),

	io:format("Process ~w~n", [Slave1]),
    receive
	{Slave1, Tup} ->
	    io:format("     Error. Empty tuplespace, but received: ~w~n",[Tup])
    after
        1000 ->   
	    io:format("     Correct. Tuplespace appears to be empty.~n"),
	    exit(Slave1, this_is_it),
	    exit(TS1, this_is_it),
	    exit(TS2, this_is_it),
	    collect_exits([Slave1, Slave2, TS1, TS2]),
	    finished
    end.

%% Test with complex tuples
test2() ->
	Delay = 500,
    process_flag(trap_exit, true),
    TS = new(),
    link(TS),
    io:format("TEST: new tuplespace TS created~n", []),

    out_test(TS, {plop,any}), 
    sleep(Delay),

    out_test(TS, {any,plop}), 
    sleep(Delay),

    out_test(TS, {carrot,{carrot,{carrot,{carrot,carrotception}}}}), 
    sleep(Delay),

    out_test(TS, {1337,"5|*34|< LULZ"}), 
    sleep(Delay),

    Slave1 = spawn_in_test(TS, {carrot,{carrot,{any,any}}}),
    sleep(Delay),
    
    Slave2 = spawn_in_test(TS, {any,plop}),
    sleep(Delay),
    
    Slave3= spawn_in_test(TS, {1337,"5|*34|< LULZ"}),
    sleep(Delay),
    
    Slave4 = spawn_in_test(TS, {plop,any}),
    sleep(Delay),
    
    replytest(Slave1, {carrot,{carrot,{any,any}}}, {carrot,{carrot,{carrot,{carrot,carrotception}}}}),
    sleep(Delay),

    replytest(Slave2, {any,plop}, {any,plop}),
    sleep(Delay),

    replytest(Slave3, {1337,"5|*34|< LULZ"}, {1337,"5|*34|< LULZ"}),
    sleep(Delay),

    replytest(Slave4, {plop,any}, {plop,any}),
    sleep(Delay),
    
    
    exit(TS, this_is_it),
    collect_exits([Slave1, Slave2, Slave3, Slave4, TS]),
    finished.
    
%% Tests that we don't receive the wrong tuple
test3() ->
	Delay = 500,
    process_flag(trap_exit, true),
    TS = new(),
    link(TS),
    io:format("TEST: new tuplespace TS created~n", []),

    Slave1 = spawn_in_test(TS, {fish,potatoe}),
    sleep(Delay),

    out_test(TS, {carrot,chicken}), 
    sleep(Delay),

    io:format("Process ~w~n", [Slave1]),
    receive
	{Slave1, Tup} ->
	    io:format("     Error. Empty tuplespace, but received: ~w~n",[Tup])
    after
        1000 ->   
	    io:format("     Correct. Doesn't receive wrong tuple.~n"),
	    exit(Slave1, this_is_it),
	    exit(TS, this_is_it),
	    collect_exits([Slave1, TS]),
	    finished
    end.
    
%% Tests that we don't receive the wrong tuple, AGAIN.
test4() ->
	Delay = 500,
    process_flag(trap_exit, true),
    TS = new(),
    link(TS),
    io:format("TEST: new tuplespace TS created~n", []),

    out_test(TS, {plop,any}), 
    sleep(Delay),

    Slave1 = spawn_in_test(TS, {plop,truc}),
    sleep(Delay),

    io:format("Process ~w~n", [Slave1]),
    receive
	{Slave1, Tup} ->
	    io:format("     Error. Empty tuplespace, but received: ~w~n",[Tup])
    after
        1000 ->   
	    io:format("     Correct. Doesn't receive any tuple.~n"),
	    exit(Slave1, this_is_it)
    end,

    Slave2 = spawn_in_test(TS, {plop,any}),
    sleep(Delay),
    
    replytest(Slave2, {plop,any}, {plop,any}),
    sleep(Delay),
    
    exit(TS, this_is_it),
    collect_exits([Slave1, Slave2, TS]),
    finished.
	

%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(T) ->
    receive
    after
	T -> true
    end.

out_test(Tuplespace, Tup) ->
    io:format("TEST: out(TS, ~w)~n", [Tup]),
    out(Tuplespace, Tup).

% spawns a slave task to perform an in test. This function 
% returns the slave's Pid. The slave will forward the result of the 
% in operation to the caller.

spawn_in_test(Tuplespace, Pat) -> 
    S = spawn_link(test, slave, [Tuplespace, {self(), Pat}]),
    io:format("TEST: in(TS, ~w) by process ~w~n", [Pat, S]),
    S.

%% Get a tuple matching Item from Tuplespace T and send it to Pid
slave(T, {Pid,Item}) ->
    case in(T, Item) of
	R -> Pid!{self(), R}
    end.

%% Tests whether the reply from a Slave task matches the expected Tuple
replytest(Slave, Pat, Tup) -> 
    io:format("Process ~w~n", [Slave]),
    receive
	{Slave,Tup} ->
	    io:format("     Correct. in operation: ~w returned tuple: ~w~n", [Pat, Tup]);
        {Slave,Bad} ->
	    io:format("     Error. in with pattern: ~w returned tuple: ~w~n", [Pat, Bad])
    after 
        5000 ->   
	    io:format("     Error. No response for in operation with pattern: ~w~n", [Pat])
    end.

collect_exits([]) ->
    done;
collect_exits([Pid | Pids]) ->
    receive
	{'EXIT', Pid, _} ->
	    collect_exits(Pids)
    end.

