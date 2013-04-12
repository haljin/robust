-module(vm_coin).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([coin_to_val/1]).

-export([start/0, stop/0, get_change/1, insert_coin/1]).

-include("../include/vmdata.hrl").

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()-> gen_server:cast(?MODULE, stop).

%% Returns: ok - the machine has enough change
%%          {warning, nochange} - the machine does not have enough	
get_change(Ammount) -> gen_server:call(?MODULE, {get_change, Ammount}).

insert_coin(Coin) -> gen_server:call(?MODULE, {inseryt_coin, Coin}).

%% Returns: A list of coins as change, if there is not enough coins
%%          returns as many as possible
%check_change(Ammount) -> gen_server:call(?MODULE, {check_change, Ammount}).

% This is called when a connection is made to the server
init([]) ->
	Coins = [],
	{ok, Coins}.

% handle_call is invoked in response to gen_server:call
handle_call({insert_coin, Coin}, _From, Coins) ->
	CoinFromList = ret_coin(Coin, Coins),
	if 
	CoinFromList == [] ->
		NewCoins = Coins ++ [#coin{type = Coin, value = coin_to_val(Coin), ammount = 1}];
	true ->
		#coin{type = Name, ammount = Amount} = CoinFromList,
		ListT = lists:keydelete(Name, 2, Coins),
		NewCoins = ListT ++ [CoinFromList#coin{ammount = Amount + 1}]
	end,
	{reply, ok, NewCoins};

handle_call({get_change, Ammount}, _From, Coins) ->
	RetVal = case check_change(Ammount, Coins) of
	true -> % we will return the correct change
		io:format("not enough coins");
	false -> % problems with change
		io:format("returning all coins")
	end,
	NewCoins = return_coins(Ammount, Coins),
	{reply, RetVal, NewCoins};
	
handle_call(_Message, _From, Coins) ->
	{reply, error, Coins}.

return_all_coins([]) -> ok;
return_all_coins([{N,A}|T]) -> return_coin(N,A),
			       return_all_coins(T).

return_coin(Name, 0) -> ok;
return_coin(Name, Amount) -> 
	vm_coincase:insert(Name),
	return_coin(Name, Amount-1).

return_coins(Ammount, Coins) ->
						% sort the coins list descending by value
    SortedCoins = lists:reverse(lists:keysort(#coin.value,Coins)),
    io:format("Coins: ~p~n", [SortedCoins]),
    RetCoins = get_coins(Ammount, SortedCoins), % get as many coins as needed
    io:format("returning: ~p~n", [RetCoins]),
    return_all_coins(RetCoins), % insert them to coincase
						% return reduced coins list
    OutCoins = remove_coins(Coins, RetCoins).

remove_coins(Coins, []) -> Coins;
remove_coins(Coins, [{N,A}|T]) ->
    Coin = ret_coin(N, Coins),
    #coin{type = Name, ammount = Amount} = Coin,
    NewCoins = if 
	Amount == A -> 
	    lists:keydelete(Name, #coin.type, Coins);
	true -> 
	    ReducedCoins = lists:keydelete(Name, #coin.type,Coins),
	    ReducedCoins ++ [Coin#coin{ammount =  Amount - A}]
    end,
    remove_coins(NewCoins, T).
	
get_coins(Ammount, []) -> [];
get_coins(Ammount, [#coin{type = N, value =V, ammount = A}|T]) ->
	if 
		Ammount > V -> 	% it is possible to add this coin to list
		Rest = trunc(Ammount / V),
		if
			Rest =< A -> 
				[{N,Rest}] ++ get_coins(Ammount - (Rest*V), T);
			true ->
				[{N,A}] ++ get_coins(Ammount - (A*V), T)
		end;
		Ammount == V ->	[{N,1}] ++ get_coins(Ammount-V, T);
		true -> get_coins(Ammount, T)
	end.
	
check_change(Ammount, List) ->
	Temp = sum_coin(get_coins(Ammount, List)),
	if Ammount == Temp -> true;
		true -> false
	end.

sum_coin([]) -> 0;
sum_coin([{N,A}|T]) -> coin_to_val(N) * A + sum_coin(T).

ret_coin(CoinName,List)->
    RetCoin = [#coin{type = Name, value = Val, ammount= Am} || #coin{type = Name, value= Val, ammount= Am} <- List, Name =:= CoinName],
	if 
	RetCoin == [] -> RetCoin;
	true -> hd(RetCoin)
	end.
	
% We get compile warnings from gen_server unless we define these
handle_cast(stop, Coins) -> {stop, normal, Coins}.
handle_info(_Message, Coins) -> {noreply, Coins}.
terminate(_Reason, _Coins) -> ok.
code_change(_OldVersion, Coins, _Extra) -> {ok, Coins}.


coin_to_val(ore50)->
    0.5;
coin_to_val(kr1) ->
    1;
coin_to_val(kr2) ->
    2;
coin_to_val(kr5) ->
    5;
coin_to_val(kr10) ->
    10;
coin_to_val(kr20) ->
    20.





