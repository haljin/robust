-module(vm_coin).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, stop/0, check_change/2, get_change/1, insert_coin/1, sum_coin/1, return_coin/2, remove_coins/2, return_coins/2]).

%-record(coin, {type, value})

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()-> gen_server:cast(?MODULE, stop).

%% Returns: ok - the machine has enough change
%%          {warning, nochange} - the machine does not have enough	
get_change(Ammount) -> gen_server:call(?MODULE, {get_change, Ammount}).

insert_coin(Coin) -> gen_server:call(?MODULE, {insert_coin, Coin}).

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
		NewCoins = Coins ++ [{Coin,1}];
	true ->
		{Name, Amount} = CoinFromList,
		ListT = lists:delete({Name, Amount}, Coins),
		NewCoins = ListT ++ [{Name, Amount+1}]
	end,
	{reply, ok, NewCoins};

handle_call({get_change, Ammount}, _From, Coins) ->
	RetVal = case check_change(Ammount, Coins) of
	-1 -> % not enough coins, return all of them
		return_all_coins(Coins),
		io:format("not enough coins"),
		NewCoins = [];
	0 -> % cool! return all coins
		io:format("returning all coins"),
		return_all_coins(Coins),
		NewCoins = [];		
	1 -> % the value of coins is greater than needed change
		NewCoins = return_coins(Ammount, Coins)
	end,
	{reply, RetVal, NewCoins};
	
handle_call(_Message, _From, Coins) ->
	{reply, error, Coins}.

return_all_coins([]) -> ok;
return_all_coins([{N,A}|T]) -> return_coin(N,A).

return_coin(Name, 0) -> ok;
return_coin(Name, Amount) -> 
	vm_coincase:insert(Name),
	return_coin(Name, Amount-1).

return_coins(Ammount, Coins) ->
	% sort the coins list descending by value
	SortedCoins = lists:reverse(lists:sort(Coins)),
	RetCoins = get_coins(Ammount, SortedCoins), % get as many coins as needed
	return_all_coins(RetCoins), % insert them to coincase
	% return reduced coins list
	OutCoins = remove_coins(Coins, RetCoins).
	
remove_coins(Coins, []) -> Coins;
remove_coins(Coins, [{N,A}|T]) ->
	{Name, Amount} = ret_coin(N, Coins),
	if 
	Amount == A -> 
		lists:delete({Name, Amount},Coins);
	true -> 
		ReducedCoins = lists:delete({Name, Amount},Coins),
		ReducedCoins ++ [{Name, Amount - A}]
	end.
	
get_coins(Ammount, []) -> [];
get_coins(Ammount, [{N,A}|T]) ->
	if 
		Ammount > N -> 	% it is possible to add this coin to list
		Rest = trunc(Ammount / N),
		if
			Rest =< A -> 
				[{N,Rest}] ++ get_coins(Ammount - (Rest*N), T);
			true ->
				[{N,A}] ++ get_coins(Ammount - (A*N), T)
		end;
		Ammount == N ->	[{N,1}] ++ get_coins(Ammount-N, T);
		true -> get_coins(Ammount, T)
	end.
	
check_change(Ammount, List) ->
	CoinsSum = sum_coin(List),
	if 
		CoinsSum < Ammount -> -1;
		CoinsSum == Ammount -> 0;
		true -> 1
	end.

sum_coin([]) -> 0;
sum_coin([{N,A}|T]) -> N * A + sum_coin(T).

ret_coin(CoinName,List)->
    RetCoin = [{Name, Am} || {Name, Am} <- List, Name =:= CoinName],
	if 
	RetCoin == [] -> RetCoin;
	true -> hd(RetCoin)
	end.
	
% We get compile warnings from gen_server unless we define these
handle_cast(stop, Coins) -> {stop, normal, Coins}.
handle_info(_Message, Coins) -> {noreply, Coins}.
terminate(_Reason, _Coins) -> ok.
code_change(_OldVersion, Coins, _Extra) -> {ok, Coins}.