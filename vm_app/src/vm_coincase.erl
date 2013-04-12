-module(vm_coincase).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, stop/0, insert/1, empty/0, ret_coin/2]).

%-record(coin, {type, value})

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()-> gen_server:cast(?MODULE, stop).
	
insert(Item) -> gen_server:call(?MODULE, {insert, Item}).
empty() -> gen_server:call(?MODULE, {empty}).

% This is called when a connection is made to the server
init([]) ->
	Coins = [],
	{ok, Coins}.

% handle_call is invoked in response to gen_server:call
handle_call({insert, Item}, _From, Coins) ->
	CoinFromList = ret_coin(Item, Coins),
	if 
	CoinFromList == [] ->
		NewCoins = Coins ++ [{Item,1}];
	true ->
		{Name, Amount} = CoinFromList,
		ListT = lists:delete({Name, Amount}, Coins),
		NewCoins = ListT ++ [{Name, Amount+1}]
	end,
	{reply, ok, NewCoins};

handle_call({empty}, _From, Coins) ->
	NewCoins = [],
	{reply, ok, NewCoins};
	
handle_call(_Message, _From, Coins) ->
	{reply, error, Coins}.

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
