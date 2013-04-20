-module(vm_coincase).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/0, stop/0, insert/1, empty/0, get_all/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> 
    gen_server:cast(?MODULE, stop).
insert(Item) -> 
    gen_server:call(?MODULE, {insert, Item}).
empty() -> 
    gen_server:call(?MODULE, {empty}).
get_all()->
    gen_server:call(?MODULE, {get_all}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
	Coins = [],
	{ok, Coins}.

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
handle_call({empty}, _From, _Coins) ->
    NewCoins = [],
    {reply, ok, NewCoins};
handle_call({get_all}, _From, Coins) ->
    {reply, Coins, Coins};
handle_call(_Message, _From, Coins) ->
    {reply, error, Coins}.

handle_cast(stop, Coins) -> 
    {stop, normal, Coins}.

handle_info(_Message, Coins) -> 
    {noreply, Coins}.

terminate(_Reason, _Coins) -> 
    ok.

code_change(_OldVersion, Coins, _Extra) -> 
    {ok, Coins}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ret_coin(CoinName,List)->
    RetCoin = [{Name, Am} || {Name, Am} <- List, Name =:= CoinName],
	if 
	RetCoin == [] -> RetCoin;
	true -> hd(RetCoin)
	end.
	
