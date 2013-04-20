-module(vm_coin).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/0, stop/0, get_change/1, insert_coin/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Auxiliary functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([coin_to_val/1, generate_dets/1]).

-include("../include/vmdata.hrl").
-record(state, {table}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()-> 
    gen_server:cast(?MODULE, stop).

get_change(Ammount) -> 
    gen_server:call(?MODULE, {get_change, Ammount}).

insert_coin(Coin) -> 
    gen_server:call(?MODULE, {insert_coin, Coin}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Auxiliary functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

generate_dets(CoinList)->
    {ok, Table} = dets:open_file(tempcoins, [{type,set},{access,read_write},{keypos,#coin.type},{file,"coin.db"}]),
    lists:map(fun(El) ->
		      dets:insert(Table, El)
	      end, CoinList),
    dets:close(Table).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, Table} = dets:open_file(coins, [{type,set},
					 {access,read_write},{keypos,#coin.type},{file,"coin.db"}]),
    {ok, #state{table = Table}}.

% handle_call is invoked in response to gen_server:call
handle_call({insert_coin, Coin}, _From, #state{table = Coins} = State) ->
    Response = case dets:member(Coins, Coin) of 
		   true ->
		       dets:update_counter(Coins, Coin, {#product.ammount, 1});
		   false ->
		       dets:insert(Coin, #coin{type = Coin, value = coin_to_val(Coin), ammount = 1})
	       end,
    {reply, Response, State};
handle_call({get_change, Ammount}, _From, #state{table = Coins} = State) ->
    RetVal = check_change(Ammount, Coins),
    return_coins(Ammount, Coins),
    {reply, RetVal, State};

handle_call(_Message, _From, Coins) ->
    {reply, error, Coins}.

% We get compile warnings from gen_server unless we define these
handle_cast(stop, Coins) -> 
    {stop, normal, Coins}.
handle_info(_Message, Coins) -> 
    {noreply, Coins}.
terminate(_Reason, #state{table = Coins}) -> 
    dets:close(Coins),
    ok.
code_change(_OldVersion, Coins, _Extra) -> 
    {ok, Coins}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
return_all_coins([]) -> ok;
return_all_coins([{N,A}|T]) -> 
    return_coin(N,A),
    return_all_coins(T).

return_coin(_Name, 0) -> 
    ok;
return_coin(Name, Amount) -> 
    vm_coincase:insert(Name),
    return_coin(Name, Amount-1).

return_coins(Ammount, Coins) ->
    CoinList = lists:flatten(dets:match(Coins, '$1')),	% sort the coins list descending by value
    SortedCoins = lists:reverse(lists:keysort(#coin.value,CoinList)),
    RetCoins = get_coins(Ammount, SortedCoins), % get as many coins as needed
    return_all_coins(RetCoins), % insert them to coincase
    remove_coins(Coins, RetCoins).

get_coins(_Ammount, []) -> 
    [];
get_coins(Ammount, [#coin{type = N, value =V, ammount = A}|T]) ->
    if 
	(Ammount > V) and (A > 0) -> 	% it is possible to add this coin to list
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


remove_coins(Coins, []) -> Coins;
remove_coins(Coins, [{N,A}|T]) ->
    dets:update_counter(Coins, N, {#product.ammount, -A}),
    remove_coins(Coins, T).
	

	
check_change(Ammount, Table) ->
    CoinList = lists:flatten(dets:match(Table, '$1')),	% sort the coins list descending by value
    SortedCoins = lists:reverse(lists:keysort(#coin.value,CoinList)),
    Temp = sum_coin(get_coins(Ammount, SortedCoins)),
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
	








