-module(vm_stock).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, add_prod/2, check_prod/1, lookup/1, return/1]).

-record(product, {name, price, amount}).
%-record(coin, {type, value})

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
add_prod(Prod, Det) -> gen_server:call(?MODULE, {add_prod, Prod, Det}).
check_prod(Prod) -> gen_server:call(?MODULE, {check_prod, Prod}).
lookup(Prod) -> gen_server:call(?MODULE, {lookup, Prod}).
return(Prod) -> gen_server:call(?MODULE, {return, Prod}).

% This is called when a connection is made to the server
init([]) ->
	Products = dict:new(),
	{ok, Products}.

% handle_call is invoked in response to gen_server:call
handle_call({add_prod, Prod, Det}, _From, Products) ->
	Response = case dict:is_key(Prod, Products) of
		true ->
			NewProducts = Products, % product is already there, update the price and add to the existing ones
			Val = dict:fetch(Prod, Products),
			%{Price, Amount} = Val,
			%{PriceD, AmountD} = Det,
			%DetToAdd = {PriceD, Amount + AmountD},
			%NewProducts = dict:append(Prod, Det, Products);
			%ok;
			%NewProducts = dict:append(Prod, Det, Products);
			%NewProducts = dict:append(Prod, Det, Products),
			{already_exists, Prod};
		false ->
			NewProducts = dict:append(Prod, Det, Products),
			ok
	end,
	{reply, Response, NewProducts};

handle_call({check_prod, Prod}, _From, Products) ->
	Response = case dict:is_key(Prod, Products) of
		true ->
			true;
		false ->
			false
	end,
	{reply, Response, Products};
	
handle_call({lookup, Prod}, _From, Products) ->
	Response = case dict:is_key(Prod, Products) of
		true ->
			{who, lists:nth(1, dict:fetch(Prod, Products))};
		false ->
			{not_checked_out, Prod}
	end,
	{reply, Response, Products};

handle_call({return, Prod}, _From, Products) ->
	NewProducts = dict:erase(Prod, Products),
	{reply, ok, NewProducts};

handle_call(_Message, _From, Products) ->
	{reply, error, Products}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Products) -> {noreply, Products}.
handle_info(_Message, Products) -> {noreply, Products}.
terminate(_Reason, _Products) -> ok.
code_change(_OldVersion, Products, _Extra) -> {ok, Products}.