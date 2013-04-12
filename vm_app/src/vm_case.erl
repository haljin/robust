-module(vm_case).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/0, stop/0, insert/1, empty/0]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
	Products = [],
	{ok, Products}.

% handle_call is invoked in response to gen_server:call
handle_call({insert, Item}, _From, Products) ->
	ProdFromList = ret_prod(Item, Products),
	if 
	ProdFromList == [] ->
		NewProducts = Products ++ [{Item,1}];
	true ->
		{Name, Amount} = ProdFromList,
		ListT = lists:delete({Name, Amount}, Products),
		NewProducts = ListT ++ [{Name, Amount+1}]
	end,
	{reply, ok, NewProducts};
handle_call({empty}, _From, _Products) ->
	NewProducts = [],
	{reply, ok, NewProducts};
handle_call(_Message, _From, Products) ->
	{reply, error, Products}.

handle_cast(stop, Products) -> 
    {stop, normal, Products}.

handle_info(_Message, Products) -> 
    {noreply, Products}.

terminate(_Reason, _Products) -> 
    ok.

code_change(_OldVersion, Products, _Extra) -> 
    {ok, Products}.

ret_prod(ProdName,List)->
    RetProd = [{Name, Am} || {Name, Am} <- List, Name =:= ProdName],
	if 
	RetProd == [] -> RetProd;
	true -> hd(RetProd)
	end.
