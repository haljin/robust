-module(vm_stock).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, stop/0, insert_prod/1, check_prod/1, check_prod/2, get_prod/2]).

-include("../include/vmdata.hrl").
%-record(coin, {type, value})

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()-> gen_server:cast(?MODULE, stop).
	
insert_prod(#product{} = Prod) -> gen_server:call(?MODULE, {insert_prod, Prod}).
check_prod(ProdName) -> gen_server:call(?MODULE, {check_prod, ProdName}).
get_prod(ProdName, Money) -> gen_server:call(?MODULE, {get_prod, ProdName, Money}).

% This is called when a connection is made to the server
init([]) ->
	Products = [],
	{ok, Products}.

% handle_call is invoked in response to gen_server:call
handle_call({insert_prod, #product{name = Name, price = Price, ammount = Am} = Prod}, _From, Products) ->
	Response = case lists:keymember(Name, 2, Products) of %case check_prod(Name,Products) of
	true -> % change the existing value
		{PriceL, AmL} = ret_prod_val(Name,Products),
		ListT = lists:keydelete(Name, 2, Products),
		NewProducts = ListT ++ [Prod#product{ammount = Am + AmL}];
	false -> % append at the end
		NewProducts = Products ++ [Prod]
	end,
	{reply, Response, NewProducts};
	
handle_call({check_prod, ProdName}, _From, Products) ->
    RetProd = [{X,Y} ||  #product{name = Name, ammount = X, price = Y} <- Products, Name =:= ProdName],
	Response = retVal(RetProd),
	{reply, Response, Products};
	
handle_call({get_prod, ProdName, Money}, _From, Products) ->
	RetVal = case check_prod(ProdName,Products) of
	       true -> % reduce the amount of product
		#product{name = Name, price = Price, ammount = Am} = ret_prod(ProdName, Products),
		Temp = Money - Price,
		if 
		Temp < 0 ->
			NewProducts = Products, % not enough money
			io:format("not enough money");
		Temp == 0 ->
			% enough money input, + give product
			{PriceL, AmL} = ret_prod_val(ProdName,Products), %remove product from stock
			NewProducts = reduceProd(AmL, ProdName, Products),
			vm_case:insert(ProdName); % product to case
		true ->
			% enough money input, + give product, + give change
			{PriceL, AmL} = ret_prod_val(ProdName,Products), % remove product from stock
			NewProducts = reduceProd(AmL, ProdName, Products),			
			vm_case:insert(ProdName), % product to case
			vm_coin:get_change(Temp) % change to coincase
		end;
	false -> % product does not exist
		NewProducts = Products,
		Temp = -2,
		io:format("product does not exist")
	end,
	{reply, Temp, NewProducts};

handle_call(_Message, _From, Products) ->
	{reply, error, Products}.

reduceProd(1, ProdName, List) -> 
	Product = ret_prod(ProdName,List),
	NewList = lists:keydelete(ProdName,2,List),
	NewList;
	
reduceProd(_, ProdName, List) ->
	#product{name = Name, price = Price, ammount = Am} = ret_prod(ProdName,List),
	ListT = lists:keydelete(ProdName,2,List),
	NewList = ListT ++ [{Name, Price, Am-1}],
	NewList.

ret_prod_val(ProdName,List)->
    RetProd = [{X,Y} || #product{name = Name, price = X, ammount = Y} <- List, Name =:= ProdName],
	hd(RetProd).

ret_prod(ProdName,List)->
    RetProd = [#product{name = Name, price = X, ammount = Y} || #product{name = Name, price = X, ammount = Y} <- List, Name =:= ProdName],
	if 
	RetProd == [] -> RetProd;
	true -> hd(RetProd)
	end.
	
check_prod(ProdName,List)->
    RetProd = [{X,Y} || #product{name = Name, price = X, ammount = Y} <- List, Name  =:= ProdName],
	retVal(RetProd).
	
retVal([]) -> false;
retVal(Val) -> true.
	
% We get compile warnings from gen_server unless we define these
handle_cast(stop, Products) -> {stop, normal, Products}.
handle_info(_Message, Products) -> {noreply, Products}.
terminate(_Reason, _Products) -> ok.
code_change(_OldVersion, Products, _Extra) -> {ok, Products}.
