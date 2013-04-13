-module(vm_stock).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/0, stop/0, insert_prod/1, check_prod/1, prod_info/1, get_prod/2, generate_dets/1]).

-include("../include/vmdata.hrl").
-record(state, {table}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> 
    gen_server:cast(?MODULE, stop).	
insert_prod(#product{} = Prod) -> 
    gen_server:call(?MODULE, {insert_prod, Prod}).
check_prod(ProdName) -> 
    gen_server:call(?MODULE, {check_prod, ProdName}).
prod_info(ProdName) ->
    gen_server:call(?MODULE, {prod_info, ProdName}).
get_prod(ProdName, Money) -> 
    gen_server:call(?MODULE, {get_prod, ProdName, Money}).

generate_dets(ProdList)->
    {ok, Table} = dets:open_file(tempstock, [{type,set},
					     {access,read_write},{keypos,#product.name},{file,"stock.db"}]),
    lists:map(fun(El) ->
		      dets:insert(Table, El)
	      end, ProdList),
    dets:close(Table).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, Table} = dets:open_file(stock, [{type,set},
					 {access,read_write},{keypos,#product.name},{file,"stock.db"}]),
    {ok, #state{table = Table}}.

handle_call({insert_prod, #product{name = Name, price = Price, ammount = Am} = Prod}, _From, 
	    #state{table = Table} = Products) ->
    Response = case dets:member(Table, Name) of 
		   true -> % change the existing value
		       dets:update_counter(Table, Name, {#product.ammount, 1}); 
		   false -> % append at the end
		       dets:insert(Table,Prod)
	       end,
    {reply, Response, Products};
handle_call({check_prod, ProdName}, _From, #state{table = Table} = Products) ->
    Response = case dets:lookup(Table, ProdName) of 
		   {error,Reason} -> false;
		   [#product{ammount = 0}] ->
		       false;
		   _ ->
		       true
	       end,
    {reply, Response, Products};
handle_call({prod_info, ProdName}, _From, #state{table = Table} = Products) ->
    Response = case dets:lookup(Table, ProdName) of 
		   {error,Reason} -> false;
		   [#product{ammount = 0}] ->
		       {prod, out_of_stock};
		   [Product] ->
		       {prod, Product} 
	       end,
    {reply, Response, Products};	
handle_call({get_prod, ProdName, Money}, _From, #state{table = Table} = Products) ->
	RetVal = case dets:lookup(Table, ProdName) of
		     [#product{ammount = 0}] ->
			 Temp = -2;
		     [#product{name = Name, price = Price}] ->
			     Temp = Money - Price,
			 if 
			     Temp < 0 ->
				 io:format("not enough money");
			     Temp == 0 ->
						% enough money input, + give product
				 dets:update_counter(Table, Name, {#product.ammount, -1}),
				 vm_case:insert(ProdName); % product to case
			     true ->
						% enough money input, + give product, + give change	      
				 dets:update_counter(Table, Name, {#product.ammount, -1}),
				 vm_case:insert(ProdName), % product to case
				 vm_coin:get_change(Temp) % change to coincase
			 end
		     end,
    {reply, Temp, Products};
handle_call(_Message, _From, Products) ->
	{reply, error, Products}.

handle_cast(stop, Products) -> 
    {stop, normal, Products}.

handle_info(_Message, Products) -> 
    {noreply, Products}.

terminate(_Reason, #state{table = Table}) -> 
    dets:close(Table),
    ok.

code_change(_OldVersion, Products, _Extra) -> 
    {ok, Products}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reduceProd(1, ProdName, List) -> 
    Product = ret_prod(ProdName,List),
    NewList = lists:keydelete(ProdName,#product.name,List),
    NewList;
reduceProd(_, ProdName, List) ->
    #product{name = Name, price = Price, ammount = Am} = ret_prod(ProdName,List),
    ListT = lists:keydelete(ProdName,#product.name,List),
    NewList = ListT ++ [{Name, Price, Am-1}],
    NewList.

ret_prod_val(ProdName,List)->
    RetProd = [{X,Y} || #product{name = Name, price = X, ammount = Y} <- List, Name =:= ProdName],
	hd(RetProd).

ret_prod(ProdName,List)->
    RetProd = [#product{name = Name, price = X, ammount = Y} 
	       || #product{name = Name, price = X, ammount = Y} <- List, Name =:= ProdName],
    if 
	RetProd == [] -> RetProd;
	true -> hd(RetProd)
    end.

check_prod(ProdName,List)->
    RetProd = [{X,Y} || #product{name = Name, price = X, ammount = Y} <- List, Name  =:= ProdName],
    retVal(RetProd).

retVal([]) -> false;
retVal(Val) -> true.

