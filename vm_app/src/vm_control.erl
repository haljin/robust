-module(vm_control).
-behavior(gen_fsm).
-export([start/0, stop/0, choose_product/1, insert_coin/1, 
	 cancel/0]).
%% gen_fsm callbacks
-export([init/1, idle/2, coin_inserted/2, chosen_product/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-include("../include/vmdata.hrl").
-record(state, {product,money=0}).


start()->
   gen_fsm:start_link({local,?MODULE}, ?MODULE, [], []).

stop()->
    gen_fsm:send_all_state_event(?MODULE,stop).

choose_product(Product)->
    gen_fsm:send_event(?MODULE,{choose_product, Product}).

insert_coin(Coin)->
    gen_fsm:send_event(?MODULE, {insert_coin,Coin}).

cancel()->
    gen_fsm:send_all_state_event(?MODULE,cancel).

init([]) ->
    vm_display:display("Ready",[]),
    {ok, idle,  #state{}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
idle({choose_product, Product}, #state{} = State)->
    case vm_stock:check_prod(Product) of
	true->
	    vm_display:display("You have chosen: ~p~n",[Product]),
	    {next_state,chosen_product,State#state{product=Product}};
	false ->
	    vm_display:display("Out of stock~n",[]),
	    {next_state,idle,State}
    end.
chosen_product({choose_product,Product}, #state{}=State)->
    case vm_stock:check_prod(Product) of
	true->
	    vm_display:display("You have chosen: ~p~n",[Product]),
	    {next_state,chosen_product,State#state{product=Product}};
	false ->
	    vm_display:display("Out of stock~n",[]),
	    {next_state,chosen_product,State}
    end;
chosen_product({insert_coin,Coin}, #state{product=Prod}=State) ->
    {prod, ProdInfo} = vm_stock:prod_info(Prod),
    case vm_coin:coin_to_val(Coin)< ProdInfo#product.price  of
	true->
	     vm_display:display("You have inserted ~p coins.Not enough. The price is ~p~n",[Coin,ProdInfo#product.price]),
	    {next_state,coin_inserted,State#state{money=vm_coin:coin_to_val(Coin)}};
	false->
	    vm_stock:get_prod(Prod,vm_coin:coin_to_val(Coin)),
	    vm_display:display("Take your ~p~n",[Prod]),
	    {next_state,idle,#state{}}
    end.
coin_inserted({insert_coin,Coin},#state{product=Prod,money=Money}=State)->
    {prod, ProdInfo} = vm_stock:prod_info(Prod),
    case vm_coin:coin_to_val(Coin)< ProdInfo#product.price  of
	true->
	    vm_display:display("You have inserted ~p coins.Not enough. The price is ~p~n",[Coin,ProdInfo#product.price]),
	    {next_state,coin_inserted,State#state{money=Money+vm_coin:coin_to_val(Coin)}};
	false->
	    vm_stock:get_prod(Prod,Money+vm_coin:coin_to_val(Coin)),
	    vm_display:display("Take your ~p~n",[Prod]),
	    {next_state,idle,#state{}}
    end.


%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------

handle_event(cancel, _,#state{money=Money}) ->
    vm_coin:get_change(Money),
    {next_state,idle,#state{}};
handle_event(stop,_,State) ->
    {stop,normal,State}.



%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From,StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason,_,#state{money=Money}) ->
    vm_coin:get_change(Money),
    vm_display:display("Termination ~n",[]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
