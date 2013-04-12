-module(vm_control).
-behavior(gen_fsm).
-export([start/0, stop/0, choose_product/1, insert_coin/1, 
	  request/0, cancel/0]).
%% gen_fsm callbacks
-export([init/1, idle/2, got_coins/2, chosen_product/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).




start()->
    ok.

stop()->
    ok.

choose_product(Product)->
    ok.

insert_coin(Coin)->
    ok.

request()->
    ok.

cancel()->
    ok.

init([]) ->
    display:display(["Ready!"]),
    {ok, idle,  #state{}}.

idle({coin,Coin}, StateData) ->
	NewStateData = add_coin(Coin,StateData),
	display:display(["Money in: " ++ integer_to_list(NewStateData#state.coins)]),
	{next_state, got_coins, NewStateData};
idle({choose,Product}, StateData) ->
	{next_state, chosen_product, StateData#state{chosen_product = Product}};
idle(request, StateData) ->
	display:display(["Choose product and put some coins in, really!"]),
	{next_state, idle, StateData}.
  
got_coins({coin,Coin}, StateData) ->
	NewStateData = add_coin(Coin,StateData),
	display:display(["Money in: " ++ integer_to_list(NewStateData#state.coins)]),
	{next_state, got_coins, NewStateData};
got_coins({choose,Product},StateData) ->
	display:display(["You have chosen " ++ atom_to_list(Product)]),
	{next_state, chosen_product, StateData#state{chosen_product = Product}};
got_coins(request,StateData) ->
	display:display(["Choose product!"]),
	{next_state, got_coins, StateData}.

chosen_product({coin,Coin}, StateData) ->
	NewStateData = add_coin(Coin,StateData),
	display:display(["Money in: " ++ integer_to_list(NewStateData#state.coins)]),
	{next_state, chosen_product, NewStateData};
chosen_product({choose,Product},StateData) ->
	display:display(["You have chosen " ++ atom_to_list(Product)]),
	{next_state, chosen_product, StateData#state{chosen_product = Product}};
chosen_product(request,StateData) ->
	NewStateData=fulfill_request(StateData),
	{next_state, idle, NewStateData}.


%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(abort, _StateName, StateData) ->
	eject_coins(StateData#state.coins),
    {next_state, idle, StateData#state{coins = 0, chosen_product = undefined}};
handle_event(stop, _StateName, StateData) ->
	{stop, normal, StateData};
handle_event(display, StateName, StateData) ->
	Status = gen_server:call(genvmdb, display),
	display:display([Status]),
	{next_state, StateName, StateData};
handle_event(die, _,_) ->
	exit(error).

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
	eject_coins(StateData#state.coins),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.
