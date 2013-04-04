-module(vm_coin).
%-behavior(gen_server).
-export([start/1, stop/0, check_change/1, get_change/1, insert_coins/1]).

start({list, CoinList})->
    ok;
start({dets, DetsTable}) ->
    ok.

stop()->
    ok.

%% Returns: ok - the machine has enough change
%%          {warning, nochange} - the machine does not have enough change
check_change(Ammount)->
    ok.

%% Returns: A list of coins as change, if there is not enough coins
%%          returns as many as possible
get_change(Ammount)->
    ok.

insert_coins(CoinList)->
    ok.
