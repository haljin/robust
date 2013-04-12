-module(vmdb_sup).
-behavior(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/0, stop/0]).


start()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

stop()->
    exit(whereis(?MODULE),normal).

init([])->
    Stock = {vm_stock, {vm_stock, start, []}, permanent, 2000, worker, [vm_stock]},
    Coins = {vm_coin, {vm_coin, start, []}, permanent, 2000, worker, [vm_coin]},
    {ok, {{one_for_one,2,1}, [Stock, Coins]}}.
