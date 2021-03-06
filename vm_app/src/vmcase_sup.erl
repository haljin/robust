-module(vmcase_sup).
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
    StockCase = {vm_case, {vm_case, start, []}, permanent, 2000, worker, [vm_case]},
    CoinCase = {vm_coincase, {vm_coincase, start, []}, permanent, 2000, worker, [vm_coincase]},
    {ok, {{one_for_one,2,1}, [StockCase, CoinCase]}}.
