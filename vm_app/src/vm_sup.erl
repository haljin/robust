-module(vm_sup).
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
    StockCase = {vmdb, {vmdb_sup, start, []}, permanent, infinity, supervisor, [vmdb_sup]},
    CoinCase = {vmcases, {vmcase_sup, start, []}, permanent, infinity, supervisor, [vmcase_sup]},
    {ok, {{one_for_one,2,1}, [StockCase, CoinCase]}}.
