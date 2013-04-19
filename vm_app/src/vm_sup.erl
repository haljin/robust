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
    Database = {vmdb, {vmdb_sup, start, []}, permanent, infinity, supervisor, [vmdb_sup]},
    Cases = {vmcases, {vmcase_sup, start, []}, permanent, infinity, supervisor, [vmcase_sup]},
    Control = {vmuser, {vmuser_sup, start, []}, permanent, infinity, supervisor, [vmuser_sup]},
    {ok, {{one_for_one,2,1}, [Database, Cases, Control]}}.
