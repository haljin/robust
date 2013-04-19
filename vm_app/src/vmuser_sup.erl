-module(vmuser_sup).
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
    Control = {vm_control, {vm_control, start, []}, permanent, 2000, worker, [vm_control]},
    Display = {vm_display, {vm_display, start, []}, permanent, 2000, worker, dynamic},
    {ok, {{one_for_one,2,1}, [Display, Control]}}.
