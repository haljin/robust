%% Author: vdw678
%% Created: Jul 25, 2012
%% Description: TODO: Add description to vmapp
-module(vmapp).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(Type, StartArgs) ->
    case vm_sup:start() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    io:format("~p~n",[Error])
    end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(State) ->
	vm_sup:stop(),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
