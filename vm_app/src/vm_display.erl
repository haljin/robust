-module(vm_display).
-behavior(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).

-export([start/0, display/2]).

start() ->
    gen_event:start_link({local,?MODULE}),
    gen_event:add_handler(?MODULE, vm_display, []).

display(Info, Args)->
   gen_event:notify(?MODULE,{display,Info,Args}).

 
init([]) ->
{ok, []}.
 
handle_event({display,Info,Args}, State) ->
    io:format("<<"++Info++">>~n",Args),
{ok, State}.
 
handle_call(_, State) ->
{ok, ok, State}.
 
handle_info(_, State) ->
{ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
{ok, State}.
 
terminate(_Reason, _State) ->
ok.
