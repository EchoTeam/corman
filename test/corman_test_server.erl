%%% vim: ts=4 sts=4 sw=4 expandtab

-module(corman_test_server).
-behaviour(gen_server).

-export([start_link/2, stop/1, set_flag/2, get_flag/1, get_state/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-record(state, {flag=false, % used to verify the server is restarted 
                state=undefined % used to check for changes in spec
}).

%% ==========================
%% API
%% ==========================

start_link(Param, Name) ->
    gen_server:start({local, Name}, ?MODULE, [Param], []).

stop(Name) ->
    gen_server:call(Name, stop).

set_flag(Name, Value) ->
    gen_server:call(Name, {set_flag, Value}).

get_flag(Name) ->
    gen_server:call(Name, get_flag).

get_state(Name) ->
    gen_server:call(Name, get_state).

%% ==========================
%% Gen server callbacks
%% ==========================

init([State]) ->
    {ok, #state{state=State}}.

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

handle_call({set_flag, Value}, _From, S) ->
    {reply, ok, S#state{flag=Value}};

handle_call(get_flag, _From, S=#state{flag=Flag}) ->
    {reply, Flag, S};

handle_call(get_state, _From, S=#state{state=State}) ->
    {reply, State, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(normal, _S) ->
    ok.

code_change(_OldV, S, _Extra) ->
    {ok, S}.
