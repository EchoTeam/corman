%%% vim: ts=4 sts=4 sw=4 expandtab
%%%
%%% Copyright (c) 2013 JackNyfe. All rights reserved.
%%% THIS SOFTWARE IS PROPRIETARY AND CONFIDENTIAL. DO NOT REDISTRIBUTE.
%%%
%%% Supervisor Reconfiguration Manager
%%%
-module(superman).

-export([
    reconfigure_supervisor/2,
    reconfigure_supervisor_tree/2,
    start_children/2,
    reconfigure_supervisor_init_args/2,
    reconfigure_supervisor_tree_init_args/2,
    get_child_specs/2,
    restart_children/2
]).

%%%=================================================
%%% API
%%%=================================================

get_child_specs(Module, Config) ->
    {ok, {_, Specs}} = Module:init(Config),
    Specs.

reconfigure_supervisor_init_args(Module, Config) ->
    reconfigure_supervisor(Module, get_child_specs(Module, Config)).

reconfigure_supervisor(SupName, Specs) ->
    io:format("~s* Reconfiguring supervisor ~p~n", [s(0), SupName]),

    {Deleted, New, Changed, _Unchanged} = extract_children(SupName, Specs),
    reconfigure_supervisor_ll(SupName, Deleted, New, Changed, 0).
    
start_children(Module, Config) ->
    ChildSpecs = get_child_specs(Module, Config),
    [{element(1, ChildSpec), Result} ||
        ChildSpec <- ChildSpecs,
        Result <- [case supervisor:start_child(Module, ChildSpec) of
                {ok, R} -> {started, R};
                {error, {already_started, _} = R} -> R;
                {error, already_present = R} -> R;
                {error, _} = R -> R
            end],
        element(1, Result) /= already_started
    ].

restart_children(SupName, Children) ->
    restart_children(SupName, Children, 0).

reconfigure_supervisor_tree_init_args(Module, Config) ->
    reconfigure_supervisor_tree(Module, get_child_specs(Module, Config)).

reconfigure_supervisor_tree(SupName, Specs) ->
    reconfigure_supervisor_tree(SupName, Specs, 0).

%%%======================================================
%%% Internal Functions
%%%======================================================

get_supervisor_state(SupPid) ->
    {status, _Pid, {module, _Mod}, 
     [_PDict, _SysState, _Parent, _Dbg, Misc]} = sys:get_status(SupPid),
    case Misc of
        [_Name, State1, _Type, _Time] -> State1;
        [_Header, _Data, {data, [{"State", State2}]}] -> State2
    end.

reconfigure_supervisor_ll(SupName, Deleted, New, Changed, N) ->
    % Terminate old children
    delete_children(SupName, Deleted, N),
    % Start new children
    start_children(SupName, New, N),
    % Restart changed children
    restart_children(SupName, Changed, N).

delete_children(SupName, Children, N) ->
    lists:foreach(
        fun(C) ->
            V = element(1, C),
            io:format("~sDelete child: ~p~n",[s(N), V]),
            supervisor:terminate_child(SupName, V),
            supervisor:delete_child(SupName, V)
        end, Children).

start_children(SupName, Children, N) ->
    lists:foreach(
        fun(C) ->
            io:format("~sStart child: ~p~n",[s(N), element(1, C)]),
            {ok, _} = supervisor:start_child(SupName, C)
        end, Children).

reconfigure_supervisor_tree(SupName, Specs, N) ->
    io:format("~s* Reconfiguring supervisor tree ~p~n", [s(N), SupName]),

    NextN = N + 2,

    {Deleted, New, Changed, Unchanged} = extract_children(SupName, Specs),
    reconfigure_supervisor_ll(SupName, Deleted, New, Changed, NextN),

    Children = supervisor:which_children(SupName),

    % Recursively call reconfigure for unchanged children (supervisors only)
    lists:foreach(
        fun ({Id, MFA,_Restart,_Shutdown, supervisor,_Modules} = Spec) ->
                case lists:keyfind(Id, 1, Children) of
                    {Id, ChildPid, _, _} ->
                        SupState = get_supervisor_state(ChildPid),
                        case element(#supervisor_state.strategy, SupState) of
                            simple_one_for_one ->
                                io:format("~sSkip child supervisor: ~p because of simple_one_for_one strategy~n", [s(NextN), Id]),
                                ok;
                            _ ->
                                io:format("~sReconfiguring child supervisor: ~p~n", [s(NextN), Id]),
                                Module = sup_module_by_pid(ChildPid),
                                try sup_specs(Module, MFA) of
                                    SupSpecs ->
                                        reconfigure_supervisor_tree(ChildPid, SupSpecs, NextN)
                                catch
                                    error:no_child_specs_function ->
                                        io:format("~sSupervisor ~p has no children_specs function, restart it...~n", [s(NextN), Id]),
                                        supervisor:terminate_child(SupName, Id),
                                        supervisor:delete_child(SupName, Id),
                                        {ok, _} = supervisor:start_child(SupName, Spec)
                                end
                        end;
                    false ->
                        io:format("~sSkip child ~p because it isn't running~n", [s(NextN), Id])
                end;
            (_) ->
                ok % Nothing to do with workers
        end, Unchanged).

restart_children(SupName, Children, N) ->
    lists:foreach(
        fun(C) ->
            case element(1, C) of
                data_server -> ok;      % exclude it for some reason
                udp_stats_server -> ok; % (moved here from reconfigure_supervisor/2)
                V ->
                    io:format("~sRestart child: ~p~n",[s(N), V]),
                    supervisor:terminate_child(SupName, V),
                    supervisor:delete_child(SupName, V),
                    {ok, _} = supervisor:start_child(SupName, C)
            end
        end, Children).

extract_children(SupName, ChildrenSpec) ->
    SupPid =
        if  is_atom(SupName) -> whereis(SupName);
            is_pid(SupName)  -> SupName
        end,
    State = get_supervisor_state(SupPid),
    Children = element(#supervisor_state.children, State),
    OldChildren = lists:keysort(1, [{Name, StartFunc, Restart, ShutDown, Type, Modules} || {child, _Pid, Name, StartFunc, Restart, ShutDown, Type, Modules} <- Children]),
    NewChildren = lists:keysort(1, ChildrenSpec),
    get_children_diff(OldChildren, NewChildren).

get_children_diff(OldChildren, NewChildren) ->
    get_children_diff(OldChildren, NewChildren, [], [], [], []).

get_children_diff([], L2, Old, New, Ch, Unch) ->
    {Old, New ++ L2, Ch, Unch};
get_children_diff(L1, [], Old, New, Ch, Unch) ->
    {Old ++ L1, New, Ch, Unch};
get_children_diff([H|T1], [H|T2], Old, New, Ch, Unch) ->
    get_children_diff(T1, T2, Old, New, Ch, [H|Unch]);
get_children_diff([H1|T1], [H2|T2], Old, New, Ch, Unch) when element(1, H1) == element(1, H2) ->
    {_OName, OStartFunc, ORestart, OShutDown, OType, _OModules} = H1,
    {_NName, NStartFunc, NRestart, NShutDown, NType, _NModules} = H2,
    case (OStartFunc == NStartFunc) andalso
        (ORestart == NRestart) andalso
        (OShutDown == NShutDown) andalso
        (OType == NType) of
        true -> get_children_diff(T1, T2, Old, New, Ch, [H2|Unch]);
        false -> get_children_diff(T1, T2, Old, New, [H2|Ch], Unch)
    end;
get_children_diff([H1|T1], [H2|_] = L2, Old, New, Ch, Unch) when element(1, H1) < element(1, H2) ->
    get_children_diff(T1, L2, [H1|Old], New, Ch, Unch);
get_children_diff([H1|_] = L1, [H2|T2], Old, New, Ch, Unch) when element(1, H1) > element(1, H2) ->
    get_children_diff(L1, T2, Old, [H2|New], Ch, Unch).

sup_module_by_pid(SupName) when is_atom(SupName) ->
    case erlang:whereis(SupName) of
        Pid when is_pid(Pid) -> sup_module_by_pid(Pid);
        undefined -> erlang:error({unknown_proc, SupName})
    end;

sup_module_by_pid(Sup) when is_pid(Sup) ->
    Dict =
        case process_info(Sup, dictionary) of
            undefined -> erlang:error({unknown_proc, Sup});
            {_, D} -> D
        end,

    case proplists:get_value('$initial_call', Dict, undefined) of
        undefined -> erlang:error({no_initial_call, Sup});
        {supervisor, Module, _} -> Module;
        BadRes -> erlang:error({module_not_sup, {Sup, BadRes}})
    end.

% Function that translates supervisor start MFA to child specs
sup_specs(Module, MFA) when is_atom(Module) ->
    Funs =
        try Module:module_info(exports)
        catch
            _:_ -> erlang:error({bad_module_info, Module})
        end,

    case lists:member({children_specs, 1}, Funs) of
        true  -> Module:children_specs(MFA);
        false -> erlang:error(no_child_specs_function)
    end.

s(N) -> [ $\s || _ <- lists:seq(1, N)].

