%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Connection Mower.
%%
%% The Developer of this component is Erlang Solutions, Ltd.
%% Copyright (c) 2017-2018 Erlang Solutions, Ltd.  All rights reserved.
%%

-module(rabbit_connection_mower).

-behaviour(gen_server).

-include_lib("rabbit_connection_mower.hrl").

-export([start_link/0, mow/0, mow/1, stop_mowing/0, stop_mowing/1, stop/0]).

-export([mow_idle_connections/0,
         mow_idle_connections/1,
         mow_idle_connections/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% ------------------------------------------------------------
-spec start_link()       -> rabbit_types:ok_pid_or_error().
-spec mow()              -> rabbit_types:ok_pid_or_error().
-spec mow(integer())     -> rabbit_types:ok_pid_or_error().
-spec stop_mowing()      -> 'ok'.
-spec stop_mowing(pid()) -> 'ok'.
-spec stop()             -> 'ok'.
-spec mow_idle_connections()                  -> 'ok'.
-spec mow_idle_connections(integer())         -> 'ok'.
-spec mow_idle_connections(integer(), atom()) -> 'ok'.
% ------------------------------------------------------------

-define(MOWER,  ?MODULE).

-record(state,
                {ref,                   %% next scheduled message reference
                 channel_max_idle_t,    %% max allowed channel idle time
                 mowing_interval,       %% max allowed channel idle time
                 log_level,             %% log level, 'high | low'
                 scheduled,             %% operation mode, 'true | false'
                 t_ref,                 %% next scheduled timer ref
                 monitors        = [],  %% monitors of internal mowing procs
                 internal_mowers = []   %% internal mowing pids
                }).

% ----
% API
% ----
start_link() ->
    gen_server:start_link({local, ?MOWER}, ?MODULE, [], []).

mow() ->
    gen_server:call(?MOWER, mow).

mow(IDLE_TTL) ->
    gen_server:call(?MOWER, {mow, IDLE_TTL}).

stop_mowing() ->
    gen_server:call(?MOWER, stop_mowing).

stop_mowing(MPid) ->
    gen_server:call(?MOWER, {stop_mowing, MPid}).

stop() ->
    gen_server:stop(?MOWER).

% -----------
% Direct API
% -----------
mow_idle_connections() ->
    mow_idle_connections(?DEFAULT_CHANNEL_MAX_IDLE_T).

mow_idle_connections(IDLE_TTL) ->
    mow_idle_connections(IDLE_TTL, high).

mow_idle_connections(IDLE_TTL, LogLevel) ->
    mow_idle_connections(ets:first(?CHANNEL_METRICS_TAB), IDLE_TTL, LogLevel).

% ----------
% Callbacks
% ----------
init(_Args) ->
    State = #state{scheduled = Scheduled,
                   mowing_interval = I} = set_config(#state{}),
    {ok, {TRef, Ref}} = if Scheduled -> schedule_next_mow(I);
                            true     -> {ok, {undefined, undefined}}
                        end,
    {ok, validate(State#state{ref = Ref, t_ref = TRef, scheduled = Scheduled})}.

handle_call(mow, _From, State = #state{channel_max_idle_t = IDLE_TTL,
                                       monitors           = MRefs,
                                       internal_mowers    = MPids}) ->
    MPid = spawn(fun() -> mow_idle_connections(IDLE_TTL) end),
    MRef = erlang:monitor(process, MPid),
    {reply, {ok, MPid}, State#state{internal_mowers = [MPid | MPids],
                                    monitors        = [MRef | MRefs]}};
handle_call({mow, IDLE_TTL}, _From, State = #state{monitors         = MRefs,
                                                   internal_mowers  = MPids,
                                                   log_level        = LogLevel}) ->
    MPid = spawn(fun() -> mow_idle_connections(IDLE_TTL, LogLevel) end),
    MRef = erlang:monitor(process, MPid),
    {reply, {ok, MPid}, State#state{internal_mowers = [MPid | MPids],
                                    monitors        = [MRef | MRefs]}};
handle_call(stop_mowing, _From, State = #state{t_ref           = TRef,
                                               internal_mowers = MPids}) ->
    stop_internal(TRef, MPids),
    %% State updated/cleared on reception of 'DOWN' messages...
    {reply, ok, State#state{t_ref = undefined}};
handle_call({stop_mowing, MPid}, _From, State) ->
    exit(MPid, normal),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Ref, mow}, S = #state{ref                = Ref,
                                   channel_max_idle_t = IDLE_TTL,
                                   mowing_interval    = Interval,
                                   monitors           = MRefs,
                                   internal_mowers    = MPids}) ->
    MPid = spawn(fun() -> mow_idle_connections(IDLE_TTL) end),
    MRef = erlang:monitor(process, MPid),
    {ok, {TRef, MsgRef}} = schedule_next_mow(Interval),
    {noreply, S#state{ref             = MsgRef,
                      t_ref           = TRef,
                      monitors        = [MRef | MRefs],
                      internal_mowers = [MPid | MPids]}};

handle_info({'DOWN', MRef, process, MPid, _Reason},
            S = #state{monitors = MRefs0, internal_mowers = MPids0}) ->
    {MRefs, MPids} =
        case {lists:member(MRef, MRefs0), lists:member(MPid, MPids0)} of
             {true, true} ->
                  {MRefs0 -- [MRef], MPids0 -- [MPid]};
             _ ->
                  {MRefs0, MPids0}
        end,
    {noreply, S#state{internal_mowers = MPids, monitors = MRefs}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{t_ref = TRef, internal_mowers = MPids}) ->
    ok = stop_internal(TRef, MPids).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ---------
% Internal
% ---------
schedule_next_mow(Interval) ->
    {ok, TRef} = timer:send_after(Interval, ?MOWER, {Ref = make_ref(), mow}),
    {ok, {TRef, Ref}}.

mow_idle_connections('$end_of_table', _IDLE_TTL, _LogLevel) -> 'ok';
mow_idle_connections(Ch, IDLE_TTL, LogLevel) when is_pid(Ch) ->
    try ets:next(?CHANNEL_METRICS_TAB, Ch) of
        NextCh when is_pid(NextCh); NextCh =:= '$end_of_table' ->
            case rabbit_misc:pget(idle_since, get_stats(Ch)) of
                undefined ->
                    void;
                T ->
                    CurrentElapsedIdle = ts() - T,
                    case {CurrentElapsedIdle >= IDLE_TTL, get_conn(Ch)} of
                        {true, Conn} when is_pid(Conn) ->
                            maybe_terminate(Conn, LogLevel);
                        _ ->
                            void
                    end
            end,
            mow_idle_connections(NextCh, IDLE_TTL, LogLevel)
    catch
        _:_ ->
            ?WARN("channel ~p already terminated. Exiting mowing cycle", [Ch]),
            ok
    end.

stop_internal(TRef, MPids) ->
    timer:cancel(TRef),
    [exit(MPid, normal) || MPid <- MPids],
    ok.

maybe_terminate(Conn, LogLevel) ->
    %% if 'channel_aggregate' =< MAX_CHANNEL_CUTOFF_AGGREGATE,
    %% terminate an elapsed idle connection. Current cutoff aggregate=1
    %% TODO: Configurable MAX_CHANNEL_CUTOFF_AGGREGATE > 1.
    try
        ?RABBIT_CONNECTION_INFO(Conn, [channels])
    of
        [{channels, ChN}] when ChN =< ?MAX_CHANNEL_CUTOFF_AGGREGATE ->
            ok = ?RABBIT_CONNECTION_CLOSE(Conn,
                    ?MSG_PREFIX ++ "closed idle connection");
        [{channels, ChN}] ->
            if LogLevel == high ->
                  ?WARN("connection termination forbidden. ~p active channels "
                        "exceeds maximum allowed channel aggregate ~p per "
                        "connection", [ChN, ?MAX_CHANNEL_CUTOFF_AGGREGATE]);
               true ->
                  'ok'
            end
    catch
        _:_ -> 'ok'
    end.

get_conn(Ch) ->
    try
        [{connection, Conn}] = ?RABBIT_CHANNEL_INFO(Ch, [connection]),
        Conn
    catch
        _:_ -> undefined
    end.

get_stats(Ch) when is_pid(Ch) ->
    case ets:lookup(?CHANNEL_METRICS_TAB, Ch) of
        [{Ch, Stats}] -> Stats;
        _zilch        -> []
    end.

ts() ->
    {Mega, Sec, USec} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + round(USec/1000).

validate(S = #state{channel_max_idle_t = T, mowing_interval = I}) ->
    T0 = validate_max_idle_t(?TO_INTEGER(T)),
    I0 = validate_mowing_interval(?TO_INTEGER(I)),
    S#state{channel_max_idle_t = T0, mowing_interval = I0}.

validate_max_idle_t(T) when is_integer(T) -> T;
validate_max_idle_t(T) -> throw({?MODULE, {bad_max_idle_time_config, T}}).

validate_mowing_interval(I) when is_integer(I); I >= ?MIN_MOWING_INTERVAL -> I;
validate_mowing_interval(I) ->
    ?WARN("bad mowing interval ~pms configuration. Setting interval to minimum "
          "allowed, ~pms", [I, ?MIN_MOWING_INTERVAL]),
    ?MIN_MOWING_INTERVAL.

set_config(S = #state{}) ->
    MOWING_INTERVAL = get_env(mowing_interval, ?MIN_MOWING_INTERVAL),
    SHEDULED        = get_env(scheduled, ?DEFAULT_SCHEDULED),
    CH_IDLE_TTL     = get_env(channel_max_idle_t, ?DEFAULT_CHANNEL_MAX_IDLE_T),
    LOG_LEVEL       = get_env(log_level, ?DEFAULT_LOG_LEVEL),
    S#state{channel_max_idle_t = CH_IDLE_TTL,
            mowing_interval    = MOWING_INTERVAL,
            scheduled          = SHEDULED,
            log_level          = LOG_LEVEL}.

get_env(Env, Default) ->
    rabbit_misc:get_env(rabbitmq_connection_mower, Env, Default).
