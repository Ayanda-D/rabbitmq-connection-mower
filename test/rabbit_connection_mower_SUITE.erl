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
%% Copyright (c) 2017-2018 Erlang Solutions, Ltd. All rights reserved.
%%

-module(rabbit_connection_mower_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include("rabbit_connection_mower.hrl").

-compile(export_all).

all() ->
    [
      {group, non_parallel_tests}
    ].

groups() ->
    [
      {non_parallel_tests, [], [
          successful_idle_connection_mowering,
          unsuccessful_idle_connection_mowering
        ]}
    ].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config1 = rabbit_ct_helpers:set_config(Config, [
        {rmq_nodename_suffix, ?MODULE}
      ]),
    rabbit_ct_helpers:run_setup_steps(Config1,
      rabbit_ct_broker_helpers:setup_steps() ++
      rabbit_ct_client_helpers:setup_steps()).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config,
      rabbit_ct_client_helpers:teardown_steps() ++
      rabbit_ct_broker_helpers:teardown_steps()).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

%% -----------
%% Test Cases
%% -----------
successful_idle_connection_mowering(Config) ->
    ok = setup_connection_mower(Config, 0, 100, 1000),
    {ok, Ch} = init_connection_and_channel(Config, 0),
    1 = connections_count(Config, 0),
    enforce_idle_since_stat(Config, 0, Ch),
    delay(150),
    0 = connections_count(Config, 0),
    passed.

unsuccessful_idle_connection_mowering(Config) ->
    ok = setup_connection_mower(Config, 0, 100, 1000),
    {ok, Ch} = init_connection_and_channel(Config, 0),
    1 = connections_count(Config, 0),
    enforce_idle_since_stat(Config, 0, Ch),
    delay(50),
    1 = connections_count(Config, 0),
    passed.

%% ---------
%% Internal
%% ---------
init_connection_and_channel(Config, Node) ->
  Ch = rabbit_ct_client_helpers:open_channel(Config, Node),
  {ok, Ch}.

setup_connection_mower(Config, Node, IDLE_TTL, Interval) ->
  ok = rabbit_ct_broker_helpers:rpc(Config, Node,
         ?MODULE, init_connection_mower_remote, [IDLE_TTL, Interval]).

init_connection_mower_remote(IDLE_TTL, Interval) ->
  application:set_env(rabbitmq_connection_mower, channel_max_idle_t, IDLE_TTL),
  application:set_env(rabbitmq_connection_mower, scheduled, true),
  application:set_env(rabbitmq_connection_mower, mowing_interval, Interval),
  application:set_env(rabbitmq_connection_mower, log_level, high),

  ok = application:start(rabbitmq_connection_mower).

enforce_idle_since_stat(Config, Node, Ch) ->
  ok = rabbit_ct_broker_helpers:rpc(Config, Node,
          rabbit_core_metrics, channel_stats, [Ch, [{idle_since, current_ts()}]]).

connections_count(Config, Node) ->
  length(rabbit_ct_broker_helpers:rpc(Config, Node,
            rabbit_networking, connection_info_all, [[pid]])).

current_ts() -> os:system_time(milli_seconds).

delay(T) -> timer:sleep(T).
