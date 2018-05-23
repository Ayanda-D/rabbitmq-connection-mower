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

-define(CHANNEL_METRICS_TAB,           channel_metrics).
-define(DEFAULT_CHANNEL_MAX_IDLE_T,    20000).
-define(DEFAULT_CHANNEL_AGGREGATE,     10).
-define(MAX_CHANNEL_CUTOFF_AGGREGATE,  1).
-define(MIN_MOWING_INTERVAL,           1000).
-define(DEFAULT_SCHEDULED,             true).
-define(DEFAULT_LOG_LEVEL,             high).

-define(RABBIT_CHANNEL_INFO(Ch, I),    rabbit_channel:info(Ch, I)).
-define(RABBIT_CONNECTION_INFO(C, I),  rabbit_networking:connection_info(C, I)).
-define(RABBIT_CONNECTION_CLOSE(C, M), rabbit_networking:close_connection(C, M)).
-define(TO_INTEGER(I),                 rabbit_data_coercion:to_integer(I)).

-define(MSG_PREFIX,     "Connection Mower Plugin ").
-define(ERR(MSG, A),    error_logger:error_msg(?MSG_PREFIX ++ "error: " ++ MSG, A)).
-define(WARN(MSG, A),   error_logger:warning_msg(?MSG_PREFIX ++ "warning: " ++ MSG, A)).
-define(ERR(MSG),       ?ERR(MSG,  [])).
-define(WARN(MSG),      ?WARN(MSG, [])).