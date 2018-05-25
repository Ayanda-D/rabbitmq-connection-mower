# RabbitMQ Connection Mower Plugin

Purpose of this plugin is to mow/cut/terminate connections which have been idle for periods of time exceeding the preconfigured maximum idle channel time. Not all RabbitMQ client libraries provide means of terminating idle connections, which can be resource heavy and make inefficient use of the broker if left unused over prolonged periods of time.

<p style="text-align:center"><img src="./priv/images/lawn_mower.jpg" align="centre" height="220" width="850"></p>
<p style="text-align:center"></p>


An example is in the [Internet of Things](https://en.wikipedia.org/wiki/Internet_of_things), where thousands, or even millions of devices connecting to a RabbitMQ broker/cluster may be idle for long durations of time, without necessarily making much use of the broker. In such instances, terminating idle connections, regardless of network stability, and leaving onus on the connecting client/device to reconnect and make use of the broker only when necessary would provide a more efficient use of the broker and system resources.

**NB:** The plugin's operation of cutting-off idle connections draws analogy to the purpose and use of a typical lawn mower, hence the name.


## Supported RabbitMQ Versions

This plugin targets **RabbitMQ 3.6.7** and later versions.

## Installation

See [Plugin Installation](http://www.rabbitmq.com/installing-plugins.html) for details
about how to install plugins that do not ship with RabbitMQ.

## Build

Clone and build the plugin by executing `make`. To create a package, execute `make dist` and find the `.ez` package file in the plugins directory.

## Testing

Upon cloning the plugin, execute `make tests` to trigger a test run. View test results from the generated HTML files.

## Configuration

The Connection Mower plugin is configured in the `rabbitmq.config` or `advanced.config` files for RabbitMQ versions `3.6.x` and/or `3.7.x` respectively, as follows:

```
[{rabbitmq_connection_mower,
     [{channel_max_idle_t,     60000},
      {scheduled,              true},
      {mowing_interval,        30000},
      {log_level,              low}]
 }].
```

The following is a summary of the configuration parameter descriptions, types and defaults.


| PARAMETER NAME  | DESCRIPTION  | TYPE  |  DEFAULT |
|---|---|---|---|
| channel\_max\_idle_t  | Maximum allowed idle period (in milliseconds) for a channel/connection to be considered inactive to be mowed/terminated | Integer | 60000 |  
| scheduled | Mode of operation of the plugin. In scheduled operation, the plugin will periodically check for idle connections, and mow connections off, if any | Boolean | true |
| mowing\_interval | Time period (in milliseconds) after which the plugin will execute/engage into the connection mowing cycle | Integer | 30000 |
| log\_level | Flag (`high` or `low`) indicating level of operational detail the plugin will log during its operation cycles | Atom | low  |

## Usage ##

Once installed, i.e. `rabbitmq_connection_mower-<VERSION>.ez` file available in the `<RABBITMQ-HOME-DIR>/plugins/` directory, enable the plugin with the following command:

```
rabbitmq-plugins enable rabbitmq_connection_mower
```
If configured for **scheduled** operation, the plugin will periodically, after every **mowing_interval**, search for reported idle channels within the broker and terminate the parent connections of those exceeding the preconfigured **channel\_max\_idle_t**. The plugin may also be manually executed, to forcefully initiate mowing of idle connections from the broker.

```
rabbitmqctl eval 'rabbit_connection_mower:mow().'
```
or, passing a custom **channel\_max\_idle_t** period.

```
rabbitmqctl eval 'rabbit_connection_mower:mow(30000).'
```


## Limitations ##

The plugin is intended for idle, non-multiplexed AMQP connections. i.e. only connections with single channels may be closed when idle for periods of time exceeding `channel_max_idle_t`. Multiplexed connections may reflect inaccuracies on the aggregate instantaneous measurements carried out for the idleness of channels, and subsequently, the parent connections.

## LICENSE ##

See the LICENSE file

**(c) Erlang Solutions Ltd. 2016-2018**

[https://www.erlang-solutions.com/](https://www.erlang-solutions.com/)
