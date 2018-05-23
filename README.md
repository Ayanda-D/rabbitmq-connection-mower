# RabbitMQ Connection Mower Plugin

Purpose of this plugin is to mow/cut/terminate connections which have been idle for periods of time exceeding the preconfigured maximum idle channel time. Not all RabbitMQ client libraries provide means of terminating idle connections, which can be resource heavy and make inefficient use of the broker if left unused over prolonged periods of time.

<p style="text-align:center"><img src="./priv/images/lawn_mower.jpg" align="centre" height="250" width="800"></p>
<p style="text-align:center"><b>Fig 1: General purpose lawn mower</b></p>


An example is in the [Internet of Things](https://en.wikipedia.org/wiki/Internet_of_things), where thousands, or even millions of devices connecting to a RabbitMQ broker/cluster may be idle for long durations of time, without necessarily making much use of the broker. In such instances, terminating idle connections, regardless of network stability, and leaving onus on the connecting client/device to reconnect and make use of the broker only when necessary would provide a more efficient use of the broker and system resources.

**NB:** The plugin's operation of cutting-off idle connections draws anology to the purpose and use of a typical lawn mower, hence the name.


## Supported RabbitMQ Versions

This plugin targets RabbitMQ 3.6.0 and later versions.

## Installation

See [Plugin Installation](http://www.rabbitmq.com/installing-plugins.html) for details
about how to install plugins that do not ship with RabbitMQ.

## Build

Clone and build the plugin by executing `make`. To create a package, execute `make dist` and find the `.ez` package file in the plugins directory.

## Usage ##

Just enable the plugin with the following command:

```bash
rabbitmq-plugins enable rabbitmq_connection_mower
```
If configured `scheduled` operation, the plugin will periodically, after every `mowing_interval`, search for reported idle channels within the broker and terminate the parent connections of those exceeding the preconfigured `channel_max_idle_t`. The plugin may also also be manually executed, to forcefully initiate mowing of idle connections from the broker.

## Limitations ##

The plugin currently supports idle, non-multiplexed AMQP connections. In other words, only connections with single channels may be closed when idle for periods of time exceeding `channel_max_idle_t`. 

## LICENSE ##

See the LICENSE file

**(c) Erlang Solutions Ltd. 2016-2018**

[https://www.erlang-solutions.com/](https://www.erlang-solutions.com/)