PROJECT = rabbitmq_connection_mower
PROJECT_DESCRIPTION = RabbitMQ Connection Mower
PROJECT_MOD = rabbit_connection_mower_app

define PROJECT_ENV
[
  {channel_max_idle_t,  5000},
  {scheduled,           true},
  {mowing_interval,     2000},
  {log_level,           high}
     ]
  }
endef

define PROJECT_APP_EXTRA_KEYS
	{broker_version_requirements, ["3.7.0"]}
endef

DEPS = rabbit_common rabbit
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers amqp_client

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
