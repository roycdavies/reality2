FULL = 1
PROJECT = mrserver
PROJECT_DESCRIPTION = Imersia Mixed Reality Server
PROJECT_VERSION = 3.2.0
PROJECT_RELEASE_DATE = 2023.07.30
# Also change in relx.config

DEPS = gun cowboy ej uuid eunit geode emagick mongodb gen_smtp bcrypt simple_oauth2 iso8601 edate etest_http jiffy mimerl etoml zucchini jsx bson # qrcode rust_mk jsx
dep_cowboy_commit = master
dep_gun_commit = master
dep_ranch_commit = master
# LOCAL_DEPS = crypto ssl inets mnesia
LOCAL_DEPS = crypto ssl inets mnesia observer wx

define PROJECT_ENV
[
  {version, "$(PROJECT_VERSION)-$(PROJECT_RELEASE_DATE)"},
  {admin_exception, "exploremine"}
]
endef

# ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +native

BUILD_DEPS += relx

include erlang.mk
