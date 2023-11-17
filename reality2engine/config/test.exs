import Config

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :reality2engine, Reality2engine.Repo,
  username: "postgres",
  password: "#6DDf9f9ce4",
  hostname: "localhost",
  database: "reality2engine_test#{System.get_env("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :reality2engine, Reality2engineWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "6LZF3gz9To6N+qWSm7H5yM0TRdlWN0zmDeTEMQyCBphcz3XjH9rv1j1KG/s3LX59",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime
