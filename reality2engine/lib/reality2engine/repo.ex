defmodule Reality2engine.Repo do
  use Ecto.Repo,
    otp_app: :reality2engine,
    adapter: Ecto.Adapters.Postgres
end
