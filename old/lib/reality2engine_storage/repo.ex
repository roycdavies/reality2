defmodule Reality2engineStorage.Repo do
  use Ecto.Repo,
    otp_app: :reality2engine,
    adapter: Ecto.Adapters.Postgres
end
