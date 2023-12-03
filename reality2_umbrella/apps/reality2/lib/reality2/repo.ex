defmodule Reality2.Repo do
  use Ecto.Repo,
    otp_app: :reality2,
    adapter: Ecto.Adapters.Postgres
end
