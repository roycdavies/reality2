defmodule Reality2engine.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      Reality2engineWeb.Telemetry,
      # Start the Ecto repository
      Reality2engineStorage.Repo,
      # Start the PubSub system
      {Phoenix.PubSub, name: Reality2engine.PubSub},
      # Start the Endpoint (http/https)
      Reality2engineWeb.Endpoint,

      Reality2engine.SentantSupervisor

      # Start a worker by calling: Reality2engine.Worker.start_link(arg)
      # {Reality2engine.Worker, []}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Reality2engine.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    Reality2engineWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
