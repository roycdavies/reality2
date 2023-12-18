defmodule Reality2.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false
  # alias Reality2.SentantsSup

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Reality2.Repo,
      # {DNSCluster, query: Application.get_env(:reality2, :dns_cluster_query) || :ignore},
      # {Phoenix.PubSub, name: Reality2.PubSub},
      {PartitionSupervisor, child_spec: DynamicSupervisor, name: Reality2.Sentants}
      # Start a worker by calling: Reality2.Worker.start_link(arg)
      # {Reality2.Worker, arg}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: __MODULE__)
  end
end
