defmodule Reality2engine.SentantSupervisor do
  use Supervisor

  def start_link(init_arg) do
    GenServer.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = []
    opts = [strategy: :one_for_one, name: Reality2engine.SentantSupervisor]

    Supervisor.init(children, opts)
  end

end
