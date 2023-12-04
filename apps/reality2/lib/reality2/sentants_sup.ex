defmodule Reality2.Sentants.Supervisor do
  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init (_args) do
    children = []

    Supervisor.init(children, strategy: :simple_one_for_one)
  end
end
