defmodule Reality2.Plugins do
  @moduledoc false

  use DynamicSupervisor

  def start_link({name, init_arg}) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: String.to_atom(name <> "_plugins"))
  end

  def start_child({name, init_arg}) do
    # If MyWorker is not using the new child specs, we need to pass a map:
    # spec = %{id: MyWorker, start: {MyWorker, :start_link, [foo, bar, baz]}}
    spec = {Reality2.Plugin, {name, init_arg}}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init(
      strategy: :one_for_one,
      extra_arguments: [init_arg]
    )
  end
end
