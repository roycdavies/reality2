defmodule Reality2.Plugins do
  @moduledoc false

  use DynamicSupervisor

  def start_link({name, id, definition_map}) do
    DynamicSupervisor.start_link(__MODULE__, {name, id, definition_map}, name: String.to_atom(id <> "|plugins"))
  end

  def start_child({name, id, definition_map}) do
    # If MyWorker is not using the new child specs, we need to pass a map:
    # spec = %{id: MyWorker, start: {MyWorker, :start_link, [foo, bar, baz]}}
    spec = {Reality2.Plugin, {name, id, definition_map}}
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
