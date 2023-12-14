defmodule Reality2.Sentants.Supervisor do
  use DynamicSupervisor

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def start_child(name, init_arg) do
    # If MyWorker is not using the new child specs, we need to pass a map:
    # spec = %{id: MyWorker, start: {MyWorker, :start_link, [foo, bar, baz]}}
    spec = {Reality2.Sentants.Sentant, value: init_arg}
    DynamicSupervisor.start_child(name, spec)
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init( strategy: :one_for_one, extra_arguments: [init_arg] )
  end
end
