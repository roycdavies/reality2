defmodule Reality2.Sentants do
  use DynamicSupervisor

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def start_child(init_arg) do
    # If MyWorker is not using the new child specs, we need to pass a map:
    # spec = %{id: MyWorker, start: {MyWorker, :start_link, [foo, bar, baz]}}
    spec = {Reality2.Sentant, value: init_arg}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init( strategy: :one_for_one, extra_arguments: [init_arg] )
  end

  def new_sentant(data) do
    # TODO: Weight the choice of process to supervisors with fewer processes already
    num_partitions = PartitionSupervisor.partitions(Reality2.Sentants) - 1
    DynamicSupervisor.start_child(
      {:via, PartitionSupervisor, {Reality2.Sentants, Enum.random(0..num_partitions)}},
      {Reality2.Sentant, data}
    )
  end

end
