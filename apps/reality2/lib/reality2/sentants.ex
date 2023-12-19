# *********************************************************************************************************************
# Module for creating and managing Sentants.
# *********************************************************************************************************************
defmodule Reality2.Sentants do
  use DynamicSupervisor

  # -------------------------------------------------------------------------------------------------------------------
  # Supervisor Callbacks
  # -------------------------------------------------------------------------------------------------------------------
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
  # -------------------------------------------------------------------------------------------------------------------



  # -------------------------------------------------------------------------------------------------------------------
  # Sentant Management Functions
  # -------------------------------------------------------------------------------------------------------------------
  def new_sentant(data) do
    # Get the largest number of child processes on all Supervisors (m),
    # then create an array with the inverse number of elements from each Supervisor (m-n) where n = number of child processes on the Supervisor.
    # Then, choose a random element from the array and start the child process on that Supervisor.
    # This will result in a roughly even distribution of child processes across all Supervisors.
    DynamicSupervisor.start_child(
      {:via, PartitionSupervisor, {Reality2.Sentants, choose_supervisor()}},
      {Reality2.Sentant, data}
    )
  end

  def create_many_sentants(name, num) do
    Enum.each(1..num, fn x ->
      new_sentant({name <> Integer.to_string(x), %{}})
    end)
    result = count_processes(PartitionSupervisor.which_children(Reality2.Sentants))
    IO.puts("Max children: #{inspect(result)}")
  end
  # -------------------------------------------------------------------------------------------------------------------



  # -------------------------------------------------------------------------------------------------------------------
  # Helper Functions
  # -------------------------------------------------------------------------------------------------------------------
  # Returns a random Supervisor index weighted by the number of child processes on all Supervisors.
  def choose_supervisor() do
    Reality2.Sentants
    |> PartitionSupervisor.which_children
    |> count_processes
    |> choose_minimum
  end

  def count_processes([]), do: []
  def count_processes([{id, pid, _, _} | tail]) do
    %{active: num_children} = DynamicSupervisor.count_children(pid)
    [{id, num_children} | count_processes(tail)]
  end

  defp choose_minimum([]), do: 0
  defp choose_minimum(list), do: Enum.min_by(list, &elem(&1, 1)) |> elem(0)
  # -------------------------------------------------------------------------------------------------------------------
end
