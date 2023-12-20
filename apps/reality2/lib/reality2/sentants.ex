defmodule Reality2.Sentants do
# ********************************************************************************************************************************************
@moduledoc """
  Module for creating and managing Sentants, and the DynamicSupervisor that manages them.

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)
"""
# ********************************************************************************************************************************************

  use DynamicSupervisor

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Supervisor Callbacks
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @doc false
  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc false
  def start_child(init_arg) do
    spec = {Reality2.Sentant, value: init_arg}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init( strategy: :one_for_one, extra_arguments: [init_arg] )
  end

  @doc false
  def child_spec(arg) do
    Supervisor.child_spec({__MODULE__, arg}, id: __MODULE__)
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Sentant Management Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec new_sentant(String.t()) ::
    {:ok, pid()}
    | {:ok, pid(), info :: term()}
    | :ignore
    | {:error, {:already_started, pid()} | :max_children | term()}
    | {:error, :invalid_definition}
  @doc """
  Create a new Sentant and return the result of the operation with the pid of the new Sentant, or an appropriate error.

  **Parameters**
  - `definition` - A string containing the definition of the Sentant to be created in YAML format.
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def new_sentant(definition) do
    definition
    |> YamlElixir.read_from_string()
    |> case do
      {:ok, definition_map} ->
        case sentant_name(definition_map) do
          {:ok, name} ->
            DynamicSupervisor.start_child(
              {:via, PartitionSupervisor, {Reality2.Sentants, choose_supervisor()}},
              {Reality2.Sentant, {name, definition_map}}
            )
          _ ->
            {:error, :invalid_definition}
        end
      _ ->
        {:error, :invalid_definition}
    end
  end

  defp sentant_name(%{"sentant" => %{"name" => name}}), do: {:ok, name}
  defp sentant_name(_), do: {:error, :invalid_definition}
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec delete_sentant(String.t()) ::
    {:ok, pid()}
    | {:ok, pid(), info :: term()}
    | :ignore
    | {:error, {:already_started, pid()} | :max_children | term()}
    | {:error, :invalid_definition}
  @doc """
  Delete a Sentant and return the result of the operation with the pid of the deleted Sentant, or an appropriate error.

  **Parameters**
  - `name` - The name of the Sentant to be deleted.
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def delete_sentant(name) do
    case Process.whereis(String.to_atom(name)) do
      nil ->
        {:error, :invalid_definition}
      pid ->
        Supervisor.stop(pid, :shutdown) # Each Sentant is a Supervisor, so this will stop all child processes as well.
      end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Helper Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Returns a random Supervisor index weighted by the number of child processes on all Supervisors.
  defp choose_supervisor() do
    Reality2.Sentants
    |> PartitionSupervisor.which_children
    |> count_processes
    |> choose_minimum
  end

  # Returns a list of tuples containing the Supervisor index and the number of child processes on that Supervisor.
  defp count_processes([]), do: []
  defp count_processes([{id, pid, _, _} | tail]) do
    %{active: num_children} = DynamicSupervisor.count_children(pid)
    [{id, num_children} | count_processes(tail)]
  end

  # Returns the index of the Supervisor with the fewest child processes.
  defp choose_minimum([]), do: 0
  defp choose_minimum(list), do: Enum.min_by(list, &elem(&1, 1)) |> elem(0)
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
