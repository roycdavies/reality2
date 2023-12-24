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
    spec = child_spec(init_arg)
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init( strategy: :one_for_one, extra_arguments: [init_arg] )
  end

  @doc false
  def child_spec(data) do
    %{
      id: Reality2.Sentants,
      start: {Reality2.Sentants, :start_link, [data]},
      type: :supervisor
    }
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Types
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @typedoc """
  Each Sentant has a unique UUID.  This is checked to ensure that the Sentant is unique in the world, and it's running status is set accordingly to
  either `:main` or `:shadow`.  If the UUID is not checked (for example, if the node is offline), then it is set to `:unchecked`.
  """
  @type uuid :: String.t()

  @typedoc """
  Each Sentant can be referred to be either its name or its ID.  The name of a Sentant is unique on the node, but not necessarily in the world.
  """
  @type sentant_name_or_id :: %{:id => uuid()} | %{:name => String.t()}

  @typedoc """
  The definition of a Sentant is a string containing the definition of the Sentant in YAML format.  See the definition of `Reality2.Sentant.YAML`.
  """
  @type sentant_definition :: String.t()
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Sentant Management Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec create(definition :: sentant_definition()) ::
    {:ok, String.t()}
    | {:error, :definition}
  @doc """
  Create a new Sentant and return the result of the operation with the pid of the new Sentant, or an appropriate error.

  **Parameters**
  - `definition` - A string containing the definition of the Sentant to be created in YAML format.

  **Returns**
  - `{:ok, id}` - The Sentant was created.
  - `{:error, :definition}` if the definition is invalid.
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def create(definition) do
    definition
    |> YamlElixir.read_from_string()
    |> case do
      {:ok, definition_map} ->
        case sentant_name(definition_map) do
          {:ok, name} ->
            case sentant_id(definition_map) do
              {:ok, _, id} ->
                case DynamicSupervisor.start_child(
                  {:via, PartitionSupervisor, {Reality2.Sentants, choose_supervisor()}},
                  {Reality2.Sentant, {name, id, definition_map}}
                ) do
                    {:ok, _pid} ->
                      Reality2.Metadata.set :SentantNames, id, name
                      Reality2.Metadata.set :SentantIDs, name, id
                      {:ok, id}
                    error -> error
                end
              error -> error
            end
          error -> error
        end
      _ ->
        {:error, :definition}
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec read(name_or_id :: sentant_name_or_id()) ::
    {:ok, map()}
    | {:error, :existance}
    | {:error, :invalid}
  @doc """
  TODO: Read the definition and status of an existing Sentant.

  **Parameters**
  - `name_or_id` - The name or ID of the Sentant to be read from as a map containing either a `:name` or `:id` key.

  **Returns**
  - `{:ok, definition}` - The Sentant was read.
  - `{:error, :existance}` if the Sentant with that ID or name does not exist on this node.
  - `{:error, :invalid}` if the parameter is invalid.
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def read(%{:name => name}) do
    case Reality2.Metadata.get :SentantIDs, name do
      nil ->
        {:error, :existance}
      id ->
        read(%{:id => id})
    end
  end

  def read(%{:id => id}) do
    case Process.whereis(String.to_atom(id)) do
      nil ->
        {:error, :existance}
      _pid ->
        #TODO: Read the definition and status of the Sentant.
        {:ok, %{}}
      end
  end

  def read(_), do: {:error, :invalid}
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec update(definition :: sentant_definition()) ::
    {:ok, String.t()}
    | {:error, :definition}
  @doc """
  TODO: Update an existing Sentant.  If the Sentant does not exist, create it.

  **Parameters**
  - `definition` - A string containing the definition of the Sentant to be updated in YAML format (including its name and ID).

  **Returns**
  - `{:ok, id}` - The Sentant was updated.
  - `{:error, :definition}` if the definition is invalid.
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def update(definition) do
    definition
    |> YamlElixir.read_from_string()
    |> case do
      {:ok, definition_map} ->
        case sentant_id(definition_map) do
          {:ok, _, id} ->
            case Process.whereis(String.to_atom(id)) do
              nil ->
                create(definition)
              _pid ->
                #TODO: DO UPDATE HERE
                {:ok, id}
              end
          error -> error
        end
      _ ->
        {:error, :definition}
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec delete(name_or_id :: sentant_name_or_id()) ::
    {:ok, String.t()}
    | {:error, :existance}
    | {:error, :invalid}
  @doc """
  Delete a Sentant and return the result of the operation with the pid of the deleted Sentant, or an appropriate error.

  **Parameters**
  - `name_or_id` - The name or ID of the Sentant to be deleted as a map containing either a `:name` or `:id` key.

  **Returns**
  - `{:ok, id}` - The Sentant was deleted.
  - `{:error, :existance}` if the Sentant with that ID or name does not exist on this node.
  - `{:error, :invalid}` if the parameter is invalid.  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def delete(%{:name => name}) do
    case Reality2.Metadata.get(:SentantIDs, name) do
      nil ->
        {:error, :existance}
      id ->
        delete(%{:id => id})
    end
  end

  def delete(%{:id => id}) do
    case Process.whereis(String.to_atom(id)) do
      nil ->
        {:error, :existance}
      pid ->
        case Supervisor.stop(pid, :shutdown) do
          :ok ->
            case Reality2.Metadata.get(:SentantNames, id) do
              nil ->
                {:error, :existance}
              name ->
                Reality2.Metadata.delete(:SentantNames, id)
                Reality2.Metadata.delete(:SentantIDs, name)
            end
            {:ok, id}
        end
      end
  end

  def delete(_), do: {:error, :invalid}
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec sendto(name_or_id :: sentant_name_or_id(), message :: map()) ::
    {:ok}
    | {:error, :name}
    | {:error, :existance}
    | {:error, :invalid}
  @doc """
  Send a message to the named Sentant if it exists and return the result of the operation, or an appropriate error. This is an asynchronous operation.

  **Parameters**
  - `name_or_id` - The name or ID of the Sentant to have the message sent to as a map containing either a `:name` or `:id` key.
  - `message` - The message to be sent, which must contain a `:command` key and optionally a `:parameters` key, and optionaslly a ':passthrough' key.

  **Returns**
  - `{:ok}` - The message was sent.
  - `{:error, :existance}` if the Sentant with that ID or name does not exist on this node.
  - `{:error, :invalid}` if the first parameter is invalid.
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def sendto(%{:name => name}, message_map) do
    case Reality2.Metadata.get(:SentantIDs, name) do
      nil ->
        {:error, :name}
      id ->
        sendto(%{:id => id}, message_map)
    end
  end

  def sendto(%{:id => id}, message_map) do
    case Process.whereis(String.to_atom(id <> "_comms")) do
      nil ->
        {:error, :id}
      pid ->
        GenServer.cast(pid, message_map)
      end
  end

  def sendto(_, _), do: {:error, :existance}
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec sendto_all(message :: map()) ::
    {:ok, integer()}
  @doc """
  Send a message to all Sentants.  This is an asynchronous operation, so the result is always `{:ok}`.

  **Parameters**
  - `message` - The message to be sent, which must contain a `:command` key and optionally a `:parameters` key, and optionaslly a ':passthrough' key.

  **Returns**
  - `{:ok, num_sentants}` - The number of Sentants that the message was sent to.
  """
  def sendto_all(message_map) do
    sentants = get_all_sentant_comms()
    Enum.each(sentants, fn pid -> GenServer.cast(pid, message_map) end)
    {:ok, length(sentants)}
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


  # Get the ID of the Sentant from the definition map.
  defp sentant_id(%{"sentant" => %{"id" => id}}) do
    # TODO: Check that the Sentant identified by the ID is unique in the world and if not, detemine which should be the main and which shadows.
    # Should return either that the sentant is the main one, a shadow one, or that it is unchecked.
    case UUID.info(id) do
      {:ok, _} ->
        {:ok, :unchecked, id}
      _ ->
        {:error, :id}
    end
  end
  defp sentant_id(_), do: {:ok, UUID.uuid1} # No ID given, so assume a new Sentant is to be created with a new ID.


  # Get the name of the Sentant from the definition map.
  defp sentant_name(%{"sentant" => %{"name" => name}}), do: {:ok, name}
  defp sentant_name(_), do: {:error, :definition}


  # Returns a list of all the children of the PartitionSupervisor's children.
  defp get_all_sentant_comms do
    Reality2.Metadata.all(:SentantIDs)
    |> Enum.map(fn {_, id} -> Process.whereis(String.to_atom(id <> "_comms")) end)
    |> Enum.filter(&(&1 != nil))
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
