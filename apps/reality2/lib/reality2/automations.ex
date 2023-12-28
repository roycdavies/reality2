defmodule Reality2.Automations do
# ********************************************************************************************************************************************
@moduledoc """
  Module for creating and managing Automations on a Sentant.

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)
"""
# ********************************************************************************************************************************************

  @doc false
  use DynamicSupervisor
  alias YAML.Sentant_types

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Supervisor Callbacks
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def start_link({_name, id, _automation_map} = init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: String.to_atom(id <> "|automations"))
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one, extra_arguments: [init_arg])
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Automation Management Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec create(Sentant_types.uuid(), Sentant_types.automation()) ::
    {:ok}
    | {:error, :definition}
  @doc """
  Create a new Automation on the Sentant, returning {:ok} or an appropriate error.

  **Parameters**
  - `definition` - A map containing the definition of the Automation.
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def create(id, automation_map) do
    case Process.whereis(String.to_atom(id <> "|automations")) do
      nil->
        {:error, :existence}
      pid ->
        case DynamicSupervisor.start_child(pid, Reality2.Automation.child_spec(automation_map)) do
          {:ok, _pid} ->
            Reality2.Sentants.sendto(%{:id => id}, %{event: "init", parameters: %{}, passthrough: %{}})
            {:ok, :child_started}
          {:error, reason} ->
            {:error, reason}
        end
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------

end
