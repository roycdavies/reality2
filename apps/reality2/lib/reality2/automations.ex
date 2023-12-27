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
  def start_link({name, id, definition_map}) do
    DynamicSupervisor.start_link(__MODULE__, {name, id, definition_map}, name: String.to_atom(id <> "_automations"))
  end

  def start_child({name, id, definition_map}) do
    DynamicSupervisor.start_child(__MODULE__, {Reality2.Automation, {name, id, definition_map}})
  end

  @impl true
  def init(init_arg) do
    IO.puts("Automations.init: init_arg = #{inspect(init_arg)}")
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
    DynamicSupervisor.start_child(Reality2.Automations, {Reality2.Automation, {id, automation_map}})
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------


end
