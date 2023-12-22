defmodule Reality2.Automations do
# ********************************************************************************************************************************************
@moduledoc """
  Module for creating and managing Automations on a Sentant.

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)
"""
# ********************************************************************************************************************************************

  use DynamicSupervisor

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Supervisor Callbacks
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def start_link({name, id, definition_map}) do
    DynamicSupervisor.start_link(__MODULE__, {name, id, definition_map}, name: String.to_atom(id <> "_automations"))
  end

  def start_child({name, id, definition_map}) do
    spec = {Reality2.Automation, {name, id, definition_map}}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init(
      strategy: :one_for_one,
      extra_arguments: [init_arg]
    )
  end

  # @doc false
  # def child_spec(init_arg), do: {Reality2.Automation, value: init_arg}
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Automation Management Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec create(map()) ::
    {:ok, pid()}
    | {:ok, pid(), info :: term()}
    | {:error, :definition}
  @doc """
  Create a new Automation on the Sentant, returning {:ok, pid} or an appropriate error.

  **Parameters**
  - `definition` - A map containing the definition of the Automation.
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def create(definition_map) do
    definition_map
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------


end
