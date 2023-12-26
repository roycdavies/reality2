defmodule Reality2.Sentant do
@moduledoc false

use Supervisor, restart: :transient
alias YAML.Sentant_types

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Supervisor Callbacks
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @doc false
  @spec start_link({String.t(), Sentant_types.uuid(), Sentant_types.sentant()})
  def start_link({name, id, definition_map}) do
    Supervisor.start_link(__MODULE__, {name, id, definition_map}, name: String.to_atom(id))
  end

  @impl true
  @spec init({String.t(), Sentant_types.uuid(), Sentant_types.sentant()})
  def init({name, id, definition_map}) do
    children = [
      {Reality2.Automations, {name, id, definition_map}},
      {Reality2.Plugins, {name, id, definition_map}},
      {Reality2.Sentant.Comms, {name, id, definition_map}},
      %{id: String.to_atom((id <> "_vars")), start: {Reality2.Metadata, :start_link, [String.to_atom((id <> "_vars"))]}},
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Sentant Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Helper Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
end
