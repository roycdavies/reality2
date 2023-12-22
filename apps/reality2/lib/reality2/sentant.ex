defmodule Reality2.Sentant do
@moduledoc false

use Supervisor, restart: :transient

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Supervisor Callbacks
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @doc false
  def start_link({name, id, definition_map}) do
    Supervisor.start_link(__MODULE__, {name, id, definition_map}, name: String.to_atom(id))
  end

  @impl true
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
