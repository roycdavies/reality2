defmodule Reality2.Swarm do
# *******************************************************************************************************************************************
@moduledoc """
  Module for creating and managing Swarms on a Node.  A Swarm is a collection of Sentants that are managed together.

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)
"""
# *******************************************************************************************************************************************

  alias Reality2.Types

  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec create(Types.swarm() | String.t()) ::
    {:ok}
    | {:error, :definition}
  @doc """
  Create a new Swarm on the Node, returning {:ok} or an appropriate error.

  Uploading a Swarm creates the Sentants on the Node, and then sends the init event to each Sentant.
  If Sentants with the same name or ID already exist on the Node, then they are not created again, but redefined and restarted.

  - Parameters
    - `swarm_definition` - A map containing the definition of the Swarm, or a string containing the YAML definition of the Swarm.
  """
  def create(swarm_definition)
  def create(swarm_map) when is_map(swarm_map), do: create_from_map(swarm_map)
  def create(definition) when is_binary(definition) do
    definition
    |> YamlElixir.read_from_string()
    |> case do
      {:ok, definition_map} ->
        %{sentants: create_from_map(definition_map), name: Helpers.Map.get(definition_map, "name", ""), description: Helpers.Map.get(definition_map, "description", "")}
      _ ->
        {:error, :definition}
    end
  end
  def create(_), do: {:error, :definition}

  defp create_from_map(definition_map) do

    case Helpers.Map.get(definition_map, "swarm") do
      nil ->
        {:error, :definition}
      swarms_array ->
        case Helpers.Map.get(swarms_array, "sentants") do
          nil ->
            {:error, :definition}
          sentants ->
            Enum.map(sentants, fn sentant_map ->
              case Reality2.Sentants.create(sentant_map) do
                {:ok, id} ->
                  {Helpers.Map.get(sentant_map, "name", ""), id}
                {:error, reason} ->
                  {:error, reason}
              end
            end)
        end
    end

  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
