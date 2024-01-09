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
  def create(swarm_definition) do
    case convert_input(swarm_definition) do
      {:ok, definition_map} ->
        # IO.puts("Swarm.create: definition_map = #{inspect(definition_map, pretty: true)}")
        create_from_map(definition_map)
      _ ->
        {:error, :definition}
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp create_from_map(definition_map) do

    case Helpers.Map.get(definition_map, "swarm") do
      nil ->
        {:error, :definition}
      swarms_array ->
        name = Helpers.Map.get(swarms_array, "name", "")
        description = Helpers.Map.get(swarms_array, "description", "")
        case Helpers.Map.get(swarms_array, "sentants") do
          nil ->
            %{name: name, description: description, sentants: []}
          sentants ->
            case is_list(sentants) do
              true ->
                sentant_ids = Enum.map(sentants, fn sentant_map ->
                  # IO.puts("Swarm.create_from_map: sentant_map = #{inspect(sentant_map, pretty: true)}")
                  case Reality2.Sentants.create(sentant_map) do
                    {:ok, id} ->
                      id
                    {:error, reason} ->
                      {:error, reason}
                  end
                end)
                |> Enum.filter(fn x ->
                  case x do
                    {:error, _reason} -> false
                    _ -> true
                  end
                end)
                %{name: name, description: description, sentants: sentant_ids}
              false -> %{name: name, description: description, sentants: []}
            end
        end
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Convert an input String in either JSON, TOML or YAML format to a map.
  defp convert_input(definition) when is_map(definition), do: {:ok, definition}
  defp convert_input(definition) when is_binary(definition) do
    case Jason.decode(definition) do
      {:ok, definition_map} ->
        {:ok, definition_map}
      _ ->
        case YamlElixir.read_from_string(definition) do
          {:ok, definition_map} ->
            {:ok, definition_map}
          _ -> case Toml.decode(definition) do
            {:ok, definition_map} ->
              {:ok, definition_map}
            _ -> {:error, :definition}
          end
        end
    end
  end
  defp convert_input(_), do: {:error, :definition}
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
