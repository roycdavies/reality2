defmodule Reality2.Swarm do
# *******************************************************************************************************************************************
@moduledoc false
# Module for creating and managing Swarms on a Node.  A Swarm is a collection of Sentants that are managed together.
#
# **Author**
# - Dr. Roy C. Davies
# - [roycdavies.github.io](https://roycdavies.github.io/)
# *******************************************************************************************************************************************

  alias Reality2.Types

  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec create(Types.swarm() | String.t()) ::
    {:ok}
    | {:error, :definition}
  @doc """
  Create a new Swarm on the Node, returning {:ok} or an appropriate error.

  - Parameters
    - `swarm_map` - A map containing the definition of the Swarm.
  """
  def create(swarm_map) when is_map(swarm_map), do: create_from_map(swarm_map)
  def create(definition) when is_binary(definition) do
    definition
    |> YamlElixir.read_from_string()
    |> case do
      {:ok, definition_map} ->
        create_from_map(definition_map)
      _ ->
        {:error, :definition}
    end
  end
  def create(_), do: {:error, :definition}

  defp create_from_map(definition_map) do
    IO.puts("Swarm.create_from_map(#{inspect(definition_map)})")
    case Map.get(definition_map, "swarm") do
      nil ->
        {:error, :definition}
      swarms_array ->
        case Map.get(swarms_array, "sentants") do
          nil ->
            {:error, :definition}
          sentants ->
            Enum.map(sentants, fn sentant_map ->
              case Reality2.Sentants.create(sentant_map) do
                {:ok, id} ->
                  {Map.get(sentant_map, "name", ""), id}
                {:error, reason} ->
                  {:error, reason}
              end
            end)
        end
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
