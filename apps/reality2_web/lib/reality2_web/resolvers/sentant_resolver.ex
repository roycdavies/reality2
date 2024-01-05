defmodule Reality2Web.SentantResolver do
@moduledoc false

  def all_sentants(_, _, _) do
    {:ok, []}
  end

  def load_sentant(_root, args, _info) do
    # Create a new sentant
    case Map.get(args, :yaml_definition) do
      nil ->
        {:error, :yaml_definition}
      yaml_definition ->
        yaml_decoded = URI.decode(yaml_definition)
        IO.puts("yaml_decoded: #{inspect(yaml_decoded, pretty: true)}")
        case Reality2.Sentants.create(yaml_decoded) do
          {:ok, sentantid} ->
            case Reality2.Sentants.read(%{id: sentantid}, :definition) do
              {:ok, sentant} ->
                {:ok, convert_map_keys(sentant)}
              {:error, reason} ->
                {:error, reason}
            end
          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  def unload_sentant(_root, args, _info) do

    # Delete a sentant
    case Map.get(args, :id) do
      nil ->
        {:error, :id}
        sentantid ->
          case Reality2.Sentants.read(%{id: sentantid}, :definition) do
            {:ok, sentant} ->
              case Reality2.Sentants.delete(%{id: sentantid}) do
                {:ok, _} ->
                  {:ok, convert_map_keys(sentant)}
                {:error, reason} ->
                  {:error, reason}
              end
            {:error, reason} ->
              {:error, reason}
          end
    end
  end


  def load_swarm(_root, args, _info) do
    # Create a new swarm
    case Map.get(args, :yaml_definition) do
      nil ->
        {:error, :yaml_definition}
      yaml_definition ->
        yaml_decoded = URI.decode(yaml_definition)
        result = case Reality2.Swarm.create(yaml_decoded) do
          {:ok, sentant_names_and_ids} ->
            Enum.map(sentant_names_and_ids, fn {name, id} ->
              case Reality2.Sentants.read(%{id: id}, :definition) do
                {:ok, sentant} ->
                  convert_map_keys(sentant)
                {:error, reason} ->
                  {:error, reason}
              end
            end)
          {:error, reason} ->
            {:error, reason}
        end
        {:ok, result}
    end
  end


  def convert_map_keys(data) when is_map(data) do
    Enum.map(data, fn {k, v} ->
      cond do
        is_binary(k) -> {String.to_atom(k), convert_map_keys(v)}
        true -> {k, convert_map_keys(v)}
      end
    end)
    |> Map.new
  end
  def convert_map_keys(data) when is_list(data), do: Enum.map(data, fn x -> convert_map_keys(x) end)
  def convert_map_keys(data), do: data
end
