defmodule Helpers do

  defmodule Map do

    def get(map, key, default \\ nil)

    def get(map, key, default) when is_binary(key) do
      case Elixir.Map.get(map, key) do
        nil ->
          case Elixir.Map.get(map, String.to_atom(key)) do
            nil -> default
            value -> value
          end
        value -> value
      end
    end

    def get(map, key, default) when is_atom(key) do
      case Elixir.Map.get(map, key) do
        nil ->
          case Elixir.Map.get(map, Atom.to_string(key)) do
            nil -> default
            value -> value
          end
        value -> value
      end
    end

    def get(map, key, default), do: Elixir.Map.get(map, key, default)
  end


  defmodule Json do
    def get_value(data, path) do
      get_values(data, String.split(path, "."))
    end

    defp get_values(data, []) do
      {:ok, data}
    end
    defp get_values(data, [key | tail]) when is_map(data) do
      case Helpers.Map.get(data, key) do
        nil -> {:error, :not_found}
        value -> get_values(value, tail)
      end
    end
    defp get_values(data, [key | tail]) when is_list(data) do
      try do
        String.to_integer(key)
      rescue
        _ -> {:error, :not_found}
      else
        index -> case Enum.at(data, index) do
          nil -> {:error, :not_found}
          value -> get_values(value, tail)
        end
      end
    end
  end
end
