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
end
