defmodule AiReality2Vars do
  @moduledoc """
  The Reality2 Node Plugin `ai.reality2.vars` as it is written in the Sentant description YAML files.

  Naming restrictions require that underscores are used in the App filenames, so in this case `ai_reality2_vars`.

  To create a Plugin App such as this for a Reality2 Node, follow the general instructions for creating a new App under an umbrella project.
  Make sure the app created is a supervised app, ie created using the `--sup` option.

  ```bash
  mix new apps/ai_reality2_vars --sup
  ```

  The app name should match the plugin name, ie `ai_reality2_vars` for the plugin `ai.reality2.vars`, which is then called AiReality2Vars as the App Alias used inside the code.

  By convention, we are using a reverse domain name hence: `ai.reality2.vars`.

  The plugin app must have `create`, `delete`, `whereis` and `sendto` functions in the `lib/<app_name>/main.ex` file, which are called to create and delete instances of the app for each Sentant,
  return a process ID for communication, and send a command and parameters to the App for the given Sentant.
  See the `AiReality2Vars.Main` module for more details.
  """

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
