defmodule Reality2.Plugin do
  # ********************************************************************************************************************************************
  @moduledoc false
  # The Plugin on a Sentant.
  #
  # **Author**
  # - Dr. Roy C. Davies
  # - [roycdavies.github.io](https://roycdavies.github.io/)
  # ********************************************************************************************************************************************

    @doc false
    use GenServer, restart: :transient

    # -----------------------------------------------------------------------------------------------------------------------------------------
    # Supervisor Callbacks
    # -----------------------------------------------------------------------------------------------------------------------------------------
    @doc false
    def start_link({_sentant_name, id, _sentant_map}, plugin_map) do
      case Map.get(plugin_map, "name") do
        nil ->
          {:error, :definition}
        plugin_name ->
          GenServer.start_link(__MODULE__, {plugin_name, id, plugin_map}, name: String.to_atom(id <> "|plugin|" <> plugin_name))
      end
    end

    @impl true
    def init({name, id, plugin_map}) do
      # if the plugin is internal, then start it
      # IO.puts("Plugin.init: args = #{inspect(plugin_map)}")
      # IO.puts("Plugin.init: name = #{inspect(name)}")
      # IO.puts("Plugin.init: id = #{inspect(id)}")

      app_name_atom = name
      |> String.split(".")
      |> Enum.join("_")
      |> String.downcase
      |> String.to_atom

      # IO.puts("Plugin.init: app_name_atom = #{inspect(app_name_atom)}")

      case Enum.any?(Application.loaded_applications(), fn({app_name, _, _}) -> app_name == app_name_atom end) do
        true ->
          # Run the 'create' function in the App referenced to by the plugin name, in the module named Main
          name
          |> String.split(".")
          |> Enum.map(&String.capitalize/1)
          |> Enum.join("")
          |> Module.concat(String.to_atom("Main"))
          |> apply(:create, [id <> "|" <> name])
        false ->
          {:error, :not_found}
      end

      {:ok, {name, id, plugin_map, %{}}}
    end
    # -----------------------------------------------------------------------------------------------------------------------------------------



    # -----------------------------------------------------------------------------------------------------------------------------------------
    # Public Functions
    # -----------------------------------------------------------------------------------------------------------------------------------------

    # -----------------------------------------------------------------------------------------------------------------------------------------
    # Synchronous Calls
    # -----------------------------------------------------------------------------------------------------------------------------------------
    @impl true
    # def handle_call(%{plugin: "ai.reality2.vars", command: command}, _from, {name, id, plugin_map, state}) do
    #   # If plugin is internal, then find the App and sent the call there
    #   answer = GenServer.call(String.to_atom(id <> "|data"), command)
    #   {:reply, answer, {name, id, plugin_map, state}}
    # end
    def handle_call(command, _from, {name, id, plugin_map, state}) do
      # IO.puts("Plugin.handle_call: args = #{inspect(command)} #{inspect(name)}, #{inspect(id)}, #{inspect(plugin_map)}, #{inspect(state)}")

      case Map.get(plugin_map, "type") do
        "internal" ->
          # Internal Plugin
          case Process.whereis(String.to_atom(id <> "|" <> name)) do
            nil ->
              {:reply, {:error, :existence}, {name, id, plugin_map, state}}
            pid ->
              answer = GenServer.call(pid, command)
              {:reply, {:ok, answer}, {name, id, plugin_map, state}}
          end
        _ ->
          # External Plugin
          {:reply, {:error, :external_not_implemented}, {name, id, plugin_map, state}}
      end
    end
    def handle_call(_, _, state) do
      {:reply, {:error, :unknown_command}, state}
    end
    # -----------------------------------------------------------------------------------------------------------------------------------------



    # -----------------------------------------------------------------------------------------------------------------------------------------
    # Asynchronous Casts
    # -----------------------------------------------------------------------------------------------------------------------------------------
    @impl true
    def handle_cast(command, {name, id, plugin_map, state}) do
      case Map.get(plugin_map, "type") do
        "internal" ->
          # Internal Plugin
          case Process.whereis(String.to_atom(id <> "|" <> name)) do
            nil -> :ok
            pid -> GenServer.cast(pid, command)
          end
        _ ->
          # External Plugin
          :ok
      end
      {:noreply, {name, id, plugin_map, state}}
    end
    def handle_cast(_, state), do: {:noreply, state}

    # Useful for sending events in the future using Process.send_after
    @impl true
    def handle_info(command, {name, id, plugin_map, state}) do
      handle_cast(command, {name, id, plugin_map, state})
    end
    def handle_info(_, state), do: {:noreply, state}
    # -----------------------------------------------------------------------------------------------------------------------------------------


    # -----------------------------------------------------------------------------------------------------------------------------------------
    # Helper Functions
    # -----------------------------------------------------------------------------------------------------------------------------------------

    # -----------------------------------------------------------------------------------------------------------------------------------------
  end
