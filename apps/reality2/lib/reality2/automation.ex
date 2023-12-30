defmodule Reality2.Automation do
# ********************************************************************************************************************************************
@moduledoc false
# The Automation on a Sentant, managed as a Finite State Machine.

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
  def start_link({_sentant_name, id, _sentant_map}, automation_map) do
    case Map.get(automation_map, "name") do
      nil ->
        {:error, :definition}
      automation_name ->
        GenServer.start_link(__MODULE__, {automation_name, id, automation_map}, name: String.to_atom(id <> "|automation|" <> automation_name))
    end
  end

  @impl true
  def init({name, id, automation_map}) do
    # IO.puts("Automation.init: args = #{inspect(automation_map)}")

    {:ok, {name, id, automation_map, "start"}}
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Public Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Synchronous Calls
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @impl true
  def handle_call(:state, _from, {name, id, automation_map, state}) do
    {:reply, {name, state}, {name, id, automation_map, state}}
  end
  def handle_call(%{plugin: plugin_name, command: command}, _from, {name, id, automation_map, state}) do
    IO.puts("Automation.handle_call: args = #{inspect(plugin_name)}, #{inspect(command)}")
    {:reply, :ok, {name, id, automation_map, state}}
  end
  def handle_call(_, _, {name, id, automation_map, state}) do
    {:reply, {:error, :unknown_command}, {name, id, automation_map, state}}
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Asynchronous Casts
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @impl true
  def handle_cast(args, {name, id, automation_map, state}) do
    parameters = Map.get(args, :parameters, %{})
    passthrough = Map.get(args, :passthrough, %{})

    case Map.get(args, :event) do
      nil -> {:noreply, {name, id, automation_map, state}}
      event ->
        case Map.get(automation_map, "transitions") do
          nil ->
            {:noreply, {name, automation_map, state}}
          transitions ->
            new_state = transitions
            |> Enum.reduce_while(state, fn transition_map, acc_state ->
              case check_transition(id, transition_map, event, parameters, passthrough, acc_state) do
                {:no_match, the_state} ->
                  {:cont, the_state}
                {:ok, the_state} ->
                  {:halt, the_state}
              end
            end)

            {:noreply, {name, id, automation_map, new_state}}
        end
    end
  end

  # Useful for sending events in the future using Process.send_after
  @impl true
  def handle_info(args, {name, id, automation_map, state}) do
    handle_cast(args, {name, id, automation_map, state})
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------


  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Helper Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Check the Transition Map to see if it matches the current state and event
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp check_transition(id, transition_map, event, parameters, passthrough, state) do
    # IO.puts("Automation.check_transition: args = #{inspect(transition_map)}, #{inspect(event)}, #{inspect(state)}")
    case Map.get(transition_map, "from") do
      nil ->
        {:no_match, state}
      "*" -> check_event(id, transition_map, event, parameters, passthrough, state)
      ^state -> check_event(id, transition_map, event, parameters, passthrough, state)
      _ -> {:no_match, state}
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Check the Event Map to see if it matches the current event, and do appropiate actions and state change if it does
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp check_event(id, transition_map, event, parameters, passthrough, state) do
    case Map.get(transition_map, "event") do
      nil ->
        {:no_match, state}
      ^event ->
        case Map.get(transition_map, "to") do
          nil ->
            {:no_match, state}
          "*" ->
            IO.puts("#{inspect(state)} + #{inspect(event)} -> #{inspect(state)}")
            do_actions(id, transition_map, parameters, passthrough)
            {:ok, state}
          to ->
            IO.puts("#{inspect(state)} + #{inspect(event)} -> #{inspect(to)}")
            do_actions(id, transition_map, parameters, passthrough)
            {:ok, to}
        end
      _ ->
        {:no_match, state}
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Do the Actions in the Transition Map when the Transition triggers
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp do_actions(id, transition_map, parameters, passthrough) do
    case Map.get(transition_map, "actions") do
      nil ->
        :ok
      actions ->
        Enum.reduce(actions, :ok, fn action_map, acc ->
          do_action(id, action_map, acc, parameters, passthrough)
        end)
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Do a single Action
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp do_action(id, action_map, acc, _parameters, _passthrough) do
    # IO.puts("Automation.do_action: action_map = #{inspect(action_map)}")
    action_parameters = case Map.get(action_map, "parameters") do
      nil -> %{}
      the_parameters -> the_parameters
    end

    case Map.get(action_map, "plugin") do
      nil ->
        do_inbuilt_action(Map.get(action_map, "command"), id, action_map, action_parameters, acc)
      plugin ->
        do_plugin_action(plugin, id, action_map, action_parameters, acc)
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Do a Plugin Action
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp do_plugin_action(plugin, id, action_map, action_parameters, _acc) do
    case Process.whereis(String.to_atom(id <> "|plugin|" <> plugin)) do
      nil ->
        {:error, :no_plugin}
      pid ->
        # Call the plugin on the Sentant, which in turn will call the appropriate internal App
        GenServer.call(pid, %{command: Map.get(action_map, "command"), parameters: action_parameters})
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Do an Inbuilt Action
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp do_inbuilt_action(action, _id, _action_map, _action_parameters, _acc) do
    IO.puts("Automation.do_inbuilt_action: action = #{inspect(action)}")
    case action do
      "send" ->
        IO.puts("Automation.do_inbuilt_action: send not implemented yet")
      _ ->
        IO.puts("Automation.do_inbuilt_action: unknown action")
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
