defmodule Reality2.Automation do
# ********************************************************************************************************************************************
@moduledoc false
# The Automation on a Sentant, managed as a Finite State Machine.
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
  def start_link({_sentant_name, id, _sentant_map}, automation_map) do
    case Helpers.Map.get(automation_map, "name") do
      nil ->
        {:error, :definition}
      automation_name ->
        GenServer.start_link(__MODULE__, {automation_name, id, automation_map}, name: String.to_atom(id <> "|automation|" <> automation_name))
    end
  end

  @impl true
  def init({name, id, automation_map}) do
    {:ok, {name, id, automation_map, "start", :queue.new()}}
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Public Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Synchronous Calls
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @impl true
  def handle_call(:state, _from, {name, id, automation_map, state, event_queue}) do
    {:reply, {name, state}, {name, id, automation_map, state, event_queue}}
  end

  def handle_call(_, _, {name, id, automation_map, state, event_queue}) do
    {:reply, {:error, :unknown_command}, {name, id, automation_map, state, event_queue}}
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Asynchronous Casts
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @impl true
  def handle_cast(args, {name, id, automation_map, state, event_queue}) do
    parameters = Helpers.Map.get(args, :parameters, %{})
    IO.puts("Automation.handle_cast: #{inspect(parameters)}")
    passthrough = Helpers.Map.get(args, :passthrough, %{})

    case Helpers.Map.get(args, :event) do
      nil -> {:noreply, {name, id, automation_map, state, event_queue}}
      event ->
        case Helpers.Map.get(automation_map, "transitions") do
          nil ->
            {:noreply, {name, automation_map, state, event_queue}}
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

            {:noreply, {name, id, automation_map, {new_state, event_queue}}}
        end
    end
  end

  # Useful for sending events in the future using Process.send_after
  @impl true
  def handle_info({:send, name_or_id, details}, {name, id, automation_map, state, event_queue}) do
    Reality2.Sentants.sendto(name_or_id, details)
    {:noreply, {name, id, automation_map, state, event_queue}}
  end
  def handle_info({:tick}, ({name, id, automation_map, state, event_queue})) do
    # Pop next event from queue, if there is one
    # If there was an event, action it.
    # Send a tick event to self, to trigger the next event (using the timer store to ensure we don't get cascading ticks)
    {:noreply, {name, id, automation_map, state, event_queue}}
  end
  def handle_info(_, {name, id, automation_map, state}) do
    {:noreply, {name, id, automation_map, state, event_queue}}
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------


  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Helper Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Check the Transition Map to see if it matches the current state and event
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp check_transition(id, transition_map, event, parameters, passthrough, state) do
    case Helpers.Map.get(transition_map, "from") do
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
    sentant_name = Reality2.Metadata.get(:SentantNames, id)
    case Helpers.Map.get(transition_map, "event") do
      nil ->
        {:no_match, state}
      ^event ->
        case Helpers.Map.get(transition_map, "to") do
          nil ->
            {:no_match, state}
          "*" ->
            IO.puts("#{sentant_name}: #{inspect(state)} + #{inspect(event)} -> #{inspect(state)}")
            do_actions(id, transition_map, parameters, passthrough)
            {:ok, state}
          to ->
            IO.puts("#{sentant_name}: #{inspect(state)} + #{inspect(event)} -> #{inspect(to)}")
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
    case Helpers.Map.get(transition_map, "actions") do
      nil ->
        :ok
      actions ->
        Enum.reduce(actions, %{}, fn action_map, acc ->
          do_action(id, action_map, acc, parameters, passthrough)
        end)
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Do a single Action
  # An Action might be like:
  # %{  "command" => "send",
  #     "parameters" =>
  #         %{  "delay" => 1000,
  #             "event" => "turn_on",
  #             "to" => "Light Bulb"
  #         }
  # }
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp do_action(id, action_map, acc, parameters, passthrough) do
    action_parameters = case Helpers.Map.get(action_map, "parameters") do
      nil -> %{}
      params -> params
    end

    case Helpers.Map.get(action_map, "plugin") do
      nil ->
        do_inbuilt_action(Helpers.Map.get(action_map, "command"), id, action_map, action_parameters, acc, parameters, passthrough)
      plugin ->
        do_plugin_action(plugin, id, action_map, action_parameters, acc, parameters, passthrough)
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Do a Plugin Action
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp do_plugin_action(plugin, id, action_map, action_parameters, _acc, parameters, passthrough) do

    # The parameters for the plugin are the action parameters merged with the parameters passed to the Sentant
    joint_parameters = Map.merge(action_parameters, parameters)
    IO.puts("action_parameters: #{inspect(action_parameters)}")
    IO.puts("parameters: #{inspect(parameters)}")
    IO.puts("joint_parameters: #{inspect(joint_parameters)}")

    # When the sentant begins, there is a small possibiity that the plugin has not yet started.
    case test_and_wait(String.to_atom(id <> "|plugin|" <> plugin), 5) do
      nil ->
        {:error, :no_plugin}
      pid ->
        # Call the plugin on the Sentant, which in turn will call the appropriate internal App or extrnal plugin
        GenServer.call(pid, %{command: Helpers.Map.get(action_map, "command"), parameters: joint_parameters})
    end
  end

  defp test_and_wait(name, 0), do: nil
  defp test_and_wait(name, count) do
    case Process.whereis(name) do
      nil ->
        Process.sleep(100)
        test_and_wait(name, count - 1)
      pid -> pid
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Do an Inbuilt Action
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp do_inbuilt_action(action, id, action_map, action_parameters, acc, parameters, passthrough) do
    case action do
      "send" -> send(id, action_map, action_parameters, acc, parameters, passthrough)
      "print" -> print(id, action_map, action_parameters, acc, parameters, passthrough)
      _ ->
        IO.puts("Automation.do_inbuilt_action: unknown action")
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Private Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Send
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp send(id, _action_map, action_parameters, _acc, parameters, passthrough) do

    name_or_id = case Helpers.Map.get(action_parameters, "to") do
      nil ->
          %{id: id}     # No "to" field, so send to self.
      to_value ->
        case Reality2.Metadata.get(:SentantNames, to_value) do
          nil ->
            %{name: to_value}     # Not an Name for a Sentant on this Node, so send to the Sentant with that ID.
          to_id ->
            %{to_id: to_id}       # Must be a name.
        end
    end

    event = Helpers.Map.get(action_parameters, "event")

    # Make sure there is no timer for this event already in process.  If so, cancel it before doing the new one.
    case Reality2.Metadata.get(String.to_atom(id <> "|timers"), event) do
      nil -> :ok
      timer ->
        Process.cancel_timer(timer)
    end

    # Send the event either immediately or after a delay.
    case Helpers.Map.get(action_parameters, "delay") do
      nil ->
        Reality2.Sentants.sendto(name_or_id, %{event: event, parameters: parameters, passthrough: passthrough})
      delay ->
        timer = Process.send_after(self(), {:send, name_or_id, %{event: event, parameters: parameters, passthrough: passthrough}}, delay)
        Reality2.Metadata.set(String.to_atom(id <> "|timers"), event, timer)
    end

  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp print(id, _action_map, action_parameters, _acc, parameters, passthrough) do
    IO.puts("Received: #{inspect(parameters, pretty: true)} from #{inspect(id)}")
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
