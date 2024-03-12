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

  def handle_call(_, _, {name, id, automation_map, state}) do
    {:reply, {:error, :unknown_command}, {name, id, automation_map, state}}
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Asynchronous Casts
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @impl true
  def handle_cast(args, {name, id, automation_map, state}) do
    # IO.puts("Automation.handle_cast: #{inspect(name)} : #{inspect(state)} : #{inspect(args)}")
    parameters = Helpers.Map.get(args, :parameters, %{})
    passthrough = Helpers.Map.get(args, :passthrough, %{})

    IO.puts("Parameters  = #{inspect(parameters)}")
    IO.puts("Passthrough = #{inspect(passthrough)}")

    case Helpers.Map.get(args, :event) do
      nil -> {:noreply, {name, id, automation_map, state}}
      event ->
        case Helpers.Map.get(automation_map, "transitions") do
          nil ->
            {:noreply, {name, id, automation_map, state}}
          transitions ->
            new_state =
              Enum.reduce_while(transitions, state,
                fn transition_map, acc_state ->
                  case check_transition(id, transition_map, event, parameters, passthrough, acc_state) do
                    {:no_match, the_state} ->
                      {:cont, the_state}
                    {:ok, the_state} ->
                      {:halt, the_state}
                  end
                end
              )
            {:noreply, {name, id, automation_map, new_state}}
        end
    end
  end

  # Used for sending events in the future using Process.send_after
  @impl true
  def handle_info({:send, name_or_id, details}, {name, id, automation_map, state}) do
    Reality2.Sentants.sendto(name_or_id, details)
    {:noreply, {name, id, automation_map, state}}
  end
  def handle_info(_, {name, id, automation_map, state}) do
    {:noreply, {name, id, automation_map, state}}
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
        %{}
      actions ->
        Enum.reduce(actions, %{}, fn (action_map, acc) ->
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
    action_parameters = Helpers.Map.get(action_map, "parameters", %{})

    result = case Helpers.Map.get(action_map, "plugin") do
      nil ->
        do_inbuilt_action(Helpers.Map.get(action_map, "command"), id, action_map, action_parameters, acc, parameters, passthrough)
      plugin ->
        do_plugin_action(plugin, id, action_map, action_parameters, acc, parameters, passthrough)
    end

    # Result for accumulation
    Map.merge(acc, result)
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Do a Plugin Action
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp do_plugin_action(plugin, id, action_map, action_parameters, acc, parameters, passthrough) do

    # IO.puts("Automation.do_plugin_action: plugin = #{inspect(plugin)}")
    # IO.puts("Automation.do_plugin_action: id = #{inspect(id)}")
    # IO.puts("Automation.do_plugin_action: action_map = #{inspect(action_map)}")
    # IO.puts("Automation.do_plugin_action: action_parameters = #{inspect(action_parameters)}")

    # The parameters for the plugin are the accumulated parameters, merged with the action parameters merged with the parameters passed to the Sentant
    joint_parameters = Map.merge(Map.merge(action_parameters, parameters), acc)

    # When the sentant begins, there is a small possibiity that the plugin has not yet started.
    case test_and_wait(String.to_atom(id <> "|plugin|" <> plugin), 5) do
      nil ->
        %{}
      pid ->
        # Call the plugin on the Sentant, which in turn will call the appropriate internal App or external plugin
        case GenServer.call(pid, %{command: Helpers.Map.get(action_map, "command"), parameters: joint_parameters, passthrough: passthrough}) do
          {:ok, result} ->
            # IO.puts("Automation.do_plugin_action: command = #{inspect(Helpers.Map.get(action_map, "command"))} result = #{inspect(result)}")
            result
          {:error, _reason} ->
            # IO.puts("Automation.do_plugin_action: error = #{inspect(reason)}")
            %{}
        end
    end
  end

  defp test_and_wait(_, 0), do: nil
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

    # The parameters for the plugin are the accumulated parameters, merged with the action parameters merged with the parameters passed to the Sentant
    joint_parameters = Map.merge(Map.merge(action_parameters, parameters), acc)
    acc_and_parameters = Map.merge(parameters, acc)

    case action do
      "send" -> send(id, action_map, action_parameters, joint_parameters, passthrough)
      "print" -> print(id, action_map, action_parameters, joint_parameters, passthrough)
      "set" -> set(id, action_map, action_parameters, joint_parameters, passthrough)
      "signal" -> signal(id, action_map, action_parameters, acc_and_parameters, passthrough)
      _ -> %{}
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Private Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Send
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp send(id, _action_map, action_parameters, parameters, passthrough) do

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

    %{}
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Send a signal on the Sentant's subscription channel
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp signal(id, _action_map, action_parameters, parameters, passthrough) do
    IO.puts("Action: signal #{inspect(id)} : #{inspect(action_parameters)} #{inspect(parameters)}")

    # Send off a signal to any listening device
    case Helpers.Map.get(action_parameters, "event") do
      nil -> %{}
      event ->
        case Process.whereis(String.to_atom(id <> "|comms")) do
          nil ->
            %{}
          pid ->
            sentant_map = convert_key_strings_to_atoms(GenServer.call(pid, :definition))
            subscription_data = %{
              sentant: sentant_map,
              event: event,
              parameters: parameters,
              passthrough: passthrough
            }
            IO.puts("subscription_data: " <> inspect(subscription_data, pretty: true))
            Absinthe.Subscription.publish(Reality2Web.Endpoint, subscription_data, await_signal: id <> "|" <> event)
            %{}
        end
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Print to console
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp print(id, _action_map, _action_parameters, parameters, _passthrough) do
    name = Reality2.Metadata.get(:SentantNames, id)
    IO.puts("Action: print #{inspect(name)} : #{inspect(parameters, pretty: true)}")
    %{}
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Set a temporary value
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp set(_id, _action_map, action_parameters, parameters, _passthrough) do
    key = Helpers.Map.get(action_parameters, "key")
    value = replace_variable_in_map(Helpers.Map.get(action_parameters, "value"), parameters)
    %{key => value}
  end

  defp replace_variable_in_map(data, variables) when is_map(data) do
    Enum.map(data, fn {k, v} ->
      cond do
        is_binary(v) -> {k, replace_variables(v, variables)}
        true -> {k, replace_variable_in_map(v, variables)}
      end
    end)
    |> Map.new
  end
  defp replace_variable_in_map(data, variables) when is_list(data), do: Enum.map(data, fn x -> replace_variable_in_map(x, variables) end)
  defp replace_variable_in_map(data, variables) when is_binary(data), do: to_number(replace_variables(data, variables))
  defp replace_variable_in_map(data, _), do: data

  defp replace_variables(data, variable_map) do
    pattern = ~r/__(.+?)__/  # Matches variables enclosed in double underscores

    Regex.replace(pattern, data, fn match ->
      variable_name = String.trim(match, "_")
      to_string(Helpers.Map.get(variable_map, variable_name, "UNKNOWN_VARIABLE"))
    end)
  end

  def to_number(value) when is_binary(value) do
    case Integer.parse(value) do
      {number, _} -> number
      _ ->
        case Float.parse(value) do
          {number, _} -> number
          _ -> value
        end
    end
  end
  def to_number(value), do: value
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Helper Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp convert_key_strings_to_atoms(data) when is_map(data) do
    Enum.reduce(data, %{}, fn {key, value}, acc ->
      Map.put(acc, String.to_atom(key), convert_key_strings_to_atoms(value))
    end)
  end
  defp convert_key_strings_to_atoms(data) when is_list(data) do
    Enum.map(data, &convert_key_strings_to_atoms/1)
  end
  defp convert_key_strings_to_atoms(data) do
    data
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
