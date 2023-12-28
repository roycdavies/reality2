defmodule Reality2.Automation do
  use GenServer, restart: :transient

  # Callbacks

  @doc false
  def start_link({_sentant_name, id, _sentant_map}, automation_map) do
    case Map.get(automation_map, "name") do
      nil ->
        {:error, :definition}
      name ->
        GenServer.start_link(__MODULE__, {name, automation_map}, name: String.to_atom(id <> "|automations|" <> name))
    end
  end

  @impl true
  def init({name, automation_map}) do
    # IO.puts("Automation.init: args = #{inspect(automation_map)}")

    {:ok, {name, automation_map, "start"}}
  end

  @impl true
  def handle_call(%{:read => _}, _from, {name, automation_map, state}) do
    {:reply, {name, state}, {name, automation_map, state}}
  end

  @impl true
  def handle_cast(%{event: event, parameters: parameters, passthrough: passthrough}, {name, automation_map, state}) do

    case Map.get(automation_map, "transitions") do
      nil ->
        {:noreply, {name, automation_map, state}}
      transitions ->
        new_state = transitions
        |> Enum.reduce_while(state, fn transition_map, acc_state ->
          case check_transition(transition_map, event, parameters, passthrough, acc_state) do
            {:no_match, the_state} ->
              {:cont, the_state}
            {:ok, the_state} ->
              {:halt, the_state}
          end
        end)

        {:noreply, {name, automation_map, new_state}}
    end
  end


  defp check_transition(transition_map, event, parameters, passthrough, state) do
    # IO.puts("Automation.check_transition: args = #{inspect(transition_map)}, #{inspect(event)}, #{inspect(state)}")
    case Map.get(transition_map, "from") do
      nil ->
        {:no_match, state}
      "*" -> check_event(transition_map, event, parameters, passthrough, state)
      ^state -> check_event(transition_map, event, parameters, passthrough, state)
      _ -> {:no_match, state}
    end
  end

  defp check_event(transition_map, event, _parameters, _passthrough, state) do
    case Map.get(transition_map, "event") do
      nil ->
        {:no_match, state}
      ^event ->
        case Map.get(transition_map, "to") do
          nil ->
            {:no_match, state}
          "*" ->
            IO.puts("#{inspect(state)} + #{inspect(event)} -> #{inspect(state)}")
            # TODO: actions
            {:ok, state}
          to ->
            IO.puts("#{inspect(state)} + #{inspect(event)} -> #{inspect(to)}")
            # TODO: actions
            {:ok, to}
        end
      _ ->
        {:no_match, state}
    end
  end
end
