defmodule Reality2.Sentant.Comms do
@moduledoc false
    use GenServer

    # Client

    def start_link({name, id, sentant_map}) do
      GenServer.start_link(__MODULE__, {name, id, sentant_map}, name: String.to_atom(id <> "|comms"))
    end

    # Server (callbacks)

    @impl true
    def init({_name, id, sentant_map}) do
      {:ok, {id, sentant_map}}
    end

    @impl true
    def handle_call(command_and_parameters, _from, {id, sentant_map}) do

      result = String.to_atom(id <> "|automations")
      |> DynamicSupervisor.which_children()
      |> Enum.map( fn {_, pid_or_restarting, _, _} ->
        # Send the message to each child
        case pid_or_restarting do
          :restarting ->
            # Ignore
            :ok
          pid ->
            GenServer.call(pid, command_and_parameters)
        end
      end)

      {:reply, result, {id, sentant_map}}
    end

    @impl true
    def handle_cast(command_and_parameters, {id, sentant_map}) do
      # IO.puts("Sentant.Comms.handle_call: args = #{inspect(command_and_parameters)}")
      # IO.puts("                         : id = #{inspect(id)}")
      # IO.puts("                         : sentant_map = #{inspect(sentant_map)}")

      String.to_atom(id <> "|automations")
      |> DynamicSupervisor.which_children()
      |> Enum.each( fn {_, pid_or_restarting, _, _} ->
        # Send the message to each child
        case pid_or_restarting do
          :restarting ->
            # Ignore
            :ok
          pid ->
            GenServer.cast(pid, command_and_parameters)
        end
      end)

      {:noreply, {id, sentant_map}}
    end

    @impl true
    def handle_info(_message_map, state) do
      {:noreply, state}
    end
  end
