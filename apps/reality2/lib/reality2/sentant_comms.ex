defmodule Reality2.Sentant.Comms do
@moduledoc false
    use GenServer

    # Client

    def start_link({name, id, definition_map}) do
      GenServer.start_link(__MODULE__, {name, id, definition_map}, name: String.to_atom(id <> "_comms"))
    end

    # Server (callbacks)

    @impl true
    def init({_name, _id, definition_map}) do
      {:ok, definition_map}
    end

    @impl true
    def handle_call(_message_map, from, state) do
      {:reply, from, state}
    end

    @impl true
    def handle_cast(_message_map, state) do
      # Send to each of the Automations

      {:noreply, state}
    end

    @impl true
    def handle_info(_message_map, state) do
      {:noreply, state}
    end
  end
