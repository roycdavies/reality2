defmodule Reality2.Sentant.Comms do
@moduledoc false
    use GenServer

    # Client

    def start_link({name, id, sentant_map}) do
      GenServer.start_link(__MODULE__, {name, id, sentant_map}, name: String.to_atom(id <> "_comms"))
    end

    # Server (callbacks)

    @impl true
    def init({_name, _id, sentant_map}) do
      {:ok, sentant_map}
    end

    @impl true
  @doc false
    def handle_call(%{read: _parameters}, _from, sentant_map) do
      {:reply, %{sentant: sentant_map, states: %{}}, sentant_map}
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
