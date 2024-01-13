defmodule Reality2.Sentant.Comms do
# *******************************************************************************************************************************************
@moduledoc false
# Manage the communications to and from the Sentant and Automations.
#
# **Author**
# - Dr. Roy C. Davies
# - [roycdavies.github.io](https://roycdavies.github.io/)
# *******************************************************************************************************************************************
  use GenServer

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Supervisor Callbacks
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def start_link({name, id, sentant_map}) do
    GenServer.start_link(__MODULE__, {name, id, sentant_map}, name: String.to_atom(id <> "|comms"))
  end

  @impl true
  def init({_name, id, sentant_map}) do
    {:ok, {id, sentant_map}}
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Public Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Synchronous Calls
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # Return the current definition of the Sentant
  @impl true
  def handle_call(:definition, _from, {id, sentant_map}) do
    {:reply, sentant_map, {id, sentant_map}}
  end

  # Return the states of all the Automations on the Sentant
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
          IO.puts("Sending to: " <> inspect(pid) <> inspect(command_and_parameters))
          GenServer.call(pid, command_and_parameters)
      end
    end)

    {:reply, result, {id, sentant_map}}
  end

  def handle_call(_, _, state) do
    {:reply, :ok, state}
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Asynchronous Casts
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @impl true
  def handle_cast(command_and_parameters, {id, sentant_map}) do

    String.to_atom(id <> "|automations")
    |> DynamicSupervisor.which_children()
    |> Enum.each( fn {_, pid_or_restarting, _, _} ->
      # Send the message to each child
      case pid_or_restarting do
        :restarting ->
          # Ignore
          :ok
        pid ->
          IO.puts("Sending to: " <> inspect(pid) <> inspect(command_and_parameters))

          # Build the subscription data
          event = Helpers.Map.get(command_and_parameters, "event", "")
          subscription_data = %{
            sentant: convert_key_strings_to_atoms(sentant_map),
            event: event,
            parameters: Helpers.Map.get(command_and_parameters, "parameters", %{})
          }
          # IO.puts("subscription_data: " <> inspect(subscription_data, pretty: true))

          # Send off to any event subscriptions
          Absinthe.Subscription.publish(Reality2Web.Endpoint, subscription_data, sentant_event: id <> "|" <> event)

          # Send to each automation
          GenServer.cast(pid, command_and_parameters)
      end
    end)

    {:noreply, {id, sentant_map}}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info(command_and_parameters, {id, sentant_map}) do
    handle_cast(command_and_parameters, {id, sentant_map})
  end
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
