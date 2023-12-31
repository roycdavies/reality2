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

  # -----------------------------------------------------------------------------------------------------------------------------------------
end
