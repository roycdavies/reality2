defmodule Reality2Web.SentantResolver do
# *******************************************************************************************************************************************
@moduledoc false
# Resolvers for the GraphQL Schema for Sentants and Swarms.
#
# **Author**
# - Dr. Roy C. Davies
# - [roycdavies.github.io](https://roycdavies.github.io/)
# *******************************************************************************************************************************************

# alias Absinthe.PubSub

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Puplic Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Get the details of a single Sentant by ID or name.
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def get_sentant(_, args, _) do
    IO.puts("get_sentant: args: #{inspect(args)}")
    case Map.get(args, :name) do
      nil ->
        case Map.get(args, :id) do
          nil ->
            {:error, :name_or_id}
          sentantid ->
            case Reality2.Sentants.read(%{id: sentantid}, :definition) do
              {:ok, sentant} ->
                {:ok, sentant |> convert_map_keys |> convert_for_output}
              {:error, reason} ->
                {:error, reason}
            end
        end
      name ->
        case Reality2.Sentants.read(%{name: name}, :definition) do
          {:ok, sentant} ->
            {:ok, sentant |> convert_map_keys |> convert_for_output}
          {:error, reason} ->
            {:error, reason}
        end
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Get all the Sentants on this Node.  TODO: Search criteria and privacy / ownership
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def all_sentants(_, _, _) do
    case Reality2.Sentants.read_all(:definition) do
      {:ok, sentants} ->
        {:ok, Enum.map(sentants, fn sentant -> sentant |> convert_map_keys |> convert_for_output end)}
      {:error, reason} ->
        {:error, reason}
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Load a Sentant from the definition.
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def load_sentant(_root, args, _info) do
    case Map.get(args, :definition) do
      nil ->
        # There was no definition
        {:error, :definition}
      definition ->
        # Decode the definition from encoded uri
        decoded = URI.decode(definition)

        # Create the Sentant (or update it if it already exists and the ID is given)
        case Reality2.Sentants.create(decoded) do
          # Success, so get the Sentant details to send back
          {:ok, sentantid} ->
            # Read the sentant detals from the Sentant
            case Reality2.Sentants.read(%{id: sentantid}, :definition) do
              {:ok, sentant} ->
                # Send back the sentant details
                {:ok, sentant |> convert_map_keys |> convert_for_output}
              {:error, reason} ->
                # Something went wrong
                {:error, reason}
            end
          {:error, {error_code, reason}} -> {:error, Atom.to_string(error_code) <> ":" <> reason}
          {:error, reason} -> {:error, reason}
        end
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Unload (delete) a Sentant by ID.
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def unload_sentant(_root, args, _info) do

    # Delete a sentant
    case Map.get(args, :id) do
      nil ->
        {:error, :id}
      sentantid ->
        # Get the details of the Sentant before it is deleted
        case Reality2.Sentants.read(%{id: sentantid}, :definition) do
          {:ok, sentant} ->
            case Reality2.Sentants.delete(%{id: sentantid}) do
              {:ok, _} ->
                # Send back the sentant details
                {:ok, sentant |> convert_map_keys |> convert_for_output}
              {:error, reason} ->
                # Something went wrong
                {:error, reason}
            end
          {:error, reason} ->
            # Something went wrong
            {:error, reason}
        end
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Load a Swarm of Sentants from the definition.
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def load_swarm(_root, args, _info) do
    # Create a new swarm
    case Map.get(args, :definition) do
      nil ->
        {:error, :definition}
      definition ->
        # Decode the definition from encoded uri
        decoded = URI.decode(definition)

        # Create the Swarm
        case Reality2.Swarm.create(decoded) do
          {:error, {error_code, reason}} -> {:error, Atom.to_string(error_code) <> ":" <> reason}
          {:error, reason} -> {:error, reason}
          {:ok, swarm} ->
            name = Helpers.Map.get(swarm, "name", "")
            description = Helpers.Map.get(swarm, "description", "")
            sentant_ids = Helpers.Map.get(swarm, "sentants", [])

            # Create a list of the sentants' details
            sentants = Enum.map(sentant_ids, fn id ->
              case Reality2.Sentants.read(%{id: id}, :definition) do
                {:ok, sentant} ->
                  sentant |> convert_map_keys |> convert_for_output
                {:error, _reason} ->
                  # Something went wrong
                  false
              end
            end) |> Enum.filter(fn x -> x end)

            {:ok, %{name: name, description: description, sentants: sentants}}
        end
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Send an event to a Sentant
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def send_event(_root, args, _info) do
    # Get the Sentant ID
    case Map.get(args, :id) do
      nil ->
        {:error, :id}
      sentantid ->
        # Get the event
        case Map.get(args, :event) do
          nil ->
            {:error, :event}
          event ->
            # Get the parameters
            parameters = Map.get(args, :parameters, %{})
            # Send the event to the Sentant
            # Check if this is a valid event that can be sent from outside, and if so, send it.
            case Reality2.Sentants.read(%{id: sentantid}, :definition) do
              {:ok, sentant} ->
                events = sentant |> Helpers.Map.get(:automations, []) |> find_events_in_automations
                if (Enum.member?(events, event)) do
                  case Reality2.Sentants.sendto(%{id: sentantid}, %{event: event, parameters: parameters}) do
                    {:ok, _} -> {:ok, sentant}
                    {:error, reason} ->
                      # Something went wrong
                      {:error, reason}
                  end
                else
                  {:error, :invalid_event}
                end
            end
        end
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def check_subscribe_allowed(sentantid, signal) do
    case Reality2.Sentants.read(%{id: sentantid}, :definition) do
      {:ok, sentant} ->
        sentant |> Helpers.Map.get(:automations, []) |> find_signals_in_automations |> Enum.member?(signal)
      _ -> false
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Private Helper Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Convert map keys to atoms
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp convert_map_keys(data) when is_map(data) do
    Enum.map(data, fn {k, v} ->
      cond do
        is_binary(k) -> {String.to_atom(k), convert_map_keys(v)}
        true -> {k, convert_map_keys(v)}
      end
    end)
    |> Map.new
  end
  defp convert_map_keys(data) when is_list(data), do: Enum.map(data, fn x -> convert_map_keys(x) end)
  defp convert_map_keys(data), do: data
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Tweak the raw Sentant data to remove private elements
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp convert_for_output(data) when is_map(data) do
    automations = data |> Helpers.Map.get(:automations, [])
    events = automations |> find_events_in_automations
    signals = automations |> find_signals_in_automations

    data
    |> Map.drop([:automations, :plugins])
    |> Map.put(:events, events)
    |> Map.put(:signals, signals)
  end
  defp convert_for_output(data) when is_list(data), do: Enum.map(data, fn x -> convert_for_output(x) end)
  defp convert_for_output(data), do: data

  defp find_events_in_automations(automations) do
    Enum.map(automations, fn(automation) -> automation |> Helpers.Map.get(:transitions, []) |> find_events end) |> List.flatten
  end
  defp find_events([]), do: []
  defp find_events([transition | rest]) do
    # Only include the event if it is marked as public = true
    case Helpers.Map.get(transition, :public, false) do
      true -> [Helpers.Map.get(transition, :event) | find_events(rest)]
      _ -> find_events(rest)
    end
  end

  defp find_signals_in_automations(automations) do
    Enum.map(automations, fn(automation) -> automation |> Helpers.Map.get(:transitions, []) |> find_signals end) |> List.flatten
  end
  defp find_signals([]), do: []
  defp find_signals([transition | rest]) do
    case Helpers.Map.get(transition, :actions, []) do
      [] -> find_signals(rest)
      actions ->
        signals = Enum.map(actions, fn(action) -> get_signal_from_action(action) end) |> List.flatten
        signals ++ find_signals(rest)
    end
  end
  defp get_signal_from_action(action) do
    case Helpers.Map.get(action, :command) do
      "signal" ->
        case Helpers.Map.get(action, :parameters) do
          nil -> []
          parameters ->
            case Helpers.Map.get(action, :public, false) or Helpers.Map.get(parameters, :public, false) do
              false -> []
              true -> [Helpers.Map.get(parameters, :event, [])]
            end
        end
      _ -> []
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
