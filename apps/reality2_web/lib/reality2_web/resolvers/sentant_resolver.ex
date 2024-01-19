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
                {:ok, convert_map_keys(sentant)}
              {:error, reason} ->
                {:error, reason}
            end
        end
      name ->
        case Reality2.Sentants.read(%{name: name}, :definition) do
          {:ok, sentant} ->
            {:ok, convert_map_keys(sentant)}
          {:error, reason} ->
            {:error, reason}
        end
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Get all the Sentants on this Node.  TODO: Search criteria and privacy
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def all_sentants(_, _, _) do
    case Reality2.Sentants.read_all(:definition) do
      sentants ->
        {:ok, Enum.map(sentants, fn sentant -> convert_map_keys(sentant) end)}
      {:error, reason} ->
        {:error, reason}
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Load a Sentant from the yaml_definition.
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def load_sentant(_root, args, _info) do
    case Map.get(args, :yaml_definition) do
      nil ->
        # There was no yaml_definition
        {:error, :yaml_definition}
      yaml_definition ->
        # Decode the yaml_definition from encoded uri
        yaml_decoded = URI.decode(yaml_definition)

        # Create the Sentant (or update it if it already exists)
        case Reality2.Sentants.create(yaml_decoded) do
          # Success, so get the Sentant details to send back
          {:ok, sentantid} ->
            # Read the sentant detals from the Sentant
            case Reality2.Sentants.read(%{id: sentantid}, :definition) do
              {:ok, sentant} ->
                # Send back the sentant details
                {:ok, convert_map_keys(sentant)}
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
                {:ok, convert_map_keys(sentant)}
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
  # Load a Swarm of Sentants from the yaml_definition.
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def load_swarm(_root, args, _info) do
    # Create a new swarm
    case Map.get(args, :yaml_definition) do
      nil ->
        {:error, :yaml_definition}
      yaml_definition ->
        # Decode the yaml_definition from encoded uri
        yaml_decoded = URI.decode(yaml_definition)

        # Create the Swarm
        case Reality2.Swarm.create(yaml_decoded) do
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
                  convert_map_keys(sentant)
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
            case Reality2.Sentants.sendto(%{id: sentantid}, %{event: event, parameters: parameters}) do
              {:ok, _} ->
                # Send back the sentant details
                case Reality2.Sentants.read(%{id: sentantid}, :definition) do
                  {:ok, sentant} ->
                    # Send back the sentant details
                    {:ok, convert_map_keys(sentant)}
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
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------------------
  # def subscribe_event(_root, args, _info) do

  #   {:ok, topic} = PubSub.subscribe(Reality2Web.PubSub, "sentant_event")

  #   {:ok, topic}
  # end
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
end
