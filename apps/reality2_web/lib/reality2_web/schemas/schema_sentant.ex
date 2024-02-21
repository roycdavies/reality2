defmodule Reality2Web.Schema.Sentant do
@moduledoc """
  Sentant Schema

  Type definitions for the Sentant GraphQL schema are at: [Reality2.Types](../reality2/Reality2.Types.html)

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)
"""
  use Absinthe.Schema.Notation

  require Reality2Web.Schema.Enums

  alias Reality2Web.SentantResolver

  object :event_parameters do
    field :sentant, non_null(:sentant),         description: "Sentant"
    field :event, non_null(:string),            description: "Event name"
    field :parameters, :json,                   description: "Event parameters"
  end

  object :plugin_output do
    field :key, :string,                        description: "Plugin output key"
    field :value, :string,                      description: "Plugin output json path to interpret response, eg choices.0.message.content"
    field :event, :string,                      description: "Plugin output event sent when response received"
  end

  object :plugin do
    field :name, non_null(:string),             description: "Plugin name"
    field :description, :string,                description: "Plugin description"
    field :version, :string,                    description: "Plugin version"
    field :url, non_null(:string),              description: "URL to plugin API"
    field :headers, :json,                      description: "Plugin headers"
    field :body, :json,                         description: "Plugin body"
    field :output, non_null(:plugin_output),    description: "Plugin output"
  end

  object :action do
    field :plugin, :string,                     description: "Action plugin"
    field :command, non_null(:string),          description: "Action command"
    field :parameters, :json,                   description: "Action parameters"
  end

  object :transition do
    field :from, non_null(:string),             description: "Transition from"
    field :event, non_null(:string),            description: "Transition event"
    field :to, non_null(:string),               description: "Transition to"
    field :actions, list_of(:action),           description: "Transition actions"
  end

  object :automation do
    field :name, non_null(:string),             description: "Automation name"
    field :description, :string,                description: "Automation description"
    field :transitions, list_of(:transition),   description: "Automation transitions"
  end

  # ------------------------------------------------------------------------------------------------------
  # Sentant Schema definition
  # ------------------------------------------------------------------------------------------------------
  @doc """
  Sentant Type

  ```graphql
    type Sentant {
        id: UUID!                                   # The UUID of the sentant
        name: String!                               # The name of the sentant
        owner: User!                                # The owner of the sentant
        data: JSON!                                 # The data on the sentant
        automations: [Automation]!                  # The automations on the sentant
        plugins: [Plugin]!                          # The plugins used by the sentant
        node: Node                                  # The node the sentant is on
    }
  ```
  """
  def sentant do

  end
  object :sentant do
    field :id, non_null(:uuid4),                description: "Sentant ID"
    field :name, non_null(:string),             description: "Sentant name"
    field :description, :string,                description: "Sentant description"
    field :automations, list_of(:automation),   description: "Sentant automations"
    field :plugins, list_of(:plugin),           description: "Sentant plugins"
  end
  # ------------------------------------------------------------------------------------------------------



  # ------------------------------------------------------------------------------------------------------
  # A Swarm of Sentants
  # ------------------------------------------------------------------------------------------------------
  object :swarm do
    field :name, non_null(:string),             description: "Swarm name"
    field :description, :string,                description: "Swarm description"
    field :sentants, list_of(:sentant),         description: "Swarm sentants"
  end
  # ------------------------------------------------------------------------------------------------------



  # ------------------------------------------------------------------------------------------------------
  # Queries
  # ------------------------------------------------------------------------------------------------------
  object :sentant_queries do
    # ----------------------------------------------------------------------------------------------------
    @desc "Get a sentant details by name or id"
    # ----------------------------------------------------------------------------------------------------
    field :sentant_get, :sentant do
      arg :name, :string
      arg :id, :uuid4
      resolve(&SentantResolver.get_sentant/3)
    end

    # ----------------------------------------------------------------------------------------------------
    @desc "Get all the sentants"
    # ----------------------------------------------------------------------------------------------------
    field :sentant_all, list_of(:sentant) do
      resolve(&SentantResolver.all_sentants/3)
    end
  end
  # ------------------------------------------------------------------------------------------------------



  # ------------------------------------------------------------------------------------------------------
  # Mutations
  # ------------------------------------------------------------------------------------------------------
  object :sentant_mutations do
    # ----------------------------------------------------------------------------------------------------
    @desc "Load a sentant"
    # ----------------------------------------------------------------------------------------------------
    field :sentant_load, non_null(:sentant) do
      arg :yaml_definition, non_null(:string)
      resolve(&SentantResolver.load_sentant/3)
    end

    # ----------------------------------------------------------------------------------------------------
    @desc "Delete a sentant"
    # ----------------------------------------------------------------------------------------------------
    field :sentant_unload, non_null(:sentant) do
      arg :id, non_null(:uuid4)
      resolve(&SentantResolver.unload_sentant/3)
    end

    # ----------------------------------------------------------------------------------------------------
    @desc "Load a swarm of sentants"
    # ----------------------------------------------------------------------------------------------------
    field :swarm_load, non_null(:swarm) do
      arg :yaml_definition, non_null(:string)
      resolve(&SentantResolver.load_swarm/3)
    end

    # ----------------------------------------------------------------------------------------------------
    @desc "Send a amessage event and parameters to a sentant"
    # ----------------------------------------------------------------------------------------------------
    field :sentant_send, non_null(:sentant) do
      arg :id, non_null(:uuid4)
      arg :event, non_null(:string)
      arg :parameters, :json
      resolve(&SentantResolver.send_event/3)
    end
  end
  # ------------------------------------------------------------------------------------------------------



  # ------------------------------------------------------------------------------------------------------
  # Subscriptions
  # ------------------------------------------------------------------------------------------------------
  object :sentant_subscriptions do

    # ----------------------------------------------------------------------------------------------------
    @desc "Subscribe to sentant events"
    # ----------------------------------------------------------------------------------------------------
    field :sentant_event, :event_parameters do
      arg :id, non_null(:uuid4)
      arg :event, non_null(:string)

      config fn %{id: sentantid, event: event}, _ ->
        IO.puts("sentant_event sentantid: #{inspect(sentantid)}")
        IO.puts("sentant_event event: #{inspect(event)}")
        {:ok, topic: sentantid <> "|" <> event}
      end
    end

    # ----------------------------------------------------------------------------------------------------
    @desc "Subscribe to node events"
    # ----------------------------------------------------------------------------------------------------
    field :node_event, :event_parameters do
      arg :event, non_null(:string)

      config fn %{event: event}, _ ->
        IO.puts("node_event event: #{inspect(event)}")
        {:ok, topic: "node|" <> event}
      end
    end

    # ----------------------------------------------------------------------------------------------------
    @desc "Subscribe to transient network events"
    # ----------------------------------------------------------------------------------------------------
    field :network_event, :event_parameters do
      arg :event, non_null(:string)

      config fn %{event: event}, _ ->
        IO.puts("network_event event: #{inspect(event)}")
        {:ok, topic: "network|" <> event}
      end
    end

  end
  # ------------------------------------------------------------------------------------------------------
end
