defmodule Reality2Web.Schema.Sentant do
  use Absinthe.Schema.Notation

  require Reality2Web.Schema.Enums

  # alias Reality2engine.Sentant
  alias Reality2Web.SentantResolver

  # ------------------------------------------------------------------------------------------------------
  # Sentant Schema definition
  # ------------------------------------------------------------------------------------------------------
  object :sentant do
    field :uuid, non_null(:uuid4),    description: "Sentant unique GUID"
    field :name, non_null(:string),   description: "Sentant name"
    field :data, :json,               description: "Sentant data"
    field :automations, :json,        description: "Sentant automations"
  end
    # ------------------------------------------------------------------------------------------------------



  # ------------------------------------------------------------------------------------------------------
  # Queries
  # ------------------------------------------------------------------------------------------------------
  object :sentant_queries do
    # New Sentant
    # Load Sentant
    # Unload Sentant

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
    # New Sentant
    # Load Sentant
    # Unload Sentant

    # ----------------------------------------------------------------------------------------------------
    @desc "Create a new sentant"
    # ----------------------------------------------------------------------------------------------------
    field :sentant_create, non_null(:sentant) do
      arg :uuid, :uuid4
      arg :name, non_null(:string)
      arg :data, :json
      arg :automations, :json
      # resolve(&SentantResolver.create_sentant/3)
      {:ok, %{}}
    end

    # ----------------------------------------------------------------------------------------------------
    @desc "Update a sentant"
    # ----------------------------------------------------------------------------------------------------
    field :sentant_update, non_null(:sentant) do
      arg :id, non_null(:uuid4)
      arg :name, :string
      arg :data, :json
      arg :automations, :json
      # resolve(&SentantResolver.update_sentant/3)
      {:ok, %{}}
    end

    # ----------------------------------------------------------------------------------------------------
    @desc "Delete a sentant"
    # ----------------------------------------------------------------------------------------------------
    field :sentant_delete, non_null(:sentant) do
      arg :id, non_null(:uuid4)
      # resolve(&SentantResolver.delete_sentant/3)
      {:ok, %{}}
    end
  end
  # ------------------------------------------------------------------------------------------------------



  # ------------------------------------------------------------------------------------------------------
  # Subscriptions
  # ------------------------------------------------------------------------------------------------------
  # object :sentant_subscriptions do
  #
  # end
  # ------------------------------------------------------------------------------------------------------
end
