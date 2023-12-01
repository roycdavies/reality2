defmodule Reality2engineWeb.Schema.Sentant do
  use Absinthe.Schema.Notation

  require Reality2engineWeb.Schema.Enums

  alias Reality2engine.Sentant

  # ------------------------------------------------------------------------------------------------------
  # Sentant Schema definition
  # ------------------------------------------------------------------------------------------------------
  object :sentant do
    # field :item, non_null(:item), description: "The item being booked." do
    #   resolve fn post, _, _ ->
    #     batch({__MODULE__, :items}, post.itemname, fn batch_results ->
    #       {:ok, Map.get(batch_results, post.itemname)}
    #     end)
    #   end
    # end

    # field :person, non_null(:person), description: "The person the item is booked for" do
    #   resolve fn post, _, resolution ->
    #     batch({__MODULE__, :people, resolution.context.user}, post.upi, fn batch_results ->
    #       {:ok, Map.get(batch_results, post.upi)}
    #     end)
    #   end
    # end

    field :uuid, non_null(:uuid4), description: "Sentant unique GUID"
    field :name, non_null(:string), description: "Sentant name"

    field :data, non_null(:JSON), description: "Sentant data"
  end

  # def people(_, []) do %{} end
  # def people(user, [upi | upis]) do
  #   Map.merge(%{upi => Person.get_person_by_upi(upi) |> PersonResolver.tune_for_user(user) }, people(user, upis))
  # end

  # def items(_, []) do %{} end
  # def items(param, [name | names]) do
  #   Map.merge(%{name => Item.get_item_by_name(name)}, items(param, names))
  # end
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
    field :sentant_all, non_null(list_of(:sentant)) do
      arg :starttime, :datetime
      arg :endtime, :datetime
      # resolve(&SentantResolver.all_items/3)
      {:ok, %{}}
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
    field :createSentant, non_null(:sentant) do
      arg :name, non_null(:string)
      arg :starttime, non_null(:datetime)
      arg :endtime, non_null(:datetime)
      # resolve(&SentantResolver.create_sentant/3)
      {:ok, %{}}
    end

    # ----------------------------------------------------------------------------------------------------
    @desc "Update a sentant"
    # ----------------------------------------------------------------------------------------------------
    field :sentant_update, non_null(:sentant) do
      arg :id, non_null(:uuid4)
      arg :name, :string
      arg :starttime, :datetime
      arg :endtime, :datetime
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
  #   # Nothing here - bookings are made as part of the items mutations.
  # end
  # ------------------------------------------------------------------------------------------------------
end
