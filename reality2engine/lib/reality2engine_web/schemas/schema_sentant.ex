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

    field :id, non_null(:uuid4), description: "Sentant unique GUID"
    field :name, non_null(:string), description: "Sentant name"

    field :starttime, non_null(:datetime), description: "Starting time"
    field :endtime, non_null(:datetime), description: "Ending time"
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
  end
  # ------------------------------------------------------------------------------------------------------



  # ------------------------------------------------------------------------------------------------------
  # Mutations
  # ------------------------------------------------------------------------------------------------------
  object :sentant_mutations do
    # Nothing here - bookings are made as part of the items mutations.
  end
  # ------------------------------------------------------------------------------------------------------
end
