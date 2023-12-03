# ****************************************************************************************************
# Define the Sentant database structure for Ecto
# ****************************************************************************************************
defmodule Reality2engineStorage.Node.Sentant do
  use Ecto.Schema
  import Ecto.Changeset

  schema "sentant" do
    field :uuid, :binary_id, primary_key: true
    field :name, :string
    field :data, :map
    field :automations, :map

    # timestamps()
  end

  @doc false
  def changeset(sentant, attrs) do
    sentant
    |> cast(attrs, [:uuid, :name, :data, :automations])
    |> validate_required([:uuid, :name, :data, :automations])
  end
end
