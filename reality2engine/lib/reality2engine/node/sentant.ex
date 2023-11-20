# ****************************************************************************************************
# Define the Sentant database structure for Ecto
# ****************************************************************************************************
defmodule Reality2engine.Node.Sentant do
  use Ecto.Schema
  import Ecto.Changeset

  schema "sentant" do
    #field :id, :string
    field :name, :string

    field :starttime, :utc_datetime
    field :endtime, :utc_datetime

    timestamps()
  end

  @doc false
  def changeset(sentant, attrs) do
    sentant
    |> cast(attrs, [:name, :starttime, :endtime])
    |> validate_required([:name, :details, :starttime, :endtime])
  end
end
