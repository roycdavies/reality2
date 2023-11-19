# ****************************************************************************************************
# Define the Sentant database structure for Ecto
# ****************************************************************************************************
defmodule Reality2engine.Sentants.Sentant do
  use Ecto.Schema
  import Ecto.Changeset

  schema "sentant" do
    field :id, :guid
    field :name, :string

    field :starttime, :utc_datetime
    field :endtime, :utc_datetime

    timestamps()
  end

  @doc false
  def changeset(induction, attrs) do
    induction
    |> cast(attrs, [:id, :name, :starttime, :endtime])
    |> validate_required([:id, :name, :details, :starttime, :endtime])
  end
end
