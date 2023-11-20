defmodule Reality2engine.Repo.Migrations.CreateSentant do
  use Ecto.Migration

  def change do
    create table(:sentant) do
      add :name, :string

      timestamps()
    end
  end
end
