defmodule Reality2engineWeb.Schema do
  use Absinthe.Schema

  import_types Reality2engine.Schema.Types.Custom.JSON
  import_types Absinthe.Type.Custom
  import_types Reality2engineWeb.Schema.Sentant

  query do
    import_fields :sentant_queries
  end

  mutation do
    import_fields :sentant_mutations
  end

end
