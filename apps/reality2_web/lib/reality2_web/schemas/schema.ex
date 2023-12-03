defmodule Reality2Web.Schema do
  use Absinthe.Schema

  import_types Reality2Web.Schema.Types.Custom.JSON
  import_types Reality2Web.Schema.Types.Custom.UUID4
  import_types Absinthe.Type.Custom
  import_types Reality2Web.Schema.Sentant

  query do
    import_fields :sentant_queries
  end

  mutation do
    import_fields :sentant_mutations
  end

end
