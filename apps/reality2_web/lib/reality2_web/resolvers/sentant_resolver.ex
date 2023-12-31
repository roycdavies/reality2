defmodule Reality2Web.SentantResolver do

  def all_sentants(_, _, _) do
    {:ok, []}
  end

  def create_sentant(_, _, _) do
    # Check if there is an ID supplied, in which case this is an existing sentant, and we have to check its uniqueness first.
    # Otherwise, create a new sentant running in memory.

    Reality2.Sentants.create()
    {:ok, %{}}
  end
end
