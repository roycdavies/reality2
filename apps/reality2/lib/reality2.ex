defmodule Reality2 do
  @moduledoc """
  Reality2 is the App that manages Sentants on a Reality2 Node.

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)
  """
  @doc"""
  Run various Reality2 Unit Tests - same as running "mix test"
  """
  def test() do
    Mix.Task.run("test")
  end

  def test_one(test_name) do
    Mix.Task.run("test", [test_name])
  end

  def test_call() do
    sentant_definition = """
    sentant:
      name: fred
    """

    {:ok, _id} = Reality2.Sentants.create(sentant_definition)

    Reality2.Sentants.sendto(%{:name => "fred"}, %{:command => "test", :parameters => %{}})
  end
end
