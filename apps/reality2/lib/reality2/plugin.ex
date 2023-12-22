defmodule Reality2.Plugin do
  @moduledoc false

  use Agent, restart: :transient

  def start_link({name, initial_value}) do
    Agent.start_link(fn -> initial_value end, name: String.to_atom(name))
  end

  def value do
    Agent.get(__MODULE__, & &1)
  end

  def increment do
    Agent.update(__MODULE__, &(&1 + 1))
  end

end
