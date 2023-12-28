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
    Mix.Task.run("test", ["test/tests/" <> test_name])
  end

  def test_call() do
    sentant_definition = """
    sentant:
      name: fred
    """

    {:ok, id} = Reality2.Sentants.create(sentant_definition)

    Reality2.Sentants.sendto(%{:name => "fred"}, %{:command => "test", :parameters => %{}})

    {:ok, id}
  end

  def test_automations() do
    sentant_definition = """
    sentant:
      name: fred
      description: This is a test sentant.
      automations:
        - name: light_switch
          description: This is a test automation.
          transitions:
            - from: start
              event: init
              to: ready
              actions:
                - command: set
                  parameters:
                    name: switch
                    value: 0
            - from: "*"
              event: turn_on
              to: on
              actions:
                - command: set
                  parameters:
                    name: switch
                    value: 1
            - from: "*"
              event: turn_off
              to: off
              actions:
                - command: set
                  parameters:
                    name: switch
                    value: 0
    """

    {:ok, id} = Reality2.Sentants.create(sentant_definition)

    automation_state = Reality2.Sentants.read(%{:id => id})
    IO.puts("Automation State = #{inspect(automation_state)}")

    Reality2.Sentants.sendto(%{:id => id}, %{event: "turn_on", parameters: %{}, passthrough: %{}})
    automation_state = Reality2.Sentants.read(%{:id => id})
    IO.puts("Automation State = #{inspect(automation_state)}")

    Reality2.Sentants.sendto(%{:id => id}, %{event: "turn_off", parameters: %{}, passthrough: %{}})
    automation_state = Reality2.Sentants.read(%{:id => id})
    IO.puts("Automation State = #{inspect(automation_state)}")

    {:ok, id}
  end
end
