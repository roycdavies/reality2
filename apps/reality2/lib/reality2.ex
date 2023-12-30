defmodule Reality2 do
  @moduledoc """
  Reality2 is the App that manages Sentants on a Reality2 Node.

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)
  """


  @doc false
  def test() do
    Mix.Task.run("test")
  end

  @doc false
  def test_one(test_name) do
    Mix.Task.run("test", ["test/tests/" <> test_name])
  end

  @doc false
  def test_call() do
    sentant_definition = """
    sentant:
      name: fred
    """

    {:ok, id} = Reality2.Sentants.create(sentant_definition)

    Reality2.Sentants.sendto(%{:name => "fred"}, %{:command => "test", :parameters => %{}})

    {:ok, id}
  end

  @doc false
  def test_automations() do
    sentant_definition = """
    sentant:
      name: Light Switch
      description: This is a test sentant.
      automations:
        - name: light_switch
          description: This is a test automation.
          transitions:
            - from: start
              event: init
              to: ready
              actions:
                - plugin: ai.reality2.vars
                  command: set
                  parameters:
                    key: switch
                    value: 0
            - from: "*"
              event: turn_on
              to: on
              actions:
                - command: set
                  plugin: ai.reality2.vars
                  parameters:
                    key: switch
                    value: 1
                - command: send
                  parameters:
                    to: light_bulb
                    event: turn_on
            - from: "*"
              event: turn_off
              to: off
              actions:
                - plugin: ai.reality2.vars
                  command: set
                  parameters:
                    key: switch
                    value: 0
                - command: send
                  parameters:
                    to: light_bulb
                    event: turn_off
    """

    {:ok, id} = Reality2.Sentants.create(sentant_definition)

    automation_state = Reality2.Sentants.read(%{:id => id}, :state)
    IO.puts("Automation State = #{inspect(automation_state)}")

    Reality2.Sentants.sendto(%{:id => id}, %{event: "turn_on"})
    automation_state = Reality2.Sentants.read(%{:id => id}, :state)
    IO.puts("Automation State = #{inspect(automation_state)}")

    Reality2.Sentants.sendto(%{:name => "Light Switch"}, %{event: "turn_off"})
    automation_state = Reality2.Sentants.read(%{:id => id}, :state)
    IO.puts("Automation State = #{inspect(automation_state)}")

    returned_definition = Reality2.Sentants.read(%{:id => id}, :definition)
    IO.puts("Returned Definition = #{inspect(returned_definition, pretty: true)}")
    metadata = GenServer.call(String.to_atom(id <> "|ai.reality2.vars"), %{command: "all"})

    IO.puts("Metadata = #{inspect(metadata)}")

    {:ok, id}
  end
end
