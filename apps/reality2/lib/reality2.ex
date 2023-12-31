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
        - name: switch
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
    metadata = sendto(id, "ai.reality2.vars", %{command: "all"})

    IO.puts("Metadata = #{inspect(metadata)}")

    {:ok, id}
  end

  @doc false
  def test_swarm() do
    swarm_definition = """
    swarm:
      sentants:
        - name: Light Switch
          description: This is a test sentant.
          automations:
          - name: switch
            description: This is a test automation.
            transitions:
              - from: start
                event: init
                to: off
              - from: off
                event: turn_on
                to: on
                actions:
                  - command: send
                    parameters:
                      to: Light Bulb
                      event: turn_on
                  - command: send
                    parameters:
                      event: turn_off
                      delay: 2000
              - from: on
                event: turn_off
                to: off
                actions:
                  - command: send
                    parameters:
                      to: Light Bulb
                      event: turn_off
                  - command: send
                    parameters:
                      event: turn_on
                      delay: 2000
              - from: "*"
                event: stop
                to: off
                actions:
                  - command: send
                    parameters:
                      to: Light Bulb
                      event: turn_off
        - name: Light Bulb
          description: This is a light bulb sentant.
          automations:
          - name: bulb
            description: This is a test automation.
            transitions:
              - from: start
                event: init
                to: ready
              - from: "*"
                event: turn_on
                to: on
              - from: "*"
                event: turn_off
                to: off

    """

    swarm_definition2 = """
    swarm:
      sentants:
        - name: Light Switch
          description: This is a test sentant.
          automations:
          - name: switch2
            description: This is a test automation.
            transitions:
              - from: start
                event: init
                to: off
              - from: off
                event: turn_on
                to: on
                actions:
                  - command: send
                    parameters:
                      to: Light Bulb
                      event: turn_on
                  - command: send
                    parameters:
                      event: turn_off
                      delay: 2000
              - from: on
                event: turn_off
                to: off
                actions:
                  - command: send
                    parameters:
                      to: Light Bulb
                      event: turn_off
                  - command: send
                    parameters:
                      event: turn_on
                      delay: 2000
              - from: "*"
                event: stop
                to: off
                actions:
                  - command: send
                    parameters:
                      to: Light Bulb
                      event: turn_off
        - name: Light Bulb
          description: This is a light bulb sentant.
          automations:
          - name: bulb2
            description: This is a test2automation.
            transitions:
              - from: start
                event: init
                to: ready
              - from: "*"
                event: turn_on
                to: on
              - from: "*"
                event: turn_off
                to: off

    """
    [switch, bulb] = Reality2.Swarm.create(swarm_definition)

    {_, switch_id} = switch
    {_, bulb_id} = bulb
    Reality2.Sentants.sendto(%{:id => switch_id}, %{event: "turn_on"})

    switch_state = Reality2.Sentants.read(%{:id => switch_id}, :state)
    IO.puts("Switch State = #{inspect(switch_state)}")
    bulb_state = Reality2.Sentants.read(%{:id => bulb_id}, :state)
    IO.puts("Switch State = #{inspect(bulb_state)}")

    # Reality2.Sentants.sendto(%{:name => "Light Switch"}, %{event: "turn_off"})

    # switch_state = Reality2.Sentants.read(%{:id => switch_id}, :state)
    # IO.puts("Switch State = #{inspect(switch_state)}")
    # bulb_state = Reality2.Sentants.read(%{:id => bulb_id}, :state)
    # IO.puts("Switch State = #{inspect(bulb_state)}")

    Process.sleep(10000)
    Reality2.Sentants.sendto(%{:id => switch_id}, %{event: "stop"})

    Process.sleep(1000)
    [switch, bulb] = Reality2.Swarm.create(swarm_definition2)

    [switch, bulb]
  end

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Public Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec whereis(String.t(), String.t()) :: pid() | nil
  @doc """
  Run the whereis function in the Main module of the App with the given name, passing in the Sentant ID.

  This returns a Process ID for the plugin in the App for the given Sentant ID which can then be used for, say, a GenServer.call (or nil, if it doesn't exist).

  - Parameters
    - `sentant_id` - The id of the Sentant for which the plugin is being created.
    - `plugin_name` - The name of the App in which the plugin is defined.

  - Returns
    - `pid` - The Process ID of the plugin in the App for the given Sentant ID.
    - `nil` - If a proces for the Sentant for the plugin does not exist.

  - Example
    ```elixir
    Reality2.whereis("afb38d1e-a740-11ee-bca6-18c04dee389e", "ai.reality2.vars")
    ```
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def whereis(sentant_id, plugin_name) do
    plugin_name
    |> String.split(".")
    |> Enum.map(&String.capitalize/1)
    |> Enum.join("")
    |> Module.concat(String.to_atom("Main"))
    |> apply(:whereis, [sentant_id])
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec sendto(String.t(), String.t(), map()) :: {:ok} | any() | {:error, :command} | {:error, :plugin} | {:error, :existence} | {:error, :key}
  @doc """
  Send a command to the Main module of the App with the given name, passing in the Sentant ID.

  - Parameters
    - `sentant_id` - The id of the Sentant for which the command is being sent.
    - `plugin_name` - The name of the App in which the plugin is defined.
    - `command_and_parameters` - A map containing the command and parameters to be sent.

  - Returns
    - `{:ok}` - If the command was sent successfully or a result if it was a call.
    - `{:error, :command}` - If the command was not recognised.
    - `{:error, :plugin}` - If the plugin was not recognised.
    - `{:error, :existence}` - If the Sentant with that ID does not exist.
    - `{:error, :key}` - If the key does not exist.

  - Example
    ```elixir
    Reality2.sendto("afb38d1e-a740-11ee-bca6-18c04dee389e", "ai.reality.vars", %{command: "set", parameters: %{key: "answer", value: 42}})

    result = Reality2.sendto("afb38d1e-a740-11ee-bca6-18c04dee389e", "ai.reality.vars", %{command: "get", parameters: %{key: "answer"}})
    IO.puts("Result = \#{inspect(result)}")

    # The output would be:
    # Result = 42
    ```
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def sendto(sentant_id, plugin_name, command_and_parameters) do
    try do
      plugin_name
      |> String.split(".")
      |> Enum.map(&String.capitalize/1)
      |> Enum.join("")
      |> Module.safe_concat(String.to_atom("Main"))
      |> case do
        nil -> {:error, :plugin}
        module_name -> apply(module_name, :sendto, [sentant_id, command_and_parameters])
      end
    rescue _ ->
      {:error, :plugin}
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
