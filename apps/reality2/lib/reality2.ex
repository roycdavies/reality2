defmodule Reality2 do
  @moduledoc """
  Reality2 Sentient Agent (Sentant) based Platform with plugin architecture for Intuitive Spatial Computing supporting Assistive Technologies.

  This is the main module for managing Sentants, Swarms and Plugins on a Node and in a Cluster.
  Primarily, this will not be accessed directly, but rather through the GraphQL API [Reality2.Web](../reality2_web/api-reference.html).

  **Plugins**
  - [ai.reality2.vars](../ai_reality2_vars/AiReality2Vars.html) - A plugin for managing variables on a Sentant.
  - [ai.reality2.geospatial](../ai_reality2_geospatial/AiReality2Geospatial.html) - Coming Soon - A plugin for managing geospatial location and context on a Sentant.
  - [ai.reality2.pathing](../ai_reality2_pathing/AiReality2Pathing.html) - Coming Soon - A plugin for managing pathing on a node, cluster and globally - interfaces with the Pathing Name System (PNS) and ensures Sentant Global Uniqueness and Addressability.

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
      plugins:
        - name: com.openai.api
          url: https://api.openai.com/v1/chat/completions
          headers:
            - key: "Content-Type"
              value: "application/json"
            - key: "Authorization"
              value: "Bearer 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"
          data:
            - model: "gpt-3.5-turbo-1106"
              messages:
                - role: "user"
                  content: {{message}}

          output:
            - key: "choices"
              value: "choices.0.message.content"
              event: "chatgpt_response"

          version: 0.1.0
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
            - from: "*"
              event: chatgpt
              to: ready
              actions:
                - plugin: com.openai.api
                  command: send
                  parameters:
                    message: What is the square root of pi?
                    event: chatgpt_response
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

  def test_explugin() do
    sentant_definition = """
    sentant:
      name: Ask Question
      description: This is a test sentant for ChatGPT Plugin
      plugins:
        - name: com.openai.api
          url: https://api.openai.com/v1/chat/completions
          headers:
            - "Content-Type": "application/json"
            - "Authorization": "Bearer #{System.get_env("OPENAI_API_KEY")}"
          body:
            - model: "gpt-3.5-turbo-1106"
              messages:
                - role: "user"
                  content: __message__

          output:
            - key: "choices"
              value: "choices.0.message.content"
              event: "chatgpt_response"

          version: 0.1.0
      automations:
        - name: switch
          description: This is a test automation.
          transitions:
            - from: start
              event: init
              to: ready
            - from: "*"
              event: chatgpt
              to: ready
              actions:
                - plugin: com.openai.api
                  command: send
                  parameters:
                    message: What is the square root of pi?
    """
    IO.puts("Sentant Definition = #{inspect(sentant_definition)}")
    {:ok, id} = Reality2.Sentants.create(sentant_definition)
    Reality2.Sentants.sendto(%{:name => "Ask Question"}, %{event: "chatgpt", parameters: %{message: "What is the square root of pi?"}})
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
  # Private Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @doc false
  defp sendto(sentant_id, plugin_name, command_and_parameters) do
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
