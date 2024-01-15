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
            key: "Content-Type"
            value: "application/json"
            key: "Authorization"
            value: "Bearer #{System.get_env("OPENAI_API_KEY")}"
          body:
            model: "gpt-3.5-turbo-1106"
            messages:
              - role: "user"
                content: __message__

          output:
            key: "choices"
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

    {:ok, sentant_json} = returned_definition

    validation_result = Reality2.Types.validate(sentant_json, Reality2.Types.sentant)
    IO.puts("Validation Result = #{inspect(validation_result)}")

    IO.puts("Metadata = #{inspect(metadata)}")

    {:ok, id}
  end

  def test_explugin() do
    sentant_definition =
"""
sentant:
  name: Ask Question
  description: This is a test sentant for ChatGPT Plugin
  plugins:
    - name: com.openai.api
      url: https://api.openai.com/v1/chat/completions
      method: POST
      headers:
        "Content-Type": "application/json"
        "Authorization": "Bearer #{System.get_env("OPENAI_API_KEY")}"
      body:
        model: "gpt-3.5-turbo-1106"
        messages:
          - role: "system"
            content: "You are a helpful assistant."
          - role: "user"
            content: __message__
      output:
        key: chatgpt_says
        value: "choices.0.message.content"
        event: chatgpt_response
  automations:
    - name: ChatGPT
      description: This is a test automation.
      transitions:
        - from: start
          event: init
          to: ready
          # actions:
          #   - plugin: com.openai.api
          #     command: send
          #     parameters:
          #       message: "Translate this sentence into Italian: Hello, my name is John."
        - from: "*"
          event: chatgpt
          to: ready
          actions:
            - plugin: com.openai.api
              command: send
              parameters:
                message: "Translate this sentence into Swedish: Hello, my name is John."
        - from: "*"
          event: chatgpt_response
          to: ready
          actions:
            - command: set
              parameters:
                key: answer
                value: "The answer is: __chatgpt_says__ or __default__"
            - command: set
              parameters:
                key: answer2
                value: "__default__"
            - command: print
"""
    IO.puts("Sentant Definition = #{inspect(sentant_definition)}")
    {:ok, id} = Reality2.Sentants.create(sentant_definition)
    Reality2.Sentants.sendto(%{:name => "Ask Question"}, %{"event" => "chatgpt", "parameters" => %{"message" => "What is the square root of pi?", "default" => 10.3}, "passthrough" => %{"answer" => 42}})
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
    case Reality2.Swarm.create(swarm_definition) do
      {:error, reason} ->
        IO.puts("Swarm.create: ERROR = #{inspect(reason)}")
      {:ok, result} ->
        IO.puts("Swarm.create: result = #{inspect(result)}")
        [switch_id, bulb_id] = result[:sentants]

        Reality2.Sentants.sendto(%{:id => switch_id}, %{event: "turn_on"})

        switch_state = Reality2.Sentants.read(%{:id => switch_id}, :state)
        IO.puts("Switch State = #{inspect(switch_state)}")
        bulb_state = Reality2.Sentants.read(%{:id => bulb_id}, :state)
        IO.puts("Switch State = #{inspect(bulb_state)}")

        Process.sleep(10000)
        Reality2.Sentants.sendto(%{:id => switch_id}, %{event: "stop"})
    end

    # Reality2.Sentants.sendto(%{:name => "Light Switch"}, %{event: "turn_off"})

    # switch_state = Reality2.Sentants.read(%{:id => switch_id}, :state)
    # IO.puts("Switch State = #{inspect(switch_state)}")
    # bulb_state = Reality2.Sentants.read(%{:id => bulb_id}, :state)
    # IO.puts("Switch State = #{inspect(bulb_state)}")

    #Process.sleep(1000)
    #Reality2.Swarm.create(swarm_definition2)
  end


  def test_type_validation() do
    sentant_transition = %{"from" => "start", "to" => "ready", "actions" => [%{"command" => "set", "parameters" => %{"key" => "switch", "value" => 0}}]}

    validation_result = Reality2.Types.validate(sentant_transition, Reality2.Types.transition)
    IO.puts("Validation Result = #{inspect(validation_result)}")
  end



    @type test1 :: %{
      name: String.t,
      tester: test2
    }
    def test1, do: {%{"name" => "", "tester" => test2()}, ["name", "tester"]}

    @type test2 :: %{
      description: String.t,
      more: test3
    }
    def test2, do: {%{"description" => "", "more" => [test3()]}, [""]}

    @type test3 :: %{
      anumber: Integer,
      astring: String.t
    }
    def test3, do: {%{"anumber" => 0, "astring" => ""}, ["astring"]}

    @type test4 :: %{
      names: [String.t],
      something: test3
    }
    def test4, do: {%{"names" => [""], "something" => test3()}, ["names", "something"]}

    @type test5 :: %{
      stuff: String.t,
      something: test4
    }
    def test5, do: {%{"stuff" => "", "something" => test4()}, ["stuff", "something"]}

    @type test6 :: %{
      one: Number,
      two: test7
    }
    def test6, do: {%{"one" => 0, "two" => test7()}, ["one"]}

    @type test7 :: %{
      three: Number,
      four: test6
    }
    def test7, do: {%{"three" => 0, "four" => test6()}, ["four"]}


    def test_validate() do

      testdata1 = %{"name" => "Fred", "tester" => %{"description" => "A friend", "more" => [%{"anumber" => 42, "astring" => "Hello World"}]}}
      testdata2 = %{"name" => "Fred", "tester" => %{"description" => "A friend", "more" => [%{"anumber" => 42}]}}
      testdata3 = %{"name" => "Fred", "tester" => %{"description" => "A friend", "more" => [%{"number" => 42, "astring" => "Hello World"}]}}
      testdata4 = %{"name" => "Fred", "tester" => 23}
      testdata5 = %{"names" => ["Fred", "John"], "something" => %{"anumber" => 42, "astring" => "Hello World"}}
      testdata6 = %{"names" => ["Fred", "John"]}
      testdata7 = %{"names" => "Fred", "something" => %{"anumber" => 42, "astring" => "Hello World"}}
      testdata8 = %{"names" => ["Fred"], "something" => [%{"anumber" => 42, "astring" => "Hello World"}]}
      testdata9 = %{"names" => ["Fred"], "something" => %{"anumber" => 42, "astring" => "Hello World"}}
      testdata10 = %{"stuff" => "Hello World", "something" => %{"names" => ["Fred"], "something" => %{"anumber" => 42, "astring" => "Hello World"}}}

      testdata11 = %{"stuff" => "Hello World", "something" => %{"names" => "Fred", "something" => %{"anumber" => 42, "astring" => "Hello World"}}}
      testdata12 = %{"stuff" => "Hello World", "something" => %{"names" => ["Fred"], "something" => "Hello World"}}
      testdata13 = %{"one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{ "one" => 42, "two" => %{ "three" => 42, "four" => %{} }}}}}}}}}}}}}}}}}}}}}}}}


      result1 = Reality2.Types.validate(testdata1, test1()) # Should be OK
      IO.puts("Result 1 = #{inspect(result1)}")

      result1_1 = Reality2.Types.validate(testdata1, test2()) # Should be OK
      IO.puts("Result 1_1 = #{inspect(result1_1)}")

      result2 = Reality2.Types.validate(testdata2, test1()) # Should fail with {:error, "tester.more.astring"}
      IO.puts("Result 2 = #{inspect(result2)}")

      result3 = Reality2.Types.validate(testdata3, test1()) # Should fail with {:error, "tester.more.number"}
      IO.puts("Result 3 = #{inspect(result3)}")

      result4 = Reality2.Types.validate(testdata4, test1()) # Should fail with {:error, "tester.{}"}
      IO.puts("Result 4 = #{inspect(result4)}")

      result5 = Reality2.Types.validate(testdata5, test4()) # Should be OK
      IO.puts("Result 5 = #{inspect(result5)}")

      result6 = Reality2.Types.validate(testdata6, test4()) # Should be OK
      IO.puts("Result 6 = #{inspect(result6)}")

      result7 = Reality2.Types.validate(testdata7, test4()) # Should fail with {:error, "names.[]"}
      IO.puts("Result 7 = #{inspect(result7)}")

      result8 = Reality2.Types.validate(testdata8, test4()) # Should fail with {:error, "something.[]"}
      IO.puts("Result 8 = #{inspect(result8)}")

      result9 = Reality2.Types.validate(testdata9, test4()) # Should be OK
      IO.puts("Result 9 = #{inspect(result9)}")

      result10 = Reality2.Types.validate(testdata10, test5()) # Should be OK
      IO.puts("Result 10 = #{inspect(result10)}")

      result11 = Reality2.Types.validate(testdata11, test5()) # Should fail with {:error, "something.names.[]"}
      IO.puts("Result 11 = #{inspect(result11)}")

      result12 = Reality2.Types.validate(testdata12, test5()) # Should fail with {:error, "something.something.{}"}
      IO.puts("Result 12 = #{inspect(result12)}")

      result13 = Reality2.Types.validate(testdata13, test6()) # Should fail with {:error, "possible infinite loop"}

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
