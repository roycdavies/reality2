defmodule Reality2.Types do
  @moduledoc """
  This is a Sentant definition template.  Follow this guide to define a Sentant Template or a specific Sentant.
    - NOTE: This is a work in progress and is subject to change.
    - Version: 0.0.1
    - Date: 2023.11.18

    **Author**
    - Dr. Roy C. Davies
    - [roycdavies.github.io](https://roycdavies.github.io/)
  """

  @typedoc """
  A universal ID.
  """
  @opaque uuid :: String.t()
  def uuid, do: ""

  @typedoc """
  A description of who made or owns the Sentant.

  ### YAML
  ```yaml
  author:
    # The author's ID
    id: !guid

    # The author's name
    name: !str

    # The author's email
    email: !str
  ```
  """
  @type author :: %{
    id: String.t,
    name: String.t,
    email: String.t
  }
  def author, do: {%{"id" => "", "name" => "", "email" => ""}, ["id", "name", "email"]}

  @typedoc """
  An event definition sent to a Sentant.

  ### YAML
  ```yaml
  input_event:
    # The name of the event (to match the transition event)
    event: !str

    # The parameters of the event in JSON format
    parameters: { !str: !json }
  ```
  """
  @type input_event :: %{
    event: String.t,
    parameters: map,
    passthrough: map
  }
  def input_event, do: {%{"event" => "", "parameters" => %{}, "passthrough" => %{}}, ["event"]}

  @typedoc """
  Plugins are used to add functionality to Sentants beyond the built-in actions.

  The name is used to specify exactly how the plugin works and is defined
  separately in a plugin definition file.

  ### YAML
  ```yaml
  plugin:
    # The name of the plugin in reverse domain notation, eg: ai.reality2.storage
    name: !str

    # The version of the plugin, eg: "0.1.0"
    version: !str
  ```
  """
  @type plugin :: %{
    name: String.t,
    url: String.t,
    method: String.t,
    headers: map,
    body: map,
    output: map,
    version: String.t
  }
  def plugin, do: {%{"name" => "", "url" => "", "version" => "", "method" => "", "headers" => %{}, "body" => %{}, "output" => %{}}, ["name", "url", "output"]}

  @typedoc """
  The action definition of an automation.

  ### YAML
  ```yaml
  action:
    # Optional plugin to use for this action
    plugin: !plugin

    # The command to execute
    command: !str

    # The parameters of the command in JSON format
    parameters: { !str: !json }
  ```
  """
  @type action :: %{
    plugin: plugin,
    command: String.t,
    parameters: map
  }
  def action, do: {%{"plugin" => plugin(), "command" => "", "parameters" => %{}}, ["command"]}

  @typedoc """
  An automation transition action definition.

  When an event is sent to a Sentant, the parameters are sent to the first action, and the result of that action is sent to the next action, and so on.

  Paremeters passed in may be passed on to the next action, or may be modified, or new parameters may be added.
  Note that all automations start in the "start" state, and the first event sent is the "init" event.
  There are some events that are reserved for the Node and Sentant system, and these are:

  ** State Changing Events **
  - init - sent when the Sentant is first created (as above)
  - join_cluster - sent when the Node joins a cluster
  - leave_cluster - sent when the Node leaves a cluster
  - connect - sent when the Node the Sentant is on connects to another Node or Cluster (this is different from joining a cluster)
  - disconnect - sent when the Node the Sentant is on disconnects from another Node or Cluster (this is different from leaving a cluster)
  - go_shadow - sent when the Sentant is moved to shadow status
  - go_active - sent when the Sentant is moved to active status

  ** Query Events **
  - get_shadows - returns a list of the paths or IP address and path to all (known) shadow Sentants
  - get_active - returns the path or IP address and path to the currently active Sentant
  - get_states - returns the current states of the Sentant

  ** Other Events **
  - do_transfer - transfer control of the Sentant to one of the shadow Sentants (thus becoming a shadow Sentant itself and sending a go_shadow event)

  ### YAML
  ```yaml
  transition:
    # State to match (* = any)
    from: !str

    # Event to match
    event: !str

    # New state to transition to (* = no change)
    to: !str

    # The actions to perform when transition occurs
    actions: [ !action ]
  ```
  """
  @type transition :: %{
    from: String.t,
    event: String.t,
    to: String.t,
    actions: [action]
  }
  def transition, do: {%{"from" => "", "event" => "", "to" => "", "actions" => [action()]}, ["from", "event", "to"]}

  @typedoc """
  A sentant automation definition.

  Each Sentant has a default Automation that determines it's current status, such as active and shadow, and which is preprogrammed to handle the `init, join_cluster, leave_cluster, connect, disconnect, go_shadow, go_active, shadows, active` and `transfer` events, and change the Sentant state accordingly.
  To access the current state of that default Automation, use the automation name "_" (underscore).

  ### YAML
  ```yaml
  automation:
    # The name of the automation (must be unique within the Sentant)
    name: !str

    # The description of the automation
    description: !str

    # The transitions of this automation
    transitions: [ !transition ]
  ```
  """
  @type automation :: %{
    name: String.t,
    description: String.t,
    transitions: [transition]
  }
  def automation, do: {%{"name" => "", "description" => "", "transitions" => [transition()]}, ["name", "transitions"]}

  @typedoc """
  A stored state of an Automation.

  ### YAML
  ```yaml
  stored_state:
    # The name of the automation
    name: !str

    # The state of the automation
    state: !str
  ```
  """
  @type stored_state :: %{
    name: String.t,
    state: String.t
  }
  def stored_state, do: {%{"name" => "", "state" => ""}, ["name", "state"]}

  @typedoc """
  The current status of this Sentant on this node.

  ### YAML
  ```yaml
  "active" | "shadow" | "inactive" | "unchecked" | "unknown"
  ```
  """
  @type status :: String.t
  def status, do: ""

  @typedoc """
  The definition of a Sentant.

  ### YAML
  ```yaml
  sentant:
    # The guid of the Sentant - for a Sentant Template, this is left empty
    # Loading a Sentant with an empty guid will cause a new Sentant to be created
    # If a guid is provided, the Sentant will be loaded and checked for uniqueness before being allowed
    # to function
    id: !guid

    # The name of the Sentant - best to keep short and simple as it will become part of the pathing.
    # Avoid using spaces or special characters.
    # The dot '.' character has a special meaning in the pathing, so cannot be used in the name.
    # The name is case insensitive, so "MySentant" and "mysentant" are the same.
    # New Sentants with the same name on a Node will have a unique 6 digit hexcode hash appended to the
    # end of the name to ensure uniqueness on the Node
    name: !str

    # The version of the Sentant, eg: "0.1.0".  When used for a Sentant Template, this is the minimum
    # version of Sentant that can be used with this template.
    # When used for a specific Sentant, this is the version of the Sentant.
    version: !str

    # The class of the Sentant - in reverse domain notation, eg: ai.reality2.sentant.default
    class: !str

    # The initial (immutable) data of the Sentant in key/value pairs of JSON formatted data
    data: { !str: !json }

    # The initial (immutable) binary data of the Sentant in key/value pairs of binary data, eg images,
    # audio, video, etc
    binary: { !str: !binary }

    # A list of strings that may be used to group Sentants together in a Node
    keywords: [ !str ]

    # A list of strings that may be used to search for Sentants in a Node
    tags: [ !str ]

    # The description of the Sentant
    description: !str

    # The author of the Sentant
    author: !author

    # Sentant automations
    automations: [ !automation ]

    # Current stored states of the Sentant (empty if Sentant Template)
    states: [ !stored_state ]

    # The status of the Sentant
    status: !str
  ```
  """
  @type sentant :: %{
    id: uuid,
    name: String.t,
    version: String.t,
    class: String.t,
    data: map,
    binary: map,
    tags: [String.t],
    keywords: [String.t],
    description: String.t,
    author: author,
    plugins: [plugin],
    automations: [automation],
    states: [stored_state],
    status: status
  }
  def sentant, do: {%{"id" => uuid(), "name" => "", "version" => "", "class" => "", "data" => %{}, "binary" => %{}, "tags" => [], "keywords" => [], "description" => "", "author" => author(), "plugins" => [plugin()], "automations" => [automation()], "states" => [stored_state()], "status" => status()}, ["name"]}

  @typedoc """
  A group of Sentant Templates that work together to achieve a common goal.

  ### YAML
  ```yaml
  swarm:
    # The name of the swarm
    name: !str

    # The class of the swarm - in reverse domain notation, eg: ai.reality2.swarm.default
    class: !str

    # The description of the swarm
    description: !str

    # The author of the swarm
    author: !author

    # The version of the swarm, eg: "0.1.0"
    version: !str

    # The Sentant Templates that make up the swarm
    sentants: [ !sentant ]
  ```
  """
  @type swarm :: %{
    name: String.t,
    class: String.t,
    description: String.t,
    author: author,
    version: String.t,
    sentants: [sentant]
  }
  def swarm, do: {%{"name" => "", "class" => "", "description" => "", "author" => author(), "version" => "", "sentants" => [sentant()]}, ["sentants"]}



  def validate(map, typedef) do
    required = validate1(map, typedef, []) |> List.flatten
    optional = validate2(map, typedef, []) |> List.flatten

    IO.puts("required: #{inspect(required)} optional: #{inspect(optional)}")
  end

  defp validate1(_, {_, []}, _), do: []
  defp validate1(map, {type, [item | rest]}, acc) do
    case Helpers.Map.get(map, item) do
      nil -> [item | acc]
      _ -> validate1(map, {type, rest}, acc)
    end
  end
  defp validate1(_, _, acc), do: acc



  defp validate2(data, {type, _required}, acc) when is_map(data) do
    IO.puts("data: #{inspect(data)}")
    # Get the keys in the data to be validated
    map_keys = Map.keys(data)

    Enum.reduce_while(map_keys, acc, fn key, new_acc ->
      child_type_definition = Helpers.Map.get(type, key)
      IO.puts("key: #{inspect(key)} child_type: #{inspect(child_type_definition)}")

      case child_type_definition do
        # This key is not part of the type, so ignore it
        nil -> {:cont, acc}

        # This key is part of the type and is a map, so validate it
        {child_type, required} ->
          case validate2(Helpers.Map.get(data, key), {child_type, required}, new_acc) do
            [] -> {:cont, new_acc}
            result -> {:halt, new_acc ++ result}
          end

        # The child type is a map of items, so validate each item
        [{child_type, required}] ->
          child_data = Helpers.Map.get(data, key)
          Enum.reduce_while(child_data, new_acc, fn child_data_element, new_acc2 ->
            case validate2(child_data_element, {child_type, required}, new_acc2) do
              [] -> {:cont, new_acc2}
              result -> {:halt, new_acc2 ++ result}
            end
          end)

        # Anything else is a scalar, so ignore it
        _ -> {:cont, new_acc}
      end
    end)
  end



  # -----------------------------------------------------------------------------------------------
  # Validate a data against a type definition
  # A type definition is a tuple of {type, required} where
  #  type is a map of key/value pairs where the key is the name of the field, and
  #  required is a list of keys that are required to be present
  # Either returns {:ok} or {:error, path}
  # where path is a dot seperated key-path to the field that is incorrect in the data, and indicates
  # either which required field is missing, or which field is not in the type definition
  # -----------------------------------------------------------------------------------------------
  # def validate(map, typedef) do
  #   case validate(map, typedef, "", 0) do
  #     "" -> {:ok}
  #     "." -> {:error, "type error"}
  #     path -> {:error, String.trim(path, ".")}
  #   end
  # end

  # defp validate(map, typedef, acc, depth) do
  #   # IO.puts("depth: #{inspect(depth)}")
  #   if (depth < 10) do
  #     # IO.puts("1")
  #     acc1 = validate_required(map, typedef, acc, depth+1)
  #     # IO.puts("2")
  #     acc2 = validate_existing(map, typedef, acc, depth+1)
  #     if acc1 == "" do
  #       if acc2 == "" do
  #         ""
  #       else
  #         acc2
  #       end
  #     else
  #       acc1
  #     end
  #   else
  #     "possible infinite loop"
  #   end
  # end


  # -----------------------------------------------------------------------------------------------
  # Validate that the required fields are present in the data
  # -----------------------------------------------------------------------------------------------
  # defp validate_required(data, typedef, acc, depth) when is_list(data) and is_list(typedef) do
  #   case typedef do
  #     [{type, required}] ->
  #       Enum.reduce_while(data, acc, fn data_element, new_acc ->
  #         case validate(data_element, {type, required}, "", depth) do
  #           "" -> {:cont, new_acc}
  #           path -> {:halt, "#{new_acc}#{path}"}
  #         end
  #       end)
  #     _scalar -> acc # TODO: test that the scalar types match
  #   end
  # end
  # defp validate_required(data, typedef, acc, _) when is_list(typedef) do
  #   acc <> "." <> "[]"
  # end

  # defp validate_required(data, typedef, acc, depth) when is_map(data) and is_tuple(typedef) do
  #   case typedef do
  #     {type, required} ->
  #       Enum.reduce_while(required, acc, fn key, new_acc ->
  #         case Helpers.Map.get(data, key) do
  #           nil -> {:halt, "#{new_acc}.#{key}"}
  #           data_child ->
  #             subtype = Helpers.Map.get(type, key)
  #             case validate(data_child, subtype, key, depth) do
  #               ^key -> {:cont, new_acc}
  #               path -> {:halt, "#{new_acc}.#{path}"}
  #             end
  #         end
  #       end)
  #     _scalar -> acc # TODO: test that the scalar types match
  #   end
  # end
  # defp validate_required(data, typedef, acc, _) when is_tuple(typedef) do
  #   acc <> "." <> "{}"
  # end
  # defp validate_required(_, _, acc, _), do: acc

  # -----------------------------------------------------------------------------------------------
  # Validate that the fields given in the data exist in the type defintion, and no others
  # -----------------------------------------------------------------------------------------------
  # defp validate_existing(data, [], acc, _) when is_list(data), do: acc
  # defp validate_existing(data, typedef, acc, depth) when is_list(data) and is_list(typedef) do
  #   case typedef do
  #     [{type, required}] ->
  #       Enum.reduce_while(data, acc, fn data_element, new_acc ->
  #         case validate(data_element, {type, required}, "", depth) do
  #           "" -> {:cont, new_acc}
  #           path -> {:halt, "#{new_acc}#{path}"}
  #         end
  #       end)
  #     _scalar -> acc # TODO: test that the scalar types match
  #   end
  # end
  # defp validate_existing(data, typedef, acc, _) when is_list(data) do
  #   acc <> "." <> "[]"
  # end

  # defp validate_existing(data, %{}, acc, _) when is_map(data), do: acc
  # defp validate_existing(data, typedef, acc, depth) when is_map(data) and is_tuple(typedef) do
  #   keys = Map.keys(data)
  #   case typedef do
  #     {type, required} ->
  #       Enum.reduce_while(keys, acc, fn key, new_acc ->
  #         case Helpers.Map.get(type, key) do
  #           nil -> {:halt, "#{new_acc}.#{key}"}
  #           subtype ->
  #             data_child = Helpers.Map.get(data, key)
  #             # IO.puts("key: #{inspect(key)}")
  #             case validate(data_child, subtype, key, depth) do
  #               ^key ->
  #                 # IO.puts("acc: #{inspect(acc)}")
  #                 {:cont, new_acc}
  #               path ->
  #                 # IO.puts("path: #{new_acc}.#{path}")
  #                 {:halt, "#{new_acc}.#{path}"}
  #             end
  #         end
  #       end)
  #     _scalar -> acc # TODO: test that the scalar types match
  #   end
  # end
  # defp validate_existing(data, typedef, acc, _) when is_map(data) do
  #   acc <> "." <> "{}"
  # end
  # defp validate_existing(_, _, acc, _), do: acc
  # -----------------------------------------------------------------------------------------------
end


defmodule YAML.Sentant_example do
@moduledoc """
Reality2 Swarm Definition representing a light and switch.

### YAML
```yaml
swarm:
  # -----------------------------------------------------------------------------------------------
  # An example swarm depicting a light and switch comprising two Sentants.
  # -----------------------------------------------------------------------------------------------
  name: A Light and Switch demo
  class: ai.reality2.swarm.light_and_switch
  version: 1.0.0
  description: |
    This swarm is an example of a light and a switch.
    It is used in the Reality2 demo.
  author:
    - name: Reality2 Developer
      email: dev@reality2.ai

  # -----------------------------------------------------------------------------------------------
  # The Sentants
  # -----------------------------------------------------------------------------------------------
  sentants:
    # ---------------------------------------------------------------------------------------------
    # A Switch that can either be on or off, and responds to events switch_on and switch_off.
    # ---------------------------------------------------------------------------------------------
    - name: Switch
      description: This sentant represents a switch.
      class: ai.reality2.default
      version: 1.0.0
      automations:
        - name: Switch
          transitions:
            - from: start
              to: "off"
              event: init
              actions:
                - command: set
                  parameters: { description: "off" }

            - from: "off"
              to: "on"
              event: switch_on
              actions:
                - command: send
                  parameters: { event: light_on, sentant: Light }
                - command: set
                  parameters: { description: "on" }

            - from: "on"
              to: "off"
              event: switch_off
              actions:
                - command: send
                  parameters: { event: light_off, sentant: Light }
                - command: set
                  parameters: { description: "off" }
    # ---------------------------------------------------------------------------------------------

    # ---------------------------------------------------------------------------------------------
    # A Light that can either be on or off, and responds to events light_on and light_off.
    # ---------------------------------------------------------------------------------------------
    - name: Light
      description: This sentant represents a light.
      class: ai.reality2.default
      version: 1.0.0
      automations:
        - name: Light
          transitions:
            - from: start
              to: "off"
              event: init
              actions:
                - command: set
                  parameters: { description: "off" }

            - from: "off"
              to: "on"
              event: light_on
              actions:
                - command: set
                  parameters: { description: "on" }
                - command: trigger
                  parameters: { event: light_on }

            - from: "on"
              to: "off"
              event: light_off
              actions:
                - command: set
                  parameters: { description: "off" }
                - command: trigger
                  parameters: { event: light_off }
    # ---------------------------------------------------------------------------------------------
```
"""
def __example, do: :ok
end
