searchData={"items":[{"type":"module","title":"Reality2","doc":"Reality2 Sentient Agent (Sentant) based Platform with plugin architecture for Intuitive Spatial Computing supporting Assistive Technologies.\n\nThis is the main module for managing Sentants, Swarms and Plugins on a Node and in a Cluster.\nPrimarily, this will not be accessed directly, but rather through the GraphQL API [Reality2.Web](../reality2_web/api-reference.html).\n\n**Plugins**\n- [ai.reality2.vars](../ai_reality2_vars/AiReality2Vars.html) - A plugin for managing variables on a Sentant.\n- [ai.reality2.geospatial](../ai_reality2_geospatial/AiReality2Geospatial.html) - Coming Soon - A plugin for managing geospatial location and context on a Sentant.\n- [ai.reality2.pathing](../ai_reality2_pathing/AiReality2Pathing.html) - Coming Soon - A plugin for managing pathing on a node, cluster and globally - interfaces with the Pathing Name System (PNS) and ensures Sentant Global Uniqueness and Addressability.\n\n**Author**\n- Dr. Roy C. Davies\n- [roycdavies.github.io](https://roycdavies.github.io/)","ref":"Reality2.html"},{"type":"module","title":"Reality2.Sentants","doc":"Module for creating and managing Sentants, and the DynamicSupervisor that manages them.\n\n  When a Sentant is created, it is given a unique ID, and a name.  The name is unique on the node, but not in the world.\n  Upon creation, the Sentant is sent the init event, which is handled by the Sentant's Automations.\n\n  If a Sentant with the same name or ID already exists on the Node, then it is not created again, but redefined and restarted.\n\n  Sentants are immutable, so they cannot be changed once created.  To change a Sentant, it must be reloaded.\n  However, some data in plugins may change (such as `Reality2.AiReality2Vars`), and this is handled by the plugin itself.\n  Further, the state(s) of the Sentant Automations can change.\n\n  **Author**\n  - Dr. Roy C. Davies\n  - [roycdavies.github.io](https://roycdavies.github.io/)","ref":"Reality2.Sentants.html"},{"type":"function","title":"Reality2.Sentants.create/1","doc":"Create a new Sentant and return the result of the operation with the pid of the new Sentant, or an appropriate error.\n\n**Parameters**\n- `definition` - A string containing the definition of the Sentant to be created in YAML format.\n- (or) 'definition_map' - A map as created from a YAML definition.\n\n**Returns**\n- `{:ok, id}` - The Sentant was created.\n- `{:error, :definition}` if the definition is invalid.","ref":"Reality2.Sentants.html#create/1"},{"type":"function","title":"Reality2.Sentants.delete/1","doc":"Delete a Sentant and return the result of the operation with the pid of the deleted Sentant, or an appropriate error.\n\n- Parameters\n  - `name_or_uuid` - The name or ID of the Sentant to be deleted as a map containing either a `:name` or `:id` key.\n\n- Returns\n  - `{:ok, id}` - The Sentant was deleted.\n  - `{:error, :name}` if the Sentant with that name does not exist on this node.\n  - `{:error, :id}` if the Sentant with that ID does not exist on this node.\n  - `{:error, :existance}` if the Sentant with that ID or name does not exist on this node.\n\n- Example\n```elixir\nReality2.Sentants.delete(%{:name => \"my_sentant\"})\n```","ref":"Reality2.Sentants.html#delete/1"},{"type":"function","title":"Reality2.Sentants.read/2","doc":"Read something from an existing Sentant - determined by the command.  The result will depend on the command.\n\n- Parameters\n  - `name_or_uuid` - The name or ID of the Sentant to be read from as a map containing either a `:name` or `:id` key.\n  - `command` - The command to be executed on the Sentant, which must be either `:state` or `:definition`.\n\n- Returns\n  - `{:ok, definition}` - The Sentant was read.\n  - `{:error, :existance}` if the Sentant with that ID or name does not exist on this node.\n  - `{:error, :invalid}` if the parameter is invalid.\n\n- Example\n```elixir\ncase Reality2.Sentants.read(%{:name => \"my_sentant\"}, :definition) do\n  {:ok, definition} ->\n    # Do something with the definition\n  {:error, :existance} ->\n    # Sentant does not exist\n  {:error, :invalid} ->\n    # Invalid parameter\nend\n\nReality2.Sentants.read(%{id: \"123e4567-e89b-12d3-a456-426614174000\"}, :state)\n```","ref":"Reality2.Sentants.html#read/2"},{"type":"function","title":"Reality2.Sentants.sendto/2","doc":"Send a message to the named Sentant if it exists and return the result of the operation, or an appropriate error. This is an asynchronous operation.\n\n- Parameters\n  - `name_or_uuid` - The name or ID of the Sentant to have the message sent to as a map containing either a `:name` or `:id` key.\n  - `message` - The message to be sent, which must contain a `:command` string and optionally a `:parameters` map, and a `:passthrough` map.\n\n- Returns\n  - `{:ok}` - The message was sent.\n  - `{:error, :name}` if the Sentant with that name does not exist on this node.\n  - `{:error, :id}` if the Sentant with that ID does not exist on this node.\n  - `{:error, :existance}` if the Sentant with that ID or name does not exist on this node.\n\n- Example\n```elixir\nReality2.Sentants.sendto(%{:name => \"my_sentant\"}, %{event: \"turn_on\", delay: 1000})\n```","ref":"Reality2.Sentants.html#sendto/2"},{"type":"function","title":"Reality2.Sentants.sendto_all/1","doc":"Send a message to all Sentants.  This is an asynchronous operation, so the result is always `{:ok}`.\n\n- Parameters\n  - `message` - The message to be sent, which must contain a `:command` string and optionally a `:parameters` map, and a `:passthrough` map.\n\n- Returns\n  - `{:ok, num_sentants}` - The number of Sentants that the message was sent to.\n\n- Example\n```elixir\nReality2.Sentants.sendto_all(%{event: \"turn_on\"})\n```","ref":"Reality2.Sentants.html#sendto_all/1"},{"type":"opaque","title":"Reality2.Sentants.sentant_definition/0","doc":"The definition of a Sentant is a string containing the definition of the Sentant in YAML format.  See the definition of `YAML.Sentant`.","ref":"Reality2.Sentants.html#t:sentant_definition/0"},{"type":"type","title":"Reality2.Sentants.sentant_name_or_uuid/0","doc":"Each Sentant can be referred to by either its name or its ID.  The name of a Sentant is unique on the node, but not in the world.\nThis is used in pathing.","ref":"Reality2.Sentants.html#t:sentant_name_or_uuid/0"},{"type":"module","title":"Reality2.Swarm","doc":"Module for creating and managing Swarms on a Node.  A Swarm is a collection of Sentants that are managed together.\n\n  **Author**\n  - Dr. Roy C. Davies\n  - [roycdavies.github.io](https://roycdavies.github.io/)","ref":"Reality2.Swarm.html"},{"type":"function","title":"Reality2.Swarm.create/1","doc":"Create a new Swarm on the Node, returning {:ok} or an appropriate error.\n\nUploading a Swarm creates the Sentants on the Node, and then sends the init event to each Sentant.\nIf Sentants with the same name or ID already exist on the Node, then they are not created again, but redefined and restarted.\n\n- Parameters\n  - `swarm_definition` - A map containing the definition of the Swarm, or a string containing the YAML definition of the Swarm.","ref":"Reality2.Swarm.html#create/1"},{"type":"module","title":"Reality2.Types","doc":"This is a Sentant definition template.  Follow this guide to define a Sentant Template or a specific Sentant.\n  - NOTE: This is a work in progress and is subject to change.\n  - Version: 0.0.1\n  - Date: 2023.11.18\n\n  **Author**\n  - Dr. Roy C. Davies\n  - [roycdavies.github.io](https://roycdavies.github.io/)","ref":"Reality2.Types.html"},{"type":"type","title":"Reality2.Types.action/0","doc":"The action definition of an automation.\n\n#","ref":"Reality2.Types.html#t:action/0"},{"type":"type","title":"YAML - Reality2.Types.action/0","doc":"```yaml\naction:\n  # Optional plugin to use for this action\n  plugin: !plugin\n\n  # The command to execute\n  command: !str\n\n  # The parameters of the command in JSON format\n  parameters: { !str: !json }\n```","ref":"Reality2.Types.html#t:action/0-yaml"},{"type":"type","title":"Reality2.Types.author/0","doc":"A description of who made or owns the Sentant.\n\n#","ref":"Reality2.Types.html#t:author/0"},{"type":"type","title":"YAML - Reality2.Types.author/0","doc":"```yaml\nauthor:\n  # The author's ID\n  id: !guid\n\n  # The author's name\n  name: !str\n\n  # The author's email\n  email: !str\n```","ref":"Reality2.Types.html#t:author/0-yaml"},{"type":"type","title":"Reality2.Types.automation/0","doc":"A sentant automation definition.\n\nEach Sentant has a default Automation that determines it's current status, such as active and shadow, and which is preprogrammed to handle the `init, join_cluster, leave_cluster, connect, disconnect, go_shadow, go_active, shadows, active` and `transfer` events, and change the Sentant state accordingly.\nTo access the current state of that default Automation, use the automation name \"_\" (underscore).\n\n#","ref":"Reality2.Types.html#t:automation/0"},{"type":"type","title":"YAML - Reality2.Types.automation/0","doc":"```yaml\nautomation:\n  # The name of the automation (must be unique within the Sentant)\n  name: !str\n\n  # The description of the automation\n  description: !str\n\n  # The transitions of this automation\n  transitions: [ !transition ]\n```","ref":"Reality2.Types.html#t:automation/0-yaml"},{"type":"type","title":"Reality2.Types.input_event/0","doc":"An event definition sent to a Sentant.\n\n#","ref":"Reality2.Types.html#t:input_event/0"},{"type":"type","title":"YAML - Reality2.Types.input_event/0","doc":"```yaml\ninput_event:\n  # The name of the event (to match the transition event)\n  event: !str\n\n  # The parameters of the event in JSON format\n  parameters: { !str: !json }\n```","ref":"Reality2.Types.html#t:input_event/0-yaml"},{"type":"type","title":"Reality2.Types.plugin/0","doc":"Plugins are used to add functionality to Sentants beyond the built-in actions.\n\nThe name is used to specify exactly how the plugin works and is defined\nseparately in a plugin definition file.\n\n#","ref":"Reality2.Types.html#t:plugin/0"},{"type":"type","title":"YAML - Reality2.Types.plugin/0","doc":"```yaml\nplugin:\n  # The name of the plugin in reverse domain notation, eg: ai.reality2.storage\n  name: !str\n\n  # The version of the plugin, eg: \"0.1.0\"\n  version: !str\n```","ref":"Reality2.Types.html#t:plugin/0-yaml"},{"type":"type","title":"Reality2.Types.sentant/0","doc":"The definition of a Sentant.\n\n#","ref":"Reality2.Types.html#t:sentant/0"},{"type":"type","title":"YAML - Reality2.Types.sentant/0","doc":"```yaml\nsentant:\n  # The guid of the Sentant - for a Sentant Template, this is left empty\n  # Loading a Sentant with an empty guid will cause a new Sentant to be created\n  # If a guid is provided, the Sentant will be loaded and checked for uniqueness before being allowed\n  # to function\n  id: !guid\n\n  # The name of the Sentant - best to keep short and simple as it will become part of the pathing.\n  # Avoid using spaces or special characters.\n  # The dot '.' character has a special meaning in the pathing, so cannot be used in the name.\n  # The name is case insensitive, so \"MySentant\" and \"mysentant\" are the same.\n  # New Sentants with the same name on a Node will have a unique 6 digit hexcode hash appended to the\n  # end of the name to ensure uniqueness on the Node\n  name: !str\n\n  # The version of the Sentant, eg: \"0.1.0\".  When used for a Sentant Template, this is the minimum\n  # version of Sentant that can be used with this template.\n  # When used for a specific Sentant, this is the version of the Sentant.\n  version: !str\n\n  # The class of the Sentant - in reverse domain notation, eg: ai.reality2.sentant.default\n  class: !str\n\n  # The initial (immutable) data of the Sentant in key/value pairs of JSON formatted data\n  data: { !str: !json }\n\n  # The initial (immutable) binary data of the Sentant in key/value pairs of binary data, eg images,\n  # audio, video, etc\n  binary: { !str: !binary }\n\n  # A list of strings that may be used to group Sentants together in a Node\n  groups: [ !str ]\n\n  # A list of strings that may be used to search for Sentants in a Node\n  tags: [ !str ]\n\n  # The description of the Sentant\n  description: !str\n\n  # The author of the Sentant\n  author: !author\n\n  # Sentant automations\n  automations: [ !automation ]\n\n  # Current stored states of the Sentant (empty if Sentant Template)\n  states: [ !stored_state ]\n\n  # The status of the Sentant\n  status: !str\n```","ref":"Reality2.Types.html#t:sentant/0-yaml"},{"type":"type","title":"Reality2.Types.status/0","doc":"The current status of this Sentant on this node.\n\n#","ref":"Reality2.Types.html#t:status/0"},{"type":"type","title":"YAML - Reality2.Types.status/0","doc":"```yaml\n \"active\" | \"shadow\" | \"inactive\" | \"unchecked\" | \"unknown\"\n```","ref":"Reality2.Types.html#t:status/0-yaml"},{"type":"type","title":"Reality2.Types.stored_state/0","doc":"A stored state of an Automation.\n\n#","ref":"Reality2.Types.html#t:stored_state/0"},{"type":"type","title":"YAML - Reality2.Types.stored_state/0","doc":"```yaml\nstored_state:\n  # The name of the automation\n  name: !str\n\n  # The state of the automation\n  state: !str\n```","ref":"Reality2.Types.html#t:stored_state/0-yaml"},{"type":"type","title":"Reality2.Types.swarm/0","doc":"A group of Sentant Templates that work together to achieve a common goal.\n\n#","ref":"Reality2.Types.html#t:swarm/0"},{"type":"type","title":"YAML - Reality2.Types.swarm/0","doc":"```yaml\nswarm:\n  # The name of the swarm\n  name: !str\n\n  # The class of the swarm - in reverse domain notation, eg: ai.reality2.swarm.default\n  class: !str\n\n  # The description of the swarm\n  description: !str\n\n  # The author of the swarm\n  author: !author\n\n  # The version of the swarm, eg: \"0.1.0\"\n  version: !str\n\n  # The Sentant Templates that make up the swarm\n  sentants: [ !sentant ]\n```","ref":"Reality2.Types.html#t:swarm/0-yaml"},{"type":"type","title":"Reality2.Types.transition/0","doc":"An automation transition action definition.\n\nWhen an event is sent to a Sentant, the parameters are sent to the first action, and the result of that action is sent to the next action, and so on.\n\nParemeters passed in may be passed on to the next action, or may be modified, or new parameters may be added.\nNote that all automations start in the \"start\" state, and the first event sent is the \"init\" event.\nThere are some events that are reserved for the Node and Sentant system, and these are:\n\n** State Changing Events **\n- init - sent when the Sentant is first created (as above)\n- join_cluster - sent when the Node joins a cluster\n- leave_cluster - sent when the Node leaves a cluster\n- connect - sent when the Node the Sentant is on connects to another Node or Cluster (this is different from joining a cluster)\n- disconnect - sent when the Node the Sentant is on disconnects from another Node or Cluster (this is different from leaving a cluster)\n- go_shadow - sent when the Sentant is moved to shadow status\n- go_active - sent when the Sentant is moved to active status\n\n** Query Events **\n- get_shadows - returns a list of the paths or IP address and path to all (known) shadow Sentants\n- get_active - returns the path or IP address and path to the currently active Sentant\n- get_states - returns the current states of the Sentant\n\n** Other Events **\n- do_transfer - transfer control of the Sentant to one of the shadow Sentants (thus becoming a shadow Sentant itself and sending a go_shadow event)\n\n#","ref":"Reality2.Types.html#t:transition/0"},{"type":"type","title":"YAML - Reality2.Types.transition/0","doc":"```yaml\ntransition:\n  # State to match (* = any)\n  from: !str\n\n  # Event to match\n  event: !str\n\n  # New state to transition to (* = no change)\n  to: !str\n\n  # The actions to perform when transition occurs\n  actions: [ !action ]\n```","ref":"Reality2.Types.html#t:transition/0-yaml"},{"type":"opaque","title":"Reality2.Types.uuid/0","doc":"A universal ID.","ref":"Reality2.Types.html#t:uuid/0"},{"type":"module","title":"YAML.Sentant_example","doc":"Reality2 Swarm Definition representing a light and switch.\n\n#","ref":"YAML.Sentant_example.html"},{"type":"module","title":"YAML - YAML.Sentant_example","doc":"```yaml\nswarm:\n  # -----------------------------------------------------------------------------------------------\n  # An example swarm depicting a light and switch comprising two Sentants.\n  # -----------------------------------------------------------------------------------------------\n  name: A Light and Switch demo\n  class: ai.reality2.swarm.light_and_switch\n  version: 1.0.0\n  description: |\n    This swarm is an example of a light and a switch.\n    It is used in the Reality2 demo.\n  author:\n    - name: Reality2 Developer\n      email: dev@reality2.ai\n\n  # -----------------------------------------------------------------------------------------------\n  # The Sentants\n  # -----------------------------------------------------------------------------------------------\n  sentants:\n    # ---------------------------------------------------------------------------------------------\n    # A Switch that can either be on or off, and responds to events switch_on and switch_off.\n    # ---------------------------------------------------------------------------------------------\n    - name: Switch\n      description: This sentant represents a switch.\n      class: ai.reality2.default\n      version: 1.0.0\n      automations:\n        - name: Switch\n          transitions:\n            - from: start\n              to: \"off\"\n              event: init\n              actions:\n                - command: set\n                  parameters: { description: \"off\" }\n\n            - from: \"off\"\n              to: \"on\"\n              event: switch_on\n              actions:\n                - command: send\n                  parameters: { event: light_on, sentant: Light }\n                - command: set\n                  parameters: { description: \"on\" }\n\n            - from: \"on\"\n              to: \"off\"\n              event: switch_off\n              actions:\n                - command: send\n                  parameters: { event: light_off, sentant: Light }\n                - command: set\n                  parameters: { description: \"off\" }\n    # ---------------------------------------------------------------------------------------------\n\n    # ---------------------------------------------------------------------------------------------\n    # A Light that can either be on or off, and responds to events light_on and light_off.\n    # ---------------------------------------------------------------------------------------------\n    - name: Light\n      description: This sentant represents a light.\n      class: ai.reality2.default\n      version: 1.0.0\n      automations:\n        - name: Light\n          transitions:\n            - from: start\n              to: \"off\"\n              event: init\n              actions:\n                - command: set\n                  parameters: { description: \"off\" }\n\n            - from: \"off\"\n              to: \"on\"\n              event: light_on\n              actions:\n                - command: set\n                  parameters: { description: \"on\" }\n                - command: trigger\n                  parameters: { event: light_on }\n\n            - from: \"on\"\n              to: \"off\"\n              event: light_off\n              actions:\n                - command: set\n                  parameters: { description: \"off\" }\n                - command: trigger\n                  parameters: { event: light_off }\n    # ---------------------------------------------------------------------------------------------\n```","ref":"YAML.Sentant_example.html#module-yaml"},{"type":"extras","title":"Reality2","doc":"# Reality2\n\nExDocs documentation to be found in the docs folder.","ref":"readme.html"}],"content_type":"text/markdown"}