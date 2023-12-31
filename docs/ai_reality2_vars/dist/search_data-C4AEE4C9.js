searchData={"items":[{"type":"module","title":"AiReality2Vars","doc":"The Reality2 Node Plugin `ai.reality2.vars` as it is written in the Sentant description YAML files.\n\nNaming restrictions require that underscores are used in the App filenames, so in this case `ai_reality2_vars`.\n\nTo create a Plugin App such as this for a Reality2 Node, follow the general instructions for creating a new App under an umbrella project.\nMake sure the app created is a supervised app, ie created using the `--sup` option.\n\n```bash\nmix new apps/ai_reality2_vars --sup\n```\n\nThe app name should match the plugin name, ie `ai_reality2_vars` for the plugin `ai.reality2.vars`, which is then called AiReality2Vars as the App Alias used inside the code.\n\nBy convention, we are using a reverse domain name hence: `ai.reality2.vars`.\n\nThe plugin app must have `create`, `delete`, `whereis` and `sendto` functions in the `lib/ /main.ex` file, which are called to create and delete instances of the app for each Sentant,\nreturn a process ID for communication, and send a command and parameters to the App for the given Sentant.\nSee the `AiReality2Vars.Main` module for more details.","ref":"AiReality2Vars.html"},{"type":"module","title":"AiReality2Vars.Data","doc":"Manage key / value pairs for Sentants.\n\n  In this implementation, these are kept in memory using the state parameter of a GenServer, however, this could be a database or other persistent storage.\n\n  The GenServer for each Sentant is supervised by the `AiReality2Vars.Main` DynamicSupervisor.\n\n  For your own Reality2 Node Apps, you should follow this pattern, ie create a `GenServer` for each App, and define the `Main` module as specified.","ref":"AiReality2Vars.Data.html"},{"type":"module","title":"AiReality2Vars.Main","doc":"Module for managing the main supervisor tree for the `AiReality2Vars` App.\n\n  In this instance, the main supervisor is a DynamicSupervisor, which is used to manage the Data stores for each Sentant,\n  however, this could be a PartitionSupervisor, or indeed, just a single process for all Sentants.\n\n  Use this as a template for your own Main module for your own Apps.  The `create`, `delete`, `sendto` and `whereas` functions must be implemented.\n\n  **Author**\n  - Dr. Roy C. Davies\n  - [roycdavies.github.io](https://roycdavies.github.io/)","ref":"AiReality2Vars.Main.html"},{"type":"function","title":"AiReality2Vars.Main.create/1","doc":"Create a new Data store, returning {:ok} or an appropriate error.\n\nThis creates a new child for each Sentant where the id is passed in and has the App name appended.\n\n- Parameters\n  - `id` - The id of the Sentant for which the Data store is being created.","ref":"AiReality2Vars.Main.html#create/1"},{"type":"function","title":"AiReality2Vars.Main.delete/1","doc":"Delete a Data store, returning {:ok} or an appropriate error.\n\n- Parameters\n  - `id` - The id of the Sentant for which the Data store is being deleted.","ref":"AiReality2Vars.Main.html#delete/1"},{"type":"function","title":"AiReality2Vars.Main.sendto/2","doc":"Send a command to the Data store for the given Sentant id.\n\nDepending on the command, this might be a synchronous call or an asynchronous cast.  Ideally, use this convention when creating your own Apps\nso that the commands are consistent across all Apps.\n\n- Parameters\n  - `id` - The id of the Sentant for which the command is being sent.\n  - `command` - A map containing the command and parameters to be sent.\n\n- Returns\n  - `{:ok}` - If the command was sent successfully.\n  - `{:error, :unknown_command}` - If the command was not recognised.","ref":"AiReality2Vars.Main.html#sendto/2"},{"type":"function","title":"AiReality2Vars.Main.whereis/1","doc":"Return the process id that can be used for subsequent communications.\n\nIn this implementation, each Sentant gets its own Data store GenServer process, but other Apps might just use a single process for all Sentants.\nExternally this is transparent.\n\n- Parameters\n  - `id` - The id of the Sentant for which process id is being returned.","ref":"AiReality2Vars.Main.html#whereis/1"}],"content_type":"text/markdown"}