searchData={"items":[{"type":"module","title":"Reality2Web","doc":"The entrypoint for defining your web interface, such\nas controllers, components, channels, and so on.\n\nThis can be used in your application as:\n\n    use Reality2Web, :controller\n    use Reality2Web, :html\n\nThe definitions below will be executed for every controller,\ncomponent, etc, so keep them short and clean, focused\non imports, uses and aliases.\n\nDo NOT define functions inside the quoted expressions\nbelow. Instead, define additional modules and import\nthose modules here.","ref":"Reality2Web.html"},{"type":"macro","title":"Reality2Web.__using__/1","doc":"When used, dispatch to the appropriate controller/view/etc.","ref":"Reality2Web.html#__using__/1"},{"type":"function","title":"Reality2Web.channel/0","doc":"","ref":"Reality2Web.html#channel/0"},{"type":"function","title":"Reality2Web.controller/0","doc":"","ref":"Reality2Web.html#controller/0"},{"type":"function","title":"Reality2Web.router/0","doc":"","ref":"Reality2Web.html#router/0"},{"type":"function","title":"Reality2Web.static_paths/0","doc":"","ref":"Reality2Web.html#static_paths/0"},{"type":"function","title":"Reality2Web.verified_routes/0","doc":"","ref":"Reality2Web.html#verified_routes/0"},{"type":"module","title":"Reality2Web.Endpoint","doc":"","ref":"Reality2Web.Endpoint.html"},{"type":"function","title":"Reality2Web.Endpoint.broadcast/3","doc":"","ref":"Reality2Web.Endpoint.html#broadcast/3"},{"type":"function","title":"Reality2Web.Endpoint.broadcast!/3","doc":"","ref":"Reality2Web.Endpoint.html#broadcast!/3"},{"type":"function","title":"Reality2Web.Endpoint.broadcast_from/4","doc":"","ref":"Reality2Web.Endpoint.html#broadcast_from/4"},{"type":"function","title":"Reality2Web.Endpoint.broadcast_from!/4","doc":"","ref":"Reality2Web.Endpoint.html#broadcast_from!/4"},{"type":"function","title":"Reality2Web.Endpoint.call/2","doc":"","ref":"Reality2Web.Endpoint.html#call/2"},{"type":"function","title":"Reality2Web.Endpoint.child_spec/1","doc":"Returns the child specification to start the endpoint\nunder a supervision tree.","ref":"Reality2Web.Endpoint.html#child_spec/1"},{"type":"function","title":"Reality2Web.Endpoint.config/2","doc":"Returns the endpoint configuration for `key`\n\nReturns `default` if the key does not exist.","ref":"Reality2Web.Endpoint.html#config/2"},{"type":"function","title":"Reality2Web.Endpoint.config_change/2","doc":"Reloads the configuration given the application environment changes.","ref":"Reality2Web.Endpoint.html#config_change/2"},{"type":"function","title":"Reality2Web.Endpoint.host/0","doc":"Returns the host for the given endpoint.","ref":"Reality2Web.Endpoint.html#host/0"},{"type":"function","title":"Reality2Web.Endpoint.init/1","doc":"","ref":"Reality2Web.Endpoint.html#init/1"},{"type":"function","title":"Reality2Web.Endpoint.local_broadcast/3","doc":"","ref":"Reality2Web.Endpoint.html#local_broadcast/3"},{"type":"function","title":"Reality2Web.Endpoint.local_broadcast_from/4","doc":"","ref":"Reality2Web.Endpoint.html#local_broadcast_from/4"},{"type":"function","title":"Reality2Web.Endpoint.path/1","doc":"Generates the path information when routing to this endpoint.","ref":"Reality2Web.Endpoint.html#path/1"},{"type":"function","title":"Reality2Web.Endpoint.script_name/0","doc":"Generates the script name.","ref":"Reality2Web.Endpoint.html#script_name/0"},{"type":"function","title":"Reality2Web.Endpoint.server_info/1","doc":"Returns the address and port that the server is running on","ref":"Reality2Web.Endpoint.html#server_info/1"},{"type":"function","title":"Reality2Web.Endpoint.start_link/1","doc":"Starts the endpoint supervision tree.\n\nAll other options are merged into the endpoint configuration.","ref":"Reality2Web.Endpoint.html#start_link/1"},{"type":"function","title":"Reality2Web.Endpoint.static_integrity/1","doc":"Generates a base64-encoded cryptographic hash (sha512) to a static file\nin `priv/static`. Meant to be used for Subresource Integrity with CDNs.","ref":"Reality2Web.Endpoint.html#static_integrity/1"},{"type":"function","title":"Reality2Web.Endpoint.static_lookup/1","doc":"Returns a two item tuple with the first item being the `static_path`\nand the second item being the `static_integrity`.","ref":"Reality2Web.Endpoint.html#static_lookup/1"},{"type":"function","title":"Reality2Web.Endpoint.static_path/1","doc":"Generates a route to a static file in `priv/static`.","ref":"Reality2Web.Endpoint.html#static_path/1"},{"type":"function","title":"Reality2Web.Endpoint.static_url/0","doc":"Generates the static URL without any path information.\n\nIt uses the configuration under `:static_url` to generate\nsuch. It falls back to `:url` if `:static_url` is not set.","ref":"Reality2Web.Endpoint.html#static_url/0"},{"type":"function","title":"Reality2Web.Endpoint.struct_url/0","doc":"Generates the endpoint base URL but as a `URI` struct.\n\nIt uses the configuration under `:url` to generate such.\nUseful for manipulating the URL data and passing it to\nURL helpers.","ref":"Reality2Web.Endpoint.html#struct_url/0"},{"type":"function","title":"Reality2Web.Endpoint.subscribe/2","doc":"","ref":"Reality2Web.Endpoint.html#subscribe/2"},{"type":"function","title":"Reality2Web.Endpoint.unsubscribe/1","doc":"","ref":"Reality2Web.Endpoint.html#unsubscribe/1"},{"type":"function","title":"Reality2Web.Endpoint.url/0","doc":"Generates the endpoint base URL without any path information.\n\nIt uses the configuration under `:url` to generate such.","ref":"Reality2Web.Endpoint.html#url/0"},{"type":"module","title":"Reality2Web.ErrorJSON","doc":"","ref":"Reality2Web.ErrorJSON.html"},{"type":"function","title":"Reality2Web.ErrorJSON.render/2","doc":"","ref":"Reality2Web.ErrorJSON.html#render/2"},{"type":"module","title":"Reality2Web.Reality2Controller","doc":"","ref":"Reality2Web.Reality2Controller.html"},{"type":"function","title":"Reality2Web.Reality2Controller.index/2","doc":"","ref":"Reality2Web.Reality2Controller.html#index/2"},{"type":"module","title":"Reality2Web.Router","doc":"","ref":"Reality2Web.Router.html"},{"type":"function","title":"Reality2Web.Router.browser/2","doc":"","ref":"Reality2Web.Router.html#browser/2"},{"type":"function","title":"Reality2Web.Router.call/2","doc":"Callback invoked by Plug on every request.","ref":"Reality2Web.Router.html#call/2"},{"type":"function","title":"Reality2Web.Router.init/1","doc":"Callback required by Plug that initializes the router\nfor serving web requests.","ref":"Reality2Web.Router.html#init/1"},{"type":"function","title":"Reality2Web.Router.reality2/2","doc":"","ref":"Reality2Web.Router.html#reality2/2"},{"type":"module","title":"Reality2Web.Schema","doc":"","ref":"Reality2Web.Schema.html"},{"type":"module","title":"Reality2Web.Schema.Enums","doc":"","ref":"Reality2Web.Schema.Enums.html"},{"type":"function","title":"Reality2Web.Schema.Enums.convert/2","doc":"","ref":"Reality2Web.Schema.Enums.html#convert/2"},{"type":"module","title":"Reality2Web.Schema.Sentant","doc":"","ref":"Reality2Web.Schema.Sentant.html"},{"type":"module","title":"Reality2Web.Schema.Types.Custom.JSON","doc":"The Json scalar type allows arbitrary JSON values to be passed in and out.\nRequires `{ :jason, \"~> 1.1\" }` package: https://github.com/michalmuskala/jason","ref":"Reality2Web.Schema.Types.Custom.JSON.html"},{"type":"module","title":"Reality2Web.Schema.Types.Custom.UUID4","doc":"The UUID4 scalar type allows UUID4 compliant strings to be passed in and out.\nRequires `{ :ecto, \">= 0.0.0\" }` package: https://github.com/elixir-ecto/ecto","ref":"Reality2Web.Schema.Types.Custom.UUID4.html"},{"type":"module","title":"Reality2Web.SentantResolver","doc":"","ref":"Reality2Web.SentantResolver.html"},{"type":"function","title":"Reality2Web.SentantResolver.all_sentants/3","doc":"","ref":"Reality2Web.SentantResolver.html#all_sentants/3"},{"type":"function","title":"Reality2Web.SentantResolver.create_sentant/3","doc":"","ref":"Reality2Web.SentantResolver.html#create_sentant/3"},{"type":"module","title":"Reality2Web.Telemetry","doc":"","ref":"Reality2Web.Telemetry.html"},{"type":"function","title":"Reality2Web.Telemetry.child_spec/1","doc":"Returns a specification to start this module under a supervisor.\n\nSee `Supervisor`.","ref":"Reality2Web.Telemetry.html#child_spec/1"},{"type":"function","title":"Reality2Web.Telemetry.metrics/0","doc":"","ref":"Reality2Web.Telemetry.html#metrics/0"},{"type":"function","title":"Reality2Web.Telemetry.start_link/1","doc":"","ref":"Reality2Web.Telemetry.html#start_link/1"}],"content_type":"text/markdown"}