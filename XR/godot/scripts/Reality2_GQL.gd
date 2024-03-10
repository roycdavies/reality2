extends Node

class_name Reality2
class GQL:
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Attributes
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	var _GQL: Node = null
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	
	
		
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Constructor
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _init(GQL_Node):
		_GQL = GQL_Node
	# --------------------------------------------------------------------------------------------------------------------------------------------------



	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Public Functions
	# --------------------------------------------------------------------------------------------------------------------------------------------------

	# ------------------------------
	# Useful
	# ------------------------------

	# Convert the name of a Sentant to its ID.  If a Sentant with that name does not exist, calls back with null, otherwise the ID.
	func byName(name: String, callback): _byName(name, callback)
		
	# ------------------------------
	# Queries
	# ------------------------------

	# Get a Sentant details
	func sentantGet(id = null, callback = null, details = "id name", passthrough = {}): _sentantGet(id, callback, details, passthrough)

	# Get all the Sentants on this Node
	func sentantAll(callback, details = "id name", passthrough = {}): _sentantAll(callback, details, passthrough)
		
	# ------------------------------
	# Mutations
	# ------------------------------

	# Load a new Sentant to the Node from the definition in YAML, TOML or JSON and return the given details.  Call the callback on success or error with
	# the returned data or the errors produced.
	func sentantLoad(definition = null, callback = null, details = "id name", passthrough = {}): _sentantLoad(definition, callback, details, passthrough)
		
	# Unload a sentant by its ID, and call the callback on success or failure with the returned data or the errors produced.
	func sentantUnload(id = null, callback = null, details = "id name", passthrough = {}): _sentantUnload(id, callback, details, passthrough)
		
	# ------------------------------
	# Subscriptions
	# ------------------------------

	# Set up a subscription to await a given signal from a given Sentant.  If the signal occurs, the callback function will be called.
	func awaitSignal(id = null, the_signal = null, callback = null, details = "id name", passthrough = {}): _awaitSignal(id, the_signal, callback, details, passthrough)
	# --------------------------------------------------------------------------------------------------------------------------------------------------



	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Private Functions
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	


	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _byName(name, callback):
		var body = """
			query SentantGet($name: String!) {
				sentantGet(name: $name) {
					id
				}
			}
		"""
		var variables = {
			"name": name
		}
		_GQL.query(body, _byName_response, variables, {}, {"callback": callback})
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _byName_response(data, passthrough):
		var response = []
		var errors = {}
		if (data.has("errors")):
			errors = data["errors"]
			if (passthrough.callback):
				passthrough.callback.call(null)
			else:
				print("ERROR: ", errors)
		else:
			response = data["data"]["sentantGet"]
			if (passthrough.callback):
				passthrough.callback.call(response.id)
			else:
				print ("SENTANT_GET: ", response)	
	# --------------------------------------------------------------------------------------------------------------------------------------------------



	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _sentantGet(id, callback, details, passthrough):
		var body = """
			query SentantGet(id: UUID4!) {
				sentantGet(id: id) {
					""" + details + """
				}
			}
		"""
		var variables = {
			"id": id
		}
		_GQL.query(body, _sentantGet_response, variables, {}, {"callback": callback, "passthrough": passthrough})
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _sentantGet_response(data, passthrough):
		var response = []
		var errors = {}
		if (data.has("errors")):
			errors = data["errors"]
			if (passthrough.callback):
				passthrough.callback.call(errors[0], passthrough.passthrough)
			else:
				print("ERROR: ", errors[0])
		else:
			response = data["data"]["sentantGet"]
			if (passthrough.callback):
				passthrough.callback.call(response, passthrough.passthrough)
			else:
				print ("SENTANT_GET: ", response)	
	# --------------------------------------------------------------------------------------------------------------------------------------------------



	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Get a list of all the Sentants
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _sentantAll(callback, details, passthrough):
		var body = 'query {  sentantAll { ' + details + ' } }'
		_GQL.query(body, _sentantAll_response, {}, {}, {"callback": callback, "passthrough": passthrough})
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _sentantAll_response(data, passthrough):
		var response = []
		var errors = {}
		if (data.has("errors")):
			errors = data["errors"]
			if (passthrough.callback):
				passthrough.callback.call(errors, passthrough.passthrough)
			else:
				print("ERROR: ", errors)
		else:
			response = data["data"]["sentantAll"]
			if (passthrough.callback):
				passthrough.callback.call(response, passthrough.passthrough)
			else:
				print ("SENTANT_ALL: ", response)
	# --------------------------------------------------------------------------------------------------------------------------------------------------



	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Load a Sentant to the Node from a definition string in YAML, TOML or JSON format.
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _sentantLoad(definition, callback, details, passthrough):
		var body = """
			mutation SentantLoad($definition: String!) {
				sentantLoad(definition: $definition) {
					""" + details + """
				}
			}
		"""
		var variables = {
			"definition": definition
		}
		_GQL.mutation(body, _sentantLoad_response, variables, {}, {"callback": callback, "passthrough": passthrough})
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _sentantLoad_response(data, passthrough):
		var response = []
		var errors = {}
		if (data.has("errors")):
			errors = data["errors"]
			if (passthrough.callback):
				passthrough.callback.call(errors[0], passthrough.passthrough)
			else:
				print("ERROR: ", errors[0])
		else:
			response = data["data"]["sentantLoad"]
			if (passthrough.callback):
				passthrough.callback.call(response, passthrough.passthrough)
			else:
				print ("SENTANT_LOAD: ", response)
	# --------------------------------------------------------------------------------------------------------------------------------------------------



	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Unload a Sentant by ID
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _sentantUnload(id, callback, details, passthrough):
		if id:
			var body = """
				mutation SentantUnload($id: UUID4!) {
					sentantUnload(id: $id) {
						name
					}
				}
			"""
			var variables = {
				"id": id
			}
			_GQL.mutation(body, _sentantUnload_response, variables, {}, {"callback": callback, "passthrough": passthrough})
		else:
			if callback:
				callback.call({"message":"invalid id"})
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _sentantUnload_response(data, passthrough):
		var response = []
		var errors = {}
		if (data.has("errors")):
			errors = data["errors"]
			if passthrough.callback:
				passthrough.callback.call(errors[0], passthrough.passthrough)
			else:
				print("ERROR: ", errors[0])
		else:
			response = data["data"]["sentantUnload"]
			if passthrough.callback:
				passthrough.callback.call(response, passthrough.passthrough)
			else:
				print ("SENTANT_UNLOAD: ", response)
	# --------------------------------------------------------------------------------------------------------------------------------------------------



	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Subscription: awaitSignal
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _awaitSignal(id, the_signal, callback, details, passthrough):
		var body = """
			subscription AwaitSignal($id: UUID4!, $signal: String!) {
				awaitSignal(id: $id, signal: $signal) {
					parameters
					event
				}
			}
		"""
		var variables = {
			"signal": the_signal,
			"id": id
		}
		_GQL.subscription(body, awaitSignal_callback, variables, {}, {"callback": callback, "details": details, "passthrough": passthrough})
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func awaitSignal_callback(data, passthrough):
		var response = []
		var errors = {}
		if (data.has("errors")):
			errors = data["errors"]
			if passthrough.callback:
				passthrough.callback.call(errors[0], passthrough.passthrough)
			else:
				print("ERROR: ", errors[0])
		else:
			response = data["data"]["awaitSignal"]
			if passthrough.callback:
				passthrough.callback.call(response, passthrough.passthrough)
			else:
				print ("AWAIT_SIGNAL: ", response)
	# --------------------------------------------------------------------------------------------------------------------------------------------------
		
