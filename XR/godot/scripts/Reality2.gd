extends RigidBody3D

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Public Variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
@export_group("GraphQL")
@export var GQL: Node

@export_group("Debugging Parameters")
## Debug Mode
@export var debug : bool = false
## Number of Nodes and the number of Sentants in each Node
@export var numNodes : int = 5
@export var numSentantsInNode : int = 10
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Private Variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var node_scene = preload("res://scenes/R2Node.tscn")
var angularVelocity = Vector3(0,0,0)
var this_node

# A Sentant to give us information about the internal goings on of the node.
var monitoringSentant = {
	"sentant": {
		"name": "monitor",
		"automations": [
			{
				"name": "Monitor",
				"transitions": [
					{
						"from": "start",
						"to": "idle",
						"event": "init"
					},
					{
						"from": "idle",
						"to": "idle",
						"event": "__internal",
						"actions": [
							{
								"command": "signal",
								"parameters": {
									"event": "internal"
								}
							}
						]
					}
				]
			}
		]
	}
}
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Get a list of all the Sentants
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantAll(callback, details: String = "id"):
	var body = 'query {  sentantAll { ' + details + ' } }'
	GQL.query(body, callback)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantAll_response(data, _passthrough={}):
	var response = []
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["sentantAll"]
		add_node(response)
		print ("SENTANT_ALL: ", response)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantLoad(callback, details: String = "id", definition = ""):
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
	GQL.query(body, callback, variables)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantLoad_response(data, _passthrough={}):
	var response = []
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["sentantLoad"]
		print ("SENTANT_LOAD: ", response)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantUnload(name):
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
	GQL.query(body, sentantUnload_step2, variables)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantUnload_step2(data, _passthrough={}):
	var response = []
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["sentantGet"]
		print ("SENTANT_GET: ", response)
		var sentantid = response["id"]
		var body = """
			mutation SentantUnload($id: UUID4!) {
				sentantUnload(id: $id) {
					name
				}
			}
		"""
		var variables = {
			"id": sentantid
		}
		GQL.mutation(body, sentantUnload_step3, variables)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantUnload_step3(data, _passthrough={}):
	var response = []
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["sentantUnload"]
		print ("SENTANT_UNLOAD: ", response)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func awaitSignal(the_name, the_signal, callback):
	var body = """
		query SentantGet($name: String!) {
			sentantGet(name: $name) {
				id
			}
		}
	"""
	var variables = {
		"name": the_name
	}
	GQL.query(body, awaitSignal_step2, variables, {}, {"callback": callback, "signal": the_signal})
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func awaitSignal_step2(data, passthrough):
	var response = []
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["sentantGet"]
		print ("SENTANT_GET: ", response)
		var sentantid = response["id"]
		var body = """
			subscription AwaitSignal($id: UUID4!, $signal: String!) {
				awaitSignal(id: $id, signal: $signal) {
					parameters
					event
				}
			}
		"""
		var variables = {
			"signal": passthrough.signal,
			"id": sentantid
		}
		GQL.subscription(body, awaitSignal_step3, variables, {}, passthrough)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func awaitSignal_step3(data, passthrough):
	var response = []
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["awaitSignal"]
		print ("AWAIT_SIGNAL: ", response)
		passthrough.callback.call(response)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Called when the node enters the scene tree for the first time.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _ready():
	if debug:
		for i in range(0, numNodes):
			add_node()
	else:
		sentantUnload("monitor")
		sentantLoad(sentantLoad_response, "id name", JSON.stringify(monitoringSentant))
		sentantAll(sentantAll_response, "description id name")
		awaitSignal("monitor", "internal", _monitor)
		
func _monitor(data = {}):
	if (data.has("event")):
		if (data.parameters.has("event")):
			if (data.parameters.event == "created"):
				this_node.add_sentant(data.parameters.name)
			elif (data.parameters.event == "deleted"):
				this_node.remove_sentant(data.parameters.name)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Called every frame. 'delta' is the elapsed time since the previous frame.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _process(delta):
	# Add a bit of gentel motion
	angularVelocity = Useful.gentle_twist(delta, angularVelocity)
	rotation = rotation + angularVelocity
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Add a Node
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func add_node(sentants = []):
	this_node = node_scene.instantiate()
	if(debug):
		this_node.numSentants = numSentantsInNode
	else:
		this_node.load_sentants(sentants)
		
	this_node.name = "this"
	add_child(this_node)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
