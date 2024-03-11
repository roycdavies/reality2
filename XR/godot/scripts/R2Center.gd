# ======================================================================================================================================================
# R2 Center
# ---------
#
# The central sphere in the 3D world, from which all the Reality2 Nodes come out.  Each Reality2 Node gets its own GQL connection.
#
# Dr. Roy C.Davies
# roycdavies.github.io
# March 2024
# ======================================================================================================================================================

extends RigidBody3D

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Public Variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
@export_group("Reality2 Node")
@export var NodeNames = ["localhost"]
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Private Variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var _node_scene = preload("res://scenes/R2Node.tscn")
var _GQL_template = preload("res://scenes/GraphQL.tscn")
var _angularVelocity = Vector3(0,0,0)
var _reality2_nodes = {}
var R2GQL

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
# Called when the node enters the scene tree for the first time.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _ready():
	for nodeName in NodeNames:
		_reality2_nodes[nodeName] = {"r2gql": Reality2.GQL.new(), "node_visual": null}
		self.add_child(_reality2_nodes[nodeName].r2gql.GQL())
		
		_reality2_nodes[nodeName].r2gql.byName( "monitor", func (id): _reality2_nodes[nodeName].r2gql.sentantUnload(id) )
		_reality2_nodes[nodeName].r2gql.sentantLoad( JSON.stringify(monitoringSentant) )
		_reality2_nodes[nodeName].r2gql.sentantAll(func(sentants, passthrough): add_node(passthrough.name, sentants), "description id name", {"name": nodeName})
		_reality2_nodes[nodeName].r2gql.byName( "monitor", func(id): _reality2_nodes[nodeName].r2gql.awaitSignal(id, "internal", _monitor, "name id", {"name": nodeName}) )
	# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Monitor changes to the Node coming from the Monitor node created above.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _monitor(data = {}, passthrough = {}):
	print ("_MONITOR", data, passthrough)
	if (data.has("event")):
		if (data.parameters.has("event")):
			if (data.parameters.event == "created"):
				_reality2_nodes[passthrough.name].node_visual.add_sentant(data.parameters.name)
			elif (data.parameters.event == "deleted"):
				_reality2_nodes[passthrough.name].node_visual.remove_sentant(data.parameters.name)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Called every frame. 'delta' is the elapsed time since the previous frame.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _process(delta):
	# Add a bit of gentle motion
	_angularVelocity = Useful.gentle_twist(delta, _angularVelocity)
	rotation = rotation + _angularVelocity
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Add a Node
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func add_node(the_name: String, sentants = []):
	for nodeName in NodeNames:
		_reality2_nodes[nodeName].node_visual = _node_scene.instantiate()
		_reality2_nodes[nodeName].node_visual.load_sentants(sentants)
			
		_reality2_nodes[nodeName].node_visual.name = the_name
		add_child(_reality2_nodes[nodeName].node_visual)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
