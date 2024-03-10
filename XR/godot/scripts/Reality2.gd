extends RigidBody3D

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Public Variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
@export_group("GraphQL")
@export var GQL: Node

@export_group("Reality2 Node")
@export var NodeNames = ["this"]
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Private Variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var _node_scene = preload("res://scenes/R2Node.tscn")
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
		_reality2_nodes[nodeName] = {"r2gql": Reality2.GQL.new(GQL), "node_visual": null}
		
		_reality2_nodes[nodeName].r2gql.byName( "monitor", func (id): _reality2_nodes[nodeName].r2gql.sentantUnload(id) )
		_reality2_nodes[nodeName].r2gql.sentantLoad( JSON.stringify(monitoringSentant) )
		_reality2_nodes[nodeName].r2gql.sentantAll(func(sentants, _p): add_node(NodeNames[0], sentants), "description id name")
		_reality2_nodes[nodeName].r2gql.byName( "monitor", func(id): _reality2_nodes[nodeName].r2gql.awaitSignal(id, "internal", _monitor) )
	# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Monitor changes to the Node coming from the Monitor node created above.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _monitor(data = {}):
	if (data.has("event")):
		if (data.parameters.has("event")):
			if (data.parameters.event == "created"):
				_reality2_nodes[NodeNames[0]].add_sentant(NodeNames[0], data.parameters.name)
			elif (data.parameters.event == "deleted"):
				_reality2_nodes[NodeNames[0]].remove_sentant(NodeNames[0], data.parameters.name)
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
