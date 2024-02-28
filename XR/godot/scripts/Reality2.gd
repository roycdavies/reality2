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
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Get a list of all the Sentants
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantAll(callback, details: String = "id"):
	var body = 'query {  sentantAll { ' + details + ' } }'
	GQL.query(body, callback)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantAll_response(data):
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
# Called when the node enters the scene tree for the first time.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _ready():
	if debug:
		for i in range(0, numNodes):
			add_node()
	else:
		sentantAll(func(data): sentantAll_response(data), "description id name")
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Called every frame. 'delta' is the elapsed time since the previous frame.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _process(delta):
		# Create some gentle sway-y motion
	var stepValue = delta / 1000
	var maxValue = stepValue * 30.0
	
	var xDirection = randf() > 0.5
	var yDirection = randf() > 0.5
	var zDirection = randf() > 0.5
	
	if (xDirection):
		angularVelocity.x += stepValue
	else:
		angularVelocity.x -= stepValue
	angularVelocity.x = max(-maxValue, min(maxValue, angularVelocity.x))
		
	if (yDirection):
		angularVelocity.y += stepValue
	else:
		angularVelocity.y -= stepValue	
	angularVelocity.y = max(-maxValue, min(maxValue, angularVelocity.y))
		
	if (zDirection):
		angularVelocity.z += stepValue
	else:
		angularVelocity.z -= stepValue
	angularVelocity.z = max(-maxValue, min(maxValue, angularVelocity.z))
	
	# Set the rotation of this node	
	rotation = (rotation + angularVelocity)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Add a Node
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func add_node(sentants = []):
	var new_node = node_scene.instantiate()
	if(debug):
		new_node.numSentants = numSentantsInNode
	else:
		new_node.load_sentants(sentants)
		
	new_node.name = "this"
	add_child(new_node)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
