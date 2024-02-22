extends RigidBody3D

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Public Variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
@export_group("Debugging Parameters")
## Debug Mode
@export var debug : bool = true
## Number of Sentants in this Swarm
@export var numSwarms : int = 5
@export var numSentantsInSwarm : int = 10
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Private Variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var swarm_scene = preload("res://scenes/Swarm.tscn")
# ------------------------------------------------------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Called when the node enters the scene tree for the first time.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _ready():
	if debug:
		for i in range(0, numSwarms):
			add_swarm()
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Called every frame. 'delta' is the elapsed time since the previous frame.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _process(_delta):
	pass
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Add a Swarm
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func add_swarm():
	var new_swarm = swarm_scene.instantiate()
	new_swarm.numSentants = numSentantsInSwarm
	add_child(new_swarm)
# ------------------------------------------------------------------------------------------------------------------------------------------------------