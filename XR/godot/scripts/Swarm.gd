extends Node3D

@export_group("Debugging Parameters")
## Debug Mode
@export var debug = true
## Number of Sentants in this Swarm
@export var numSentants = 5


var sentant_scene = preload("res://scenes/Sentant.tscn")
var R2 = load("res://scripts/Reality2.gd").new()
var angularVelocity = Vector3(0,0,0)
var shape
var Shapes = load("res://scripts/Shapes.gd").new()
var connecting_line

# Called when the node enters the scene tree for the first time.
func _ready():
	shape = R2.Planet.new(self, "Swarm", Color.BLUE)
	shape.centreDistance = 20.0
	shape.closestDistance = 40.0
	connecting_line = Shapes.Line.new(self, Color.DIM_GRAY)
	
	if debug:
		for i in range(0, numSentants):
			add_sentant(i)


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	shape.update(delta)
	
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
			
	rotation = (rotation + angularVelocity)
	connecting_line.adjust_line(Vector3(0,0,0), to_local(get_parent().global_position))
	
# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Add a Sentant to the Swarm
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func add_sentant(i):
	var new_sentant = sentant_scene.instantiate()
	new_sentant.name = "Sentant_" + str(i)
	add_child(new_sentant)
	
	var title = Label3D.new()
	title.text = new_sentant.name
	title.set_outline_size(0)
	title.modulate = Color.BISQUE
	title.billboard = true
	title.pixel_size = 0.005
	title.position = Vector3(0.0, 0.0, 0.0)
	title.no_depth_test = true
	title.visibility_range_end = 15.0
	title.font_size = 30
	title.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	new_sentant.add_child(title)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
