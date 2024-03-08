extends Node3D

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Public variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
@export_group("Debugging Parameters")
## Debug Mode
@export var debug = false
## Number of Sentants in this Swarm
@export var numSentants = 5
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Private variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var sentant_scene = preload("res://scenes/Sentant.tscn")
var angularVelocity = Vector3(0,0,0)
var shape
var connecting_line
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Set up various aspects of the shape, and the initial floaty springs stuff
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _ready():
	shape = FloatySprings.Planet.new(self, "Reality2Node", Color.BLUE)
	shape.centreDistance = 10.0
	shape.closestDistance = 20.0
	connecting_line = Shapes.Line.new(self, Color.DIM_GRAY)
	
	var title = Label3D.new()
	title.text = name
	title.set_outline_size(0)
	title.modulate = Color.BISQUE
	title.billboard = true
	title.pixel_size = 0.005
	title.position = Vector3(0.0, 0.0, 0.0)
	title.no_depth_test = true
	title.visibility_range_end = 15.0
	title.font_size = 30
	title.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	add_child(title)
	
	if debug:
		for i in range(0, numSentants):
			add_sentant("Sentant_" + str(i))
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Called every frame. 'delta' is the elapsed time since the previous frame.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _process(delta):
	# Update the shapes
	shape.update(delta)
	
	# Add a bit of gentel motion
	#self.rotation = Useful.gentle_twist(delta, angularVelocity, self.rotation)
	
	var stepValue = delta / 1000
	var maxValue = stepValue * 30.0
	
	var xDirection = randf() > 0.5
	var yDirection = randf() > 0.5
	var zDirection = randf() > 0.5
	
	if (xDirection):
		angularVelocity.x += stepValue
	else:
		angularVelocity.x -= stepValue
	angularVelocity.x = clampf(angularVelocity.x, -maxValue, maxValue)
		
	if (yDirection):
		angularVelocity.y += stepValue
	else:
		angularVelocity.y -= stepValue	
	angularVelocity.y = clampf(angularVelocity.y, -maxValue, maxValue)
		
	if (zDirection):
		angularVelocity.z += stepValue
	else:
		angularVelocity.z -= stepValue
	angularVelocity.z = clampf(angularVelocity.z, -maxValue, maxValue)
	
	# Set the rotation of this node	
	rotation = rotation + angularVelocity
	
	# Update the connecting line between this node and its parent
	connecting_line.adjust_line(Vector3(0,0,0), to_local(get_parent().global_position))
# ------------------------------------------------------------------------------------------------------------------------------------------------------


	
# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Given an array of Sentant details, create the sentant graphical representations connected to this Node.
# TODO: Delete existing sentant graphical representations first.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func load_sentants(sentants = []):
	for sentant in sentants:
		add_sentant(sentant.name)
# ------------------------------------------------------------------------------------------------------------------------------------------------------


	
# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Add a Sentant to the Node
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func add_sentant(name):
	var new_sentant = sentant_scene.instantiate()
	new_sentant.name = name
	add_child(new_sentant)
# ------------------------------------------------------------------------------------------------------------------------------------------------------