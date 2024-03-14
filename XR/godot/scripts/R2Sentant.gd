# ======================================================================================================================================================
# R2 Sentant
# ----------
#
# Represents a Reality2 sentant graphically.
#
# Dr. Roy C.Davies
# roycdavies.github.io
# March 2024
# ======================================================================================================================================================

extends Node3D

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Private Variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var connecting_line
var shape
# ------------------------------------------------------------------------------------------------------------------------------------------------------



func set_color(the_color: Color):
	var child = get_child(0)
	var material = StandardMaterial3D.new()
	material.albedo_color = Color.DARK_GRAY # Red color, RGBA

	child.material_override = material

	
	

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Set up the Sentant visual representation, and connect the floaty springs code.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _ready():
	shape = FloatySprings.Planet.new(self)
	shape.centreDistance = 5.0
	shape.closestDistance = 8.0
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
	
	if(name == "monitor"):
		set_color(Color.DARK_BLUE)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Called every frame. 'delta' is the elapsed time since the previous frame.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _process(delta):
	shape.update(delta)
	connecting_line.adjust_line(Vector3(0,0,0), to_local(get_parent().global_position))
# ------------------------------------------------------------------------------------------------------------------------------------------------------
