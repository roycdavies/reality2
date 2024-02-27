extends Node3D

var connecting_line
var shape

# Called when the node enters the scene tree for the first time.
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

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	shape.update(delta)
	connecting_line.adjust_line(Vector3(0,0,0), to_local(get_parent().global_position))
