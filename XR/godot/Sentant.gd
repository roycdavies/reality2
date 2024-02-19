extends Node3D

var R2 = load("res://Reality2.gd").new()
var Shapes = load("res://Shapes.gd").new()
var connecting_line
var shape

# Called when the node enters the scene tree for the first time.
func _ready():
	shape = R2.Sentant.new(self)
	shape.centreDistance = 5.0
	shape.closestDistance = 8.0
	connecting_line = Shapes.Line.new(self, Color.DIM_GRAY)

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	shape.update(delta)
	connecting_line.adjust_line(Vector3(0,0,0), to_local(get_parent().global_position))
