extends Node3D


# Called when the node enters the scene tree for the first time.
func _ready():
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	var space = get_world_3d().get_direct_space_state()
	var mousePosViewport = get_viewport().get_mouse_position()
	var camera = get_viewport().get_camera_3d()
	var rayOrigin = camera.project_ray_origin(mousePosViewport)
	var rayEnd = rayOrigin+camera.project_ray_normal(mousePosViewport)*100
	var detectionParameters = PhysicsRayQueryParameters3D.new() 
	detectionParameters.collide_with_bodies = true
	detectionParameters.collide_with_areas = true
	detectionParameters.from = rayOrigin
	detectionParameters.to = rayEnd	

	var rayArray = space.intersect_ray(detectionParameters)
	#if rayArray.length() > 0:
	print(rayArray) # Output
