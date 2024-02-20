extends Node3D

@export var orbitControls :Node

var currentObject: Node3D

# Called when the node enters the scene tree for the first time.
func _ready():
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	if (currentObject && orbitControls):
		orbitControls.target = currentObject.global_position
		orbitControls.update()
	
func _input(event):
	if (event is InputEventMouseButton) || (event is InputEventScreenTouch):
		if event["button_index"] == 1 && event["pressed"]:
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
			
			var result = space.intersect_ray(detectionParameters)
			if result and result.collider:
				var object = result.collider.get_parent()
				if (object.name == "root"):
					object = result.collider.get_parent()
				#emit_signal("picked", object)
				print(object.name)
				print(object.position)
				currentObject = object

				
