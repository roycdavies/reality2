extends Node3D

@export var orbitControls :Node

var currentObject: Node3D
var lerper = 0.0
var xr_interface: XRInterface


# Called when the node enters the scene tree for the first time.
func _ready():
	xr_interface = XRServer.find_interface("OpenXR")
	if xr_interface and xr_interface.is_initialized():
		print("OpenXR initialized successfully")

		# Turn off v-sync!
		DisplayServer.window_set_vsync_mode(DisplayServer.VSYNC_DISABLED)

		# Change our main viewport to output to the HMD
		get_viewport().use_xr = true
	else:
		print("OpenXR not initialized, please check if your headset is connected")


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	if (currentObject && orbitControls):
		lerper = min(1.0, lerper + 0.5 * delta)
		if (lerper == 1.0):
			orbitControls.target = currentObject.global_position
		else:
			orbitControls.target = orbitControls.target.lerp(currentObject.global_position, lerper)
		orbitControls.update()

	
func _unhandled_input(event):
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
				lerper = 0.0
