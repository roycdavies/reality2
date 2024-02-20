class Planet:
	
	## Closest Distance between Sentants
	var closestDistance = 30.0
	## Stiffness of Spring between Sentants
	var springStiffness = 0.1
	## Stiffness of Spring to centre object
	var centreSpringStiffness = 10.0
	## Distance to centre object
	var centreDistance = 10.0
	## Damping / friction
	var damping = 0.1

	var _velocity = Vector3(0,0,0)
	var _force = Vector3(0,0,0)
	var _friction = 0.5
	var _mass = 1.0
	
	var _sentant: Node3D
	func get_sentant():
		return _sentant


	# Called when the node enters the scene tree for the first time.
	func _init(parent, name = "Test", color = Color.CADET_BLUE):
		#var sphere = MeshInstance3D.new()
		#var sphere_mesh = SphereMesh.new()
		#var material = StandardMaterial3D.new()
		#material.albedo_color = color
		#sphere.material_override = material
		#sphere.mesh = sphere_mesh
		
		var area = Area3D.new()
		area.set_ray_pickable(true)
		var collider = CollisionShape3D.new()
		collider.set_shape(SphereShape3D.new())
		collider.shape.set_radius(0.5)
		
		area.add_child(collider)
		#sphere.add_child(area)
		parent.add_child(area)

		# A little randomness to make sure the objects move around OK
		parent.position = Vector3(randf() - 0.5, randf() - 0.5, randf() - 0.5) * 0.1
		#self.name = name
		
		#parent.add_child(sphere)
		_sentant = parent


	# Called every frame. 'delta' is the elapsed time since the previous frame.
	func update(delta):
		_force = Vector3(0,0,0)
		
		var siblings = _sentant.get_parent().get_children()
		
		for sibling in siblings:
			if (sibling != self) && (sibling is Node3D):
				var direction = _sentant.position - sibling.position
				var forceValue = direction.length() - closestDistance
				var normalizedDirection = direction.normalized()
				var thisForce = (normalizedDirection * -springStiffness * forceValue) - (_velocity * damping)
				_force += thisForce
				
		var directionCentre = _sentant.position
		var forceValueCentre = directionCentre.length() - centreDistance
		var normalizedDirectionCentre = directionCentre.normalized()
		_force += (normalizedDirectionCentre * -centreSpringStiffness * forceValueCentre) - (_velocity * damping)

		var frictionForce = _velocity * -_friction
		_velocity = ((_force + frictionForce) / _mass) * delta
		_sentant.position = _sentant.position + _velocity
