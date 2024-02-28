class_name FloatySprings
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
	func _init(parent, _name = "Test", _color = Color.CADET_BLUE):
		# Something to interact with
		var area = Area3D.new()
		area.set_ray_pickable(true)
		var collider = CollisionShape3D.new()
		collider.set_shape(SphereShape3D.new())
		collider.shape.set_radius(0.5)
		
		# Add to the parent
		area.add_child(collider)
		parent.add_child(area)

		# A little randomness to make sure the objects move around OK
		parent.position = Vector3(randf() - 0.5, randf() - 0.5, randf() - 0.5) * 0.1
		
		# Set for later
		_sentant = parent


	# Called every frame. 'delta' is the elapsed time since the previous frame.
	func update(delta):
		_force = Vector3(0,0,0)
		
		# The children of my parent must be siblings...
		var siblings = _sentant.get_parent().get_children()
		
		# Iterate through, ignoring myself, and set a force to each other sibling
		for sibling in siblings:
			if (sibling != self) && (sibling is Node3D):
				var direction = _sentant.position - sibling.position
				var forceValue = direction.length() - closestDistance
				var normalizedDirection = direction.normalized()
				var thisForce = (normalizedDirection * -springStiffness * forceValue) - (_velocity * damping)
				_force += thisForce
		
		# Calculate a force to the centre (parent)
		var directionCentre = _sentant.position
		var forceValueCentre = directionCentre.length() - centreDistance
		var normalizedDirectionCentre = directionCentre.normalized()
		_force += (normalizedDirectionCentre * -centreSpringStiffness * forceValueCentre) - (_velocity * damping)

		# Adjust for friction
		var frictionForce = _velocity * -_friction
		
		# How much to move since the last frame
		_velocity = ((_force + frictionForce) / _mass) * delta
		
		# Set the new position
		_sentant.position = _sentant.position + _velocity
