class_name Useful

static func gentle_twist(delta, angularVelocity, the_rotation):
	# Create some gentle sway-y motion
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
	
	# Set the rotation of this node	
	return(the_rotation + angularVelocity)
