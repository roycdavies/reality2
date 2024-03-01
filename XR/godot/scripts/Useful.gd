class_name Useful

static func gentle_twist(delta, angularVelocity):
	# Create some gentle sway-y motion
	if (randf() > 0.7):
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
	
	# Set the angular velocity of this node	
	return(angularVelocity)
