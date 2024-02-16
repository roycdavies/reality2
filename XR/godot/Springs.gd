extends RigidBody3D

var closestDistance = 5
var springStiffness = 0.5
var damping = 0.05


# Called when the node enters the scene tree for the first time.
func _ready():
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	var the_children = get_parent().get_children()
	var new_force = Vector3(0,0,0)
	for child in the_children:
		var direction = Vector3(position.x - child.position.x, position.y - child.position.y, position.x - child.position.z)
		var forceValue = direction.length() - closestDistance
		var normalizedDirection = direction.normalized()
		var thisForce = Vector3((forceValue * -springStiffness * normalizedDirection.x) - (linear_velocity.x * damping), (forceValue * -springStiffness * normalizedDirection.y) - (linear_velocity.y * damping), (forceValue * -springStiffness * normalizedDirection.z) - (linear_velocity.z * damping))
		#print (child.position)
		new_force += thisForce
		#add_constant_central_force(thisForce)
	set_constant_force(new_force)
		


				 #direction = new THREE.Vector3(this.transform.position.x - thisChild.position.x, this.transform.position.y - thisChild.position.y, this.transform.position.z - thisChild.position.z);
				 #forceValue = direction.length() - this.closestDistance;
				 #normalizedDirection = new THREE.Vector3(direction.x, direction.y, direction.z);
				 #normalizedDirection.normalize();
				 #thisForce = new THREE.Vector3((forceValue * -this.springStiffness * normalizedDirection.x) - (this.velocity.x * this.damping), (forceValue * -this.springStiffness * normalizedDirection.y) - (this.velocity.y * this.damping), (forceValue * -this.springStiffness * normalizedDirection.z) - (this.velocity.z * this.damping));
				 #this.force.set(this.force.x + thisForce.x, this.force.y + thisForce.y, this.force.z + thisForce.z);
