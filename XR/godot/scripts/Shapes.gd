class_name Shapes

class Line:
	
	var _mesh
	var _material

	# ------------------------------------------------------------------------------------------------------------------------------------------------------
	# ------------------------------------------------------------------------------------------------------------------------------------------------------
	func _init(parent, color = Color.CORNFLOWER_BLUE):
		_mesh = MeshInstance3D.new()
		var immediate_mesh := ImmediateMesh.new()
		_material = StandardMaterial3D.new()

		_mesh.mesh = immediate_mesh
		_mesh.cast_shadow = GeometryInstance3D.SHADOW_CASTING_SETTING_OFF

		immediate_mesh.surface_begin(Mesh.PRIMITIVE_LINES, _material)
		immediate_mesh.surface_add_vertex(Vector3())
		immediate_mesh.surface_add_vertex(Vector3())
		immediate_mesh.surface_end()

		_material.shading_mode = BaseMaterial3D.SHADING_MODE_PER_PIXEL
		_material.albedo_color = color
		#_material.emission = Color(.5, .5, .5)
		#_material.emission_energy_multiplier = 2.0
		#_material.emission_enabled = true
		#_material.emission_intensity = 2.0
		#_material.emission_operator = BaseMaterial3D.EMISSION_OP_ADD
		parent.add_child(_mesh)
	# ------------------------------------------------------------------------------------------------------------------------------------------------------



	# ------------------------------------------------------------------------------------------------------------------------------------------------------
	# ------------------------------------------------------------------------------------------------------------------------------------------------------
	func adjust_line(pos1, pos2):
		_mesh.mesh.clear_surfaces()

		_mesh.mesh.surface_begin(Mesh.PRIMITIVE_LINES, _material)
		_mesh.mesh.surface_add_vertex(pos1)
		_mesh.mesh.surface_add_vertex(pos2)
		_mesh.mesh.surface_end()
	# ------------------------------------------------------------------------------------------------------------------------------------------------------
