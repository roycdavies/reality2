extends Node


# Called when the node enters the scene tree for the first time.
func _ready():
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	pass


func _on_yes_no_maybe_slot_selected(_slot, index):
	print(index)
	
func show_menu(index):
	if (index == -1):
		for child in get_children():
			child.visible = false


func _on_menu_button_pressed(index):
	var children = get_children()
	# Hide them all
	for child in children:
		child.visible = false
		
	# Unhide the one that is specified - any number outside of the range effectively hides thema all (eg -1)
	if (index >=0) and (index < children.size()):
		children[index].visible = true
		
