extends Node

@export var graphQL: Node
# Called when the node enters the scene tree for the first time.
func _ready():
	graphQL.sentantAll()
	graphQL.sentantGetByID("fbbfab28-c957-11ee-b44f-de59b61f7ba5")


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	pass


func _on_graph_ql_sentant_all_response(result):
	print(result)


func _on_graph_ql_sentant_get_response(result):
	print(result)
