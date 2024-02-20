extends Node

@export var graphQL: Node
# Called when the node enters the scene tree for the first time.
func _ready():
	#graphQL.sentantAll()
	graphQL.sentantGetByID("7240e77a-d011-11ee-b5fd-18c04dee389e")


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	pass


func _on_graph_ql_sentant_all_response(result):
	print(result)


func _on_graph_ql_sentant_get_response(result):
	print(result)
