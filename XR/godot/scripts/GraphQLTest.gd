extends Node

@export var GQL: Node

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Scripts used
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var FSM = preload("res://scripts/FSM.gd")
var _events = FSM.Automation.new(false)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Private Variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var lightSwitchID = ""
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Get a list of all the Sentants
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantAll(callback, details: String = "id"):
	var body = 'query {  sentantAll { ' + details + ' } }'
	GQL.query(body, callback)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantAll_response(data):
	var response = {}
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["sentantAll"]
		print ("SENTANT_ALL: ", response)
# ------------------------------------------------------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Get Sentant details by ID
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantGetByID(callback, id: String, details: String = "id"):
	var query = 'query SentantGet($id: UUID4!) { sentantGet(id: $id) {' + details + '} }'
	var variables = {"id": id}
	GQL.query(query, callback, variables)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantGetByID_response(data):
	var response = {}
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["sentantGet"]
		print ("SENTANT_GET: ", response)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Get Sentant details by Name
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantGetByName(callback, name: String, details: String = "id"):
	var query = 'query SentantGet($name: String!) { sentantGet(name: $name) {' + details + '} }'
	var variables = {"name": name}
	GQL.query(query, callback, variables)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantGetByName_response(data):
	var response = {}
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["sentantGet"]
		lightSwitchID = response["id"]
		print ("SENTANT_GET: ", response)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Send an Event to a named Sentant
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantSend(callback, id: String, event: String, details: String = "id"):
	var query = 'mutation SentantSend($id: UUID4!, $event: String!) { sentantSend(id: $id, event: $event) {' + details + '} }'
	var variables = {"id": id, "event": event}
	GQL.query(query, callback, variables)
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantSend_response(data):
	var response = {}
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["sentantSend"]
		print ("SENTANT_SEND: ", response)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# "subscription { sentantEvent(id: \"a42589c4-d270-11ee-a7ac-18c04dee389e\", event: \"turn_off\") { event parameters sentant { id name } } }"
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantEvent(callback, id: String, event: String, details: String = "id"):
	var query = 'subscription SentantEvent($id: UUID4!, $event: String!) { sentantEvent(id: $id, event: $event) { event parameters sentant {' + details + '} } }'
	GQL.subscription(query, callback, {"id": id, "event": event})
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func sentantEvent_response(data):
	var response = {}
	var errors = {}
	if (data.has("errors")):
		errors = data["errors"]
		print("ERROR: ", errors)
	else:
		response = data["data"]["sentantEvent"]
		print ("SENTANT_EVENT: ", response)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Called when the node enters the scene tree for the first time.
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _ready():
	_events.add_transition("start",				"init",					"ready", 			[func(__): _events.enqueue("go", {}, 1)])
	_events.add_transition("ready",				"go",					"ready", 			[do_stuff])
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Poll the FSM
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _process(_delta):
	_events.step()
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Do some things to test the GraphQL
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var do_stuff = func(_parameters):
	sentantAll(func(data): sentantAll_response(data), "description id name")
	sentantGetByName(func(data): sentantGetByName_response(data), "Light Switch", "id")
	sentantGetByID(func(data): sentantGetByID_response(data), lightSwitchID, "name")
	
	sentantEvent(func(data): sentantEvent_response(data), lightSwitchID, "turn_on")
	sentantSend(func(data): sentantSend_response(data), lightSwitchID, "turn_on", "name")
# ------------------------------------------------------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Do something when the subscribe button is pressed
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _on_subscribe_pressed():
	sentantEvent(func(data): sentantEvent_response(data), lightSwitchID, "turn_off")
# ------------------------------------------------------------------------------------------------------------------------------------------------------
