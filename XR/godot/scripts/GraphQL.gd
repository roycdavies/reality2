# ------------------------------------------------------------------------------------------------------------------------------------------------------
# A very simple GraphQL set of functions
# ------------------------------------------------------------------------------------------------------------------------------------------------------
extends Node

var FSM = preload("res://scripts/FSM.gd")

var _WSC = WebSocketPeer.new()
var _WSQ: String
var _WSA = FSM.FSM.new()

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Do a GraphQL Query POST call
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func query(url, query, callback, variables={}, headers={}):
	mutation(url, query, callback, variables, headers)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Do a GraphQL Mutation POST call
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func mutation(url, query, callback, variables={}, headers_dict={}):
	headers_dict["Content-Type"] = "application/json"
	headers_dict["Accept"] = "*/*"
	var headers = []
	for key in headers_dict.keys():
		headers.append(key + ":" + str(headers_dict[key]))
		
	var body = JSON.stringify({ "query": query, "variables": variables })
	await _POST(url, body, callback, headers)
# ------------------------------------------------------------------------------------------------------------------------------------------------------




# ------------------------------------------------------------------------------------------------------------------------------------------------------
# GraphQL subscription via Websockets
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func subscription(url, query, callback, variables={}, headers_dict={}):
	print ("Websocket: ", url)
	_WSQ = "subscription {sentantEvent(id: \"" + variables["id"] + "\", event: \"" + variables["event"] + "\") { event parameters sentant { id } } }"
	print(_WSQ)
	#_WSC = WebSocketPeer.new()
	#_WSC.connect_to_url(url, TLSOptions.client_unsafe())
	_WSA.queue_event("subscribe", {}, 30)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Set things up
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _ready():
	_WSA.set_debug(true)
	_WSC.connect_to_url("wss://localhost:4001/reality2/websocket", TLSOptions.client_unsafe())
	
	_WSA.add_transition("start",			"init",				"ready", 			[print_parameters])
	_WSA.add_transition("ready",			"open",				"joining", 			[send_join_message])
	_WSA.add_transition("joining",			"check_joined",		"joining", 			[check_joined])
	
	_WSA.add_transition("joining",			"joined", 			"joined",			[])
	_WSA.add_transition("joined",			"subscribe", 		"subscribing",		[send_subscribe_message])	
	_WSA.add_transition("subscribing",		"check_subscribed", "subscribing",		[check_subscribed])
	_WSA.add_transition("subscribing",		"subscribed", 		"open",				[poll])
	_WSA.add_transition("open",				"polling", 			"open",				[poll])
	_WSA.add_transition("open",				"receiving", 		"open",				[receive])
	_WSA.add_transition("open",				"closed", 			"joining",			[func(__): _WSA.queue_event("joined", {}, 1)])
	
	_WSA.add_transition("*",				"error", 			"ready",			[print_parameters, func(__): _WSA.queue_event("open", {}, 1)])

	_WSA.queue_event("open")
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Actions for the Automations
# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Print the parameters
var print_parameters = func(parameters):
	print (parameters)
	return parameters
	
# Send a join message - this is specific to the Phoenix Graphql Web Server
var send_join_message = func(parameters):
	if (_WSC != null):
		var join_message = {
			"topic": "__absinthe__:control",
			"event": "phx_join",
			"payload": {},
			"ref": 0
		}
		_WSC.send_text(JSON.stringify(join_message))
		_WSA.queue_event("check_joined", {}, 0.1)
	else:
		_WSA.queue_event("error", {"error": "Could not join"})
	return parameters

# Check whether the Join message was successful
var check_joined = func(parameters):
	_WSC.poll()
	if _WSC.get_ready_state() == WebSocketPeer.State.STATE_OPEN:
		_WSA.queue_event("joined")
	else:
		_WSA.queue_event("check_joined", {}, 0.1)
	return parameters
	
# Send the subscription message
var send_subscribe_message = func(parameters):
	if (_WSC != null):
		var subscribe = {
			"topic": "__absinthe__:control",
			"event": "doc",
			"payload": {
				"query": _WSQ
			},
			"ref": 0
		}
		print(JSON.stringify(subscribe))
		_WSC.send_text(JSON.stringify(subscribe))
		_WSA.queue_event("check_subscribed", {}, 0.1)
	else:
		_WSA.queue_event("error", {"error": "Could not subscribe"})
	return parameters
			
# Check whether the subscription message was successful
var check_subscribed = func(parameters):
	_WSC.poll()
	if _WSC.get_ready_state() == WebSocketPeer.State.STATE_OPEN:
		_WSA.queue_event("subscribed")
	else:
		_WSA.queue_event("check_subscribed", {}, 0.1)
		_WSA.queue_event("polling", {}, 0.15)
	return parameters

# Once connected and subscribed, check if anything arrives, and if so, receive it
var poll = func(parameters):
	_WSC.poll()
	if _WSC.get_ready_state() == WebSocketPeer.State.STATE_OPEN:
		if (_WSC.get_available_packet_count() > 0):
			_WSA.queue_event("receiving")
		else:
			_WSA.queue_event("polling", {}, 0.1)
	elif _WSC.get_ready_state() == WebSocketPeer.State.STATE_CLOSED:
		var code = _WSC.get_close_code()
		var reason = _WSC.get_close_reason()
		print("WebSocket closed with code: %d, reason %s. Clean: %s" % [code, reason, code != -1])
		_WSA.queue_event("closed", {}, 0.1)
	return parameters
			
var receive = func(parameters):
	while _WSC.get_available_packet_count() > 0:
		print("Packet: ", _WSC.get_packet().get_string_from_utf8())
	_WSA.queue_event("polling", {}, 0.1)
	return parameters
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Poll the
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _process(_delta):
	_WSA.step()
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Post data in the body to a URL, with headers, and returning the result to the callback function, or an appropriate error
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _POST(url, body, callback, headers):
	var uri = _URL(url)	
	var err = 0
	var http = HTTPClient.new() # Create the Client.

	if (uri.scheme == "http"):
		err = http.connect_to_host(uri.domain, uri.port.to_int())
	else:
		err = http.connect_to_host(uri.domain, uri.port.to_int(), TLSOptions.client_unsafe())
		
	if (err == OK):
		# Wait until resolved and connected.
		while http.get_status() == HTTPClient.STATUS_CONNECTING or http.get_status() == HTTPClient.STATUS_RESOLVING:
			http.poll()
			
		# Check if the connection was made successfully.
		if (http.get_status() == HTTPClient.STATUS_CONNECTED):
			err = http.request(HTTPClient.METHOD_POST, uri.path, headers, body)
			if (err == OK):
				# Wait until the requesting is done
				while http.get_status() == HTTPClient.STATUS_REQUESTING:
					http.poll()
				
				if (http.get_status() == HTTPClient.STATUS_BODY or http.get_status() == HTTPClient.STATUS_CONNECTED):
					if http.has_response():
						# Read the response
						var response = PackedByteArray()
						while http.get_status() == HTTPClient.STATUS_BODY:
							http.poll()
							var chunk = http.read_response_body_chunk()
							if chunk.size() != 0:
								response += chunk
						callback.call(JSON.parse_string(response.get_string_from_ascii()))
					else:
						callback.call({"errors": [{"message": "response error"}]})
				else:
					callback.call({"errors": [{"message": "request error"}]})
			else:
				callback.call({"errors": [{"message": "request error"}]})
		else:
			callback.call({"errors": [{"message": "connection error"}]})
	else:
		callback.call({"errors": [{"message": "connection error"}]})
# ------------------------------------------------------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Split a URL into component parts
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _URL(url: String):
	var pieces = url.split("/")
	var scheme = pieces[0].get_slice(":", 0)
	var domain_and_port = pieces[2]
	var domain = domain_and_port.get_slice(":", 0)
	var port = domain_and_port.get_slice(":", 1)
	var path = "/"
	if (pieces.size() > 2):
		path = path + pieces[3]
	
	return({"scheme": scheme, "domain": domain, "port": port, "path": path})
# ------------------------------------------------------------------------------------------------------------------------------------------------------
