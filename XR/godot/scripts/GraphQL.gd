# ------------------------------------------------------------------------------------------------------------------------------------------------------
# A very simple GraphQL set of functions
# ------------------------------------------------------------------------------------------------------------------------------------------------------
extends Node

var FSM = preload("res://scripts/FSM.gd")

var _websocket_client = WebSocketPeer.new()
var _websocket_stage = 0
var _subscription_query: String

var _websocket_automation

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
# GraphQL subscritionvia Websockets
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func subscription(url, query, callback, variables={}, headers_dict={}):
	print ("Websocket: ", url)
	_subscription_query = "subscription {sentantEvent(id: \"" + variables["id"] + "\", event: \"" + variables["event"] + "\") { event parameters sentant { id } } }"
	#_websocket_client = WebSocketPeer.new()
	#_websocket_client.connect_to_url(url, TLSOptions.client_unsafe())
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Set things up
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _ready():
	_websocket_automation = FSM.FSM.new()
	_websocket_automation.set_debug(true)
	_websocket_client.connect_to_url("wss://localhost:4001/reality2/websocket", TLSOptions.client_unsafe())
	
	_websocket_automation.add_transition("start",			"init",				"ready", 			[print_parameters])
	_websocket_automation.add_transition("ready",			"open",				"joining", 			[send_join_message])
	_websocket_automation.add_transition("joining",			"check_joined",		"joining", 			[check_joined])
	
	_websocket_automation.add_transition("joining",			"opened", 			"subscribing",		[check_subscribed])
	_websocket_automation.add_transition("subscribing",		"check_subscribed", "subscribing",		[check_subscribed])
	_websocket_automation.add_transition("subscribing",		"subscribed", 		"open",				[poll])
	
	_websocket_automation.add_transition("*",				"error", 			"ready",			[print_parameters, func(__): _websocket_automation.queue_event("open", {}, 1)])

	_websocket_automation.queue_event("open")
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Actions for Automations
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var print_parameters = func(parameters):
	print (parameters)
	return parameters
	
var send_join_message = func(_parameters):
	if (_websocket_client != null):
		var join_message = {
			"topic": "__absinthe__:control",
			"event": "phx_join",
			"payload": {},
			"ref": 0
		}
		_websocket_client.send_text(JSON.stringify(join_message))
		_websocket_automation.queue_event("check_joined", {}, 0.1)
	else:
		_websocket_automation.queue_event("error", {"error": "Could not join"})
	
var check_joined = func(_parameters):
	_websocket_client.poll()
	if _websocket_client.get_ready_state() == WebSocketPeer.State.STATE_OPEN:
		_websocket_automation.queue_event("opened")
	else:
		_websocket_automation.queue_event("check_joined", {}, 0.1)
		
var send_subscribe_message = func(_parameters):
	if (_websocket_client != null):
		var subscribe = {
			"topic": "__absinthe__:control",
			"event": "doc",
			"payload": {
				"query": _subscription_query
			},
			"ref": 0
		}
		_websocket_client.send_text(JSON.stringify(subscribe))
		_websocket_automation.queue_event("check_subscribed", {}, 0.1)
	else:
		_websocket_automation.queue_event("error", {"error": "Could not subscribe"})
		
var check_subscribed = func(_parameters):
	_websocket_client.poll()
	if _websocket_client.get_ready_state() == WebSocketPeer.State.STATE_OPEN:
		_websocket_automation.queue_event("subscribed")
	else:
		_websocket_automation.queue_event("check_subscribed", {}, 0.1)
		
var poll = func(_parameters):
	_websocket_client.poll()
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _process(_delta):
	_websocket_automation.step()
	#print(_websocket_client)
	#if (_websocket_client != null):
		#_websocket_client.poll()
		#var state = _websocket_client.get_ready_state()
		##print(state)
		#
		#if (_websocket_stage == 0):
			#var join_message = {
				#"topic": "__absinthe__:control",
				#"event": "phx_join",
				#"payload": {},
				#"ref": 0
			#}
			#print("Opening Websocket")
			#_websocket_client.send_text(JSON.stringify(join_message))
			#_websocket_stage = 1
		#elif (_websocket_stage == 1):
			#var subscribe = {
				#"topic": "__absinthe__:control",
				#"event": "doc",
				#"payload": {
					#"query": _subscription_query
				#},
				#"ref": 0
			#}
			#print("Subscribing")
			#_websocket_client.send_text(JSON.stringify(subscribe))
			#_websocket_stage = 2
		#else:
			#if state == WebSocketPeer.State.STATE_OPEN:
				#while _websocket_client.get_available_packet_count():
					#print("Packet: ", _websocket_client.get_packet())
			#elif state == WebSocketPeer.STATE_CLOSING:
				## Keep polling to achieve proper close.
				#pass
			#elif state == WebSocketPeer.STATE_CLOSED:
				#var code = _websocket_client.get_close_code()
				#var reason = _websocket_client.get_close_reason()
				#print("WebSocket closed with code: %d, reason %s. Clean: %s" % [code, reason, code != -1])
				#set_process(false) # Stop processing.
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
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
