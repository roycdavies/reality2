# ------------------------------------------------------------------------------------------------------------------------------------------------------
# A very simple GraphQL set of functions
# ------------------------------------------------------------------------------------------------------------------------------------------------------
extends Node

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Public parameters
# ------------------------------------------------------------------------------------------------------------------------------------------------------
@export var output: RichTextLabel = null
@export var queueSize: RichTextLabel = null
# ------------------------------------------------------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Private variables
# ------------------------------------------------------------------------------------------------------------------------------------------------------
var _socket = WebSocketPeer.new()
var _socket_heartbeat_time
# ------------------------------------------------------------------------------------------------------------------------------------------------------



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
	print("SUBSCRIPTION QUERY: ", query)
	
	var subscribe = {
		"topic": "__absinthe__:control",
		"event": "doc",
		"payload": {
			"query": "subscription { sentantEvent(id: \"a42589c4-d270-11ee-a7ac-18c04dee389e\", event: \"turn_off\") { event parameters sentant { id name } } }"
		},
		"ref": 0
	}
	print(JSON.stringify(subscribe))
	_socket.send_text(JSON.stringify(subscribe))

	_socket.poll()
	while _socket.get_ready_state() != WebSocketPeer.State.STATE_OPEN:
		_socket.poll()
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Set things up
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _ready():
	_SOCKET_connect("wss://localhost:4001/reality2/websocket")
	_socket_heartbeat_time = Time.get_ticks_msec() + 30000
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Poll the Websocket
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _process(_delta):
	_socket.poll()
	while _socket.get_available_packet_count() > 0:
		print("Packet: ", _socket.get_packet().get_string_from_utf8())
		
	if (Time.get_ticks_msec() > _socket_heartbeat_time):
		_SOCKET_heartbeat()
		_socket_heartbeat_time = Time.get_ticks_msec() + 30000
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Connect to the websocket in preparation for a subscription call
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _SOCKET_connect(url):
	_socket.connect_to_url(url, TLSOptions.client_unsafe())
	
	_socket.poll()
	while _socket.get_ready_state() != WebSocketPeer.State.STATE_OPEN:
		_socket.poll()
	
	var join_message = {
		"topic": "__absinthe__:control",
		"event": "phx_join",
		"payload": {},
		"ref": 0
	}
	_socket.send_text(JSON.stringify(join_message))
	
	_socket.poll()
	while _socket.get_ready_state() != WebSocketPeer.State.STATE_OPEN:
		_socket.poll()
		
	print ("Websocket Connected")
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------
func _SOCKET_heartbeat():
	var heartbeat = {
		"topic": "phoenix",
		"event": "heartbeat",
		"payload": {},
		"ref": 0
	}
	_socket.send_text(JSON.stringify(heartbeat))
	print("heartbeat")
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
