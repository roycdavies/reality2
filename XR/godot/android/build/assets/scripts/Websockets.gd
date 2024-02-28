extends Node

# The URL we will connect to
@export var websocket_url = "wss://localhost:4001/reality2/websocket"

# Our WebSocketClient instance
var _client = WebSocketPeer.new()

func _ready():
	_SOCKET_connect(websocket_url)

func _process(_delta):
	_client.poll()
	#if _client.get_available_packet_count() > 0:
		#while _client.get_available_packet_count() > 0:
			#print("Packet: ", _client.get_packet().get_string_from_utf8())
	
	

func _SOCKET_connect(url):
	_client = WebSocketPeer.new()
	_client.connect_to_url(url, TLSOptions.client_unsafe())
	
	_client.poll()
	while _client.get_ready_state() != WebSocketPeer.State.STATE_OPEN:
		_client.poll()
	
	var join_message = {
		"topic": "__absinthe__:control",
		"event": "phx_join",
		"payload": {},
		"ref": 0
	}
	_client.send_text(JSON.stringify(join_message))
	
	_client.poll()
	while _client.get_ready_state() != WebSocketPeer.State.STATE_OPEN:
		_client.poll()
		
	print("JOINED")
	var subscribe = {
		"topic": "__absinthe__:control",
		"event": "doc",
		"payload": {
			"query": "subscription { sentantEvent(id: \"a42589c4-d270-11ee-a7ac-18c04dee389e\", event: \"turn_off\") { event parameters sentant { id name } } }"
		},
		"ref": 0
	}
	print(JSON.stringify(subscribe))
	_client.send_text(JSON.stringify(subscribe))

	_client.poll()
	while _client.get_ready_state() != WebSocketPeer.State.STATE_OPEN:
		_client.poll()
		
	print ("Connected")

	#while _socket2.get_ready_state() != WebSocketPeer.State.STATE_CLOSED:
		#_socket2.poll()
#
		#if _socket2.get_available_packet_count() > 0:
			#while _socket2.get_available_packet_count() > 0:
				#print("Packet: ", _socket2.get_packet().get_string_from_utf8())
