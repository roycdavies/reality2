extends Node

signal sentantAll_response
signal sentantGet_response


var url = "https://localhost:4001/reality2"
var headers = ["Content-Type: text/plain", "Accept: */*"]	


# Called when the node enters the scene tree for the first time.
func sentantAll():
	var body = '
	query SentantAll {
	  sentantAll {
		description
		id
		name
	  }
	}'
	
	await query(body, "sentantAllDone")

func sentantGetByID(id:String):
	var body = '
	query SentantGet {
	  sentantGet(id: "' + id + '") {
		description
		id
		name
	  }
	}'
	
	await query(body, "sentantGetDone")
	

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	pass
	
	
func sentantAllDone(text):
	sentantAll_response.emit(text)
		
		

func sentantGetDone(text):
	sentantGet_response.emit(text)
		
		
		
		
func query(body, callback):
	var err = 0
	var http = HTTPClient.new() # Create the Client.

	err = http.connect_to_host("localhost", 4001, TLSOptions.client_unsafe()) # Connect to host/port.
	if (err == OK):
		# Wait until resolved and connected.
		while http.get_status() == HTTPClient.STATUS_CONNECTING or http.get_status() == HTTPClient.STATUS_RESOLVING:
			http.poll()
			await get_tree().process_frame

		if (http.get_status() == HTTPClient.STATUS_CONNECTED): # Check if the connection was made successfully.
			err = http.request(HTTPClient.METHOD_POST, "/reality2", headers, body) # Request a page from the site (this one was chunked..)
			if (err == OK): # Make sure all is OK.
				while http.get_status() == HTTPClient.STATUS_REQUESTING:
					# Keep polling for as long as the request is being processed.
					http.poll()
					await get_tree().process_frame

				if(http.get_status() == HTTPClient.STATUS_BODY or http.get_status() == HTTPClient.STATUS_CONNECTED): # Make sure request finished well.

					if http.has_response():
						# If there is a response...
						var rb = PackedByteArray() # Array that will hold the data.
						while http.get_status() == HTTPClient.STATUS_BODY:
							# While there is body left to be read
							http.poll()
							# Get a chunk.
							var chunk = http.read_response_body_chunk()
							if chunk.size() == 0:
								await get_tree().process_frame
							else:
								rb = rb + chunk # Append to read buffer.
						# Done!
						call(callback, rb.get_string_from_ascii())
					else:
						call(callback, "response error")
				else:
					call(callback, "request error")
			else:
				call(callback, "request error")
		else:
			call(callback, "connection error")
	else:
		call(callback, "connection error")
