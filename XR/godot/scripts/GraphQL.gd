extends Node

signal sentantAll_response
signal sentantGet_response


var url = "https://localhost:4001/reality2"
var headers = ["Content-Type: text/plain", "Accept: */*"]	


# Called when the node enters the scene tree for the first time.
func sentantAll():
	var query = '
	query SentantAll {
	  sentantAll {
		description
		id
		name
	  }
	}'

	$HTTPRequest.request_completed.connect(_on_sentantAll)
	$HTTPRequest.set_tls_options(TLSOptions.client_unsafe())
	$HTTPRequest.request(url, headers, HTTPClient.METHOD_POST, query)


func sentantGetByID(id:String):
	var query = '
	query SentantGet {
	  sentantGet(id: "' + id + '") {
		description
		id
		name
	  }
	}'
	
	var request = HTTPRequest.new()
	add_child(request)

	request.request_completed.connect(_on_sentantGet)
	request.set_tls_options(TLSOptions.client_unsafe())
	request.request(url, headers, HTTPClient.METHOD_POST, query)



# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	pass
	
	
func _on_sentantAll(_result, response_code, _headers, body):
	if (response_code == 200):
		var json = JSON.parse_string(body.get_string_from_utf8())
		if (json.has("errors")):
			print (json["errors"])
		else:
			print(json["data"]["sentantAll"])
			sentantAll_response.emit(json["data"]["sentantAll"])
	else:
		print("ERROR")
		
		

func _on_sentantGet(_result, response_code, _headers, body):
	if (response_code == 200):
		var json = JSON.parse_string(body.get_string_from_utf8())
		if (json.has("errors")):
			print (json["errors"])
		else:
			print(json)
			sentantGet_response.emit(json["data"]["sentantGet"])
	else:
		print("ERROR")
