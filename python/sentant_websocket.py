#!/usr/bin/env python

from websockets.sync.client import connect, ssl
import json
import time
import threading

def check_status(message):
    message_dict = json.loads(message)
    if "payload" in message_dict:
        if "status" in message_dict["payload"]:
            if message_dict["payload"]["status"] == "ok":
                return True
            else:
                return False
        else:
            return False
    else:
        return False
        

def subscribe(server, sentantid, event):
    join_message = {
        "topic": "__absinthe__:control",
        "event": "phx_join",
        "payload": {},
        "ref": 0
    }
    subscribe = {
        "topic": "__absinthe__:control",
        "event": "doc",
        "payload": {
            "query": "subscription {sentantEvent(id: \"" + sentantid + "\", event: \"" + event + "\") { event parameters sentant { id name } } }"
        },
        "ref": 0
    }
    heartbeat = {
        "topic": "phoenix",
        "event": "heartbeat",
        "payload": {},
        "ref": 0
    }
    
    # Define the heartbeat thread that keeps the connection alive
    def heartbeat_thread(websocket):
        while True:
            time.sleep(30)
            websocket.send(json.dumps(heartbeat))
            
    ssl_context = ssl.create_default_context() 
    ssl_context.check_hostname = False
    ssl_context.verify_mode = ssl.CERT_NONE

    # Connect to the server, join the channel and subscribe to the sentant event        
    with connect(server, ssl_context=ssl_context) as websocket:
        # Join the channel
        websocket.send(json.dumps(join_message))
        message = websocket.recv()
        if (check_status(message)): 
            print(f"Joined: {server}")
        else:
            print(f"Failed to join: {server}")
            return
            
        # Subscribe to the Sentant and event    
        websocket.send(json.dumps(subscribe))
        message = websocket.recv()
        if (check_status(message)): 
            print(f"Subscribed to {sentantid}|{event}")
        else:
            print(f"Failed to subscribe to {sentantid}|{event}")
            return
                    
        # Start the heartbeat thread
        threading.Thread(target=heartbeat_thread, args=(websocket,)).start()
        
        # Listen for messages
        while True:
            message = websocket.recv()
            message_json = json.loads(message)
            payload = message_json["payload"]
            
            if check_status(message):
                print(f"heartbeat")
            else:       
                if "result" in payload:
                    data = payload["result"]["data"]
                    print(f"Received: {data}")
                else:
                    print(f"Received: {payload}")