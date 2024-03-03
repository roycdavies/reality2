# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Reality2 class for connecting to a Reality2 Node
# Author: Roy Davies, 2024, roycdavies.github.io
# ------------------------------------------------------------------------------------------------------------------------------------------------------
import json
import time
import threading
from gql import gql, Client
from gql.transport.requests import RequestsHTTPTransport
from websockets.sync.client import connect, ssl

import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Reality2 class for connecting to a Reality2 Node
# ------------------------------------------------------------------------------------------------------------------------------------------------------
class Reality2:
    
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Private attributes
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    __graphql_http_url: str
    __graphql_webs_url: str
    
    __client: Client
    __transport: RequestsHTTPTransport
    # --------------------------------------------------------------------------------------------------------------------------------------------------


    
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Constructor
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __init__(self, graphql_http_url, graphql_webs_url):
        self.__graphql_http_url = graphql_http_url
        self.__graphql_webs_url = graphql_webs_url
        
        # Select your transport with a defined url endpoint
        self.__transport = RequestsHTTPTransport(url=self.__graphql_http_url, verify=False, retries=3)
        
        # Create a GraphQL client using the defined transport
        self.__client = Client(transport=self.__transport, fetch_schema_from_transport=True)
    # --------------------------------------------------------------------------------------------------------------------------------------------------
        
        
        
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Public methods
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def loadSwarm(self, yamlDefinition, details = "id name"):
        return self.__client.execute(self.__load_swarm(details), variable_values={"yamlDefinition": yamlDefinition})
    
    def sendEvent(self, id, event, details = "description name"):
        return self.__client.execute(self.__send_event(details), variable_values={"id": id, "event": event})
    
    def sentantEvent(self, id, event, callback=None):
        threading.Thread(target=self.__subscribe, args=(self.__graphql_webs_url, id, event, callback,)).start()
    # --------------------------------------------------------------------------------------------------------------------------------------------------

        
    
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Private methods
    # --------------------------------------------------------------------------------------------------------------------------------------------------



    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Check the status of the websocket
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __check_status(self, message):
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
    # --------------------------------------------------------------------------------------------------------------------------------------------------



    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Define the heartbeat thread that keeps the websocket connection alive
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __heartbeat_thread(self, websocket):
        heartbeat = {
            "topic": "phoenix",
            "event": "heartbeat",
            "payload": {},
            "ref": 0
        }
        
        while True:
            time.sleep(30)
            websocket.send(json.dumps(heartbeat))
    # --------------------------------------------------------------------------------------------------------------------------------------------------

 

    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Scubscribe to the Node channel representing the sentant and signal
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __subscribe(self, server, sentantid, event, callback):
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
        
        # Create the SSL context
        ssl_context = ssl.create_default_context() 
        ssl_context.check_hostname = False
        ssl_context.verify_mode = ssl.CERT_NONE

        # Connect to the server, join the channel and subscribe to the sentant event        
        with connect(server, ssl_context=ssl_context) as websocket:
            # Join the channel
            websocket.send(json.dumps(join_message))
            message = websocket.recv()
            if (self.__check_status(message)): 
                print(f"Joined: {server}")
            else:
                print(f"Failed to join: {server}")
                return
                
            # Subscribe to the Sentant and event    
            websocket.send(json.dumps(subscribe))
            message = websocket.recv()
            if (self.__check_status(message)): 
                print(f"Subscribed to {sentantid}|{event}")
            else:
                print(f"Failed to subscribe to {sentantid}|{event}")
                return
                        
            # Start the heartbeat thread
            threading.Thread(target=self.__heartbeat_thread, args=(websocket,)).start()
            
            # Listen for messages
            while True:
                message = websocket.recv()
                message_json = json.loads(message)
                payload = message_json["payload"]
                
                if self.__check_status(message):
                    print(f"heartbeat")
                else:       
                    if "result" in payload:
                        data = payload["result"]["data"]
                        if callback:
                            callback(data)
                    elif "errors" in payload:
                        if callback:
                            callback(payload["errors"])
                    else:
                        print(f"Received: {payload}")
    # --------------------------------------------------------------------------------------------------------------------------------------------------



    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Load Swarm definition
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __load_swarm (self, details):
        return (gql(
        """
        mutation SwarmLoad($yamlDefinition: String!) {
            swarmLoad(yamlDefinition: $yamlDefinition) {
                description
                name
                sentants {
                    """ + details + """
                }
            }
        }
        """
    ))
    # --------------------------------------------------------------------------------------------------------------------------------------------------



    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Send Event definition
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __send_event (self, details):
        return (gql(
        """
        mutation SentantSend($id: UUID4!, $event: String!) {
            sentantSend(id: $id, event: $event) {
            """ + details + """
            }
        }
        """
    ))
    # --------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------
