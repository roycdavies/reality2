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

# Avoid errors with self-signed certificates.
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
    __secure: True
    
    __client: Client
    __transport: RequestsHTTPTransport
    
    __events = []
    # --------------------------------------------------------------------------------------------------------------------------------------------------


    
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Constructor
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __init__ (self, domain_name, port, ssl = True):
        self.__secure = ssl
        if (ssl):
            self.__graphql_http_url = "https://" + domain_name + ":" + str(port) + "/reality2"
            self.__graphql_webs_url = "wss://" + domain_name + ":" + str(port) + "/reality2/websocket"
        else:
            self.__graphql_http_url = "http://" + domain_name + ":" + str(port) + "/reality2"
            self.__graphql_webs_url = "ws://" + domain_name + ":" + str(port) + "/reality2/websocket"
        
        # Select your transport with a defined url endpoint
        self.__transport = RequestsHTTPTransport(url=self.__graphql_http_url, verify=False, retries=3)
        
        # Create a GraphQL client using the defined transport
        self.__client = Client(transport=self.__transport, fetch_schema_from_transport=True)
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def close(self):
        for thread in self.__events:
            thread.set()
        self.__client.close_sync()
        self.__transport.close()  
    # --------------------------------------------------------------------------------------------------------------------------------------------------

    
    
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Destructor - Close the connection(s)
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __del__ (self):
        self.close()
    # --------------------------------------------------------------------------------------------------------------------------------------------------
        
        
        
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Public GraphQL methods
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Queries
    def sentantAll (self, details = "id name"):
        try:
            return self.__client.execute(self.__sentant_all(details))
        except:
            return None
    
    def sentantGet (self, id="", name = "", details = "id name"):
        try:
            if (id != ""):
                return self.__client.execute(self.__sentant_get_by_id(details), variable_values={"id": id})
            else:
                return self.__client.execute(self.__sentant_get_by_name(details), variable_values={"name": name})
        except:
            return None
    
    # Mutations
    def sentantLoad (self, definition, details = "id name"):
        try:
            return self.__client.execute(self.__sentant_load(details), variable_values={"definition": definition})
        except:
            return None
    
    def swarmLoad (self, definition, details = "id name"):
        try:
            return self.__client.execute(self.__swarm_load(details), variable_values={"definition": definition})
        except:
            return None
    
    def sentantSend (self, id, event, parameters = {}, details = "description name"):
        try:
            return self.__client.execute(self.__sentant_send(details), variable_values={"id": id, "event": event, "parameters": json.dumps(parameters)})
        except:
            return None
    
    def sentantUnload (self, id, details = "id name"):
        try:
            return self.__client.execute(self.__sentant_unload(details), variable_values={"id": id})
        except:
            return None
    
    def sentantUnloadByName (self, name, details = "id name"):
        sentant = self.sentantGet(name=name, details="id")
        if (sentant and sentant["sentantGet"]):
            try:
                return self.__client.execute(self.__sentant_unload(details), variable_values={"id": sentant["sentantGet"]["id"]})
            except:
                return None
        else:
            return None
    
    def sentantUnloadAll (self):
        try:
            sentants = self.sentantAll()
            for sentant in sentants["sentantAll"]:
                self.sentantUnload(sentant["id"])
        except:
            return None
    
    # Subscriptions
    def awaitSignal (self, id, signal, callback=None, details="event parameters passthrough sentant { id name }"):
        newEvent = threading.Event()
        self.__events.append(newEvent)
        newThread = threading.Thread(target=self.__subscribe, args=(self.__graphql_webs_url, id, signal, callback, details, newEvent, ))
        newThread.start()
    # --------------------------------------------------------------------------------------------------------------------------------------------------

        
    
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Private methods
    # --------------------------------------------------------------------------------------------------------------------------------------------------



    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Check the status of the websocket
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __check_status (self, message):
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
    # Define the heartbeat thread that keeps the websocket connection alive (to be called in it's own a thread)
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __heartbeat_thread (self, websocket, running: threading.Event):
        heartbeat = {
            "topic": "phoenix",
            "event": "heartbeat",
            "payload": {},
            "ref": 0
        }
        
        while not running.is_set():
            for i in range(30):
                time.sleep(1)
                if running.is_set(): break
            websocket.send(json.dumps(heartbeat))
    # --------------------------------------------------------------------------------------------------------------------------------------------------

 

    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Scubscribe to the Node channel representing the sentant and signal
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __subscribe (self, server, sentantid, signal, callback, details, running: threading.Event):
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
                "query": self.__await_signal(details),
                "variables": {
                    "id": sentantid,
                    "signal": signal
                }
            },
            "ref": 0
        }
        
        # Connect to the server, join the channel and subscribe to the sentant event
        if (self.__secure):
            # Create the SSL context
            ssl_context = ssl.create_default_context() 
            ssl_context.check_hostname = False
            ssl_context.verify_mode = ssl.CERT_NONE
            
            with connect(server, ssl_context=ssl_context) as websocket:
                self.__after_connect(websocket, join_message, subscribe, sentantid, signal, callback, server, running)
        else:
            with connect(server) as websocket:
                self.__after_connect(websocket, join_message, subscribe, sentantid, signal, callback, server, running)
            
            
    def __after_connect(self, websocket, join_message, subscribe, sentantid, signal, callback, server, running: threading.Event):
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
            print(f"Subscribed to {sentantid}|{signal}")
        else:
            print(f"Failed to subscribe to {sentantid}|{signal}")
            return
                    
        # Start the heartbeat thread
        threading.Thread(target=self.__heartbeat_thread, args=(websocket, running, )).start()
        
        # Listen for messages
        while not running.is_set():
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
    # Await Signal definition (note the lack of gql() is on purpose as this is a subscription to the websocket
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __await_signal (self, details):
        return (
        """
        subscription AwaitSignal($id: UUID4!, $signal: String!) {
            awaitSignal(id: $id, signal: $signal) {
                """ + details + """
            }
        }
        """
    )
    # --------------------------------------------------------------------------------------------------------------------------------------------------




    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Load Swarm definition
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __swarm_load (self, details):
        return (gql(
        """
        mutation SwarmLoad($definition: String!) {
            swarmLoad(definition: $definition) {
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
    def __sentant_send (self, details):
        return (gql(
        """
        mutation SentantSend($id: UUID4!, $event: String!, $parameters: Json) {
            sentantSend(id: $id, event: $event, parameters: $parameters) {
            """ + details + """
            }
        }
        """
    ))
    # --------------------------------------------------------------------------------------------------------------------------------------------------



    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Load a Sentant definition
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __sentant_load (self, details):
        return (gql(
        """
        mutation SentantLoad($definition: String!) {
            sentantLoad(definition: $definition) {
                """ + details + """
            }
        }
        """
    ))
    # --------------------------------------------------------------------------------------------------------------------------------------------------



    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Unload a Sentant
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __sentant_unload (self, details):
        return (gql(
        """
        mutation SentantUnload($id: UUID4!) {
            sentantUnload(id: $id) {
                """ + details + """
            }
        }
        """
    ))
    # --------------------------------------------------------------------------------------------------------------------------------------------------



    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Get a Sentant's details
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __sentant_get_by_id (self, details):
        return (gql(
        """
        query SentantGet($id: UUID4) {
            sentantGet(id: $id) {
                """ + details + """
            }
        }
        """
    ))
    # --------------------------------------------------------------------------------------------------------------------------------------------------



    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Get a Sentant's details
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __sentant_get_by_name (self, details):
        return (gql(
        """
        query SentantGet($name: String) {
            sentantGet(name: $name) {
                """ + details + """
            }
        }
        """
    ))
    # --------------------------------------------------------------------------------------------------------------------------------------------------



    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Get all Sentant's details
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    def __sentant_all (self, details):
        return (gql(
        """
        query SentantAll {
            sentantAll {
                """ + details + """
            }
        }
        """
    ))
    # --------------------------------------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------------------------------------------
