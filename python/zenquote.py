from gql import gql, Client
from gql.transport.requests import RequestsHTTPTransport
import threading
from sentant_websocket import subscribe   
   
if __name__ == '__main__':
    # Select your transport with a defined url endpoint
    transport = RequestsHTTPTransport(url="http://localhost:8080/reality2", verify=False, retries=3)

    # Create a GraphQL client using the defined transport
    client = Client(transport=transport, fetch_schema_from_transport=True)

    # Read the file
    with open('zenquote.yaml', 'r') as file:
        yamlDefinition = file.read()  
        
    # Print it out for checking
    print (yamlDefinition)

    # Create a GraphQL mutation for loading the sentant
    load_sentant = gql(
        """
        mutation SentantLoad($yamlDefinition: String!) {
            sentantLoad(yamlDefinition: $yamlDefinition) {
                id
            }
        }
        """
    )

    # Create the Sentant from the YAML file
    result = client.execute(load_sentant, variable_values={"yamlDefinition": yamlDefinition})
    print(result)

    # Grab the ID of the Sentant
    id = result["sentantLoad"]["id"]
    
    # Start the subscription to the Sentant
    threading.Thread(target=subscribe, args=("ws://localhost:8080/reality2/websocket", id, "zenquote_response",)).start()

    # Set up the send event mutation
    send_event = gql(
        """
        mutation SentantSend($id: UUID4!, $event: String!) {
            sentantSend(id: $id, event: $event) {
                description
                name
            }
        }
        """
    )

    # Send the event to the Sentant
    client.execute(send_event, variable_values={"id": id, "event": "zenquote"})

