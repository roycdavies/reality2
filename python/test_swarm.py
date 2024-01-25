from gql import gql, Client
from gql.transport.requests import RequestsHTTPTransport
import time

# Select your transport with a defined url endpoint
transport = RequestsHTTPTransport(url="https://localhost:4001/reality2", verify=False, retries=3)

# Create a GraphQL client using the defined transport
client = Client(transport=transport, fetch_schema_from_transport=True)

with open('light_and_switch.yaml', 'r') as file:
    yamlDefinition = file.read()

# Provide a GraphQL query
load_swarm = gql(
    """
    mutation SwarmLoad($yamlDefinition: String!) {
        swarmLoad(yamlDefinition: $yamlDefinition) {
            description
            name
            sentants {
                id
                name
            }
        }
    }
    """
)

# Execute the query on the transport
result = client.execute(load_swarm, variable_values={"yamlDefinition": yamlDefinition})
print(result)

id = result["swarmLoad"]["sentants"][0]["id"]

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

client.execute(send_event, variable_values={"id": id, "event": "turn_on"})
