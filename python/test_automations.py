import time
from reality2 import Reality2

graphql_http_url = "https://localhost:4001/reality2"
graphql_webs_url = "wss://localhost:4001/reality2/websocket"

reality2_node = Reality2(graphql_http_url, graphql_webs_url)

def printout(data):
    print(data)

with open('simple_light_and_switch.yaml', 'r') as file:
    yamlDefinition = file.read()
    
result = reality2_node.loadSwarm(yamlDefinition)
print(result)

# Get the resulting IDs
id_switch = result["swarmLoad"]["sentants"][0]["id"]
id_bulb = result["swarmLoad"]["sentants"][1]["id"]

# Start the subscription to the Sentant
reality2_node.sentantEvent(id_bulb, "turn_on", printout)
reality2_node.sentantEvent(id_bulb, "turn_off", printout)

# Wait a moment
time.sleep(1)

# Send some events
reality2_node.sendEvent(id_switch, "turn_on")
reality2_node.sendEvent(id_switch, "turn_off")
