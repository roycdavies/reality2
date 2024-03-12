import time
from reality2 import Reality2

reality2_node = Reality2("localhost", 4001)
#reality2_node.sentantUnloadByName("monitor")

def printout(data):
    print(data)

#with open('monitor.json', 'r') as file:
    #definition = file.read()
    
#result = reality2_node.sentantLoad(definition)
#print(result)

result = reality2_node.sentantGet("", "monitor")

# Get the resulting ID
id_monitor = result["sentantGet"]["id"]

# Start the subscriptions to the Sentant
reality2_node.awaitSignal(id_monitor, "internal", printout)

# Doesn't exit without CTRL-C
