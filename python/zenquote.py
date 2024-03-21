from reality2 import Reality2
import time

def printout(data):
    if ("awaitSignal" in data):
        print(data["awaitSignal"]["parameters"]["zenquote"])
    else:
        print(data)
    

reality2_node = Reality2("localhost", 4001)
reality2_node.sentantUnloadByName("Zen Quote")

# Read the files
with open('zenquote.yaml', 'r') as file:
    yamlDefinition = file.read()  
    
# Load the Sentant
result = reality2_node.sentantLoad(yamlDefinition)
print(result)

# Grab the ID of the Sentant
id = result["sentantLoad"]["id"]

# Start the subscription to the Sentant
reality2_node.awaitSignal(id, "zenquote_response", printout)

time.sleep(1)
# Send the event to the Sentant
result = reality2_node.sentantSend(id, "zenquote")
if (result != None):
    if ("message" in result):
        print ("ERROR: " + result["message"])
    else:
        print ("Event sent")
else:
    print("ERROR")

time.sleep(3)
reality2_node.close()
    