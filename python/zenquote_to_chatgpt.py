from reality2 import Reality2
import time

def printout(data):
    print(data["awaitSignal"]["parameters"]["zenquote"])
    

reality2_node = Reality2("localhost", 4001)
  
# Read the files
with open('zenquote_to_chatgpt.yaml', 'r') as file:
    yamlDefinition = file.read()  
    
# Load the Sentant
result = reality2_node.loadSentant(yamlDefinition)
print(result)

# Grab the ID of the Sentant
id = result["sentantLoad"]["id"]

# Start the subscription to the Sentant
reality2_node.awaitSignal(id, "zenquote_answer", printout)
reality2_node.awaitSignal(id, "chatgpt_answer", printout)

time.sleep(1)
# Send the event to the Sentant
reality2_node.sendEvent(id, "zenquote")
    