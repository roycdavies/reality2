from reality2 import Reality2
import time

def printout(data):
    print(data["awaitSignal"])
    # print(data["awaitSignal"]["parameters"]["zenquote"])
    

reality2_node = Reality2("localhost", 4001, True)
reality2_node.sentantUnloadByName("Zen ChatGPT")

# Read the files
with open('../../OPENAI_API_KEY.txt', 'r') as file:
    OPENAI_API_KEY = file.read()
with open('zenquote_to_chatgpt.yaml', 'r') as file:
    yamlDefinition = file.read()
    
# Replace the OPENAI_API_KEY in the YAML file
yamlDefinition = yamlDefinition.replace("__openai_api_key__", OPENAI_API_KEY)
    
# Load the Sentant
result = reality2_node.sentantLoad(yamlDefinition)
print(result)

# Grab the ID of the Sentant
id = result["sentantLoad"]["id"]

# Start the subscription to the Sentant
reality2_node.awaitSignal(id, "zenquote_answer", printout)
reality2_node.awaitSignal(id, "chatgpt_answer", printout)


time.sleep(1)
# Send the event to the Sentant
reality2_node.sentantSend(id, "zenquote")
    