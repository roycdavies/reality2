from reality2 import Reality2
import time

def printout(data):
    print(data["awaitSignal"]["parameters"]["answer"])
    

reality2_node = Reality2("localhost", 4002, False)
reality2_node.sentantUnloadByName("Ask Question")
  
# Read the files
with open('../../OPENAI_API_KEY.txt', 'r') as file:
    OPENAI_API_KEY = file.read()
with open('chatgpt_question.yaml', 'r') as file:
    yamlDefinition = file.read()  
    
# Replace the OPENAI_API_KEY in the YAML file
yamlDefinition = yamlDefinition.replace("__openai_api_key__", OPENAI_API_KEY)

# Load the Sentant
result = reality2_node.sentantLoad(yamlDefinition)

# Grab the ID of the Sentant
id = result["sentantLoad"]["id"]

# Start the subscription to the Sentant
reality2_node.awaitSignal(id, "chatgpt_answer", printout)

# Wait a moment
time.sleep(1)

# Send the event to the Sentant
reality2_node.sentantSend(id, "chatgpt")
reality2_node.sentantSend(id, "chatgpt", {"message": "What is the meaning of life?"})
reality2_node.sentantSend(id, "chatgpt", {"message": "Give me 10 topics for teaching about 3D printers"})

time.sleep(10)
reality2_node.close()