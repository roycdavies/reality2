defmodule Reality2engine.Sentant do
  # Each Sentant consists of a number of sub processes
  # Sentant (which is a supervisor)
  #   +- Automations Supervisor
  #   |   +- Automation 1
  #   |   +- Automation 2
  #   |   +- ...
  #   +- Comms (PubSub) Supervisor
end
