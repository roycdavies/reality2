# Reality2

Reality2 is a distributed platform for 'sentient' digital agents.  We call them sentient because the digital agents can be aware of the network, physical and electronic environment they exist in.  These, we call 'Sentants' - and a group of Sentants is a Swarm.

Small devices running Linux 'run' the Digital Agents, and they are aware of the network and device they are running on.  These devices, or 'Nodes', can form 'Clusters' in what we call a Transient Network.

Users interact with the digital agents directly, so the focus of attention is at the Sentant level, not the device level.

That said, at least initially, internet connected devices such as browsers, which communicate using TCP-IP, work at the device level, so the interaction with the Sentants does have to go via the node.

## GraphQL

Presently, we use a GraphQL API with queries, mutations and subscriptions.  I'll get a Postman defition file in here soon, though of course you can create your own using introspection.

## Sentant definition files

When a Node is started, it is empty of Sentants.  You load the Sentants from a text file (see the definitions folder) in YAML, TOML or JSON format.  This is somewhat equivalent to a webserver loading web-page definitions, except that Sentants can be loaded at any time.

## Clients

In the python folder is some example client code that uses the definitions in the definitions folder.

In the XR folder, there is some example client code and visualisation for godot, and soon also for unity, and perhaps later threejs.

In the node-red folder, there is some example setup for that graphical tool.  You will need to have node-red running first, of course.

## Plans

There are many plans for this platform which will be made public in due course.  For now, this is very early alpha stage.