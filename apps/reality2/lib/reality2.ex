defmodule Reality2 do
  @moduledoc """
  Reality2 Sentient Agent (Sentant) based Platform with plugin architecture for Intuitive Spatial Computing supporting Assistive Technologies.

  This is the main module for managing Sentants, Swarms and Plugins on a Node and in a Cluster.
  Primarily, this will not be accessed directly, but rather through the GraphQL API [Reality2.Web](../reality2_web/api-reference.html).

  **Plugins**
  - [ai.reality2.vars](../ai_reality2_vars/AiReality2Vars.html) - A plugin for managing variables on a Sentant.
  - [ai.reality2.geospatial](../ai_reality2_geospatial/AiReality2Geospatial.html) - Coming Soon - A plugin for managing geospatial location and context on a Sentant.
  - [ai.reality2.pathing](../ai_reality2_pathing/AiReality2Pathing.html) - Coming Soon - A plugin for managing pathing on a node, cluster and globally - interfaces with the Pathing Name System (PNS) and ensures Sentant Global Uniqueness and Addressability.

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)
  """
  @doc false
  def test() do
    Mix.Task.run("test")
  end

  @doc false
  def test_one(test_name) do
    Mix.Task.run("test", ["test/tests/" <> test_name])
  end












  def test_geospatial() do
    sentant_definition = """
    sentant:
      name: fred
      plugins:
        - name: ai.reality2.geospatial
          type: internal
      automations:
        - name: geospatial
          transitions:
            - from: start
              event: init
              to: ready
              actions:
                - plugin: ai.reality2.geospatial
                  command: set
                  parameters:
                    latitude: 10.0
                    longitude: 20.0
                    altitude: 0.0
            - from: "*"
              event: get
              to: ready
              actions:
                - command: get
                  plugin: ai.reality2.geospatial
                - command: print
            - from: "*"
              event: search
              to: ready
              actions:
                - command: search
                  plugin: ai.reality2.geospatial
                - command: print

    """

    id = case Reality2.Sentants.create(sentant_definition) do
      {:ok, id} -> IO.puts("Sentant created: #{inspect(id)}")
      {:error, reason} -> IO.puts("Sentant creation failed: #{inspect(reason)}")
    end

    # Reality2.Sentants.sendto(%{:name => "fred"}, %{:command => "set", :parameters => %{"latitude" => 0.0, "longitude" => 0.0, "altitude" => 0.0}})
    Reality2.Sentants.sendto(%{:name => "fred"}, %{:event => "get"})
    Reality2.Sentants.sendto(%{:name => "fred"}, %{:event => "search"})

    {:ok, id}
  end




  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Public Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------


  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Private Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
end
