defmodule AiReality2Vars.Main do
# *******************************************************************************************************************************************
@moduledoc """
  Module for managing the main supervisor tree for the AiReality2Vars App.

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)
"""
# *******************************************************************************************************************************************
  @doc false
  use DynamicSupervisor, restart: :transient

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Supervisor Callbacks
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @doc false
  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end


  @impl true
  def init(init_arg) do
    DynamicSupervisor.init( strategy: :one_for_one, extra_arguments: [init_arg] )
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  def create(name) do
    case Process.whereis(String.to_atom(name)) do
      nil->
        case DynamicSupervisor.start_child(__MODULE__, AiReality2Vars.Data.child_spec(String.to_atom(name))) do
          {:ok, _pid} ->
            {:ok}
          error -> error
        end
      _pid ->
        # Clear the data store so there is no old data that hackers might be able to access in the case this was a reused ID
        GenServer.call(String.to_atom(name), %{command: "clear"})
        {:ok}
    end
  end

  def delete(name) do
    case Process.whereis(String.to_atom(name)) do
      nil->
        {:error, :existance}
      pid ->
        DynamicSupervisor.terminate_child(__MODULE__, pid)
    end
  end
end
# -----------------------------------------------------------------------------------------------------------------------------------------
