defmodule Reality2.Metadata do
# ********************************************************************************************************************************************
@moduledoc """
  Manage key / values pairs.  This may be implemented however desired, but in this case, it is simply using a Map inside a GenServer.

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)
"""
# ********************************************************************************************************************************************

  use GenServer

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # GenServer callbacks
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @doc false
  def start_link(name),                                 do: GenServer.start_link(__MODULE__, %{}, name: name)

  @doc false
  def init(state),                                      do: {:ok, state}

  @doc false
  def child_spec(name),                                 do: %{id: Reality2.Metadata, start: {Reality2.Metadata, :start_link, [name]}}
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec set(atom() | pid() | {atom(), any()} | {:via, atom(), any()}, any(), any()) :: any()
  @doc """
  Set a key / value pair.

  **Parameters**
  - `name` - The name of the Store.
  - `key` - The key of the key / value pair.
  - `value` - The value of the key / value pair.
  """
  def set(name, key, value),                            do: GenServer.call(name, {:set, key, value})
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec get(atom() | pid() | {atom(), any()} | {:via, atom(), any()}, any()) :: any()
  @doc """
  Get a value.

  **Parameters**
  - `name` - The name of the Store.
  - `key` - The key of the key / value pair.
  """
  def get(name, key),                                   do: GenServer.call(name, {:get, key})
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec delete(atom() | pid() | {atom(), any()} | {:via, atom(), any()}, any()) :: any()
  @doc """
  Delete a key / value pair.

  **Parameters**
  - `name` - The name of the Store.
  - `key` - The key of the key / value pair.
  """
  def delete(name, key),                                do: GenServer.call(name, {:delete, key})
  # -----------------------------------------------------------------------------------------------------------------------------------------

  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec all(atom() | pid() | {atom(), any()} | {:via, atom(), any()}) :: any()
  @doc """
  Get all key / value pairs.

  **Parameters**
  - `name` - The name of the Store.
  """
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def all(name),                                        do: GenServer.call(name, {:all})
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # GenServer callbacks
  # -----------------------------------------------------------------------------------------------------------------------------------------
  def handle_call({:set, key, value}, _from, state),    do: {:reply, :ok, Map.put(state, key, value)}
  def handle_call({:delete, key}, _from, state),        do: {:reply, :ok, Map.delete(state, key)}
  def handle_call({:get, key}, _from, state),           do: {:reply, Map.get(state, key, nil), state}
  def handle_call({:all}, _from, state),                do: {:reply, state, state}
  # -----------------------------------------------------------------------------------------------------------------------------------------

end
