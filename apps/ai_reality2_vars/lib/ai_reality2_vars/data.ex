defmodule AiReality2Vars.Data do
# *******************************************************************************************************************************************
@moduledoc """
  Manage key / value pairs.  This may be implemented however desired, but in this case, it is simply using a Map inside a GenServer.
  Put this in your supervisor tree.

  **Author**
  - Dr. Roy C. Davies
  - [roycdavies.github.io](https://roycdavies.github.io/)

```elixir
  defmodule MyApplication do
    use Application

    @impl true
    def start(_type, _args) do
      children = [
        %{id: :Data, start: {Reality2.Data, :start_link, [:Data]}}
      ]

      Supervisor.start_link(children, strategy: :one_for_one, name: __MODULE__)
    end
  end
```
"""
# *******************************************************************************************************************************************

  @doc false
  use GenServer, restart: :transient

  # -----------------------------------------------------------------------------------------------------------------------------------------
  # GenServer callbacks
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @doc false
  def start_link(_, name),                              do: GenServer.start_link(__MODULE__, %{}, name: name)

  @doc false
  def init(state),                                      do: {:ok, state}
  # -----------------------------------------------------------------------------------------------------------------------------------------


  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # # Public Functions
  # # -----------------------------------------------------------------------------------------------------------------------------------------

  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # # Set a key / value pair
  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # @spec set(String.t() | atom() | pid() | {atom(), any()} | {:via, atom(), any()}, String.t() | atom(), any()) :: any()
  # @doc """
  # Set a key / value pair.

  # - Parameters
  #   - `name` - The name of the Store.
  #   - `key` - The key of the key / value pair.
  #   - `value` - The value of the key / value pair.

  # - Returns
  #   - `:ok`

  # - Example
  #   ```elixir
  #   Reality2.Data.set(:Data, :the_answer, 42)
  #   ```
  # """
  # def set(name, key, value) when is_binary(name),       do: check_existance(String.to_atom(name), fn () -> GenServer.call(String.to_atom(name), {:set, key, value}) end)
  # def set(name, key, value) when is_atom(name),         do: check_existance(name, fn () -> GenServer.call(name, {:set, key, value}) end)
  # def set(_, _, _),                                     do: {:error, :name}
  # # -----------------------------------------------------------------------------------------------------------------------------------------

  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # # Get a value
  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # @spec get(String.t() | atom() | pid() | {atom(), any()} | {:via, atom(), any()}, String.t() | atom()) :: any()
  # @doc """
  # Get a value.

  # - Parameters
  #   - `name` - The name of the Store.
  #   - `key` - The key of the key / value pair.

  # - Returns
  #   - The value of the key / value pair, or `nil` if the key does not exist.

  # - Example
  #   ```elixir
  #   Reality2.Data.get(:Data, :the_answer)
  #   ```
  # """
  # def get(name, key) when is_binary(name),              do: check_existance(String.to_atom(name), fn () -> GenServer.call(String.to_atom(name), {:get, key}) end)
  # def get(name, key) when is_atom(name),                do: check_existance(name, fn () -> GenServer.call(name, {:get, key}) end)
  # def get(_, _),                                        do: {:error, :name}
  # # -----------------------------------------------------------------------------------------------------------------------------------------

  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # # Delete a key / value pair
  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # @spec delete(String.t() | atom() | pid() | {atom(), any()} | {:via, atom(), any()}, String.t() | atom()) :: any()
  # @doc """
  # Delete a key / value pair.

  # - Parameters
  #   - `name` - The name of the Store.
  #   - `key` - The key of the key / value pair.

  # - Returns
  #   - `:ok`

  # - Example
  #   ```elixir
  #   Reality2.Data.delete(:Data, :the_answer)
  #   ```
  # """
  # def delete(name, key) when is_binary(name),           do: check_existance(String.to_atom(name), fn () -> GenServer.call(String.to_atom(name), {:delete, key}) end)
  # def delete(name, key) when is_atom(name),             do: check_existance(name, fn () -> GenServer.call(name, {:delete, key}) end)
  # def delete(_, _),                                     do: {:error, :name}
  # # -----------------------------------------------------------------------------------------------------------------------------------------

  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # # Get all key / value pairs
  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # @spec all(String.t() | atom() | pid() | {atom(), any()} | {:via, atom(), any()}) :: any()
  # @doc """
  # Get all key / value pairs.

  # - Parameters
  #   - `name` - The name of the Store.

  # - Returns
  #   - A Map of all key / value pairs.

  # - Example
  #   ```elixir
  #   Reality2.Data.all(:Data)
  #   ```
  # """
  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # def all(name) when is_binary(name),                   do: check_existance(String.to_atom(name), fn () -> GenServer.call(String.to_atom(name), {:all}) end)
  # def all(name) when is_atom(name),                     do: check_existance(name, fn () -> GenServer.call(name, {:all}) end)
  # def all(_),                                           do: {:error, :name}
  # # -----------------------------------------------------------------------------------------------------------------------------------------


  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Clear all key / value pairs
  # -----------------------------------------------------------------------------------------------------------------------------------------
  @spec clear(String.t() | atom() | pid() | {atom(), any()} | {:via, atom(), any()}) :: any()
  @doc """
  Clear all key / value pairs.

  - Parameters
    - `name` - The name of the Store.

  - Returns
    - `:ok`

  - Example
    ```elixir
    Reality2.Data.clear(:Data)
    ```
  """
  def clear(name) when is_binary(name),                 do: check_existance(String.to_atom(name), fn () -> GenServer.call(String.to_atom(name), {:clear}) end)
  def clear(name) when is_atom(name),                   do: check_existance(name, fn () -> GenServer.call(name, {:clear}) end)
  def clear(_),                                         do: {:error, :name}
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # GenServer callbacks
  # # -----------------------------------------------------------------------------------------------------------------------------------------
  # def handle_call({:set, key, value}, _from, state),    do: {:reply, :ok, Map.put(state, key, value)}
  # def handle_call({:delete, key}, _from, state),        do: {:reply, :ok, Map.delete(state, key)}
  # def handle_call({:get, key}, _from, state),           do: {:reply, Map.get(state, key, nil), state}
  # def handle_call({:all}, _from, state),                do: {:reply, state, state}
  # def handle_call({:clear}, _from, _state),             do: {:reply, :ok, %{}}
  # def handle_call(_, _from, state),                     do: {:reply, {:error, :unknown_command}, state}

  def handle_call(%{command: "set", parameters: %{key: key, value: value}}, _from, state),          do: {:reply, :ok, Map.put(state, key, value)}
  def handle_call(%{command: "set", parameters: %{"key" => key, "value" => value}}, _from, state),  do: {:reply, :ok, Map.put(state, key, value)}
  def handle_call(%{command: "delete", parameters: %{key: key}}, _from, state),                     do: {:reply, :ok, Map.delete(state, key)}
  def handle_call(%{command: "delete", parameters: %{"key" => key}}, _from, state),                 do: {:reply, :ok, Map.delete(state, key)}
  def handle_call(%{command: "get", parameters: %{key: key}}, _from, state),                        do: {:reply, Map.get(state, key, nil), state}
  def handle_call(%{command: "get", parameters: %{"key" => key}}, _from, state),                    do: {:reply, Map.get(state, key, nil), state}
  def handle_call(%{command: "all"}, _from, state),                                                 do: {:reply, state, state}
  def handle_call(%{command: "clear"}, _from, _state),                                              do: {:reply, :ok, %{}}
  def handle_call(_, _from, state),                                                                 do: {:reply, {:error, :unknown_command}, state}
  # -----------------------------------------------------------------------------------------------------------------------------------------



  # -----------------------------------------------------------------------------------------------------------------------------------------
  # Private Functions
  # -----------------------------------------------------------------------------------------------------------------------------------------
  defp check_existance(name, func) do
    case Process.whereis(name) do
      nil->
        {:error, :name}
      _pid ->
        func.()
    end
  end
  # -----------------------------------------------------------------------------------------------------------------------------------------
end
