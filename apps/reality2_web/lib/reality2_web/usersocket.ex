defmodule Reality2Web.UserSocket do
  use Phoenix.Socket
  use Absinthe.Phoenix.Socket, schema: Reality2Web.Schema

  @impl true
  def connect(query_params, socket, connect_info) do
    IO.puts("Connect #{inspect(connect_info)}")
    case authorize_user(socket, query_params) do
      {:ok, user} ->
        {:ok, Absinthe.Phoenix.Socket.put_options(socket, context: %{current_user: user})}
      {:error, _reason} ->
        # Reject the connection
        :error
    end
  end

  defp authorize_user(socket, query_params) do
    # Fetch the user here (for example, from a token in the query_params)
    IO.puts("Checking User, #{inspect(socket)} #{inspect(query_params)}")
    {:ok, :fred}
  end

  def id(socket) do
    IO.puts("UserSocket.id socket: #{inspect(socket)}")
    nil
  end
end
