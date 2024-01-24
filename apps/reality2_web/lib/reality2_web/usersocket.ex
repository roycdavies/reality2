defmodule Reality2Web.UserSocket do
  use Phoenix.Socket
  use Absinthe.Phoenix.Socket, schema: Reality2Web.Schema

  def connect(params, socket) do
    IO.puts("UserSocket.connect params: #{inspect(params)}")

    {:ok, socket}
  end

  def id(socket) do
    IO.puts("UserSocket.id socket: #{inspect(socket)}")
    nil
  end
end
