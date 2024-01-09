defmodule Reality2Web.UserSocket do
  use Phoenix.Socket
  use Absinthe.Phoenix.Socket, schema: Reality2Web.Schema

  def connect(params, socket) do
    %{"id" => id} = params
    case Reality2.Sentants.read(%{id: id}, :definition) do
      {:ok, sentant} ->
        socket = Absinthe.Phoenix.Socket.put_options(socket, context: %{
          sentant: sentant
        })
        {:ok, socket}
      {:error, reason} ->
        {:error, reason}
      end
  end
end
