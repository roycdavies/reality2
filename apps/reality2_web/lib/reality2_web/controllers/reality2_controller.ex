defmodule Reality2Web.Reality2Controller do
  use Reality2Web, :controller

  def index(conn, _params) do
    html(conn, File.read!("apps/reality2_web/priv/static/sentants/dist/index.html"))
  end
end
