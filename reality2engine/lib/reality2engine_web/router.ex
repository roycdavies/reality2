defmodule Reality2engineWeb.Router do
  use Reality2engineWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :graphql do
    # Will be used later
  end

  scope "/api", Reality2engineWeb do
    pipe_through :graphql

    forward "/", Absinthe.Plug, schema: Reality2engineWeb.Schema
  end

  if Mix.env == :dev do
    forward "/graphiql", Absinthe.Plug.GraphiQL,
    schema: Reality2engineWeb.Schema,
    # interface: :simple,
    # interface: :advanced,
    interface: :playground,
    context: %{pubsub: Reality2engineWeb.Endpoint}
  end

  # Enable LiveDashboard in development
  if Application.compile_env(:reality2engine, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through [:fetch_session, :protect_from_forgery]

      live_dashboard "/dashboard", metrics: Reality2engineWeb.Telemetry
    end
  end
end
