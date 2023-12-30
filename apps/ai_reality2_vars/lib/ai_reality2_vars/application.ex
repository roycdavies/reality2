defmodule AiReality2Vars.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Starts a worker by calling: AiReality2Vars.Worker.start_link(arg)
      # {AiReality2Vars.Worker, arg}
      %{id: AiReality2Vars.Main, start: {AiReality2Vars.Main, :start_link, [AiReality2Vars.Main]}}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: AiReality2Vars.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
