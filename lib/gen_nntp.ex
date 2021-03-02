defmodule GenNNTP do
  @moduledoc """
  A behaviour for implementing a NNTP server.
  """

  @type option :: :gen_nntp.option()

  @typep state :: any

  @port String.to_integer(System.get_env("PORT", "119"))

  @doc """
  Starts a NNTP server with a callback module.

  Similar to starting a `gen_server`.
  """
  @spec start(module(), any, [option]) :: :gen_nntp.on_start()
  defdelegate start(module, args, options), to: :gen_nntp

  @doc """
  Stops a NNTP server by its reference.

  The reference is usually the callback module.
  """
  @spec stop(module()) :: :ok
  defdelegate stop(ref), to: :gen_nntp

  @doc """
  Connects to a NNTP server.
  """
  defdelegate connect(address \\ "localhost", port \\ @port, options \\ []), to: :gen_nntp

  @doc """
  Sends a command and receives server's response.
  """
  defdelegate command(socket, command, args \\ []), to: :gen_nntp

  @callback init(any()) ::
    {:ok, state} | {:ok, state, timeout | :hibernate} |
    :ignore | {:stop, reason :: term}

  @callback handle_CAPABILITIES(state) :: {:ok, capabilities :: [String.t()], state}

  @callback handle_command(command :: String.t(), state) ::
    {:reply, response :: any(), state} |
    {:noreply, state} |
    {:stop, reason :: any(), state} |
    {:stop, reason :: any(), response :: any(), state}
end
