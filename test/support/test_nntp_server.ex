defmodule TestNNTPServer do
  @moduledoc """
  A simple NNTP server for testing purpose.

  In order to test various callbacks of GenNNTP without defining
  many different modules.

  To start a test server, call `start/2` with a keyword list
  of callbacks to test, in the form of {name, function}.
  """

  @behaviour GenNNTP

  @type callback :: {atom(), fun}
  @typep state :: any()

  ## API
  @spec start([callback], [GenNNTP.option()]) :: :ignore | {:error, any()} | {:ok, pid()}
  def start(callbacks \\ [], options \\ []) do
    GenNNTP.start(__MODULE__, callbacks, options)
  end

  ## GenNNTP callbacks

  @impl GenNNTP
  @spec init(keyword()) ::
          {:ok, state}
          | :ignore
          | {:stop, reason :: term}
  def init(options \\ []) do
    # Init arguments if any.
    args = Keyword.get(options, :args)

    case maybe_apply(options, :init, [args], {:ok, args}) do
      {:ok, state} ->
        client = Keyword.put(options, :state, state)
        {:ok, client}

      {:ok, state, delay} ->
        client = Keyword.put(options, :state, state)
        {:ok, client, delay}

      other ->
        other
    end
  end

  @impl GenNNTP
  def handle_command(_command, state) do
    {:noreply, state}
  end

  defp maybe_apply(server, fun, args, default_reply) do
    case Keyword.get(server, fun) do
      nil ->
        default_reply

      callback when is_function(callback) ->
        apply(callback, args)
    end
  end
end
