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

  ## API
  @spec start([callback], [GenNNTP.option()]) :: :ignore | {:error, any()} | {:ok, pid()}
  def start(callbacks \\ [], options \\ []) do
    GenNNTP.start(__MODULE__, callbacks, options)
  end

  ## GenNNTP callbacks

  @impl GenNNTP
  def init(options \\ []) do
    # Init arguments if any.
    args = Access.get(options, :args)

    case maybe_apply(options, :init, [args], {:ok, args}) do
      {:ok, state} ->
        client = put_in(options[:state], state)
        {:ok, client}

      {:ok, state, delay} ->
        client = put_in(options[:state], state)
        {:ok, client, delay}

      other ->
        other
    end
  end

  @impl GenNNTP
  def handle_CAPABILITIES(client) do
    state = client[:state]

    case maybe_apply(client, :handle_CAPABILITIES, [state], {:ok, [], state}) do
      {:ok, capabilities, state} ->
        client = put_in(client[:state], state)
        {:ok, capabilities, client}
      other ->
        other
    end
  end

  @impl GenNNTP
  def handle_GROUP(group, client) do
    state = client[:state]

    case maybe_apply(client, :handle_GROUP, [group, state], {:ok, {group, 0, 0, 0}, state}) do
      {:ok, group_info, state} ->
        client = put_in(client[:state], state)
        {:ok, group_info, client}
      other ->
        other
    end
  end

  @impl GenNNTP
  def handle_LISTGROUP(group, client) do
    state = client[:state]

    case maybe_apply(client, :handle_LISTGROUP, [group, state], {:ok, {group, 0, 0, 0}, state}) do
      {:ok, group_info, state} ->
        client = put_in(client[:state], state)
        {:ok, group_info, client}
      other ->
        other
    end
  end

  @impl GenNNTP
  def handle_ARTICLE(arg, client) do
    state = client[:state]

    case maybe_apply(client, :handle_ARTICLE, [arg, state], {:ok, {0, {arg, %{}, ""}}, state}) do
      {:ok, article_info, state} ->
        client = put_in(client[:state], state)
        {:ok, article_info, client}
      other ->
        other
    end
  end

  @impl GenNNTP
  def handle_HEAD(arg, client) do
    state = client[:state]

    case maybe_apply(client, :handle_HEAD, [arg, state], {:ok, {0, {arg, %{}, ""}}, state}) do
      {:ok, article_info, state} ->
        client = put_in(client[:state], state)
        {:ok, article_info, client}
      other ->
        other
    end
  end

  @impl GenNNTP
  def handle_BODY(arg, client) do
    state = client[:state]

    case maybe_apply(client, :handle_BODY, [arg, state], {:ok, {0, {arg, %{}, ""}}, state}) do
      {:ok, article_info, state} ->
        client = put_in(client[:state], state)
        {:ok, article_info, client}
      other ->
        other
    end
  end

  @impl GenNNTP
  def handle_STAT(arg, client) do
    state = client[:state]

    case maybe_apply(client, :handle_STAT, [arg, state], {:ok, {0, {arg, %{}, ""}}, state}) do
      {:ok, article_info, state} ->
        client = put_in(client[:state], state)
        {:ok, article_info, client}
      other ->
        other
    end
  end

  @impl GenNNTP
  def handle_HELP(client) do
    state = client[:state]

    case maybe_apply(client, :handle_HELP, [state], {:ok, "", state}) do
      {:ok, help_text, state} ->
        client = put_in(client[:state], state)
        {:ok, help_text, client}
      other ->
        other
    end
  end

  @impl GenNNTP
  def handle_command(_command, state) do
    {:noreply, state}
  end

  defp maybe_apply(server, fun, args, default_reply) do
    case Access.get(server, fun) do
      nil ->
        default_reply

      callback when is_function(callback) ->
        apply(callback, args)
    end
  end
end
