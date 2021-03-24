defmodule GenNNTP do
  @moduledoc ~S"""
  The NNTP client and server library.

  This module provides both the behaviour for an NNTP server, and the client API
  to interact with a NNTP server.

  ## Example

  The GenNNTP behaviour abstracts the common NNTP client-server interaction.
  Developers are only required to implement the callbacks and functionality
  they are interested in to respond to client's commands, per specs.

  Let's start with a code example and then explore the available callbacks.
  Imagine we want a NNTP server that serves content from a directory.

      defmodule NServ do
        @behaviour GenNNTP

        # Callbacks

        @impl true
        def init(datadir) do
          {:ok, datadir}
        end

        @impl true
        def handle_CAPABILITIES(datadir) do
          {:ok, ["READER"], datadir}
        end

        @impl true
        def handle_HELP(datadir) do
          {:ok, "This is some help text", datadir}
        end

      end

      # Start the server
      {:ok, pid} = GenNNTP.start(NServ, "priv", port: 6791)

      # This is the client
      {:ok, socket} = GenNNTP.connect("localhost", 6791)

      {:ok, response} = GenNNTP.command(socket, "CAPABILITIES")
      #=> {:ok, "101 Capability list:\r\nVERSION 2\r\nREADER"}

  We start our `NServ` by calling `start/3`, passing the module
  with the server implementation and its initial argument (a path to a folder
  containing the data to serve from). We can primarily interact with the server
  by sending a command to the socket returned after connectiong to the server.

  Every time you do a `GenNNTP.command/2`, the client will send a command that
  must be handled by one of the callbacks defined in the GenNNTP, based on the
  issued command. There are many callbckas to be implemented when you use a
  `GenNNTP`. The required callbacks are `c:init/1`, `c:handle_CAPABILITIES/1`
  and `c:handle_HELP/1`. Other callbacks are optional in the sense that they are
  still required to be implemented when your server has the capability for them.
  For example, if your server has "READER" capability, you MUST provide these
  callbacks: `c:handle_GROUP/2`, `c:handle_LISTGROUP/2`, `c:handle_NEXT/2`,
  `c:handle_LAST/2`, `c:handle_ARTICLE/2`, `c:handle_HEAD/2`, `c:handle_BODY/2`,
  `c:handle_STAT/2`.
  """

  @type option :: :gen_nntp.option()

  @typep state :: any

  # Default port from "PORT" environment variable or 199.
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

  Both single and multi-line response are handled.

  For commands that are followed by a multi-line data block, such as
  "POST", place the block as the argument to `command/3` call.
  """
  defdelegate command(socket, command, args \\ []), to: :gen_nntp

  @doc """
  Invoked when a client is connecting to the server.

  `init_arg` is the argument term (second argument) passed to `start/3`.

  Returning `{:ok, state}` wll start the handshake to establish the socket.

  Returning `{:ok, state, timeout}` is similar to `{:ok, state}`, except that
  it also sets a delay before establishing the handshake.

  Returning `:ignore` will make the process exit normally without entering the
  loop, closing the socket.

  Returning `{:stop, reason}` will cause the process to exit with reason
  `reason` without entering the loop, also closing the socket.
  """
  @callback init(init_arg :: term()) ::
    {:ok, state} | {:ok, state, timeout} |
    :ignore | {:stop, reason :: term}

  @doc """
  Invoked when a client asks for the server's capabilities.

  `state` is the current state of the NNTP server.

  The returning `capabilities` list is responded to the client, with "VERSION"
  always the first in the list. Server containues the loop with the new state.

  Only standard capabilities are responded to the client. Invalid ones in the
  callback's return are ignored.
  """
  @callback handle_CAPABILITIES(state) :: {:ok, capabilities :: [String.t()], state}

  @doc """
  Invoked when a client selects a newsgroup.

  `group` is the name of the newsgroup to be selected (e.g., "news.software").

  Returning `{:ok, group_summary, new_state}` sends the group summary to client
  and continues the loop with new state `new_state`.

  Returning `{:ok, false, new_state}` sends 411 response to tell the client of
  unavailable grop and continues the loop with new state `new_state`.

  Returning `{:error, reason}` to respond with `reason` and closes the client.
  """
  @callback handle_GROUP(group, state) ::
    {:ok, {
      group,
      number: non_neg_integer(),
      low: non_neg_integer(),
      high: non_neg_integer()
    }, state} |
    {:ok, false, state} |
    {:error, reason :: String.t(), state}
    when group: String.t()

  @doc """
  Invoked when a client selects a newsgroup.

  `group` is the name of the newsgroup to be selected (e.g., "news.software").

  Returning `{:ok, group_summary, new_state}` sends the group summary to client
  and continues the loop with new state `new_state`. The group summary is
  similar to the one responded by "GROUP" command, but also has a list of
  article numbers in the newsgroup.

  Returning `{:ok, false, new_state}` sends 411 response to tell the client of
  unavailable grop and continues the loop with new state `new_state`.

  Returning `{:error, reason}` to respond with `reason` and closes the client.
  """
  @callback handle_LISTGROUP(group, state) ::
    {:ok, {
      group,
      number: non_neg_integer(),
      low: non_neg_integer(),
      high: non_neg_integer(),
      numbers: [non_neg_integer()]
    }, state} |
    {:ok, false, state} |
    {:error, reason :: String.t(), state}
    when group: String.t()

  @doc """
  Invoked when a client selects the next article in the current newsgroup.

  The next article in that newsgroup is the lowest existing article number
  greater than the current article number.

  Returning `{:ok, { number, article }, new_state}` sends the new current
  article number and the message-id of that article to the client and continues
  the loop with new state `new_state`.

  If `number` is the same as the current article number, a 421 is responded to
  tell the client of no next article in this group.

  Returning `{:ok, false, new_state}` sends 421 response to tell the client of
  unavailable article and continues the loop with new state `new_state`.

  Returning `{:error, reason}` to respond with `reason` and closes the client.

  Note that this callback is not invoked when currently selected newsgroup is
  invalid, or the current article number is invalid. GenNNTP handles those
  cases with appropriate response code.
  """
  @callback handle_NEXT(arg, state) ::
    {:ok, { number, :gen_nttp.article() }, state }|
    {:ok, false, state} |
    {:error, reason :: String.t(), state}
    when number: non_neg_integer(),
        arg: :gen_nttp.message_id() | {number, group :: String.t()}

  @doc """
  Invoked when a client selects the previous article in the current newsgroup.

  The previous article in that newsgroup is the highest existing article number
  less than the current article number.

  Returning `{:ok, { number, article }, new_state}` sends the new current
  article number and the message-id of that article to the client and continues
  the loop with new state `new_state`.

  If `number` is the same as the current article number, a 422 is responded to
  tell the client of no next article in this group.

  Returning `{:ok, false, new_state}` sends 422 response to tell the client of
  unavailable article and continues the loop with new state `new_state`.

  Returning `{:error, reason}` to respond with `reason` and closes the client.

  Note that this callback is not invoked when currently selected newsgroup is
  invalid, or the current article number is invalid. GenNNTP handles those
  cases with appropriate response code.
  """
  @callback handle_LAST(arg, state) ::
    {:ok, { number, :gen_nttp.article() }, state }|
    {:ok, false, state} |
    {:error, reason :: String.t(), state}
    when number: non_neg_integer(),
        arg: :gen_nttp.message_id() | {number, group :: String.t()}

  @doc """
  Invoked when a client selects an article.

  The `arg` is the argument used to specify the article to retrieve. It has 2
  forms:

  - A message-id.
  - A 2-tuple containing the article number and the group name.

  Returning `{:ok, { number, article }, new_state}` sends the new current
  article number and the entire article to the client and continues
  the loop with new state `new_state`. A full article has message-id, the
  headers, and a body.

  Returning `{:ok, false, new_state}` sends 423 response to tell the client of
  unavailable article and continues the loop with new state `new_state`.

  Returning `{:error, reason}` to respond with `reason` and closes the client.

  Note that this callback is not invoked when currently selected newsgroup is
  invalid, or the current article number is invalid. GenNNTP handles those
  cases with appropriate response code. GenNNTP also handles the ARTICLE
  command with no argument, by taking the current article number instead before
  passing it to this callback.
  """
  @callback handle_ARTICLE(arg, state) ::
    {:ok, { number, :gen_nttp.article() }, state }|
    {:ok, false, state} |
    {:error, reason :: String.t(), state}
    when number: non_neg_integer(),
        arg: :gen_nttp.message_id() | {number, group :: String.t()}

  @doc """
  Invoked when a client selects headers of an article.

  This callback behaves identically to the `c:handle_ARTICLE/2` except that
  only the headers are returned in the article.
  """
  @callback handle_HEAD(arg, state) ::
    {:ok, { number, :gen_nttp.article() }, state }|
    {:ok, false, state} |
    {:error, reason :: String.t(), state}
    when number: non_neg_integer(),
        arg: :gen_nttp.message_id() | {number, group :: String.t()}

  @doc """
  Invoked when a client selects body of an article.

  This callback behaves identically to the `c:handle_ARTICLE/2` except that
  only the body is returned in the article.
  """
  @callback handle_BODY(arg, state) ::
    {:ok, { number, :gen_nttp.article() }, state }|
    {:ok, false, state} |
    {:error, reason :: String.t(), state}
    when number: non_neg_integer(),
        arg: :gen_nttp.message_id() | {number, group :: String.t()}

  @doc """
  Invoked when a client checks if an article exists.

  This callback behaves identically to the `c:handle_ARTICLE/2` except that
  only the message-id is returned in the article.

  This callback allows the client to determine whether an article exists and
  in the second forms, what its message-id is, without having to process an
  arbitrary amount of text.
  """
  @callback handle_STAT(arg, state) ::
    {:ok, { number, :gen_nttp.article() }, state }|
    {:ok, false, state} |
    {:error, reason :: String.t(), state}
    when number: non_neg_integer(),
        arg: :gen_nttp.message_id() | {number, group :: String.t()}

  @doc """
  Invoked when a client sends an article to be posted.

  The callback receives a map of type `article()` without `id` field.

  Returning `{:ok, new_state}` to accept the article.

  Returning `{:error, reason}` to reject the article with specific reason.
  """
  @callback handle_POST(article :: :gen_nttp.article(), state) ::
    {:ok, state} | {:error, reason :: String.t(), state}

  @doc """
  Invoked when a client wants summary of the server.

  Returning `{:ok, help_test, new_state}` to respond the `help_text` to the
  client and continues the loop with new state `new_state`.
  """
  @callback handle_HELP(state) :: {:ok, help_text :: String.t(), state}

  @doc """
  Invoked when an uknown command is asked by the client.

  This optional callback can be used to handle commands not understood by the
  current GenNNTP implementation. If not defined, GenNNTP responds with 500.

  The `command` is full command line sent by the client, minus the LFCR pair at
  the end. The callback can choose to `reply` or `noreply` to the command. The
  `response` does not need the ending LFCR pair.

  Returning `{:error, reason}` to respond with `reason` and closes the client.
  """
  @callback handle_command(command :: String.t(), state) ::
    {:reply, response :: any(), state} |
    {:noreply, state} |
    {:stop, reason :: any(), state} |
    {:stop, reason :: any(), response :: any(), state}

  @optional_callbacks [
    handle_GROUP: 2,
    handle_LISTGROUP: 2,
    handle_NEXT: 2,
    handle_LAST: 2,
    handle_ARTICLE: 2,
    handle_HEAD: 2,
    handle_BODY: 2,
    handle_STAT: 2,
    handle_POST: 2,
    handle_command: 2
  ]
end
