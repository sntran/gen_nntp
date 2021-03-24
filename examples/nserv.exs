# Usage: mix run examples/nserv.exs -d <data-dir> [optional switches]
#   -d <data-dir>   - directory whose files will be served
# Optional switches:
#   -b <address>    - ip address to bind to (default is 0.0.0.0)
#   -p <port>       - port number for the first instance (default is 6791)
#
# Hit Ctrl+C twice to stop it.

require Logger

defmodule NServ do
  @moduledoc """
  A simple NNTP server that serves content from a directory.

  Clients can request articles that are parts of a file using a specific format
  for message-id. See `c:handle_ARTICLE/2` for that format.
  """
  @behaviour GenNNTP

  @regex ~r/<([\w\/.-]+)\?(\d+)=(\d+):(\d+)>/

  @defaults [port: 6791]

  def child_spec(arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start, arg}
    }
  end

  def start(datadir, options \\ []) do
    options = Keyword.merge(@defaults, options)
    Logger.info("Listening on port #{options[:port]}")
    GenNNTP.start(__MODULE__, datadir, options)
  end

  # Callbacks

  @impl true
  def init(datadir) do
    Logger.info("[S] Incoming connection")
    {:ok, datadir}
  end

  @doc """
  Returns an article based on its `message_id`.

  In NServ `message_id` has a special format which includes information about
  file and portion of the file to be returned:

  `ARTICLE <file-path-relative-to-datapath?xxx=yyy:zzz>`

  where:

  - `xxx` - part number (integer)
  - `yyy` - offset from which to read the files (integer)
  - `zzz` - size of file block to return (integer)
  """
  @impl true
  def handle_ARTICLE(message_id, datadir) when is_binary(message_id) do
    Logger.info("[S] Received: ARTICLE #{message_id}")
    [_, file_path, xxx, yyy, zzz] = Regex.run(@regex, message_id)
    Logger.info("[S] Serving: #{message_id}")
    filename = Path.basename(file_path)

    Logger.info("[S] Sending segment #{filename} (#{xxx}=#{yyy}:#{zzz})")

    body = datadir
    |> Path.join(file_path)
    |> Path.absname
    |> File.stream!([], String.to_integer(zzz))
    |> Enum.at(String.to_integer(xxx))

    article = %{
      id: message_id,
      headers: %{
        "Message-ID" => message_id,
        "Subject" => filename
      },
      body: body
    }

    {:ok, {0, article}, datadir}
  end

  @impl true
  def handle_CAPABILITIES(datadir) do
    {:ok, ["READER"], datadir}
  end

  @impl true
  def handle_HELP(datadir) do
    {:ok, "Self-serve", datadir}
  end

end

defmodule App do
  @moduledoc """
  Your application entry-point.
  For actual applications, start/1 should be start/2.
  """

  def start({data_dir, options}) do
    import Supervisor.Spec

    children = [
      {NServ, [data_dir, options]},
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

# Start the app and wait forever
Logger.info("NServ 1.0 (Test NNTP server)")
Logger.info("Press Ctrl+C to quit")

{options, _argv, _invalid} = System.argv()
|> OptionParser.parse(
  aliases: [d: :data_dir, b: :address, p: :port],
  strict: [data_dir: :string, address: :string, port: :integer]
)

if options[:data_dir] do
  App.start(Keyword.pop(options, :data_dir))

  Process.sleep(:infinity)
end
