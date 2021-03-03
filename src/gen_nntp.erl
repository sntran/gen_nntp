-module(gen_nntp).
-author('dev@sntran.com').
-behaviour(gen_server).

%% API
-export([
  start/3,
  stop/1,
  connect/0,
  connect/1,
  connect/2,
  connect/3,
  command/2,
  command/3
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%% ranch_protocol callbacks
-export([
  start_link/3,
  start_link/4
]).

-export_type([
  name/0,
  option/0,
  on_start/0,
  message_id/0,
  article/0
]).

-include("gen_nntp.hrl").
-include("gen_nntp_internal.hrl").

-record(client, {
  transport :: module(),
  module :: module(),
  state :: state()
}).

-define(PORT, list_to_integer(os:getenv("PORT", "119"))).
-define(NNTP_VERSION, <<"2">>).
-define(CAPABILITIES, [
  <<"HDR">>,
  <<"IHAVE">>,
  <<"LIST">>,
  <<"MODE-READER">>,
  <<"NEWNEWS">>,
  <<"OVER">>,
  <<"POST">>,
  <<"READER">>
]).

-callback init(Args :: term()) ->
            {ok, state()}
            | ignore
            | {stop, Reason :: any()}.

-callback handle_CAPABILITIES(state()) -> {ok, Capabilities :: [binary()], state()}.

-callback handle_GROUP(Group, state()) ->
            {ok, {
              Group, Number :: non_neg_integer(),
              Low :: non_neg_integer(),
              High :: non_neg_integer()
            }, state()}
            | {ok, false, state()}
            | {error, Reason :: binary(), state()}
            when Group :: binary().

-callback handle_ARTICLE(Arg, state()) ->
            {ok, { Number, article()}, state()}
            | {ok, false, state()}
            | {error, Reason :: binary(), state()}
            when Number :: non_neg_integer(),
                 Arg :: message_id() | Number.

-callback handle_command(Command :: binary(), state()) ->
            {reply, Response :: binary(), state()}
            | {noreply, state()}
            | {stop, Reason :: any(), state()}
            | {stop, Reason :: any, Response :: binary(), state()}.

-optional_callbacks([handle_GROUP/2, handle_ARTICLE/2]).

%% ==================================================================
%% API
%% ==================================================================

%%-------------------------------------------------------------------
%% @doc Starts a NNTP server with a callback module.
%%
%% Similar to starting a `gen_server`.
%% @end
%%-------------------------------------------------------------------
-spec start(module(), term(), [option()]) -> on_start().
start(Module, Args, Options) when is_atom(Module) ->
  ok = application:ensure_started(ranch),

  Port = proplists:get_value(port, Options, ?PORT),
  Options1 = proplists:delete(port, Options),

  ProtocolOpts = { Module, Args, Options1 },
  case ranch:start_listener(
    Module,
    ranch_tcp, [{port, Port}],
    ?MODULE, ProtocolOpts
  ) of
    {ok, Listener} -> {ok, Listener};
     {error, {already_started, Listener}} -> {ok, Listener}
  end.

%%-------------------------------------------------------------------
%% @doc Stops a NNTP server by its reference.
%%
%% The reference is usually the callback module.
%% @end
%%-------------------------------------------------------------------
-spec stop(module()) -> ok.
stop(Ref) ->
  try ranch:stop_listener(Ref) of
    _ -> ok
  catch
    _:_ -> ok
  end.

%%-------------------------------------------------------------------
%% @doc Connects to a NNTP server.
%%
%% @end
%%-------------------------------------------------------------------
-spec connect() -> {ok, socket(), Greeting :: binary()} | {error, Reason :: timeout | inet:posix()}.
connect() ->
  connect("localhost", ?PORT, []).

-spec connect(address()) -> {ok, socket(), Greeting :: binary()} | {error, Reason :: timeout | inet:posix()}.
connect(Address) ->
  connect(Address, ?PORT, []).

-spec connect(address(), port_number()) -> {ok, socket(), Greeting :: binary()} | {error, Reason :: timeout | inet:posix()}.
connect(Address, Port) ->
  connect(Address, Port, []).

-spec connect(address(), port_number(), [gen_tcp:connect_option()]) -> {ok, socket(), Greeting :: binary()} | {error, Reason :: timeout | inet:posix()}.
connect(Address, Port, Options) when is_binary(Address) ->
  connect(binary_to_list(Address), Port, Options);

connect(Address, Port, _Options) ->
  % @TODO: Merge default options with user-supplied options.
  Options = [binary, {packet, line}, {active, false}],
  case gen_tcp:connect(Address, Port, Options) of
    {ok, Socket} ->
      {ok, Greeting} = gen_tcp:recv(Socket, 0, 1000),
      {ok, Socket, Greeting};
    {error, Reason} ->
      {error, Reason}
  end.

%%-------------------------------------------------------------------
%% @doc Sends a command to a NNTP socket
%%
%% The function will also wait for the response from server.
%% @end
%%-------------------------------------------------------------------
command(Socket, Commamd) ->
  command(Socket, Commamd, []).

command(Socket, Command, _Args) when is_binary(Command) ->
  ok = gen_tcp:send(Socket, <<Command/binary, "\r\n">>),
  gen_tcp:recv(Socket, 0, 1000).

%% ==================================================================
%% ranch_protocol Callbacks
%% ==================================================================

% ranch 1
start_link(Module, _Socket, Transport, { Module, _, _ } = Options) when is_atom(Transport) ->
  start_link(Module, Transport, Options).

% ranch_protocol 2
start_link(Module, Transport, { Module, _, _ } = Options) when is_atom(Transport) ->
  gen_server:start_link(?MODULE, { Transport, Options }, []).

%% ==================================================================
%% gen_server Callbacks
%% ==================================================================

%% @private
init({ Transport, { Module, Args, _Options } }) ->
  % traps exit signals so we can clean up when terminated by supervisor.
  process_flag(trap_exit, true),

  Client = #client{
    transport = Transport,
    module = Module
  },

  case Module:init(Args) of
    {ok, State} ->
      % Set timeout to 0 so we can handle handshake.
      {ok, Client#client{state = State}, 0};
    {ok, State, Delay} when is_integer(Delay) ->
      {ok, Client#client{state = State}, Delay};
    ignore ->
      ignore;
    {stop, Reason} ->
      {stop, Reason};
    Else ->
      Else
  end.

%% @private
handle_call(_, _From, Client) -> {reply, ok, Client}.

%% @private
handle_cast(stop, Client) -> {stop, normal, Client}.

% Received after initialization timeout. Starts the handshake.
handle_info(timeout, #client{module =Module, transport = Transport} = Client) ->
  {ok, Socket} = ranch:handshake(Module),
  ok = Transport:setopts(Socket, [{active, once}, {packet, line}]),

  Transport:send(Socket, "200 Service available, posting allowed\r\n"),
  {noreply, Client};

% Client asks for server's capabilities. Responds with 101 code.
% Follows with the capabilities returned from `handle_CAPABILITIES/1` callback.
handle_info({tcp, Socket, <<"CAPABILITIES\r\n">>}, Client) ->
  #client{transport = Transport, module = Module, state = State} = Client,
  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  % Retrieve the VERSION capability from returned list if any.
  Version = case lists:search(fun is_version/1, Capabilities) of
    false -> <<"VERSION ", ?NNTP_VERSION/binary>>;
    {value, Value} -> Value
  end,

  % Build multi-line data block responsefollowing the 101 response code.
  Response = join(<<"\r\n">>, [
    <<"101 Capability list:">>, % Command response with code
    Version % Then the version
    | [
      % And all the standard capabilities.
      X || X <- Capabilities, is_capability(X)
    ]
  ]),

  % Ends the multi-line data block with a termination line.
  Transport:send(Socket, <<Response/binary, "\r\n.\r\n">>),

  {noreply, Client#client{state = State1}};

% Client requests for an article by message ID.
handle_info({tcp, Socket, <<"ARTICLE ", ArgCRLF/binary>>}, Client) ->
  % Removes the CRLF pair.
  Arg = string:chomp(ArgCRLF),
  #client{transport = Transport, module = Module, state = State} = Client,
  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  State2 = case lists:member(<<"READER">>, Capabilities) of
    true ->
      {ok, ArticleInfo, NewState} = Module:handle_ARTICLE(Arg, State1),

      Response = case ArticleInfo of
        % Article exists
        {Number, {Id, Headers, Body}} ->

          join(<<"\r\n">>, [
            join(<<" ">>, [<<"220">>, integer_to_binary(Number), Id]),
            maps:fold(fun(Header, Content, AccIn) ->
              <<AccIn/binary, Header/binary, ": ", Content/binary, "\r\n">>
            end, <<"">>, Headers),
            Body,
            <<".">>
          ]);
        % No article
        false ->
          <<"430 No article with that message-id">>
        end,

        Transport:send(Socket, <<Response/binary, "\r\n">>),
      NewState;
    false ->
      Transport:send(Socket, <<"430 No article with that message-id\r\n">>),
      State1
  end,

  % Ready for the next command.
  ok = Transport:setopts(Socket, [{active, once}]),

  {noreply, Client#client{state = State2}};

% Client selects a newsgroup as the currently selected newsgroup and returns
% summary information about it with 211 code, or 411 if not available.
handle_info({tcp, Socket, <<"GROUP ", GroupCRLF/binary>>}, Client) ->
  % Removes the CRLF pair.
  Group = string:chomp(GroupCRLF),
  #client{transport = Transport, module = Module, state = State} = Client,
  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  State2 = case lists:member(<<"READER">>, Capabilities) of
    true ->
      {ok, GroupInfo, NewState} = Module:handle_GROUP(Group, State1),

      Response = case GroupInfo of
        % Group exists
        {Group, Number, Low, High} ->
          join(<<" ">>, [
            <<"211">>,
            integer_to_binary(Number),
            integer_to_binary(Low),
            integer_to_binary(High),
            Group,
            <<"Group successfully selected">>
          ]);
        % No group
        false ->
          <<"411 No such newsgroup">>
        end,

        Transport:send(Socket, <<Response/binary, "\r\n">>),
      NewState;
    false ->
      Transport:send(Socket, <<"411 No such newsgroup\r\n">>),
      State1
  end,

  % Ready for the next command.
  ok = Transport:setopts(Socket, [{active, once}]),

  {noreply, Client#client{state = State2}};

% The client uses the QUIT command to terminate the session. The server
% MUST acknowledge the QUIT command and then close the connection to
% the client.
handle_info({tcp, Socket, <<"QUIT\r\n">>}, #client{transport = Transport} = Client) ->
  Transport:send(Socket, <<"205 Connection closing\r\n">>),
  Transport:close(Socket),
  {noreply, Client};

handle_info({tcp, Socket, Line}, Client) ->
  #client{transport = Transport, module = Module, state = State} = Client,
  ok = Transport:setopts(Socket, [{active, once}]),

  Command = string:trim(Line, trailing, "\r\n"),

  NewState = case Module:handle_command(Command, State) of
    {reply, Reply, State1} ->
      Transport:send(Socket, <<Reply/binary, "\r\n">>),
      State1;
    {noreply, State1} ->
      State1;
    {stop, _Reason, State1} ->
      Transport:close(Socket),
      State1;
    {stop, _Reason, Reply, State1} ->
      Transport:send(Socket, <<Reply/binary, "\r\n">>),
      Transport:close(Socket),
      State1
  end,

  {noreply, Client#client{state = NewState}};

handle_info({tcp_closed, _Socket}, Client) ->
  {stop, normal, Client};

handle_info({tcp_error, _Socket, Reason}, Client) ->
  {stop, Reason, Client};

% handles exit message, if the gen_server is linked to other processes (than
% the supervisor) and trapping exit signals.
handle_info({'EXIT', _Pid, _Reason}, Client) ->
  % ..code to handle exits here..
  {noreply, Client};

handle_info(_Info, Client) ->
  {noreply, Client}.

%% @private
terminate(normal, _Client) ->
  % handles normal termination when callback retruns `{stop, normal, Client}`
  ok;

terminate(shutdown, _Client) ->
  % ..code for cleaning up here..
  ok;

terminate(_, _Client) -> ok.

code_change(_OldVsn, Client, _Extra) ->
  % ..code to convert state (and more) during code change
  {ok, Client}.

% Checks if a text match "VERSION" capability.
is_version(<<"VERSION ", _N/binary>>) -> true;
is_version(_) -> false.

% VERSION is handled at server's level, so it's not a capability.
is_capability(<<"VERSION ", _N/binary>>) ->
  false;

% Checks if the capability is in the standard list.
is_capability(Capability) ->
  lists:member(Capability, ?CAPABILITIES).

% Join binary
join(_Separator, []) ->
    <<>>;
join(Separator, [H|T]) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary, Separator/binary, Value/binary>> end, H, T).
