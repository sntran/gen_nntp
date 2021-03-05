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
  % currently selected newsgroup
  group :: invalid | binary(),
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

-callback handle_LISTGROUP(Group, state()) ->
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

-optional_callbacks([
  handle_GROUP/2,
  handle_LISTGROUP/2,
  handle_ARTICLE/2
]).

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
    module = Module,
    % At the start of an NNTP session, selected newsgroup is set to invalid.
    group = invalid
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

% Command message from the client.
handle_info({tcp, Socket, Line}, #client{transport = Transport} = Client) ->
  % Removes the CRLF pair.
  Command = string:chomp(Line),

  case handle_command(Command, Client) of
    {reply, Reply, NewClient} ->
      Transport:send(Socket, [Reply, "\r\n"]),
      ok = Transport:setopts(Socket, [{active, once}]),
      {noreply, NewClient};

    {noreply, NewClient} ->
      {noreply, NewClient};

    {stop, Reason, Reply, NewClient} ->
      Transport:send(Socket, [Reply, "\r\n"]),
      Transport:close(Socket),
      {stop, Reason, NewClient}
  end;

% handle_info({tcp, Socket, <<"GROUP ", Group/binary, "\r\n">>}, Client) ->
%   #client{transport = Transport, module = Module, state = State} = Client,

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

%% ==================================================================
%% Command Handlers
%% ==================================================================

% Client asks for server's capabilities. Responds with 101 code.
% Follows with the capabilities returned from `handle_CAPABILITIES/1` callback.
handle_command(<<"CAPABILITIES">>, Client) ->
  #client{module = Module, state = State} = Client,
  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  % Retrieve the VERSION capability from returned list if any.
  Version = case lists:search(fun is_version/1, Capabilities) of
    false -> <<"VERSION ", ?NNTP_VERSION/binary>>;
    {value, Value} -> Value
  end,

  % Build multi-line data block responsefollowing the 101 response code.
  Reply = [
    join(<<"\r\n">>, [
      <<"101 Capability list:">>, % Command response with code
      Version % Then the version
      | [
        % And all the standard capabilities.
        X || X <- Capabilities, is_capability(X)
      ]
    ]),
    % Ends the multi-line data block with a termination line.
    <<"\r\n.">>
  ],

  {reply, Reply, Client#client{state = State1}};

% Client selects a newsgroup as the currently selected newsgroup and returns
% summary information about it with 211 code, or 411 if not available.
% When a valid group is selected by means of this command, the currently
% selected newsgroup MUST be set to that group.
handle_command(<<"GROUP ", Group/binary>> = Cmd, Client) ->
  #client{
    module = Module,
    group = SelectedGroup, state = State
  } = Client,

  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  {Reply, NewGroup, NewState} = case is_capable(Cmd, Capabilities) of
    true ->
      {ok, GroupInfo, State2} = Module:handle_GROUP(Group, State1),

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

      {Response, Group, State2};
    false ->
      {<<"411 No such newsgroup">>, SelectedGroup, State1}
  end,

  {reply, Reply, Client#client{group = NewGroup, state = NewState}};

% Client selects a newsgroup as the currently selected newsgroup and returns
% summary information about it with 211 code, or 411 if not available.
% It also provides a list of article numbers in the newsgroup.
% If no group is specified and the currently selected newsgroup is invalid,
% a 412 response MUST be returned.
% @TODO: Handle `range` argument.
handle_command(<<"LISTGROUP">>, #client{group = invalid} = Client) ->
  {reply, <<"412 No newsgroup selected">>, Client};

handle_command(<<"LISTGROUP">>, #client{group = Group} = Client) ->
  handle_command(<<"LISTGROUP ", Group/binary>>, Client);

handle_command(<<"LISTGROUP ", Group/binary>> = Cmd, Client) ->
  #client{module = Module, state = State} = Client,
  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  {Reply, NewState} = case is_capable(Cmd, Capabilities) of
    true ->
      {ok, GroupInfo, State2} = Module:handle_LISTGROUP(Group, State1),

      Response = case GroupInfo of
        % Group exists
        {Group, Number, Low, High, ArticleNumbers} ->
          GroupLine = join(<<" ">>, [
            <<"211">>,
            integer_to_binary(Number),
            integer_to_binary(Low),
            integer_to_binary(High),
            Group,
            <<"list follows">>
          ]),

          join(<<"\r\n">>, [
            GroupLine,
            lists:foldr(fun(ArticleNumber, AccIn) ->
              ArticleNumberBinary = integer_to_binary(ArticleNumber),
              <<ArticleNumberBinary/binary, "\r\n", AccIn/binary>>
            end, <<".">>, ArticleNumbers)
          ]);
        % No group
        false ->
          <<"411 No such newsgroup">>
        end,

      {Response, State2};
    false ->
      {<<"411 No such newsgroup">>, State1}
  end,

  {reply, Reply, Client#client{state = NewState}};

% Client requests for an article by message ID.
handle_command(<<"ARTICLE ", Arg/binary>> = Cmd, Client) ->
  #client{module = Module, state = State} = Client,
  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  {Reply, NewState} = case is_capable(Cmd, Capabilities) of
    true ->
      {ok, ArticleInfo, State2} = Module:handle_ARTICLE(Arg, State1),

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

      {Response, State2};
    false ->
      {<<"430 No article with that message-id">>, State1}
  end,

  {reply, Reply, Client#client{state = NewState}};

% The client uses the QUIT command to terminate the session. The server
% MUST acknowledge the QUIT command and then close the connection to
% the client.
handle_command(<<"QUIT">>, Client) ->
  {stop, normal, <<"205 Connection closing">>, Client};

% Any other commands.
handle_command(Command, Client) ->
  #client{module = Module, state = State} = Client,

  case Module:handle_command(Command, State) of
    {reply, Reply, State1} ->
      {reply, Reply, Client#client{state = State1}};
    {noreply, State1} ->
      {noreply, Client#client{state = State1}};
    {stop, Reason, State1} ->
      {stop, Reason, Client#client{state = State1}};
    {stop, Reason, Reply, State1} ->
      {stop, Reason, Reply, Client#client{state = State1}}
  end.

%% ==================================================================
%% Internal Funtions
%% ==================================================================

% Checks if a text match "VERSION" capability.
%% @private
is_version(<<"VERSION ", _N/binary>>) -> true;
is_version(_) -> false.

% VERSION is handled at server's level, so it's not a capability.
%% @private
is_capability(<<"VERSION ", _N/binary>>) ->
  false;
% Checks if the capability is in the standard list.
is_capability(Capability) ->
  lists:member(Capability, ?CAPABILITIES).

% Check if a command or capability is within a capability list.
%% @private
is_capable(<<"GROUP", _Arg/binary>>, Capabilities) ->
  is_capable(<<"READER">>, Capabilities);
is_capable(<<"LISTGROUP", _Arg/binary>>, Capabilities) ->
  is_capable(<<"READER">>, Capabilities);
is_capable(<<"ARTICLE", _Arg/binary>>, Capabilities) ->
  is_capable(<<"READER">>, Capabilities);

is_capable(Capability, Capabilities) ->
  lists:member(Capability, Capabilities).

% Join binary
%% @private
join(_Separator, []) ->
    <<>>;
join(Separator, [H|T]) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary, Separator/binary, Value/binary>> end, H, T).
