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
  socket :: socket(),
  module :: module(),
  % currently selected newsgroup
  group :: invalid | binary(),
  % current article number
  article_number :: invalid | non_neg_integer(),
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

-callback handle_NEXT(Arg, state()) ->
            {ok, { Number, article() }, state()}
            | {ok, false, state()}
            | {error, Reason :: binary(), state()}
            when Number :: non_neg_integer(),
                 Arg :: message_id() | {Number, Group :: binary()}.

-callback handle_LAST(Arg, state()) ->
            {ok, { Number, article() }, state()}
            | {ok, false, state()}
            | {error, Reason :: binary(), state()}
            when Number :: non_neg_integer(),
                 Arg :: message_id() | {Number, Group :: binary()}.

-callback handle_ARTICLE(Arg, state()) ->
            {ok, { Number, article() }, state()}
            | {ok, false, state()}
            | {error, Reason :: binary(), state()}
            when Number :: non_neg_integer(),
                 Arg :: message_id() | {Number, Group :: binary()}.

-callback handle_HEAD(Arg, state()) ->
            {ok, { Number, article() }, state()}
            | {ok, false, state()}
            | {error, Reason :: binary(), state()}
            when Number :: non_neg_integer(),
                 Arg :: message_id() | {Number, Group :: binary()}.

-callback handle_BODY(Arg, state()) ->
            {ok, { Number, article() }, state()}
            | {ok, false, state()}
            | {error, Reason :: binary(), state()}
            when Number :: non_neg_integer(),
                 Arg :: message_id() | {Number, Group :: binary()}.

-callback handle_STAT(Arg, state()) ->
            {ok, { Number, article() }, state()}
            | {ok, false, state()}
            | {error, Reason :: binary(), state()}
            when Number :: non_neg_integer(),
                 Arg :: message_id() | {Number, Group :: binary()}.

-callback handle_POST(article(), state()) ->
            {ok, state()} | {error, Reason :: binary(), state()}.

-callback handle_HELP(state()) -> {ok, HelpText :: binary(), state()}.

-callback handle_command(Command :: binary(), state()) ->
            {reply, Response :: binary(), state()}
            | {noreply, state()}
            | {stop, Reason :: any(), state()}
            | {stop, Reason :: any, Response :: binary(), state()}.

-optional_callbacks([
  handle_GROUP/2,
  handle_LISTGROUP/2,
  handle_NEXT/2,
  handle_LAST/2,
  handle_ARTICLE/2,
  handle_HEAD/2,
  handle_BODY/2,
  handle_STAT/2,
  handle_POST/2,
  handle_command/2
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
-spec connect() -> {ok, socket(), Greeting :: binary()} | {error, connect_error()}.
connect() ->
  connect("localhost", ?PORT, []).

-spec connect(address()) -> {ok, socket(), Greeting :: binary()} | {error, connect_error()}.
connect(Address) ->
  connect(Address, ?PORT, []).

-spec connect(address(), port_number()) -> {ok, socket(), Greeting :: binary()} | {error, connect_error()}.
connect(Address, Port) ->
  connect(Address, Port, []).

-spec connect(address(), port_number(), [gen_tcp:connect_option()]) -> {ok, socket(), Greeting :: binary()} | {error, connect_error()}.
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
%% The function will also wait for the response from server, both
%% single and mult-line responses.
%%
%% For commands that are followed by a multi-line data block, such as
%% "POST", place the block as the argument to `command/3` call.
%% @end
%%-------------------------------------------------------------------
-spec command(socket(), binary()) -> {ok, binary()} | {error, recv_error()}.
command(Socket, Commamd) ->
  command(Socket, Commamd, []).

-spec command(socket(), binary(), Args :: list()) -> {ok, binary()} | {error, recv_error()}.
command(Socket, <<"POST">> = Command, [Article]) ->
  EmptyList = [],
  {ok, Response} = command(Socket, Command, EmptyList),
  case Response of
    <<"340 ", _/binary>> ->
      MultiLine = join(<<"\r\n">>, [to_binary(Article), <<".">>]),
      command(Socket, MultiLine, EmptyList);
    <<"440 ">> ->
      {ok, Response}
  end;

command(Socket, Command, Args) when is_binary(Command), is_list(Args) ->
  Line = join(<<" ">>, [Command | to_binary(Args)]),
  ok = gen_tcp:send(Socket, [Line, <<"\r\n">>]),
  recv(Socket, Command).

recv(Socket, <<"CAPABILITIES">>) ->
  multiline(Socket, gen_tcp:recv(Socket, 0, 1000));
recv(Socket, <<"LISTGROUP">>) ->
  multiline(Socket, gen_tcp:recv(Socket, 0, 1000));
recv(Socket, <<"ARTICLE">>) ->
  multiline(Socket, gen_tcp:recv(Socket, 0, 1000));
recv(Socket, <<"HELP">>) ->
  multiline(Socket, gen_tcp:recv(Socket, 0, 1000));
recv(Socket, _Command) ->
  {ok, Line} = gen_tcp:recv(Socket, 0, 1000),
  {ok, string:chomp(Line)}.

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
    group = invalid,
    % At the start of an NNTP session, current article number is invalid.
    article_number = invalid
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

  case handle_command(Command, Client#client{socket = Socket}) of
    {reply, Reply, NewClient} ->
      Transport:send(Socket, [Reply, <<"\r\n">>]),
      ok = Transport:setopts(Socket, [{active, once}]),
      {noreply, NewClient};

    {noreply, NewClient} ->
      {noreply, NewClient};

    {stop, Reason, Reply, NewClient} ->
      Transport:send(Socket, [Reply, <<"\r\n">>]),
      Transport:close(Socket),
      {stop, Reason, NewClient}
  end;

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
    article_number = CurrentArticleNumber,
    group = SelectedGroup,
    state = State
  } = Client,

  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  {Reply, NewArticleNumber, NewGroup, NewState} = case is_capable(Cmd, Capabilities) of
    true ->
      {ok, GroupInfo, State2} = Module:handle_GROUP(Group, State1),

      {ArticleNumber, Response} = case GroupInfo of
        % An empty newsgroup is selected,
        {Group, 0, 0, 0} ->
          % The current article number is made invalid
          {invalid, <<"211 0 0 0 ", Group/binary, " Group successfully selected">>};
        % A valid group is selected
        {Group, Number, Low, High} ->
          {Low, join(<<" ">>, [
            <<"211">>,
            integer_to_binary(Number),
            integer_to_binary(Low),
            integer_to_binary(High),
            Group,
            <<"Group successfully selected">>
          ])};
        % No group
        false ->
          {CurrentArticleNumber, <<"411 No such newsgroup">>}
        end,

      {Response, ArticleNumber, Group, State2};
    false ->
      {<<"411 No such newsgroup">>, CurrentArticleNumber, SelectedGroup, State1}
  end,

  {reply, Reply, Client#client{
    article_number = NewArticleNumber,
    group = NewGroup,
    state = NewState
  }};

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

% The currently selected group is invalid, and no argument is specified.
handle_command(<<"NEXT">>, #client{group = invalid} = Client) ->
  {reply, <<"412 No newsgroup selected">>, Client};

% Have currently selected group, but current article number is invalid.
handle_command(<<"NEXT">>, #client{article_number = invalid} = Client) ->
  {reply, <<"420 Current article number is invalid">>, Client};

handle_command(<<"NEXT">> = Cmd, Client) ->
  #client{
    module = Module,
    state = State,
    group = Group,
    article_number = ArticleNumber
  } = Client,

  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  {Reply, NewNumber, NewState} = case is_capable(Cmd, Capabilities) of
    true ->
      {ok, ArticleInfo, State2} = Module:handle_NEXT({ArticleNumber, Group}, State1),
      case ArticleInfo of
        false -> {<<"421 No article with that number">>, ArticleNumber, State2};

        % Current article number is already the last article
        {ArticleNumber, _} ->
          Response = <<"421 No next article in this group">>,
          {Response, ArticleNumber, State2};

        % Next article exists
        {Number, #{id := Id}} ->
          Response = [<<"223 ">>, integer_to_binary(Number), <<" ">>, Id, <<" Article found">>],
          {Response, Number, State2}
      end;
    false ->
      {<<"412 No newsgroup selected">>, State1}
  end,
  {reply, Reply, Client#client{article_number = NewNumber, state = NewState}};

% The currently selected group is invalid, and no argument is specified.
handle_command(<<"LAST">>, #client{group = invalid} = Client) ->
  {reply, <<"412 No newsgroup selected">>, Client};

% Have currently selected group, but current article number is invalid.
handle_command(<<"LAST">>, #client{article_number = invalid} = Client) ->
  {reply, <<"420 Current article number is invalid">>, Client};

handle_command(<<"LAST">> = Cmd, Client) ->
  #client{
    module = Module,
    state = State,
    group = Group,
    article_number = ArticleNumber
  } = Client,

  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  {Reply, NewState} = case is_capable(Cmd, Capabilities) of
    true ->
      {ok, ArticleInfo, State2} = Module:handle_LAST({ArticleNumber, Group}, State1),
      case ArticleInfo of
        false -> {<<"422 No article with that number">>, State2};

        % Current article number is already the first article
        {ArticleNumber, _} ->
          Response = <<"422 No previous article in this group">>,
          {Response, State2};

        % Previous article exists
        {Number, #{id := Id}} ->
          Response = [<<"223 ">>, integer_to_binary(Number), <<" ">>, Id, <<" Article found">>],
          {Response, State2}
      end;
    false ->
      {<<"412 No newsgroup selected">>, State1}
  end,
  {reply, Reply, Client#client{state = NewState}};

% Both ARTICLE, HEAD, BODY, and STAT have similar response.
handle_command(<<"ARTICLE", Arg/binary>>, Client) ->
  handle_article(<<"ARTICLE">>, Arg, Client);

handle_command(<<"HEAD", Arg/binary>>, Client) ->
  handle_article(<<"HEAD">>, Arg, Client);

handle_command(<<"BODY", Arg/binary>>, Client) ->
  handle_article(<<"BODY">>, Arg, Client);

handle_command(<<"STAT", Arg/binary>>, Client) ->
  handle_article(<<"STAT">>, Arg, Client);

% Client wants to post an article. Responds with 340 to tell the client to send
% the article as multi-line block data; otherwise, responds with 440 if POSTING
% is not allowed.
% The multi-line block data is parsed into a map of type `article()` and sent
% to the `handle_POST/2` callback for processing.
% The callback can decide wheether to accept the article with an ok-tuple, or
% reject it with an error-tuple.
% The callback only needs to accept the article, and do the actual transfer in
% async fashion if needs to.
handle_command(<<"POST">>, Client) ->
  #client{
    transport = Transport, socket = Socket,
    module = Module, state = State
  } = Client,

  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  case is_capable(<<"POST">>, Capabilities) of
    true ->
      Transport:send(Socket, [<<"340 Input article; end with <CR-LF>.<CR-LF>">>, <<"\r\n">>]),

      case gen_tcp:recv(Socket, 0, 1000) of
        % The socket can be closed at any point.
        {error, closed} ->
          {noreply, Client};
        Result ->
          {ok, Lines} = multiline(Socket, Result),
          Article = to_article(Lines),

          {Reply, NewState} = case Module:handle_POST(Article, State1) of
            {ok, State2} ->
              {<<"240 Article received OK">>, State2};
            {error, Reason, State2} ->
              {<<"441 ", Reason/binary>>, State2}
          end,

          {reply, Reply, Client#client{state = NewState}}
      end;
    false ->
      {reply, <<"440 Posting not permitted">>, Client#client{state = State1}}
  end;

% Clients wants to find out the current Coordinated Universal Time from the
% server's perspective. Responds with 111 and the date and time on the server
% in the form yyyymmddhhmmss.
handle_command(<<"DATE">>, Client) ->
  Now = erlang:timestamp(),
  {{Years, Months, Days},{Hours, Minutes, Seconds}} = calendar:now_to_universal_time(Now),

  Reply = [
    <<"111 ">>,
    integer_to_binary(Years),
    string:pad(integer_to_binary(Months), 2, leading, <<"0">>),
    string:pad(integer_to_binary(Days), 2, leading, <<"0">>),
    string:pad(integer_to_binary(Hours), 2, leading, <<"0">>),
    string:pad(integer_to_binary(Minutes), 2, leading, <<"0">>),
    string:pad(integer_to_binary(Seconds), 2, leading, <<"0">>)
  ],

  {reply, Reply, Client};

% This command provides a short summary of the commands that are
% understood by this implementation of the server.  The help text will
% be presented as a multi-line data block following the 100 response
% code. This text is not guaranteed to be in any particular format (but must
% be UTF-8) and MUST NOT be used by clients as a replacement for the
% CAPABILITIES command
handle_command(<<"HELP">>, Client) ->
  #client{module = Module, state = State} = Client,
  {ok, Help, NewState} = Module:handle_HELP(State),

  Reply = [
    <<"100 Help text follows\r\n">>,
    Help, <<"\r\n">>,
    <<".">>
  ],
  {reply, Reply, Client#client{state = NewState}};

% The client uses the QUIT command to terminate the session. The server
% MUST acknowledge the QUIT command and then close the connection to
% the client.
handle_command(<<"QUIT">>, Client) ->
  {stop, normal, <<"205 Connection closing">>, Client};

% Any other commands.
handle_command(Command, Client) ->
  #client{module = Module, state = State} = Client,

  case erlang:function_exported(Module, handle_command, 2) of
    false ->
      {reply, <<"500 Unknown command">>, State};
    true ->
      case Module:handle_command(Command, State) of
        {reply, Reply, State1} ->
          {reply, Reply, Client#client{state = State1}};
        {noreply, State1} ->
          {noreply, Client#client{state = State1}};
        {stop, Reason, State1} ->
          {stop, Reason, Client#client{state = State1}};
        {stop, Reason, Reply, State1} ->
          {stop, Reason, Reply, Client#client{state = State1}}
      end
  end.

% Trim the leading whitespace if any.
handle_article(Type, <<" ", Arg/binary>>, Client) ->
  handle_article(Type, Arg, Client);

% The currently selected group is invalid, and no argument is specified.
handle_article(_Type, <<"">>, #client{group = invalid} = Client) ->
  {reply, <<"412 No newsgroup selected">>, Client};

% Have currently selected group, but current article number is invalid.
handle_article(_Type, <<"">>, #client{article_number = invalid} = Client) ->
  {reply, <<"420 Current article number is invalid">>, Client};

handle_article(Type, <<"">>, #client{article_number = ArticleNumber} = Client) ->
  % @FIXME: Double conversion.
  handle_article(Type, to_binary(ArticleNumber), Client);

% Client requests for an article by message ID or article number.
handle_article(Type, Arg, Client) ->
  #client{module = Module, group = CurrentGroup, state = State} = Client,
  % Asks the callback module to provide the capacitities at this moment.
  {ok, Capabilities, State1} = Module:handle_CAPABILITIES(State),

  {Reply, NewState} = case is_capable(Type, Capabilities) of
    true ->
      SuccessCode = case Type of
        <<"ARTICLE">> -> <<"220">>;
        <<"HEAD">> -> <<"221">>;
        <<"BODY">> -> <<"222">>;
        <<"STAT">> -> <<"223">>
      end,

      Callback = binary_to_existing_atom(<<"handle_", Type/binary>>),

      % Checks if the argument is a number or a message ID.
      try {CurrentGroup, binary_to_integer(Arg)} of
        % Argument is a number, but current group is invalid, responds with
        {invalid, _ArticleNumber} ->
          {<<"412 No newsgroup selected">>, State1};
        % Argument is a number, and current group is valid, ask the callback for article.
        {_, ArticleNumber} ->
          {ok, ArticleInfo, State2} = apply(Module, Callback, [{ArticleNumber, CurrentGroup}, State1]),
          case ArticleInfo of
            false -> {<<"423 No article with that number">>, State2};

            % Article specified by article number exists
            {Number, #{id := Id} = Article} ->
              Response = join(<<"\r\n">>, [
                join(<<" ">>, [SuccessCode, to_binary(Number), Id]),
                to_binary(Article),
                <<".">>
              ]),
              {Response, State2}
          end
      catch
        error:badarg ->
          {ok, ArticleInfo, State2} = apply(Module, Callback, [Arg, State1]),
          case ArticleInfo of
            false -> {<<"430 No article with that message-id">>, State2};

            % Article specified by message ID exists
            {Number, #{id := Id} = Article} ->
              Line = join(<<" ">>, [SuccessCode, to_binary(Number), Id]),

              Response = case to_binary(Article) of
                <<"">> -> Line;
                MultiLine -> [
                  Line, <<"\r\n">>,
                  MultiLine, <<"\r\n">>,
                  <<".">>
                ]
              end,

              {Response, State2}
          end
      end;
    false ->
      {<<"430 No article with that message-id">>, State1}
  end,

  {reply, Reply, Client#client{state = NewState}}.

%% ==================================================================
%% Internal Funtions
%% ==================================================================

% Collects a multi-line response from a socket.
% Stops when encountered a ".". Last CRLF is removed.
%% @private
-spec multiline(socket(), {ok, binary()} | {error, recv_error()}) ->
  {ok, binary()} | {error, Reason :: binary()}.
multiline(_Socket, {error, Reason}) ->
  {error, Reason};

multiline(Socket, {ok, Acc}) ->
  case gen_tcp:recv(Socket, 0, 1000) of
    % End of the multi-line response.
    {ok, <<".\r\n">>} ->
      {ok, string:chomp(Acc)};
    {ok, Line} when is_binary(Line) ->
      multiline(Socket, {ok, <<Acc/binary, Line/binary>>});
    {error, Reason} ->
      {error, Reason}
  end.

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
is_capable(<<"NEXT", _Arg/binary>>, Capabilities) ->
  is_capable(<<"READER">>, Capabilities);
is_capable(<<"LAST", _Arg/binary>>, Capabilities) ->
  is_capable(<<"READER">>, Capabilities);
is_capable(<<"ARTICLE", _Arg/binary>>, Capabilities) ->
  is_capable(<<"READER">>, Capabilities);
is_capable(<<"HEAD", _Arg/binary>>, Capabilities) ->
  is_capable(<<"READER">>, Capabilities);
is_capable(<<"BODY", _Arg/binary>>, Capabilities) ->
  is_capable(<<"READER">>, Capabilities);
is_capable(<<"STAT", _Arg/binary>>, Capabilities) ->
  is_capable(<<"READER">>, Capabilities);

is_capable(Capability, Capabilities) ->
  lists:member(Capability, Capabilities).

% Join binary
%% @private
join(_Separator, []) ->
  <<>>;
join(Separator, [H|T]) ->
  lists:foldl(fun (Value, Acc) ->
    <<Acc/binary, Separator/binary, Value/binary>>
  end, H, T).

% @TODO: Tail-recursion?
to_binary([]) -> [];
to_binary([H | T]) ->
  [to_binary(H) | to_binary(T)];

to_binary(Number) when is_integer(Number) -> integer_to_binary(Number);
to_binary(Binary) when is_binary(Binary) -> Binary;

% Full article
to_binary(#{headers := Headers, body := Body}) ->
  join(<<"\r\n">>, [
    to_binary(#{headers => Headers}),
    <<"">>,
    Body
  ]);
% Headers only
to_binary(#{headers := Headers}) ->
  join(
    <<"\r\n">>,
    lists:map(fun({Header, Content}) ->
      <<Header/binary, ": ", Content/binary>>
    end, maps:to_list(Headers))
  );
% Body only
to_binary(#{body := Body}) ->
  Body;

to_binary(#{id := _Id}) ->
  <<"">>.

% Converts a multi-line data block into an article.
%% @private
-spec to_article(binary()) -> article().
to_article(MultiLineDataBlock) when is_binary(MultiLineDataBlock) ->
  Lines = binary:split(string:chomp(MultiLineDataBlock), <<"\r\n">>, [global]),
  { HeaderLines, Body } = lists:splitwith(
    fun(Line) -> Line =/= <<"">> end,
    Lines
  ),

  Headers = maps:from_list(
    lists:map(
      fun(Line) ->
        [Name, Content] = binary:split(Line, <<": ">>),
        {Name, Content}
      end,
      HeaderLines
    )
  ),

  #{
    id => <<"">>,
    headers => Headers,
    body => join(<<"">>, Body)
  }.
