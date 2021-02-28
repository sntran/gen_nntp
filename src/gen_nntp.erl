-module(gen_nntp).
-author('dev@sntran.com').
-behaviour(gen_server).

%% API
-export([
  start/3,
  stop/1
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
  on_start/0
]).

-include("gen_nntp.hrl").
-include("gen_nntp_internal.hrl").

-record(client, {
  transport :: module(),
  module :: module(),
  state :: state()
}).

-callback init(Args :: term()) ->
            {ok, State :: state()}
            | ignore
            | {stop, Reason :: any()}.

-callback handle_command(Command :: binary(), State :: state()) ->
            {reply, Response :: binary(), NewState}
            | {noreply, NewState}
            | {stop, Reason :: any(), NewState}
            | {stop, Reason :: any, Response :: binary(), NewState}
          when NewState :: state().

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

  Port = proplists:get_value(port, Options, 119),
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
