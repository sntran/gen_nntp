-type state() :: term().
-type socket() :: gen_tcp:socket().
-type connect_error() :: timeout | inet:posix().
-type recv_error() :: closed | inet:posix().
