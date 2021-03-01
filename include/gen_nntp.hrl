-type name() :: atom() | {'local', term()} | {'global', term()} | {'via', module(), term()}.
-type option() ::
  {'name', name()}
  | {'port', port_number()}.
-type on_start() :: {'ok', pid()} | 'ignore' |  {'error', {'already_started', pid()} | term()}.
-type address() :: inet:socket_address() | inet:hostname() | binary().
-type port_number() :: inet:port_number().
