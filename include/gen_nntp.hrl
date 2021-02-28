-type name() :: atom() | {'local', term()} | {'global', term()} | {'via', module(), term()}.
-type option() ::
  {'name', name()}
  | {'port', pos_integer()}.
-type on_start() :: {'ok', pid()} | 'ignore' |  {'error', {'already_started', pid()} | term()}.
