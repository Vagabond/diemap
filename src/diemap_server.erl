-module(diemap_server).

-export([start/1]).

start(Module) ->
	application:start(cowboy),
	%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
	cowboy:start_listener(imap, 100,
		cowboy_tcp_transport, [{port, 1430}],
		diemap_session, [Module]
	).
