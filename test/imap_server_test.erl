-module(imap_server_test).

-include_lib("eunit/include/eunit.hrl").

get_msg(Socket) ->
	inet:setopts(Socket, [{active, once}]),
	receive {tcp, Socket, Packet} -> ok end,
	Packet.

server_test_() ->
	{foreach,
		local,
		fun() ->
				Self = self(),
				spawn(fun() ->
							{ok, ListenSock} = gen_tcp:listen(9876, [binary, {reuseaddr, true}, {packet, line}]),
							{ok, X} = gen_tcp:accept(ListenSock),
							gen_tcp:controlling_process(X, Self),
							Self ! X
					end),
				{ok, CSock} = gen_tcp:connect("localhost", 9876, [{packet, line}]),
				receive
					SSock when is_port(SSock) ->
						ok
				end,
				{ok, Pid} = diemap_session:start(SSock, cowboy_tcp_transport, [imap_server_example, {moduleoptions, [{rootdir, "../testdata"}]}]),
				gen_tcp:controlling_process(SSock, Pid),
				{CSock, Pid}
		end,
		fun({CSock, _Pid}) ->
				gen_tcp:close(CSock)
		end,
		[
			fun({CSock, _Pid}) ->
					{"A new connection should get a banner",
						fun() ->
								?assertMatch("* OK "++_Stuff,  get_msg(CSock))
						end
					}
			end,
			fun({CSock, _Pid}) ->
					{"NOOP should work",
						fun() ->
								?assertMatch("* OK "++_Stuff,  get_msg(CSock)),
								gen_tcp:send(CSock, "1 NOOP\r\n"),
								?assertEqual("1 OK NOOP completed\r\n",  get_msg(CSock))
						end
					}
			end,
			fun({CSock, _Pid}) ->
					{"CAPABILITY should work",
						fun() ->
								?assertMatch("* OK "++_Stuff,  get_msg(CSock)),
								gen_tcp:send(CSock, "1 CAPABILITY\r\n"),
								?assertEqual("* CAPABILITY IMAP4rev1\r\n",  get_msg(CSock)),
								?assertEqual("1 OK CAPABILITY completed\r\n",  get_msg(CSock))
						end
					}
			end,
			fun({CSock, _Pid}) ->
					{"LOGOUT should work",
						fun() ->
								?assertMatch("* OK "++_Stuff,  get_msg(CSock)),
								gen_tcp:send(CSock, "1 LOGOUT\r\n"),
								?assertMatch("* BYE"++_,  get_msg(CSock)),
								?assertEqual("1 OK LOGOUT completed\r\n",  get_msg(CSock))
						end
					}
			end,
			fun({CSock, _Pid}) ->
					{"LOGIN should work for a valid user",
						fun() ->
								?assertMatch("* OK "++_Stuff,  get_msg(CSock)),
								gen_tcp:send(CSock, "1 LOGIN lay-k password\r\n"),
								?assertEqual("1 OK LOGIN completed\r\n",  get_msg(CSock))
						end
					}
			end,
			fun({CSock, _Pid}) ->
					{"LIST should work after LOGIN, but not before",
						fun() ->
								?assertMatch("* OK "++_Stuff,  get_msg(CSock)),
								gen_tcp:send(CSock, "1 LIST inbox\r\n"),
								?assertMatch("1 BAD"++_,  get_msg(CSock)),
								gen_tcp:send(CSock, "2 LOGIN lay-k password\r\n"),
								?assertEqual("2 OK LOGIN completed\r\n",  get_msg(CSock)),
								gen_tcp:send(CSock, "3 LIST \"\" inbox\r\n"),
								?assertEqual("* LIST () \"/\" inbox\r\n",  get_msg(CSock)),
								?assertEqual("3 OK LIST completed\r\n",  get_msg(CSock))
						end
					}
			end,
			fun({CSock, _Pid}) ->
					{"STATUS should work",
						fun() ->
								?assertMatch("* OK "++_Stuff,  get_msg(CSock)),
								gen_tcp:send(CSock, "1 LOGIN lay-k password\r\n"),
								?assertEqual("1 OK LOGIN completed\r\n",  get_msg(CSock)),
								gen_tcp:send(CSock, "2 STATUS inbox (MESSAGES)\r\n"),
								?assertEqual("* STATUS inbox (MESSAGES 9)\r\n",  get_msg(CSock)),
								?assertEqual("2 OK STATUS completed\r\n",  get_msg(CSock))
						end
					}
			end,
			fun({CSock, _Pid}) ->
					{"SELECT should work",
						fun() ->
								?assertMatch("* OK "++_Stuff,  get_msg(CSock)),
								gen_tcp:send(CSock, "1 LOGIN lay-k password\r\n"),
								?assertEqual("1 OK LOGIN completed\r\n",  get_msg(CSock)),
								gen_tcp:send(CSock, "2 SELECT inbox\r\n"),
								?assertEqual("* 9 EXISTS\r\n",  get_msg(CSock)),
								?assertEqual("* 0 RECENT\r\n",  get_msg(CSock)),
								?assertEqual("* OK [UNSEEN 10]\r\n",  get_msg(CSock)),
								?assertMatch("* OK [UIDNEXT"++_,  get_msg(CSock)),
								?assertMatch("* OK [UIDVALIDITY"++_,  get_msg(CSock)),
								?assertMatch("* OK [PERMANENTFLAGS"++_,  get_msg(CSock)),
								?assertMatch("* FLAGS"++_,  get_msg(CSock)),
								?assertEqual("2 OK [READ-ONLY] SELECT completed\r\n",  get_msg(CSock))
						end
					}
			end
		]
	}.

