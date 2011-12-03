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
			end,
			fun({CSock, _Pid}) ->
					{"FETCH should work",
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
								?assertEqual("2 OK [READ-ONLY] SELECT completed\r\n",  get_msg(CSock)),
								gen_tcp:send(CSock, "3 FETCH 1 (RFC822.SIZE)\r\n"),
								[First|_] = lists:sort(element(2, file:list_dir("../testdata/lay-k/inbox"))),
								{ok, Bin} = file:read_file(filename:join(["..", "testdata", "lay-k", "inbox", First])),
								{Type, SubType, Headers, Params, _Body} = _Email = mimemail:decode(Bin),
								?assertEqual("* 1 FETCH (RFC822.SIZE "++integer_to_list(byte_size(Bin))++")\r\n",  get_msg(CSock)),
								?assertEqual("3 OK FETCH completed\r\n",  get_msg(CSock)),
								gen_tcp:send(CSock, "4 FETCH 1 (UID)\r\n"),
								?assertEqual("* 1 FETCH (UID 32001)\r\n",  get_msg(CSock)),
								?assertEqual("4 OK FETCH completed\r\n",  get_msg(CSock)),
								Subject = list_to_binary(["SUBJECT: ", mimemail:get_header_value(<<"Subject">>, Headers), "\r\n"]),
								gen_tcp:send(CSock, "5 FETCH 1 BODY[HEADER.FIELDS (SUBJECT)]\r\n"),
								?assertEqual("* 1 FETCH (BODY[HEADER.FIELDS (SUBJECT)] {"++integer_to_list(byte_size(Subject))++"}\r\n",  get_msg(CSock)),
								?assertEqual(binary_to_list(Subject), get_msg(CSock)),
								?assertEqual(")\r\n", get_msg(CSock)),
								?assertEqual("5 OK FETCH completed\r\n",  get_msg(CSock)),
								gen_tcp:send(CSock, "6 FETCH 1 BODYSTRUCTURE\r\n"),
								[_Header, RawBody] = binary:split(Bin, <<"\r\n\r\n">>),
								Lines = length(binary:split(RawBody, <<"\n">>, [global, trim])),
								?debugFmt("params ~p~n", [Params]),
								?assertEqual("* 1 FETCH (BODYSTRUCTURE (\""++binary_to_list(Type)++"\" \""++binary_to_list(SubType)++"\" (\"charset\" \"us-ascii\") NIL NIL \"7bit\" "++integer_to_list(byte_size(RawBody))++" "++integer_to_list(Lines)++"))\r\n",  get_msg(CSock)),
								?assertEqual("6 OK FETCH completed\r\n",  get_msg(CSock)),
								ok
						end
					}
			end,
			fun({CSock, _Pid}) ->
					{"UID FETCH should work",
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
								?assertEqual("2 OK [READ-ONLY] SELECT completed\r\n",  get_msg(CSock)),
								gen_tcp:send(CSock, "3 UID FETCH 32001 (RFC822.SIZE)\r\n"),
								[First|_] = lists:sort(element(2, file:list_dir("../testdata/lay-k/inbox"))),
								{ok, Bin} = file:read_file(filename:join(["..", "testdata", "lay-k", "inbox", First])),
								?assertEqual("* 1 FETCH (UID 32001 RFC822.SIZE "++integer_to_list(byte_size(Bin))++")\r\n",  get_msg(CSock)),
								?assertEqual("3 OK UID FETCH completed\r\n",  get_msg(CSock)),
								ok
						end
					}
			end,
			fun({CSock, _Pid}) ->
					{"LOGIN using literals",
						fun() ->
								?assertMatch("* OK "++_Stuff,  get_msg(CSock)),
								gen_tcp:send(CSock, "1 LOGIN {9}\r\n"),
								?assertEqual("+ Ready\r\n",  get_msg(CSock)),
								gen_tcp:send(CSock, "Some Dude {9}\r\n"),
								?assertEqual("+ Ready\r\n",  get_msg(CSock)),
								gen_tcp:send(CSock, "Pass Word\r\n"),
								?assertEqual("1 BAD LOGIN failed\r\n",  get_msg(CSock)),
								ok
						end
					}
			end

		]
	}.

