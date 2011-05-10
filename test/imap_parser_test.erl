-module(imap_parser_test).

-include_lib("eunit/include/eunit.hrl").

capability_test() ->
	?assertEqual({<<"Tag">>, capability}, imap_parser:parse("Tag CAPABILITY\r\n")),
	?assertMatch({fail, _}, imap_parser:parse("Tag CAPABILITY illegal arguments\r\n")).

logout_test() ->
	?assertEqual({<<"Tag">>, logout}, imap_parser:parse("Tag LOGOUT\r\n")),
	?assertMatch({fail, _}, imap_parser:parse("Tag LOGOUT bye bye\r\n")).

noop_test() ->
	?assertEqual({<<"Tag">>, noop}, imap_parser:parse("Tag NOOP\r\n")),
	?assertMatch({fail, _}, imap_parser:parse("Tag NOOP don't do anything\r\n")).

login_test() ->
	?assertEqual({<<"Tag">>, {login, <<"user">>, <<"pass">>}}, imap_parser:parse("Tag LOGIN user pass\r\n")),
	?assertMatch({fail, _}, imap_parser:parse("Tag LOGIN \"user name\" \"pass word\"\r\n")).

authenticate_test() ->
	?assertEqual({<<"Tag">>, {authenticate, <<"GSSAPI">>}}, imap_parser:parse("Tag AUTHENTICATE GSSAPI\r\n")),
	?assertMatch({fail, _}, imap_parser:parse("Tag AUTHENTICATE user pass\r\n")).

starttls_test() ->
	?assertEqual({<<"Tag">>, starttls}, imap_parser:parse("Tag STARTTLS\r\n")),
	?assertMatch({fail, _}, imap_parser:parse("Tag STARTTLS GSSAPI\r\n")).

parse_fetch_test() ->
	?assertEqual({<<"Tag">>, {fetch, [15, 16], <<"BODY">>}}, imap_parser:parse("Tag FETCH 15,16 BODY\r\n")),
	?assertEqual({<<"Tag">>, {fetch, [15, 16], [<<"BODY">>, [1, 2, 3]]}}, imap_parser:parse("Tag FETCH 15,16 BODY[1.2.3]\r\n")),
	?debugFmt("~p~n", [imap_parser:parse("Tag FETCH 15,16 BODY[1.2.3.HEADER]\r\n")]),
	?assertEqual({<<"Tag">>, {fetch, [15, 16], [<<"BODY">>, [1, 2, 3, <<"HEADER">>]]}}, imap_parser:parse("Tag FETCH 15,16 BODY[1.2.3.HEADER]\r\n")),
	ok.
