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
	?assertMatch({<<"Tag">>, {login, <<"user name">>, <<"pass word">>}}, imap_parser:parse("Tag LOGIN \"user name\" \"pass word\"\r\n")).

authenticate_test() ->
	?assertEqual({<<"Tag">>, {authenticate, <<"GSSAPI">>}}, imap_parser:parse("Tag AUTHENTICATE GSSAPI\r\n")),
	?assertMatch({fail, _}, imap_parser:parse("Tag AUTHENTICATE user pass\r\n")).

starttls_test() ->
	?assertEqual({<<"Tag">>, starttls}, imap_parser:parse("Tag STARTTLS\r\n")),
	?assertMatch({fail, _}, imap_parser:parse("Tag STARTTLS GSSAPI\r\n")).

append_test() ->
	?assertEqual({<<"Tag">>, {append, inbox, undefined, undefined, 4096}}, imap_parser:parse("Tag APPEND InBoX {4096}\r\n")),
	?assertEqual({<<"Tag">>, {append, inbox, [<<"\\Draft">>], undefined, 4096}}, imap_parser:parse("Tag APPEND InBoX (\\Draft) {4096}\r\n")),
	?assertEqual({<<"Tag">>, {append, inbox, undefined, <<"10-May-2011 11:10:00 +0500">>, 4096}}, imap_parser:parse("Tag APPEND InBoX \"10-May-2011 11:10:00 +0500\" {4096}\r\n")).

create_test() ->
	?assertEqual({<<"Tag">>, {create, <<"mybox">>}}, imap_parser:parse("Tag CREATE mybox\r\n")).

delete_test() ->
	?assertEqual({<<"Tag">>, {delete, <<"mybox">>}}, imap_parser:parse("Tag DELETE mybox\r\n")).

examine_test() ->
	?assertEqual({<<"Tag">>, {examine, <<"mybox">>}}, imap_parser:parse("Tag EXAMINE mybox\r\n")).

list_test() ->
	?assertEqual({<<"Tag">>, {list, <<>>, <<>>}}, imap_parser:parse("Tag LIST \"\" \"\"\r\n")),
	?assertEqual({<<"Tag">>, {list, <<"/usr/staff/jones">>, <<>>}}, imap_parser:parse("Tag LIST /usr/staff/jones \"\"\r\n")),
	?assertEqual({<<"Tag">>, {list, <<"~/Mail/">>, <<"%">>}}, imap_parser:parse("Tag LIST ~/Mail/ %\r\n")),
	?assertEqual({<<"Tag">>, {list, <<>>, <<"*">>}}, imap_parser:parse("Tag LIST \"\" *\r\n")).

lsub_test() ->
	?assertEqual({<<"Tag">>, {lsub, <<>>, <<>>}}, imap_parser:parse("Tag LSUB \"\" \"\"\r\n")),
	?assertEqual({<<"Tag">>, {lsub, <<"/usr/staff/jones">>, <<>>}}, imap_parser:parse("Tag LSUB /usr/staff/jones \"\"\r\n")),
	?assertEqual({<<"Tag">>, {lsub, <<"~/Mail/">>, <<"%">>}}, imap_parser:parse("Tag LSUB ~/Mail/ %\r\n")).

rename_test() ->
	?assertEqual({<<"Tag">>, {rename, <<"mybox">>, <<"mynewbox">>}}, imap_parser:parse("Tag RENAME mybox mynewbox\r\n")).

select_test() ->
	?assertEqual({<<"Tag">>, {select, <<"mybox">>}}, imap_parser:parse("Tag SELECT mybox\r\n")).

status_test() ->
	?assertEqual({<<"Tag">>, {status, <<"mybox">>, [<<"MESSAGES">>]}}, imap_parser:parse("Tag STATUS mybox (MESSAGES)\r\n")),
	?assertEqual({<<"Tag">>, {status, <<"mybox">>, [<<"MESSAGES">>, <<"UNSEEN">>]}}, imap_parser:parse("Tag STATUS mybox (MESSAGES UNSEEN)\r\n")).

subscribe_test() ->
	?assertEqual({<<"Tag">>, {subscribe, <<"mybox">>}}, imap_parser:parse("Tag SUBSCRIBE mybox\r\n")).

unsubscribe_test() ->
	?assertEqual({<<"Tag">>, {unsubscribe, <<"mybox">>}}, imap_parser:parse("Tag UNSUBSCRIBE mybox\r\n")).

check_test() ->
	?assertEqual({<<"Tag">>, check}, imap_parser:parse("Tag CHECK\r\n")).

close_test() ->
	?assertEqual({<<"Tag">>, close}, imap_parser:parse("Tag CLOSE\r\n")).

expunge_test() ->
	?assertEqual({<<"Tag">>, expunge}, imap_parser:parse("Tag EXPUNGE\r\n")).

copy_test() ->
	?assertEqual({<<"Tag">>, {copy, [1, 2, 3, {9, '*'}], <<"mybox">>}}, imap_parser:parse("Tag COPY 1,2,3,9:* mybox\r\n")).
	
fetch_test() ->
	?assertEqual({<<"Tag">>, {fetch, [15, 16], <<"BODY">>}}, imap_parser:parse("Tag FETCH 15,16 BODY\r\n")),
	?assertEqual({<<"Tag">>, {fetch, [15, 16], [<<"BODY">>, [1, 2, 3]]}}, imap_parser:parse("Tag FETCH 15,16 BODY[1.2.3]\r\n")),
	?assertEqual({<<"Tag">>, {fetch, [15, 16], [<<"BODY">>, [1, 2, 3, [<<"HEADER">>]]]}}, imap_parser:parse("Tag FETCH 15,16 BODY[1.2.3.HEADER]\r\n")),
	?assertEqual({<<"Tag">>, {fetch, [15, 16], [<<"BODY">>, [1, 2, 3, [<<"HEADER.FIELDS">>, [<<"From">>]]]]}}, imap_parser:parse("Tag FETCH 15,16 BODY[1.2.3.HEADER.FIELDS (From)]\r\n")),
	?assertEqual({<<"Tag">>, {fetch, [15, 16], [<<"BODY">>, [1, 2, 3, [<<"HEADER.FIELDS">>, [<<"From">>, <<"To">>, <<"Date">>, <<"Subject">>]]]]}}, imap_parser:parse("Tag FETCH 15,16 BODY[1.2.3.HEADER.FIELDS (From To Date Subject)]\r\n")),
	?assertEqual({<<"Tag">>, {fetch, [15, 16], [<<"UID">>, <<"FLAGS">>, [<<"BODY">>, [1, 2, 3, [<<"HEADER.FIELDS">>, [<<"From">>, <<"To">>, <<"Date">>, <<"Subject">>]]]]]}}, imap_parser:parse("Tag FETCH 15,16 (UID FLAGS BODY[1.2.3.HEADER.FIELDS (From To Date Subject)])\r\n")),
	?assertEqual({<<"Tag">>, {fetch, [15, 16], [<<"BODY">>, [1], {0, 65535}]}}, imap_parser:parse("Tag FETCH 15,16 BODY[1]<0.65535>\r\n")),
	ok.

store_test() ->
	?assertEqual({<<"Tag">>, {store, [1, 2, 3, {9, '*'}], {<<"FLAGS">>, [<<"\\Seen">>]}}}, imap_parser:parse("Tag STORE 1,2,3,9:* FLAGS (\\Seen)\r\n")),
	?assertEqual({<<"Tag">>, {store, [1, 2, 3, {9, '*'}], {<<"FLAGS">>, [<<"\\Seen">>, <<"\\Draft">>]}}}, imap_parser:parse("Tag STORE 1,2,3,9:* FLAGS (\\Seen \\Draft)\r\n")),
	?assertEqual({<<"Tag">>, {store, [1, 2, 3, {9, '*'}], {<<"-FLAGS">>, [<<"\\Seen">>, <<"\\Draft">>]}}}, imap_parser:parse("Tag STORE 1,2,3,9:* -FLAGS (\\Seen \\Draft)\r\n")),
	?assertEqual({<<"Tag">>, {store, [1, 2, 3, {9, '*'}], {<<"+FLAGS.SILENT">>, [<<"\\Seen">>, <<"\\Draft">>]}}}, imap_parser:parse("Tag STORE 1,2,3,9:* +FLAGS.SILENT (\\Seen \\Draft)\r\n")).

search_test() ->
	?assertEqual({<<"Tag">>, {search, <<"us-ascii">>, [<<"ALL">>]}}, imap_parser:parse("Tag SEARCH ALL\r\n")),
	?assertEqual({<<"Tag">>, {search, <<"utf-8">>, [<<"ALL">>]}}, imap_parser:parse("Tag SEARCH CHARSET utf-8 ALL\r\n")).

uid_test() ->
	?assertEqual({<<"Tag">>, {uid, {copy, [1, 2, 3, {9, '*'}], <<"mybox">>}}}, imap_parser:parse("Tag UID COPY 1,2,3,9:* mybox\r\n")),
	?assertEqual({<<"Tag">>, {uid, {fetch, [15, 16], [<<"UID">>, <<"BODY">>]}}}, imap_parser:parse("Tag UID FETCH 15,16 BODY\r\n")),
	?assertEqual({<<"Tag">>, {uid, {fetch, [15, 16], [<<"UID">>, <<"FLAGS">>]}}}, imap_parser:parse("Tag UID FETCH 15,16 (FLAGS)\r\n")),
	?assertEqual({<<"Tag">>, {uid, {fetch, [15, 16], [<<"FLAGS">>, <<"UID">>]}}}, imap_parser:parse("Tag UID FETCH 15,16 (FLAGS UID)\r\n")),
	?assertEqual({<<"Tag">>, {uid, {fetch, [{27446589, 27446606}], [<<"INTERNALDATE">>, <<"UID">>, <<"RFC822.SIZE">>, <<"FLAGS">>, [<<"BODY.PEEK">>, [<<"HEADER.FIELDS">>, [<<"date">>, <<"subject">>, <<"from">>, <<"to">>, <<"cc">>, <<"message-id">>, <<"in-reply-to">>, <<"references">>]]]]}}}, imap_parser:parse("Tag UID FETCH 27446589:27446606 (INTERNALDATE UID RFC822.SIZE FLAGS BODY.PEEK[HEADER.FIELDS (date subject from to cc message-id in-reply-to references)])\r\n")),
	?assertEqual({<<"Tag">>, {uid, {search, <<"us-ascii">>, [<<"ALL">>]}}}, imap_parser:parse("Tag UID SEARCH ALL\r\n")),
	?assertEqual({<<"Tag">>, {uid, {store, [1, 2, 3, {9, '*'}], {<<"+FLAGS.SILENT">>, [<<"\\Seen">>, <<"\\Draft">>]}}}}, imap_parser:parse("Tag UID STORE 1,2,3,9:* +FLAGS.SILENT (\\Seen \\Draft)\r\n")).

invalid_command_test() ->
	?assertEqual({<<"Tag">>, {invalid, <<"gobbledohoomasdasd">>}}, imap_parser:parse("Tag gobbledohoomasdasd\r\n")),
	?assertEqual({<<"Tag">>, {invalid, <<"XAUTH">>}}, imap_parser:parse("Tag XAUTH\r\n")),
	?assertEqual({<<"Tag">>, {invalid, <<"SEARCH">>}}, imap_parser:parse("Tag SEARCH\r\n")),
	?assertEqual({<<"Tag">>, {invalid, <<"EHLO google.com">>}}, imap_parser:parse("Tag EHLO google.com\r\n")),
	?assertMatch({fail, _}, imap_parser:parse("wtf\r\n")).
