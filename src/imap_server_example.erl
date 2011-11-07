-module(imap_server_example).

-export([init/2, handle_CAPABILITY/2, handle_NOOP/1, handle_LOGOUT/1, handle_LOGIN/3, handle_SELECT/2, handle_FETCH/5, handle_LIST/3, handle_CLOSE/1, handle_STATUS/3, handle_LSUB/3]).

-include_lib("kernel/include/file.hrl").

-record(state, {
		rootdir :: string(),
		options,
		user,
		folder
	}).

init(Peername, Options) ->
	io:format("client connection from ~p~n", [Peername]),
	RootDir = proplists:get_value(rootdir, Options, "maildir"),
	{ok, #state{options=Options, rootdir=RootDir}}.

handle_CAPABILITY(Capabilities, State) ->
	%% Add or remove from the capabilities list here
	{ok, Capabilities, State}.

handle_NOOP(State) ->
	{ok, State}.

handle_LOGOUT(State) ->
	{ok, "Have a super day!", State}.

handle_LOGIN(User, _Pass, #state{rootdir=RootDir} = State) ->
	case filelib:is_dir(filename:join(RootDir, User)) of
		true ->
			{ok, State#state{user=User}};
		false ->
			?debugFmt("~p does not exist~n", [filename:join(RootDir, User)]),
			{error, State}
	end.

handle_SELECT(inbox, #state{rootdir=RootDir, user=User} = State) ->
 case filelib:wildcard(binary_to_list(list_to_binary([RootDir, "/", User,"/[Ii][Nn][Bb][Oo][Xx]"]))) of
	 [] ->
		 {error, "No INBOX for this user", State#state{folder=undefined}};
	 [Inbox|_] ->
			Folder = lists:last(string:tokens(Inbox, "/")),
			%io:format("Folder is ~p", [Folder]),
		 {ok, mailbox_details(Folder, State), State#state{folder=Folder}}
 end;
handle_SELECT(Folder, #state{rootdir=RootDir, user=User} = State) ->
	FolderDir = filename:join([RootDir, User, Folder]),
	%io:format("Folder ~p~n", [FolderDir]),
	case filelib:is_dir(FolderDir) of
		true ->
			{ok, mailbox_details(Folder, State), State#state{folder=Folder}};
		_ ->
		 {error, "Folder does not exist", State#state{folder=undefined}}
 end.

mailbox_details(Mailbox, #state{rootdir=RootDir} = State) ->
	FolderDir = filename:join([RootDir, State#state.user, safe_mailbox(Mailbox, State)]),
	{ok, Contents} = file:list_dir(FolderDir),
	Files = [filename:join(FolderDir, X) || X <- Contents, filelib:is_regular(filename:join(FolderDir, X))],
	UidFirst = 32001,
	UidNext = UidFirst + length(Files),
	{ok, DirInfo} = file:read_file_info(FolderDir),
	MTime = DirInfo#file_info.mtime,
	%% this is close enough to the unix timestamp
	UIDValidity = calendar:datetime_to_gregorian_seconds(MTime) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	{length(Files), 0, length(Files) + 1, ["\\Answered", "\\Flagged", "\\Deleted", "\\Seen", "\\Draft"], ["\\Seen"], {UidFirst, UidNext+1}, UIDValidity, read_only}.

mail_details(Uid, Attributes, true, Files, Cb, State) -> %% UID fetch
	mail_details(Uid-32000, Attributes, false, Files, Cb, State);
mail_details(Seq, Attributes, _, Files, Cb, _State) when Seq > 0->
	%io:format("contents ~p~n", [Files]),
	try lists:nth(Seq, Files) of
		File ->
			Result = {fetch_response, Seq, get_attribute(File, Seq+32000, undefined, undefined, Attributes, [])},
			Cb(Result)
	catch _:_ ->
		ok
	end;
mail_details(_Seq, _Attributes, _, _Files, _Cb, _State) ->
	ok.

safe_mailbox(inbox, #state{rootdir=RootDir} = State) ->
	case filelib:wildcard(binary_to_list(list_to_binary([RootDir, "/", State#state.user,"/[Ii][Nn][Bb][Oo][Xx]"]))) of
	 [] ->
		 "inbox";
	 [Inbox|_] ->
			Folder = lists:last(string:tokens(Inbox, "/")),
			Folder
 end;
safe_mailbox(M, _State) ->
	M.

get_finfo(File, undefined) ->
	{ok, FInfo} = file:read_file_info(File),
	FInfo;
get_finfo(_, FInfo) ->
	FInfo.

get_message(File, undefined) ->
	{ok, Bin} = file:read_file(File),
	try mimemail:decode(Bin) of
		Message ->
			Message
	catch
		What:Why ->
			io:format("Failed to parse message ~p:~p~n", [What, Why]),
			error
	end;
get_message(_, Message) ->
	Message.

get_attribute(_, _, _, _, [], Acc) ->
	lists:reverse(Acc);
get_attribute(File, UID, FInfo, Message, [<<"UID">>|T], Acc) ->
	get_attribute(File, UID, FInfo, Message, T, [{<<"UID">>, UID} | Acc]);
get_attribute(File, UID, FInfo, Message, [<<"FLAGS">>|T], Acc) ->
	get_attribute(File, UID, FInfo, Message, T, [{<<"FLAGS">>, [[<<"\\Seen">>]]} | Acc]);
get_attribute(File, UID, FInfo0, Message, [<<"INTERNALDATE">>|T], Acc) ->
	FInfo = get_finfo(File, FInfo0),
	MTime = FInfo#file_info.mtime,
	Date = dh_date:format("j-M-Y G:i:s +0500", MTime),
	get_attribute(File, UID, FInfo, Message, T, [{<<"INTERNALDATE">>, list_to_binary([$", Date, $"])} | Acc]);
get_attribute(File, UID, FInfo0, Message, [<<"RFC822.SIZE">>|T], Acc) ->
	FInfo = get_finfo(File, FInfo0),
	get_attribute(File, UID, FInfo, Message, T, [{<<"RFC822.SIZE">>, FInfo#file_info.size} | Acc]);
get_attribute(File, UID, FInfo, Message0, [[<<"BODY", _/binary>>, [<<"HEADER.FIELDS">>, Fields]]|T], Acc) ->
	Message = get_message(File, Message0),
	Headers = get_headers(Fields, Message, []),
	%io:format("Headers ~p~n", [Headers]),
	Att = {list_to_binary(["BODY[HEADER.FIELDS (", string:join(lists:map(fun(Z) -> [Z] end, Fields), " "), ")]"]), list_to_binary(["{", integer_to_list(byte_size(Headers)), "}\r\n", Headers])},
	get_attribute(File, UID, FInfo, Message, T, [Att | Acc]);
get_attribute(File, UID, FInfo, Message, [[<<"BODY", _/binary>>, [], {Start, Len}]|T], Acc) ->
	{ok, FH} = file:open(File, [read, binary]),
	Data = case file:pread(FH, {bof, Start}, Len) of
		{ok, D} ->
			D;
		eof ->
			{ok, Bin} = file:read_file(File),
			Bin
	end,
	ok = file:close(FH),
	Att = {list_to_binary(["BODY[]"]), list_to_binary(["{", integer_to_list(byte_size(Data)), "}\r\n", Data])},
	get_attribute(File, UID, FInfo, Message, T, [Att | Acc]);
get_attribute(File, UID, FInfo, Message, [[<<"BODY", _/binary>>, []]|T], Acc) ->
	{ok, Bin} = file:read_file(File),
	%{_Headers, Body} = mimemail:parse_headers(Bin),
	Att = {<<"BODY[]">>, list_to_binary(["{", integer_to_list(byte_size(Bin) + 2), "}\r\n", Bin, "\r\n"])},
	get_attribute(File, UID, FInfo, Message, T, [Att | Acc]);
get_attribute(File, UID, FInfo, Message0, [[<<"BODY", _/binary>>, [1]]|T], Acc) ->
	{_, _, _, _, Body} = Message = get_message(File, Message0),
	Att = {<<"BODY[1]">>, list_to_binary(["{", integer_to_list(byte_size(Body) + 2), "}\r\n", Body, "\r\n"])},
	get_attribute(File, UID, FInfo, Message, T, [Att | Acc]);
get_attribute(File, UID, FInfo, Message, [[<<"BODY", _/binary>>, [<<"HEADER">>]]|T], Acc) ->
	{ok, Bin} = file:read_file(File),
	[Header, _] = binstr:split(Bin, <<"\r\n\r\n">>, 2),
	Att = {<<"BODY[HEADER]">>, list_to_binary(["{", integer_to_list(byte_size(Header) + 2), "}\r\n", Header, "\r\n"])},
	get_attribute(File, UID, FInfo, Message, T, [Att | Acc]);
get_attribute(File, UID, FInfo, Message, [[<<"BODY", _/binary>>, [<<"TEXT">>]]|T], Acc) ->
	{ok, Bin} = file:read_file(File),
	[_, Body] = binstr:split(Bin, <<"\r\n\r\n">>, 2),
	Att = {<<"BODY[TEXT]">>, list_to_binary(["{", integer_to_list(byte_size(Body) + 2), "}\r\n", Body, "\r\n"])},
	get_attribute(File, UID, FInfo, Message, T, [Att | Acc]);
get_attribute(File, UID, FInfo, Message0, [<<"BODYSTRUCTURE">>|T], Acc) ->
	Message = get_message(File, Message0),
	BS = make_bodystructure(Message),
	%io:format("Bodystructure ~p~n", [list_to_binary(BS)]),
	get_attribute(File, UID, FInfo, Message, T, [{<<"BODYSTRUCTURE">>, list_to_binary(BS)}|Acc]);
get_attribute(File, UID, FInfo, Message0, [<<"ENVELOPE">>|T], Acc) ->
	{_, _, Headers, _, _} = Message = get_message(File, Message0),
	%% date, subject, from, sender, reply-to, to, cc, bcc, in-reply-to, and message-id
	%% sender & reply-to should fall back to From
	Env = ["(", get_header(<<"Date">>, Headers), " ", get_header(<<"Subject">>, Headers), " ",
		get_address_struct(<<"From">>, Headers), " ", get_address_struct(<<"Sender">>, <<"From">>, Headers), " ",
		get_address_struct(<<"Reply-To">>, <<"From">>, Headers), " ", get_address_struct(<<"To">>, Headers), " ",
		get_address_struct(<<"Cc">>, Headers), " ", get_address_struct(<<"Bcc">>, Headers), " ",
		get_header(<<"In-Reply-To">>, Headers), " ", get_header(<<"Message-ID">>, Headers), ")"],
	get_attribute(File, UID, FInfo, Message, T, [{<<"ENVELOPE">>, list_to_binary(Env)}|Acc]);
get_attribute(File, UID, FInfo, Message, [Att|T], Acc) ->
	io:format("unsupported attribute ~p~n", [Att]),
	get_attribute(File, UID, FInfo, Message, T, Acc).

make_bodystructure({<<"multipart">>, SubType, _Header, _Params, BodyParts}) ->
	["(", [make_bodystructure(Part) || Part <- BodyParts], " \"", SubType, "\")"];
make_bodystructure({Type, SubType, _Header, _Params, Body}) ->
	["(\"", Type, "\" \"", SubType, "\" (\"CHARSET\" \"us-ascii\") NIL NIL \"7BIT\" ", integer_to_list(byte_size(Body)), " ", integer_to_list(length(binstr:split(Body, <<"\n">>))), ")"].

get_header(Header, Headers) ->
	%% either NIL or a quoted string
	case mimemail:get_header_value(Header, Headers) of
		undefined ->
			"NIL";
		Value ->
			[$", Value, $"]
	end.

get_address_struct(Header, Fallback, Headers) ->
	case get_address_struct(Header, Headers) of
		"NIL" ->
			get_address_struct(Fallback, Headers);
		Res ->
			Res
	end.

get_address_struct(Header, Headers) ->
	case mimemail:get_header_value(Header, Headers) of
		undefined ->
			"NIL";
		Value ->
			%% lazy hack, not reliable.
			Users = binstr:split(Value, <<",">>),
			["(", [begin [Mailbox, Host] = binstr:split(binstr:strip(User), <<"@">>), ["(NIL NIL \"", Mailbox, "\" \"", Host, "\")"] end || User <- Users], ")"]
	end.

get_headers([], _, Acc) ->
	list_to_binary([lists:reverse(Acc), "\r\n"]);
get_headers([Header|T], {_, _, Headers, _, _} = Message, Acc) ->
	case mimemail:get_header_value(Header, Headers) of
		undefined ->
			get_headers(T, Message, Acc);
		Value ->
			get_headers(T, Message, [[Header, ": ", Value, "\r\n"] | Acc])
	end.


handle_FETCH(Seq, Attributes, UID, Callback, #state{rootdir=RootDir} = State) ->
	FolderDir = filename:join([RootDir, State#state.user, safe_mailbox(State#state.folder, State)]),
	{ok, Contents} = file:list_dir(FolderDir),
	%% haskish natural sort
	SortedContents = lists:sort(fun(A, B) -> (catch list_to_float(A++"0")) < (catch list_to_float(B++"0")) end, Contents),
	Files = [filename:join(FolderDir, X) || X <- SortedContents, filelib:is_regular(filename:join(FolderDir, X))],
	do_fetch(Seq, Attributes, UID, Files, Callback, State, []),
	{ok, State}.


do_fetch([], _, _UID, _Contents, _Cb, _State, Acc) ->
	Acc;
do_fetch([{S, '*'}|T], Attributes, UID, Contents, Cb, State, Acc) ->
	{Size, _, _, _, _, {UIDFirst, _UIDNext}, _, _} = mailbox_details(State#state.folder, State),
	End = case UID of
		true ->
			Size + 32000;
		_ ->
			Size
	end,

	Start = case S < UIDFirst of
		true ->
			UIDFirst;
		false ->
			S
	end,

	Acc2 = do_fetch(lists:seq(Start, End), Attributes, UID, Contents, Cb, State, Acc),
	do_fetch(T, Attributes, UID, Contents, Cb, State, Acc2);
do_fetch([{Start, End}|T], Attributes, UID, Contents, Cb, State, Acc) ->
	Acc2 = do_fetch(lists:seq(Start, End), Attributes, UID, Contents, Cb, State, Acc),
	do_fetch(T, Attributes, UID, Contents, Cb, State, Acc2);
do_fetch([H|T], Attributes, UID, Contents, Cb, State, Acc) ->
	do_fetch(T, Attributes, UID, Contents, Cb, State, [mail_details(H, Attributes, UID, Contents, Cb, State) | Acc]).

handle_LIST(_, M, #state{rootdir=RootDir} = State) when M == <<"*">>; M == <<"%">> ->
	FolderDir = filename:join([RootDir, State#state.user]),
	{ok, Contents} = file:list_dir(FolderDir),
	Directories = [filename:join(FolderDir, X) || X <- Contents, filelib:is_dir(filename:join(FolderDir, X))],
	[["* LIST () \"/\" ", binstr:substr(Dir, byte_size(FolderDir) + 2), "\r\n"] || Dir <- Directories];
handle_LIST(_, M, #state{user=User, rootdir=RootDir}) ->
	case binstr:to_lower(M) of
		<<"inbox">> ->
			case filelib:wildcard(binary_to_list(list_to_binary([RootDir, "/", User,"/[Ii][Nn][Bb][Oo][Xx]"]))) of
				[] ->
					[];
				[Inbox|_] ->
					Folder = lists:last(string:tokens(Inbox, "/")),
					["* LIST () \"/\" ", Folder, "\r\n"]
			end;
		Folder ->
			FolderDir = filename:join([RootDir, User, Folder]),
			case filelib:is_dir(FolderDir) of
				true ->
					["* LIST () \"/\" ", Folder, "\r\n"];
				_ ->
					[]
			end
	end.

handle_LSUB(_, M, #state{rootdir=RootDir} = State) when M == <<"*">>; M == <<"%">> ->
	FolderDir = filename:join([RootDir, State#state.user]),
	{ok, Contents} = file:list_dir(FolderDir),
	Directories = [filename:join(FolderDir, X) || X <- Contents, filelib:is_dir(filename:join(FolderDir, X))],
	[["* LSUB () \"/\" ", binstr:substr(Dir, byte_size(FolderDir) + 2), "\r\n"] || Dir <- Directories].


handle_CLOSE(State) ->
	{ok, State#state{folder=undefined}}.

handle_STATUS(Mailbox, _Flags, #state{user=User, rootdir=RootDir} = State) ->
	FolderDir = filename:join([RootDir, User, safe_mailbox(Mailbox, State)]),
	%io:format("Folder ~p~n", [FolderDir]),
	case filelib:is_dir(FolderDir) of
		true ->
			{Count, Recent, Unseen, _Flags2, _PermanentFlags, {_, UIDNext}, UIDValidity, _RW} = mailbox_details(Mailbox, State),
			{ok, {safe_mailbox(Mailbox, State), Count, Recent, Unseen, UIDNext, UIDValidity}, State};
		_ ->
		 {error, "Folder does not exist", State}
 end.




