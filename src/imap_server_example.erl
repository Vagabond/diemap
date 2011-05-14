-module(imap_server_example).

-export([init/2, handle_CAPABILITY/2, handle_NOOP/1, handle_LOGOUT/1, handle_LOGIN/3, handle_SELECT/2, handle_FETCH/5, handle_LIST/3, handle_CLOSE/1, handle_STATUS/3]).

-include_lib("kernel/include/file.hrl").

-record(state, {
		options,
		user,
		folder
	}).

init(Peername, Options) ->
	io:format("client connection from ~p~n", [Peername]),
	{ok, #state{options=Options}}.

handle_CAPABILITY(Capabilities, State) ->
	%% Add or remove from the capabilities list here
	{ok, Capabilities, State}.

handle_NOOP(State) ->
	{ok, State}.

handle_LOGOUT(State) ->
	{ok, "Have a super day!", State}.

handle_LOGIN(User, _Pass, State) ->
	case filelib:is_dir(list_to_binary(["maildir/", User])) of
		true ->
			{ok, State#state{user=User}};
		false ->
			{error, State}
	end.

handle_SELECT(inbox, #state{user=User} = State) ->
 case filelib:wildcard(binary_to_list(list_to_binary(["maildir/", User,"/[Ii][Nn][Bb][Oo][Xx]"]))) of
	 [] ->
		 {error, "No INBOX for this user", State#state{folder=undefined}};
	 [Inbox|_] ->
			Folder = lists:last(string:tokens(Inbox, "/")),
			%io:format("Folder is ~p", [Folder]),
		 {ok, mailbox_details(Folder, State), State#state{folder=Folder}}
 end;
handle_SELECT(Folder, #state{user=User} = State) ->
	FolderDir = list_to_binary(["maildir/", User, "/", Folder]),
	%io:format("Folder ~p~n", [FolderDir]),
	case filelib:is_dir(FolderDir) of
		true ->
			{ok, mailbox_details(Folder, State), State#state{folder=Folder}};
		_ ->
		 {error, "Folder does not exist", State#state{folder=undefined}}
 end.

mailbox_details(Mailbox, State) ->
	FolderDir = list_to_binary(["maildir/", State#state.user, "/", safe_mailbox(Mailbox)]),
	{ok, Contents} = file:list_dir(FolderDir),
	Files = [filename:join(FolderDir, X) || X <- Contents, filelib:is_regular(filename:join(FolderDir, X))],
	Uids = lists:sort([ begin {ok, FInfo} = file:read_file_info(F), FInfo#file_info.inode end || F <- Files]),
	case Uids of
		[] ->
			UidFirst = 0,
			UidNext = 1;
		_ ->
			UidFirst = hd(Uids),
			UidNext = lists:last(Uids)
	end,
	{length(Files), 0, 0, ["\\Answered", "\\Flagged", "\\Deleted", "\\Seen", "\\Draft"], [], {UidFirst, UidNext+1}, 987654, read_only}.

mail_details(Uid, Attributes, true, Files, Cb, _State) -> %% UID fetch
	case lists:dropwhile(fun(F) -> {ok, FInfo} = file:read_file_info(F), FInfo#file_info.inode =/= Uid end, Files) of
		[File|Rest] ->
			%io:format("found file ~p for UID ~p~n", [File, Uid]),
			{ok, FInfo} = file:read_file_info(File),
			{ok, Bin} = file:read_file(File),
			Seq = length(Files) - length(Rest),
			%% XXX make sure UID is in the list of Attributes
			try mimemail:decode(Bin) of
				Message ->
					Result = ["* ", integer_to_list(Seq), " FETCH ",  encode_fetch_response([get_attribute(File, FInfo, Message, Attribute) || Attribute <- Attributes], []), "\r\n"],
					%io:format("Results ~p~n", [list_to_binary(Result)]),
					Cb(Result)
			catch
				What:Why ->
					io:format("Failed to parse message ~p:~p~n", [What, Why]),
					[]
			end;
		[] ->
			io:format("no such UID id ~p~n", [Uid]),
			[]
	end;
mail_details(Seq, Attributes, _, Files, Cb, _State) ->
	File = lists:nth(Seq, Files),
	{ok, FInfo} = file:read_file_info(File),
	{ok, Bin} = file:read_file(File),
	try mimemail:decode(Bin) of
		Message ->
			%io:format("fetching message ~p~n", [Seq]),
			Result = ["* ", integer_to_list(Seq), " FETCH ",  encode_fetch_response([get_attribute(File, FInfo, Message, Attribute) || Attribute <- Attributes], []), "\r\n"],
			%io:format("Results ~p~n", [list_to_binary(Result)]),
			Cb(Result)
	catch
		What:Why ->
			io:format("Failed to parse message ~p:~p~n", [What, Why]),
			[]
	end.

safe_mailbox(inbox) ->
	"inbox";
safe_mailbox(M) ->
	M.

get_attribute(_File, FInfo, _Message, <<"UID">>) ->
	{<<"UID">>, FInfo#file_info.inode};
get_attribute(_File, _FInfo, _Message, <<"FLAGS">>) ->
	{<<"FLAGS">>, [[<<"\\Seen">>]]};
get_attribute(_File, FInfo, _Message, <<"INTERNALDATE">>) ->
	MTime = FInfo#file_info.mtime,
	Date = dh_date:format("j-M-Y i:G:s +0500", MTime),
	{<<"INTERNALDATE">>, list_to_binary([$", Date, $"])};
get_attribute(_File, FInfo, _Message, <<"RFC822.SIZE">>) ->
	{<<"RFC822.SIZE">>, FInfo#file_info.size};
get_attribute(_File, _FInfo, Message, [<<"BODY", _/binary>>, [Y, Fields]] = _Key) ->
	Headers = get_headers(Fields, Message, []),
	%io:format("Headers ~p~n", [Headers]),
	{list_to_binary(["BODY[",Y," (", string:join(lists:map(fun(Z) -> [Z] end, Fields), " "), ")]"]), list_to_binary(["{", integer_to_list(byte_size(Headers)), "}\r\n", Headers])};
	%{Key, {literal, list_to_binary(get_headers(Fields, Message, []))};
get_attribute(File, _FInfo, _Message, [<<"BODY", _/binary>>, []] = _Key) ->
	{ok, Bin} = file:read_file(File),
	%{_Headers, Body} = mimemail:parse_headers(Bin),
	{<<"BODY[]">>, list_to_binary(["{", integer_to_list(byte_size(Bin) + 4), "}\r\n", Bin, "\r\n\r\n"])};
get_attribute(_File, _FInfo, _Message, Att) ->
	io:format("unsupported attribute ~p~n", [Att]),
	error.

get_headers([], _, Acc) ->
	list_to_binary([lists:reverse(Acc), "\r\n"]);
get_headers([Header|T], {_, _, Headers, _, _} = Message, Acc) ->
	case mimemail:get_header_value(Header, Headers) of
		undefined ->
			get_headers(T, Message, Acc);
		Value ->
			get_headers(T, Message, [[Header, ": ", Value, "\r\n"] | Acc])
	end.

encode_fetch_response([], Acc) ->
	["(", string:join(lists:reverse(Acc), " "), ")"];
encode_fetch_response([{Key, Value}|T], Acc) ->
	encode_fetch_response(T, [[Key, 32, encode_value(Value)] | Acc]);
encode_fetch_response([error|T], Acc) ->
	encode_fetch_response(T, Acc).

encode_value(Val) when is_list(Val) ->
	["(", string:join(Val, " "), ")"];
encode_value(Val) when is_integer(Val) ->
	integer_to_list(Val);
encode_value(Val) ->
	Val.

handle_FETCH(Seq, Attributes, UID, Callback, State) ->
	FolderDir = list_to_binary(["maildir/", State#state.user, "/", safe_mailbox(State#state.folder)]),
	{ok, Contents} = file:list_dir(FolderDir),
	Files = [filename:join(FolderDir, X) || X <- Contents, filelib:is_regular(filename:join(FolderDir, X))],
	do_fetch(Seq, Attributes, UID, Files, Callback, State, []).


do_fetch([], _, _UID, _Contents, _Cb, _State, Acc) ->
	Acc;
do_fetch([{S, '*'}|T], Attributes, UID, Contents, Cb, State, Acc) ->
	{Size, _, _, _, _, {UIDFirst, UIDNext}, _, _} = mailbox_details(State#state.folder, State),
	End = case UID of
		true ->
			UIDNext;
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

handle_LIST(_, M, State) when M == <<"*">>; M == <<"%">> ->
	FolderDir = list_to_binary(["maildir/", State#state.user]),
	{ok, Contents} = file:list_dir(FolderDir),
	Directories = [filename:join(FolderDir, X) || X <- Contents, filelib:is_dir(filename:join(FolderDir, X))],
	[["* LIST () \"/\" ", binstr:substr(Dir, byte_size(FolderDir) + 2), "\r\n"] || Dir <- Directories];
handle_LIST(_, M, #state{user=User}) ->
	case binstr:to_lower(M) of
		<<"inbox">> ->
			case filelib:wildcard(binary_to_list(list_to_binary(["maildir/", User,"/[Ii][Nn][Bb][Oo][Xx]"]))) of
				[] ->
					[];
				[Inbox|_] ->
					Folder = lists:last(string:tokens(Inbox, "/")),
					["* LIST () \"/\" ", Folder, "\r\n"]
			end;
		Folder ->
			FolderDir = list_to_binary(["maildir/", User, "/", Folder]),
			case filelib:is_dir(FolderDir) of
				true ->
					["* LIST () \"/\" ", Folder, "\r\n"];
				_ ->
					[]
			end
	end.

handle_CLOSE(State) ->
	{ok, State#state{folder=undefined}}.

handle_STATUS(Mailbox, Flags, #state{user=User} = State) ->
	FolderDir = list_to_binary(["maildir/", User, "/", Mailbox]),
	%io:format("Folder ~p~n", [FolderDir]),
	case filelib:is_dir(FolderDir) of
		true ->
			{Count, Recent, Unseen, _Flags, _PermanantFlags, {_, UIDNext}, UIDValidity, _RW} = mailbox_details(Mailbox, State),
			Reply = lists:map(fun(<<"UIDNEXT">>) ->
						["UIDNEXT ", integer_to_list(UIDNext)];
					(<<"RECENT">>) ->
						["RECENT ", integer_to_list(Recent)];
					(<<"MESSAGES">>) ->
						["MESSAGES ", integer_to_list(Count)];
					(<<"UIDVALIDITY">>) ->
						["UIDVALIDITY ", integer_to_list(UIDValidity)];
					(<<"UNSEEN">>) ->
						["UNSEEN ", integer_to_list(Unseen)]
				end, Flags),

			{ok, ["* STATUS ", Mailbox, " (", Reply, ")\r\n"], State};
		_ ->
		 {error, "Folder does not exist", State}
 end.




