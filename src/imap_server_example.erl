-module(imap_server_example).

-export([init/2, handle_CAPABILITY/2, handle_NOOP/1, handle_LOGOUT/1, handle_LOGIN/3, handle_SELECT/2]).

-record(state, {
		options,
		user
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
		 {error, "No INBOX for this user", State};
	 [Inbox|_] ->
		 %% collect some info about the mailbox
		 {ok, Contents} = file:list_dir(Inbox),
		 Files = [X || X <- Contents, filelib:is_regular(filename:join(Inbox, X))],
		 {ok, {length(Files), 0, 0, ["\\Answered", "\\Flagged", "\\Deleted", "\\Seen", "\\Draft"], [], 5551234, 987654, read_only}, State}
 end;
handle_SELECT(Folder, #state{user=User} = State) ->
	Folder = list_to_binary(["maildir/", User, "/", Folder]),
	case filelib:is_dir(Folder) of
		true ->
			{ok, Contents} = file:list_dir(Folder),
			Files = [X || X <- Contents, filelib:is_regular(filename:join(Folder, X))],
			{ok, {length(Files), 0, 0, ["\\Answered", "\\Flagged", "\\Deleted", "\\Seen", "\\Draft"], [], 5551234, 987654, read_only}, State};
		_ ->
		 {error, "Folder does not exist", State}
 end.






