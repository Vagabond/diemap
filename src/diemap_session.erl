
-module(diemap_session).
-behaviour(gen_server).

%% gen_server exports$
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% other exports
-export([start_link/3]).

-define(UNAUTH, 0).
-define(AUTH, 1).
-define(SELECTED, 2).

-record(state,
	{
		socket :: port(),
		transport :: atom(),
		mailbox :: binary(),
		module :: atom(),
		modstate :: term(),
		connstate :: ?UNAUTH | ?AUTH | ?SELECTED
	}
).

start_link(Socket, Transport, Options) ->
	gen_server:start_link(?MODULE, [Socket, Transport, Options], []).

init([Socket, Transport, [Module | Options]]) ->
	{ok, Peername} = Transport:peername(Socket),
	case Module:init(Peername, proplists:get_value(moduleoptions, Options, [])) of
		{ok, State} ->
			reply(Transport, Socket, "* OK server ready\r\n"),
			Transport:setopts(Socket, [{active, once}, {packet, line}]),
			{ok, #state{socket=Socket, transport=Transport, module=Module, modstate=State, connstate=?UNAUTH}};
		{stop, Message, Reason} ->
			reply(Transport, Socket, ["* BYE ", Message, "\r\n"]),
			{stop, Reason}
	end.

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({_SocketType, Socket, Packet},
	#state{socket = Socket, transport=Transport, connstate=C} = State) ->
	io:format("C: [~p] ~s", [C, Packet]),
	Result = case imap_parser:parse(Packet) of
		{fail, _} ->
			io:format("Failed to parse ~p~n", [Packet]),
			{stop, normal, State};
		{Tag, {invalid, _Command}} ->
			io:format("Failed to parse ~p~n", [Packet]),
			reply(Transport, Socket, [Tag, " BAD Command not recognized or bad arguments\r\n"]),
			{noreply, State};
		{Tag, Command} ->
			handle_command(Tag, Command, State)
	end,
	Transport:setopts(Socket, [{active, once}]),
	Result;
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
	io:format("closed~n"),
	{stop, normal, State};
handle_info({ssl_closed, Socket}, #state{socket = Socket} = State) ->
	io:format("closed~n"),
	{stop, normal, State};
handle_info(_Info, State) ->
	io:format("got info ~p~n", [_Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

reply(Transport, Socket, Message) ->
	io:format("S: ~s", [list_to_binary(Message)]),
	Transport:send(Socket, Message).

handle_command(Tag, capability,
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState} = State) ->
	case Module:handle_CAPABILITY([], ModState) of
		{ok, Capabilities, NewModState} ->
			FullCapabilities = lists:usort(["IMAP4rev1" | Capabilities]),
			reply(Transport, Socket, ["* CAPABILITY ", string:join(FullCapabilities, " "), "\r\n",
					Tag, " OK CAPABILITY completed\r\n"]),
			{noreply, State#state{modstate=NewModState}};
		{error, Message, NewModState} ->
			reply(Transport, Socket, [Tag, " BAD ", Message, "\r\n"]),
			{noreply, State#state{modstate=NewModState}}
	end;
handle_command(Tag, noop,
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState} = State) ->
	case Module:handle_NOOP(ModState) of
		{ok, NewModState} ->
			reply(Transport, Socket, [Tag, " OK NOOP completed\r\n"]),
			{noreply, State#state{modstate=NewModState}};
		{error, Message, NewModState} ->
			reply(Transport, Socket, [Tag, " BAD ", Message, "\r\n"]),
			{noreply, State#state{modstate=NewModState}}
	end;
handle_command(Tag, logout,
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState} = State) ->
	case Module:handle_LOGOUT(ModState) of
		{ok, QuitMessage, NewModState} ->
			reply(Transport, Socket, ["* BYE ", QuitMessage, "\r\n", Tag, " OK LOGOUT completed\r\n"]),
			{stop, normal, State#state{modstate=NewModState}};
		{error, Message, NewModState} ->
			reply(Transport, Socket, [Tag, " BAD ", Message, "\r\n"]),
			{noreply, State#state{modstate=NewModState}}
	end;
handle_command(Tag, {login, User, Pass},
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState} = State) ->
	case Module:handle_LOGIN(User, Pass, ModState) of
		{ok, NewModState} ->
			reply(Transport, Socket, [Tag, " OK LOGIN completed\r\n"]),
			{noreply, State#state{modstate=NewModState, connstate=?AUTH}};
		{error, NewModState} ->
			reply(Transport, Socket, [Tag, " NO LOGIN failed\r\n"]),
			{noreply, State#state{modstate=NewModState}}
	end;
handle_command(Tag, {list, Reference, Mailbox},
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState,connstate=C} = State) when C > ?UNAUTH ->
	case [Reference, Mailbox] of
		[Reference, <<>>] ->
			Root = case Reference of
				<<>> ->
					<<>>;
				_ ->
					[H|_] = binstr:split(Reference, <<"/">>),
					H
			end,
			reply(Transport, Socket, ["* LIST (\\NOSELECT) \"/\" \"", Root, "\"\r\n", Tag, " OK LIST completed\r\n"]),
			{noreply, State};
		%[<<>>, <<"*">>] ->
			%reply(Transport, Socket, ["* LIST () \"/\" inbox\r\n", "* LIST () \"/\" sent_items\r\n", Tag, " OK LIST Completed\r\n"]),
			%{noreply, State};
		[Reference, Mailbox] ->
			ListResults = Module:handle_LIST(Reference, Mailbox, ModState),
			reply(Transport, Socket, ListResults ++ [Tag, " OK LIST Completed\r\n"]),
			%reply(Transport, Socket, [Tag, " BAD LIST no such mailbox\r\n"]),
			{noreply, State}
	end;
handle_command(Tag, {lsub, Reference, Mailbox},
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState,connstate=C} = State) when C > ?UNAUTH ->
	case [Reference, Mailbox] of
		[Reference, <<>>] ->
			Root = case Reference of
				<<>> ->
					<<>>;
				_ ->
					[H|_] = binstr:split(Reference, <<"/">>),
					H
			end,
			reply(Transport, Socket, ["* LSUB (\\NOSELECT) \"/\" \"", Root, "\"\r\n", Tag, " OK LSUB completed\r\n"]),
			{noreply, State};
		%[<<>>, <<"*">>] ->
			%reply(Transport, Socket, ["* LIST () \"/\" inbox\r\n", "* LIST () \"/\" sent_items\r\n", Tag, " OK LIST Completed\r\n"]),
			%{noreply, State};
		[Reference, Mailbox] ->
			ListResults = Module:handle_LSUB(Reference, Mailbox, ModState),
			reply(Transport, Socket, ListResults ++ [Tag, " OK LSUB Completed\r\n"]),
			%reply(Transport, Socket, [Tag, " BAD LIST no such mailbox\r\n"]),
			{noreply, State}
	end;
handle_command(Tag, {create, _Mailbox},
	#state{socket=Socket,transport=Transport,module=_Module,modstate=_ModState,connstate=C} = State) when C > ?UNAUTH ->
	reply(Transport, Socket, [Tag, " OK CREATE completed\r\n"]),
	{noreply, State};
handle_command(Tag, {subscribe, _Mailbox},
	#state{socket=Socket,transport=Transport,module=_Module,modstate=_ModState,connstate=C} = State) when C > ?UNAUTH ->
	reply(Transport, Socket, [Tag, " OK SUBSCRIBE completed\r\n"]),
	{noreply, State};
handle_command(Tag, {status, Mailbox, Flags},
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState,connstate=C} = State) when C > ?UNAUTH ->
	case Module:handle_STATUS(Mailbox, Flags, ModState) of
		{ok, Reply, NewModState} ->
			reply(Transport, Socket, Reply ++ [Tag, " OK STATUS completed\r\n"]),
			{noreply, State#state{modstate=NewModState}};
		{error, Message, NewModState} ->
			reply(Transport, Socket, [Tag, " BAD ", Message, "\r\n"]),
			{noreply, State#state{modstate=NewModState}}
	end;
handle_command(Tag, check,
	#state{socket=Socket,transport=Transport,module=_Module,modstate=_ModState,connstate=?SELECTED} = State) ->
	reply(Transport, Socket, [Tag, " OK CHECK completed\r\n"]),
	{noreply, State};
handle_command(Tag, {select, Mailbox},
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState,connstate=C} = State) when C > ?UNAUTH ->
	case Module:handle_SELECT(Mailbox, ModState) of
		{ok, {Count, Recent, Unseen, Flags, PermanentFlags, {_, UIDNext}, UIDValidity, RW}, NewModState} ->
			ReadWrite = case RW of
				read_only ->
					"READ-ONLY";
				read_write ->
					"READ-WRITE"
			end,
			reply(Transport, Socket, [
					"* ", integer_to_list(Count), " EXISTS\r\n",
					"* ", integer_to_list(Recent), " RECENT\r\n",
					"* OK [UNSEEN ", integer_to_list(Unseen), "]\r\n",
					"* OK [UIDNEXT ", integer_to_list(UIDNext), "]\r\n",
					"* OK [UIDVALIDITY ", integer_to_list(UIDValidity), "]\r\n",
					"* OK [PERMANENTFLAGS (", string:join(PermanentFlags, " "), ")]\r\n",
					"* FLAGS (", string:join(Flags, " "), ")\r\n",
					Tag, " OK [", ReadWrite, "] SELECT completed\r\n"]),
			{noreply, State#state{mailbox=Mailbox, modstate=NewModState, connstate=?SELECTED}};
		{error, Message, NewModState} ->
			reply(Transport, Socket, [Tag, " NO ", Message]),
			{noreply, State#state{modstate=NewModState, mailbox=undefined, connstate=?AUTH}}
	end;
handle_command(Tag, {fetch, Sequence, Spec},
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState,connstate=?SELECTED} = State) ->
	%io:format("fetch ~p ~p~n", [Sequence, Spec]),
	F = fun(Msg) -> reply(Transport, Socket, Msg) end,
	Module:handle_FETCH(Sequence, Spec, false, F, ModState),
	%io:format("Fetch results ~p~n", [list_to_binary(FetchResults)]),
	reply(Transport, Socket, [Tag, " OK FETCH completed\r\n"]),
	%halt(),
	{noreply, State};
handle_command(Tag, {uid, {fetch, Sequence, Spec}},
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState,connstate=?SELECTED} = State) ->
	%io:format("fetch ~p ~p~n", [Sequence, Spec]),
	F = fun(Msg) -> reply(Transport, Socket, Msg) end,
	Module:handle_FETCH(Sequence, Spec, true, F, ModState),
	%io:format("Fetch results ~p~n", [list_to_binary(FetchResults)]),
	reply(Transport, Socket, [Tag, " OK FETCH completed\r\n"]),
	%halt(),
	{noreply, State};
handle_command(Tag, {uid, {store, _Sequence, _Flags}},
	#state{socket=Socket,transport=Transport,module=_Module,modstate=_ModState,connstate=?SELECTED} = State) ->
	%io:format("fetch ~p ~p~n", [Sequence, Spec]),
	%io:format("Fetch results ~p~n", [list_to_binary(FetchResults)]),
	reply(Transport, Socket, [Tag, " OK STORE completed\r\n"]),
	%halt(),
	{noreply, State};
handle_command(Tag, close,
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState,connstate=?SELECTED} = State) ->
	case Module:handle_CLOSE(ModState) of
		{ok, NewModState} ->
			reply(Transport, Socket, [Tag, " OK CLOSE completed\r\n"]),
			{noreply, State#state{mailbox=undefined, modstate=NewModState}};
		{error, Message, NewModState} ->
			reply(Transport, Socket, [Tag, " BAD ", Message, "\r\n"]),
			{noreply, State#state{modstate=NewModState}}
	end;
handle_command(Tag, _Cmd,
	#state{socket=Socket,transport=Transport} = State) ->
	io:format("unhandled or forbidden command ~p~n", [_Cmd]),
	reply(Transport, Socket, [Tag, " BAD Command not allowed\r\n"]),
	{noreply, State}.


