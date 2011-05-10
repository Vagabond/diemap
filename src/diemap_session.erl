
-module(diemap_session).
-behaviour(gen_fsm).

%% gen_fsm exports$
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%% state exports
-export([unauthenticated/2, authenticated/2, selected/2]).
-export([unauthenticated/3, authenticated/3, selected/3]).
%% other exports
-export([start_link/3, parse_quoted_list/1, parse_argument_list/1]).

-record(state,
	{
		socket :: port(),
		transport :: atom(),
		mailbox :: binary(),
		module :: atom(),
		modstate :: term()
	}
).

start_link(Socket, Transport, Options) ->
	Transport:setopts(Socket, [{active, once}, {packet, line}]),
	gen_fsm:start_link(?MODULE, [Socket, Transport, Options], []).

init([Socket, Transport, [Module | Options]]) ->
	{ok, Peername} = Transport:peername(Socket),
	case Module:init(Peername, proplists:get_value(moduleoptions, Options, [])) of
		{ok, State} ->
			reply(Transport, Socket, "* OK server ready\r\n"),
			{ok, unauthenticated, #state{socket=Socket, transport=Transport, module=Module, modstate=State}};
		{stop, Message, Reason} ->
			reply(Transport, Socket, ["* BYE ", Message, "\r\n"]),
			{stop, Reason}
	end.

unauthenticated({command, Tag, <<"LOGIN">>, Args},
	#state{transport=Transport,socket=Socket,module=Module,modstate=ModState} = State) ->
	case parse_argument_list(Args) of
			[User, Pass] ->
				case Module:handle_LOGIN(User, Pass, ModState) of
					{ok, NewModState} ->
						reply(Transport, Socket, [Tag, " OK LOGIN completed\r\n"]),
						{next_state, authenticated, State#state{modstate=NewModState}};
					{error, NewModState} ->
						reply(Transport, Socket, [Tag, " NO LOGIN failed\r\n"]),
						{next_state, unauthenticated, State#state{modstate=NewModState}}
				end;
			_ ->
				reply(Transport, Socket, [Tag, " BAD LOGIN bad arguments\r\n"]),
				{next_state, unauthenticated, State}
		end;
unauthenticated(_Msg, State) ->
	{next_state, unauthenticated, State}.

unauthenticated(_Msg, _From, State) ->
	{next_state, unauthenticated, State}.

authenticated({command, Tag, <<"LIST">>, Args}, #state{transport=Transport,socket=Socket} = State) ->
	case parse_argument_list(Args) of
		[Reference, <<>>] ->
			Root = case Reference of
				<<>> ->
					<<>>;
				_ ->
					[H|_] = binstr:split(Reference, <<"/">>),
					H
			end,
			reply(Transport, Socket, ["* LIST (\\NOSELECT) \"/\" \"", Root, "\"\r\n", Tag, " OK LIST completed\r\n"]),
			{next_state, authenticated, State};
		[_Reference, _Mailbox] ->
			reply(Transport, Socket, [Tag, " BAD LIST no such mailbox\r\n"]),
			{next_state, authenticated, State}
	end;
authenticated({command, Tag, <<"SELECT">>, Mailbox},
	#state{transport=Transport,socket=Socket,module=Module,modstate=ModState} = State) ->
	MailboxName = case binstr:to_lower(Mailbox) of
		<<"inbox">> ->
			%% inbox is a magical special case that needs to be treated case insensitively
			inbox;
		X -> X
	end,
	case Module:handle_SELECT(MailboxName, ModState) of
		{ok, {Count, Recent, Unseen, Flags, PermanantFlags, UIDNext, UIDValidity, RW}, NewModState} ->
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
					"* OK [PERMANANTFLAGS (", string:join(PermanantFlags, " "), ")\r\n",
					"* FLAGS (", string:join(Flags, " "), ")\r\n",
					Tag, " OK [", ReadWrite, "] SELECT completed\r\n"]),
			{next_state, selected, State#state{mailbox=MailboxName, modstate=NewModState}};
		{error, Message, NewModState} ->
			reply(Transport, Socket, [Tag, " NO ", Message]),
			{next_state, authenticated, State#state{modstate=NewModState}}
	end;
authenticated({command, Tag, Command, _Args}, #state{socket=Socket, transport=Transport} = State) ->
	reply(Transport, Socket, [Tag, " BAD ", Command, " not recognized\r\n"]),
	{next_state, authenticated, State};
authenticated(_Msg, State) ->
	{next_state, authenticated, State}.

authenticated(_Msg, _From, State) ->
	{next_state, authenticated, State}.

selected({command, _Tag, <<"FETCH">>, Args}, State) ->
	[SeqSet, Fields] = binstr:split(Args, <<" ">>, 2),
	Sequence = parse_range(SeqSet),
	[MsgFields] = parse_argument_list(Fields),
	io:format("fields ~p~n", [Fields]),
	io:format("sequence ~p~n", [Sequence]),
	io:format("message fields ~p~n", [MsgFields]),
	{next_state, selected, State};
selected(_Msg, State) ->
	{next_state, selected, State}.

selected(_Msg, _From, State) ->
	{next_state, selected, State}.

handle_info({_SocketType, Socket, Packet}, StateName,
	#state{socket = Socket, transport=Transport} = State) ->
	io:format("C: [~p] ~s", [StateName, Packet]),
	Command = parse_packet(Packet, State),
	Result = case Command of
		{command, Tag, <<"CAPABILITY">>, []} ->
			do_capability(Tag, StateName, State);
		{command, Tag, <<"CAPABILITY">>, _} ->
			reply(Transport, Socket, [Tag, " BAD CAPABILITY takes no arguments\r\n"]),
			{next_state, StateName, State};
		{command, Tag, <<"NOOP">>, []} ->
			do_noop(Tag, StateName, State);
		{command, Tag, <<"NOOP">>, _} ->
			reply(Transport, Socket, [Tag, " BAD NOOP takes no arguments\r\n"]),
			{next_state, StateName, State};
		{command, Tag, <<"LOGOUT">>, []} ->
			do_logout(Tag, StateName, State);
		{command, Tag, <<"LOGOUT">>, _} ->
			reply(Transport, Socket, [Tag, " BAD LOGOUT takes no arguments\r\n"]),
			{next_state, StateName, State};
		_ ->
			?MODULE:StateName(Command, State)
	end,
	Transport:setopts(Socket, [{active, once}]),
	Result;
handle_info({tcp_closed, Socket}, _StateName, #state{socket = Socket} = State) ->
	io:format("closed~n"),
	{stop, normal, State};
handle_info({ssl_closed, Socket}, _StateName, #state{socket = Socket} = State) ->
	io:format("closed~n"),
	{stop, normal, State};
handle_info(_Info, StateName, State) ->
	io:format("got info ~p~n", [_Info]),
	{next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	{reply, ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

do_capability(Tag, StateName,
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState} = State) ->
	case Module:handle_CAPABILITY([], ModState) of
		{ok, Capabilities, NewModState} ->
			FullCapabilities = lists:usort(["IMAPrev1" | Capabilities]),
			reply(Transport, Socket, ["* CAPABILITY ", string:join(FullCapabilities, " "), "\r\n",
					Tag, " OK CAPABILITY completed\r\n"]),
			{next_state, StateName, State#state{modstate=NewModState}};
		{error, Message, NewModState} ->
			reply(Transport, Socket, [Tag, " BAD ", Message, "\r\n"]),
			{next_state, StateName, State#state{modstate=NewModState}}
	end.

do_noop(Tag, StateName, 
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState} = State) ->
	case Module:handle_NOOP(ModState) of
		{ok, NewModState} ->
			reply(Transport, Socket, [Tag, " OK NOOP completed\r\n"]),
			{next_state, StateName, State#state{modstate=NewModState}};
		{error, Message, NewModState} ->
			reply(Transport, Socket, [Tag, " BAD ", Message, "\r\n"]),
			{next_state, StateName, State#state{modstate=NewModState}}
	end.

do_logout(Tag, StateName,
	#state{socket=Socket,transport=Transport,module=Module,modstate=ModState} = State) ->
	case Module:handle_LOGOUT(ModState) of
		{ok, QuitMessage, NewModState} ->
			reply(Transport, Socket, ["* BYE ", QuitMessage, "\r\n", Tag, " OK LOGOUT completed\r\n"]),
			{stop, normal, State#state{modstate=NewModState}};
		{error, Message, NewModState} ->
			reply(Transport, Socket, [Tag, " BAD ", Message, "\r\n"]),
			{next_state, StateName, State#state{modstate=NewModState}}
	end.

parse_packet(Packet, _State) ->
	case binstr:split(binstr:chomp(Packet), <<" ">>, 3) of
		[Tag, Command, Args] ->
			{command, Tag, binstr:to_upper(Command), Args};
		[Tag, Command] ->
			{command, Tag, binstr:to_upper(Command), []}
	end.

parse_argument_list(List) ->
	parse_argument_list(List, []).

parse_argument_list(<<>>, Acc) ->
	lists:reverse(Acc);
parse_argument_list(<<$", Tail/binary>>, Acc) ->
	{Arg, Rest} = read_string_argument(Tail),
	parse_argument_list(Rest, [Arg | Acc]);
parse_argument_list(<<$(, Tail/binary>>, Acc) ->
	{Arg, Rest} = read_parenthesized_list(Tail),
	parse_argument_list(Rest, [Arg | Acc]);
parse_argument_list(<<$\s, Tail/binary>>, Acc) ->
	parse_argument_list(Tail, Acc);
parse_argument_list(Binary, Acc) ->
	case binstr:strpos(Binary, <<" ">>) of
		0 ->
			lists:reverse([Binary | Acc]);
		Index ->
			Arg = binstr:substr(Binary, 1, Index-1),
			Tail = binstr:substr(Binary, Index+1),
			parse_argument_list(Tail, [Arg | Acc])
	end.

read_string_argument(Binary) ->
	read_string_argument(Binary, []).

read_string_argument(<<>>, Acc) ->
	{error, unterminated_string, Acc};
read_string_argument(<<$\\, $", Tail/binary>>, Acc) ->
	read_string_argument(Tail, [$"|Acc]);
read_string_argument(<<$", Tail/binary>>, Acc) ->
	{list_to_binary(lists:reverse(Acc)), Tail};
read_string_argument(<<H, Tail/binary>>, Acc) ->
	read_string_argument(Tail, [H|Acc]).

read_parenthesized_list(Binary) ->
	read_parenthesized_list(Binary, []).

read_parenthesized_list(<<$), Tail/binary>>, Acc) ->
	{lists:reverse(Acc), Tail};
read_parenthesized_list(<<$", Tail/binary>>, Acc) ->
	{Elem, Rest} = read_string_argument(Tail),
	read_parenthesized_list(Rest, [Elem | Acc]);
read_parenthesized_list(<<$(, Tail/binary>>, Acc) ->
	{SubList, Rest} = read_parenthesized_list(Tail),
	read_parenthesized_list(Rest, [SubList | Acc]);
read_parenthesized_list(<<$\s, Tail/binary>>, Acc) ->
	read_parenthesized_list(Tail, Acc);
read_parenthesized_list(Binary, Acc) ->
	case binstr:strpos(Binary, <<" ">>) of
		0 ->
			case binstr:last(Binary) of
				<<")">> ->
					{lists:reverse([binstr:substr(Binary, 1, byte_size(Binary) -1) | Acc]), <<>>};
				_ ->
					{error, unterminated_list}
			end;
		Index ->
			{Continue, CutPoint} = case binstr:strpos(Binary, <<")">>) of
				0 ->
					{true, Index};
				EndIndex when EndIndex < Index ->
					{false, EndIndex};
				_ ->
					{true, Index}
			end,
			Arg = binstr:substr(Binary, 1, CutPoint-1),
			Tail = binstr:substr(Binary, CutPoint+1),
			case Continue of
				true ->
					read_parenthesized_list(Tail, [Arg | Acc]);
				_ ->
					{lists:reverse([Arg | Acc]), Tail}
			end
	end.


parse_quoted_list(List) ->
	parse_quoted_list(List, [], false).

parse_quoted_list(<<>>, Acc, false) ->
	list_to_binary(lists:reverse(Acc));
parse_quoted_list(<<>>, _, true) ->
	error;
parse_quoted_list(<<$\\, $", Rest/binary>>, Acc, Quoted) ->
	parse_quoted_list(Rest, [$"|Acc], Quoted);
parse_quoted_list(<<$\s, Rest/binary>>, Acc, false) ->
	parse_quoted_list(Rest, [0|Acc], false);
parse_quoted_list(<<$", Rest/binary>>, Acc, false) ->
	parse_quoted_list(Rest, Acc, true);
parse_quoted_list(<<$", Rest/binary>>, Acc, true) ->
	parse_quoted_list(Rest, Acc, false);
parse_quoted_list(<<H, Rest/binary>>, Acc, Quoted) ->
	parse_quoted_list(Rest, [H|Acc], Quoted).

reply(Transport, Socket, Message) ->
	io:format("S: ~s", [list_to_binary(Message)]),
	Transport:send(Socket, Message).

parse_range(Range) ->
	parse_range(binstr:split(Range, <<",">>), []).

parse_range([], Acc) ->
	Acc;
parse_range([H|T], Acc) ->
	case binstr:split(H, <<":">>) of
		[Start, End] ->
			parse_range(T, lists:seq(binary_to_integer(Start), binary_to_integer(End)) ++ Acc);
		[Number] ->
			parse_range(T, [binary_to_integer(Number) | Acc])
	end.


binary_to_integer(Bin) ->
	list_to_integer(binary_to_list(Bin)).
