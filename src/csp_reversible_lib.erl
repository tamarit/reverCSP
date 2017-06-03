-module(csp_reversible_lib).

-export([
			csp2string/1,
			print_event/1,
			decide_eval_order/0,
			format/2,
			send_message2regprocess/2,
			get_self/0,
			get_leaves/1,
			get_max_vertex/1,
			% extract_span/1,
			same_span/5,
			copy_digraph/1,
			node2str/1,
			register_printer/0,
			print_trace/1,
			ask_questions/3,
			get_answer/2,
			build_sync_edges/1,
			build_str_tuples/1
		]).

csp2string({prefix, _, _, Event, ProcessPrefixing, _}) ->
	atom_to_list(Event) ++ " -> " ++ csp2string(ProcessPrefixing);
csp2string({'|~|', PA, PB, _}) ->
	brackets(csp2string(PA) ++ " |~| " ++ csp2string(PB));
% El external choice se queda sempre que al processar les rames no cambien. Si cambien y el que s'ha llançat era un event (no tau o tick) aleshores llevem el external choice i deixem la rama que ha canviat.
csp2string({agent_call, _, ProcessName, Arguments}) ->
	atom_to_list(ProcessName) ++ printer:string_arguments(Arguments);
csp2string({'|||', PA, PB, SPAN}) ->
	brackets(csp2string(PA) ++ " ||| " ++ csp2string(PB));
csp2string({'|||', PA, PB, _, _, SPAN}) ->
	brackets(csp2string(PA) ++ " ||| " ++ csp2string(PB));
csp2string({sharing, {closure, Events}, PA, PB, _}) ->
	brackets(csp2string(PA) ++ " [|{|" ++ events_to_str(Events) ++ "|}|] " ++ csp2string(PB));
csp2string({sharing, {closure, Events}, PA, PB, _, _, _}) ->
	brackets(csp2string(PA) ++ " [|{|" ++ events_to_str(Events) ++ "|}|] " ++ csp2string(PB));
csp2string({skip, SPAN}) ->
	"SKIP";
csp2string({';', PA, PB, _}) ->
	brackets(csp2string(PA))++ ";" ++ brackets(csp2string(PB));
csp2string({finished_skip, _, _}) ->
	"T";
csp2string(L = [_|_]) ->
	"\t" ++ string:join([csp2string(E) || E <- L], "\n\t");
csp2string(Other) ->
	io:format("Other: ~p\n", [Other]).

brackets(S) ->
	"(" ++ S ++ ")".

events_to_str(L) ->
	string:join([atom_to_list(E) || E <- L], ",").

print_event(Event0) -> 
	Event = 
		case is_list(Event0) of 
			true -> 
				io:format("~p\n", [Event0]),
				list_to_atom(Event0);
			false -> 
				Event0
		end,
	send_message2regprocess(
		printer,
		{print, Event, get_self()}),
	receive
		{printed, Event} -> 
			ok
	end.

decide_eval_order() -> 
	case rand:uniform(2) of 
		1 -> 
			reverse;
		2 -> 
			forward
	end.

format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

send_message2regprocess(Process, Message) ->
 	ProcessPid = whereis(Process),
 	case ProcessPid of 
 		undefined -> 
 			no_sent;
 		_ -> 
         	case is_process_alive(ProcessPid) of 
         		true -> 
			        ProcessPid!Message;
				false -> 
					no_sent
			end
	end.

get_self() ->
	catch self().

get_leaves(G) ->
	[V || V <- digraph:vertices(G), digraph:out_degree(G, V) == 0].

% search_root(G) ->
% 	hd([V || V <- digraph:vertices(G), digraph:in_degree(G, V) == 0]).

get_max_vertex(G) ->
	% io:format("~p\n", [digraph:vertices(G)]),
	lists:max([V || V <- digraph:vertices(G)]).

% extract_span({prefix, SPAN, _, _, _, _}) ->
% 	SPAN;
% extract_span({'|~|', _, _, SPAN}) ->
% 	SPAN;
% extract_span({agent_call, SPAN, _, _}) ->
% 	SPAN;
% extract_span({'|||', _, _, SPAN}) ->
% 	SPAN;
% extract_span({'|||', _, _, _, _, SPAN}) ->
% 	SPAN;
% extract_span({';', _, _, SPAN}) ->
% 	SPAN;
% extract_span({skip, SPAN}) ->
% 	SPAN.

same_span(SPAN, Track, Current, Parent, Dict) ->
	try 
		{Current, {_, SPANTrack}} = 
		 	digraph:vertex(Track, Current),
		case SPANTrack =:= SPAN of 
			false -> 
				false;
			true -> 
				Inputs = 
					lists:concat(
						[begin
							{E, From, To, Type} = 
								digraph:edge(Track, E),
							case Type of 
								"control" -> 
									[From];
								_ -> 
									[] 
							end
						end
						|| E <- digraph:in_edges(Track, Current)]),
				case Inputs of 
					[] -> 
						% io:format("\nEMPTY INPUTS\n"),
						true;
					[NodeParent] -> 
						ExpectedNodeParent = 
							hd([OldParent || {OldParent, NewParent} <- Dict, NewParent == Parent]),
						NodeParent == ExpectedNodeParent
				end
		end
	catch 
		_:_ ->
			% io:format("ERROR: NOT FOUND\n"),
			false
	end.
	
copy_digraph(G) ->
    digraph_utils:subgraph(G, digraph:vertices(G)).


node2str({Label, SPAN}) -> 
	{FL,FC,TL,TC} = printer:extractFromTo(SPAN,Label),
		Label
	++ " from (" ++ integer_to_list(FL) 
	++ "," ++ integer_to_list(FC) 
	++ ") to (" ++ integer_to_list(TL) 
	++ "," ++ integer_to_list(TC) 
	++ ")". 

register_printer() ->
	case lists:member(printer,registered()) of
	     true -> 
	     	ok;
	     false -> 
	     	Self = self(),
	     	register(
	     		printer, 
		        spawn(printer,loop,
		            [all, false])),
	     	wait_registered()
	end.

wait_registered() -> 
	case lists:member(printer,registered()) of
	     false -> 
	     	register_printer(),
	     	wait_registered();
	     true -> 
	     	ok
	end.

print_trace(Trace) -> 
	io:format(
		"\n*********** Trace ************\n\n~s\n******************************\n",
		[Trace]).

ask_questions(List, ProcessAnswer, Data) ->
	{FAnswer, FAnsDict} = 
		case lists:last(List) of 
			% To place finish option allways in the number 0
			{f, _} -> 
				 {_, Lines0, Ans0, AnsDict0} = 
				    lists:foldl(
				        fun build_question_option/2,
				        {1, [], [], dict:new()}, 
				        lists:droplast(List)),
				{_, Lines, Ans, AnsDict} =
					build_question_option(
						lists:last(List), 
						{0, Lines0, Ans0, AnsDict0}),
			    QuestionLines = 
			        ["These are the available options: " | lists:reverse(Lines)]
			    ++  ["What do you want to do?" 
			         | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
			    Answer = 
			        get_answer(
			        	string:join(QuestionLines,"\n"), 
			        	[0 | lists:seq(1, length(Ans) - 1)]),
			    {Answer, AnsDict};
			% When there is no finish
			_ -> 
				 {_, Lines, Ans, AnsDict} = 
				    lists:foldl(
				        fun build_question_option/2,
				        {1, [], [], dict:new()}, 
				        List),
			    QuestionLines = 
			        ["These are the available options: " | lists:reverse(Lines)]
			    ++  ["What do you want to do?" 
			         | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
			    Answer = 
			        get_answer(
			        	string:join(QuestionLines,"\n"), 
			        	lists:seq(1, length(Ans))),
			    {Answer, AnsDict}
		end,
    ProcessAnswer(
    	dict:fetch(FAnswer, FAnsDict), 
    	fun() -> ask_questions(List, ProcessAnswer, Data) end, 
    	Data).

build_question_option({O, Name}, {N, Lines, Answers, Dict}) ->
    NLines = 
        [csp_reversible_lib:format("~p .- ~s", [N, Name]) |Lines],
    {
    	N + 1, 
    	NLines, 
    	[csp_reversible_lib:format("~p", [N]) | Answers], 
    	dict:store(N, O, Dict)
    };
build_question_option(Other, {N, Lines, Answers, Dict}) ->
    build_question_option({Other, Other}, {N, Lines, Answers, Dict}).


get_answer(Message, Answers) ->
   [_|Answer] = 
     	lists:reverse(io:get_line(standard_io, Message)),
   AtomAnswer = 
        try 
            list_to_integer(lists:reverse(Answer))
        catch 
            _:_ ->
                try 
                    list_to_atom(lists:reverse(Answer))
                catch 
                    _:_ -> get_answer(Message,Answers)
                end
        end,
   case lists:member(AtomAnswer, Answers) of
        true -> AtomAnswer;
        false -> get_answer(Message, Answers)
   end.

build_sync_edges([H|T]) ->
	[
		begin 
			csp_reversible_lib:send_message2regprocess(
				printer,
				{print_sync, H, E, csp_reversible_lib:get_self()}),
			receive
				{printed_sync, H, E} ->
					ok
			end
		end 
	|| E <- T],
	build_sync_edges(T);
build_sync_edges(_) ->
	ok.


build_str_tuples(List) ->
	[{E, csp_reversible_lib:csp2string(E)} || E <- List].
