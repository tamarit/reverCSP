-module(csp_process_interactive).

-export([start/1, start_from_track/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(FirstProcess) -> 
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	execute_csp({FirstExp, -1}, []),
	send_message2regprocess(printer,{info_graph,get_self()}),
	InfoGraph = 
		receive 
			{info_graph, InfoGraph_} ->
				% io:format("~p\n", [InfoGraph_]),
				InfoGraph_
		after 
			1000 -> 
				{{{0,0,0,now()},"",""},{[],[]}}
		end,
	send_message2regprocess(printer,stop),
	InfoGraph.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Process Execution (interactive)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute_csp({Exp, Parent}, Previous) ->
	Questions = get_questions(Exp, []),
	% io:get_line(standard_io, format("****State****\n~s\n****Options****\n~s\n************\n", [csp_expression_printer:csp2string(Exp), lists:join("\n", [ csp_expression_printer:csp2string(Q) || Q <- Questions])])),
	io:format("\n\nCurrent expression:\n~s\n\n", [csp_expression_printer:csp2string(Exp)]),
	case Questions of 
		[] ->
			ok;
		_ ->
			{Answer, NPrevious} = ask_questions(Questions, Previous),
			NExp = 
				case Previous of 
					[Answer | _] ->
						% In order to enable UNDO we need a way to remove from te trace and from the track.
						Answer;
					_ ->
						process_answer(Exp, Answer, Parent)
				end,
			% io:get_line(standard_io, format("****Answer****\n~s\n************\n", [csp_expression_printer:csp2string(Answer)])),
			execute_csp(NExp, NPrevious)
	end.

ask_questions(List, Previous) ->
	 {_, Lines, Ans, AnsDict} = 
	    lists:foldl(
	        fun build_question_option/2,
	        {1, [], [], dict:new()},
	        	[{E, csp_expression_printer:csp2string(E)} || E <- List] 
	        ++ 	[{t, "See current trace."}]),
	        % ++ 	case Previous of 
	        % 		[] -> 
	        % 			[];
	        % 		_ ->
	        % 			[{u, "Undo last choice."}]
	        % 	end),
    QuestionLines = 
        ["These are the available options: " | lists:reverse(Lines)]
    ++  ["What do you want to do?" 
         | ["[" ++ string:join(lists:reverse(Ans), "/") ++ "]: "]],
    Answer = 
        get_answer(
        	string:join(QuestionLines,"\n"), 
        	lists:seq(1, length(Ans))),
    case dict:fetch(Answer, AnsDict) of 
    	t ->
			send_message2regprocess(printer,{get_trace, get_self()}),
			InfoGraph = 
				receive 
					{trace, Trace} ->
						io:format("\n*********** Trace ************\n\n~s\n******************************\n",[Trace])
				end, 
			ask_questions(List, Previous);
		% u ->
		% 	{hd(Previous), tl(Previous)};
		Other ->
			{Other, [Other | Previous]}		
    end.
	% rand:seed(exs64),
	% Selected = rand:uniform(length(List)),
	% lists:nth(Selected, List).

build_question_option({O, Name}, {N, Lines, Answers, Dict}) ->
    NLines = 
        [format("~p .- ~s", [N, Name]) |Lines],
    {N + 1, NLines, [format("~p", [N]) | Answers], dict:store(N, O, Dict)};
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

% Para el Seq Comp las preguntas las sacamos del primer proceso. Cuando estemos procesando el segundo ya no habrá SC
% Cuando se llegue a ser skip el primer proceso entonces se quita. Igual debería de guardarse en un skip especial o algo así los nodos para saber a que tiene que unirse. i.e. process_answer({skip, SPAN}, _) -> SE DIBUJA SKIP y se devuelve {skip, SPAN, [nodo_skip]}.
% Cuando un paralslismo acaben los dos con {skip,_,_}, meter un {skip, SPAN, Nods_skip}.
% El SC cuando se encuntre que su primer proceso se evalue a esto, se unira a los acabados y desaparecerá
get_questions({prefix, SPAN1, Channels, Event, ProcessPrefixing, SPAN2}, Renamings) ->
	[{prefix,SPAN1,Channels,Event,ProcessPrefixing,SPAN2}];
get_questions({'|~|', PA, PB, SPAN}, Renamings) ->
	[{'|~|', PA, PB, SPAN}];
% El external choice se queda sempre que al processar les rames no cambien. Si cambien y el que s'ha llançat era un event (no tau o tick) aleshores llevem el external choice i deixem la rama que ha canviat.
get_questions({agent_call, SPAN, ProcessName, Arguments}, Renamings) ->
	[{agent_call, SPAN, ProcessName, Arguments}];
get_questions({'|||', PA, PB, SPAN}, Renamings) ->
	get_questions(PA, Renamings) ++ get_questions(PB, Renamings);
get_questions({'|||', PA, PB, _, _, SPAN}, Renamings) ->
	get_questions(PA, Renamings) ++ get_questions(PB, Renamings);
% get_questions({sharing, {closure, Events}, PA, PB, SPAN}, Renamings) ->
% 	% Descartar els no factibles (per sincronitzacio)
% 	% Juntar opcions quan syncronitzacio (contemplant totes les combinacions)
% 	% la resta fer append
% 	% Lo que tiene que sincronizarse podría enviarse cuando se decide aquí a un servidor. Luego cada vez que se dibuje un prefixing que se le pregunte a ese servidor si queire info de sincronización. Si es así entonces la envía. El único problema es como identificar quien es quien por ejemplo (P || P) al desplegase si es (P = a -> P) no distinguiremos entre las a's facilmente. 
% 	% Otra opción sería que process_answer devolviese también info de sincronización juno al nuevo proceso. Esta info se usará siempre que lleguemos a un paralelismo que sincronice en dicho evento.
% 	get_questions(PA, Renamings) ++ get_questions(PB, Renamings);
% get_questions({sharing, {closure, Events}, PA, PB, _, _, SPAN}, Renamings) ->
% 	get_questions(PA, Renamings) ++ get_questions(PB, Renamings);
% get_questions({procRenaming, ListRenamings, P, SPAN}, Renamings) ->
% 	get_questions(P, [ListRenamings | Renamings]);
get_questions({skip, SPAN}, Renamings) ->
	[];
get_questions({finished_skip, _, _}, Renamings) ->
	[].

process_answer(P = {prefix, SPAN1, Channels, Event, ProcessPrefixing, SPAN}, P, Parent) ->
	send_message2regprocess(
		printer,
		{print, Event, get_self()}),
	send_message2regprocess(
		printer,
		{create_graph, P, Parent, get_self()}),
	receive
		{printed, Event} -> 
			ok
	end,
	receive
		{created, NParent} ->
			process_answer(ProcessPrefixing, P, NParent)
	end;
process_answer(P = {'|~|', PA, PB, SPAN}, P, Parent) ->
	{Selected, _} = 
		ask_questions([PA, PB], []),
	Event = 
		list_to_atom("   tau -> Internal Choice. Branch: "
			++ csp_expression_printer:csp2string(Selected)),
	send_message2regprocess(
		printer,
		{print, Event, get_self()}),
	send_message2regprocess(
		printer,
		{create_graph, P, Parent, get_self()}),
	receive
		{printed, Event} -> 
			ok
	end,
	receive
		{created, NParent} ->
			{Selected, NParent}
	end;
process_answer(P = {agent_call, SPAN, ProcessName, Arguments}, P, Parent) ->
	send_message2regprocess(codeserver, {ask_code, ProcessName, Arguments, get_self()}),
	NCode = 
		receive
			{code_reply, Code} -> 
				Code
		end,
	Event = 
		list_to_atom("   tau -> Call to process "
			++ atom_to_list(ProcessName)
			++ printer:string_arguments(Arguments)),
	send_message2regprocess(
		printer,
		{print, Event, get_self()}),
	send_message2regprocess(
		printer,
		{create_graph, P, Parent, get_self()}),
	receive
		{printed, Event} -> 
			ok
	end,
	receive
		{created, NParent} ->
			{NCode, NParent}
	end;
process_answer(IL = {'|||', PA, PB, SPAN}, P, Parent) ->
	send_message2regprocess(
		printer,
		{create_graph, IL, Parent, get_self()}),
	NParent = 
		receive
			{created, NParent0} ->
				NParent0
		end,
	{NPA, ParentA} = 
		process_answer(PA, P, NParent),
	{NPB, ParentB} = 
		process_answer(PB, P, NParent),
	{{'|||', 
		NPA, 
		NPB, 
		ParentA,
		ParentB,
		SPAN}, 
	 NParent};
process_answer({'|||', PA, PB, ParentA, ParentB, SPAN}, P, Parent) ->
	{NPA, NParentA} = 
		process_answer(PA, P, ParentA),
	{NPB, NParentB} = 
		process_answer(PB, P, ParentB),
	{{'|||', 
		NPA, 
		NPB, 
		NParentA,
		NParentB,
		SPAN}, 
	 Parent};
% process_answer({sharing, {closure, Events}, PA, PB, SPAN}, P) ->
% 	{sharing, 
% 		{closure, Events}, 
% 		process_answer(PA, P), 
% 		process_answer(PB, P), 
% 		SPAN};
% process_answer({procRenaming, ListRenamings, Proc, SPAN}, P) ->
% 	{procRenaming, ListRenamings, process_answer(Proc, P), SPAN};
process_answer(P = {skip, SPAN}, _, Parent) ->
	Event = 
		'   tau (SKIP)',
	send_message2regprocess(
		printer, 
		{print, Event, get_self()}),
	send_message2regprocess(
		printer,
		{create_graph, P, Parent, get_self()}),
	receive
		{printed, Event} -> 
			ok
	end,
	receive
		{created, NParent} ->
			{{finished_skip, SPAN, NParent}, NParent}
	end;
process_answer(P, _, Parent) ->
	{P, Parent}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Process Execution (based on a track)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_from_track(FirstProcess, Track) -> 
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	execute_csp_from_track({FirstExp, -1}, Track, 0, get_max_vertex(Track) + 1),
	send_message2regprocess(printer,{info_graph,get_self()}),
	InfoGraph = 
		receive 
			{info_graph, InfoGraph_} ->
				% io:format("~p\n", [InfoGraph_]),
				InfoGraph_
		after 
			1000 -> 
				{{{0,0,0,now()},"",""},{[],[]}}
		end,
	send_message2regprocess(printer,stop),
	InfoGraph.

execute_csp_from_track({Exp, Parent}, Track, Current, Top) ->
	io:format(
		"\n\nCurrent expression:\n~s\n\n", 
		[csp_expression_printer:csp2string(Exp)]),
	{NExpParent, NCurrent} = 
		process_answer_from_track(Exp, Parent, Track, Current),
	case NCurrent of 
		Top ->
			ok;
		Current ->
			execute_csp_from_track(NExpParent, Track, Current + 1, Top);
		_ ->
			execute_csp_from_track(NExpParent, Track, NCurrent, Top)
	end.

process_answer_from_track(P = {prefix, SPAN1, Channels, Event, ProcessPrefixing, SPAN}, Parent, Track, Current) ->
	case same_span(SPAN1, Track, Current) of 
		true -> 
			send_message2regprocess(
				printer,
				{print, Event, get_self()}),
			send_message2regprocess(
				printer,
				{create_graph, P, Parent, get_self()}),
			receive
				{printed, Event} -> 
					ok
			end,
			receive
				{created, NParent} ->
					process_answer_from_track(ProcessPrefixing, NParent, Track, Current + 1)
			end;
		false ->
			{{P, Parent}, Current}
	end;
process_answer_from_track(P = {'|~|', PA, PB, SPAN}, Parent, Track, Current) ->
	case same_span(SPAN, Track, Current) of 
		true ->
			case digraph:out_neighbours(Track, Current) of 
				[] -> 
					{P, Current + 1};
				[V_CHILD] -> 
					{_,{_,SPAN_CHILD}} = 
						digraph:vertex(Track, V_CHILD),
					Selected = 
						case extract_span(PA) of 
							SPAN_CHILD -> 
								PA; 
							_ -> 
								PB 
						end,
					Event = 
						list_to_atom("   tau -> Internal Choice. Branch: "
							++ csp_expression_printer:csp2string(Selected)),
					send_message2regprocess(
						printer,
						{print, Event, get_self()}),
					send_message2regprocess(
						printer,
						{create_graph, P, Parent, get_self()}),
					receive
						{printed, Event} -> 
							ok
					end,
					receive
						{created, NParent} ->
							process_answer_from_track(Selected, NParent, Track, Current + 1)
					end
			end;
		false ->
			{{P, Parent}, Current}
	end;
process_answer_from_track(P = {agent_call, SPAN, ProcessName, Arguments}, Parent, Track, Current) ->
	case same_span(SPAN, Track, Current) of 
		true -> 
			send_message2regprocess(codeserver, {ask_code, ProcessName, Arguments, get_self()}),
			NCode = 
				receive
					{code_reply, Code} -> 
						Code
				end,
			Event = 
				list_to_atom("   tau -> Call to process "
					++ atom_to_list(ProcessName)
					++ printer:string_arguments(Arguments)),
			send_message2regprocess(
				printer,
				{print, Event, get_self()}),
			send_message2regprocess(
				printer,
				{create_graph, P, Parent, get_self()}),
			receive
				{printed, Event} -> 
					ok
			end,
			receive
				{created, NParent} ->
					process_answer_from_track(NCode, NParent, Track, Current + 1)
			end;
		false -> 
			{{P, Parent}, Current}
	end;
process_answer_from_track(IL = {'|||', PA, PB, SPAN}, Parent, Track, Current) ->
	case same_span(SPAN, Track, Current) of 
		true -> 
			send_message2regprocess(
				printer,
				{create_graph, IL, Parent, get_self()}),
			NParent = 
				receive
					{created, NParent0} ->
						NParent0
				end,
			{{NPA, ParentA}, CurrentA} =
				process_answer_from_track(PA, NParent, Track, Current + 1), 
			{{NPB, ParentB}, CurrentB} = 
				process_answer_from_track(PB, NParent, Track, Current + 1),
			{
				{{'|||', 
					NPA, 
					NPB, 
					ParentA,
					ParentB,
					SPAN}, 
				 NParent},
				max(CurrentA, CurrentB)
			};
		false -> 
			{{IL, Parent}, Current}
	end;
process_answer_from_track({'|||', PA, PB, ParentA, ParentB, SPAN}, Parent, Track, Current) ->
	{{NPA, NParentA}, CurrentA}  = 
		process_answer_from_track(PA, ParentA, Track, Current), 
	{{NPB, NParentB}, CurrentB} = 
		process_answer_from_track(PB, ParentB, Track, Current), 
	{
		{{'|||', 
			NPA, 
			NPB, 
			ParentA,
			ParentB,
			SPAN}, 
		 Parent},
		max(CurrentA, CurrentB)
	};
process_answer_from_track(P = {skip, SPAN}, Parent, Track, Current) ->
	case same_span(SPAN, Track, Current) of 
		true ->
			Event = 
				'   tau (SKIP)',
			send_message2regprocess(
				printer, 
				{print, Event, get_self()}),
			send_message2regprocess(
				printer,
				{create_graph, P, Parent, get_self()}),
			receive
				{printed, Event} -> 
					ok
			end,
			receive
				{created, NParent} ->
					{{{finished_skip, SPAN, NParent}, NParent}, Current + 1}
			end;
		false ->
			{{P, Parent}, Current}
	end;
process_answer_from_track(P = {finished_skip, SPAN, NodeSkip}, Parent, Track, Current) ->
	{{P, Parent}, Current}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Other functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

send_message2regprocess(Process,Message) ->
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

% search_root(G) ->
% 	hd([V || V <- digraph:vertices(G), in_degree(G, V) == 0]).

get_max_vertex(G) ->
	lists:max([V || V <- digraph:vertices(G)]).

extract_span({prefix, SPAN, _, _, _, _}) ->
	SPAN;
extract_span({'|~|', _, _, SPAN}) ->
	SPAN;
extract_span({agent_call, SPAN, _, _}) ->
	SPAN;
extract_span({'|||', _, _, SPAN}) ->
	SPAN;
extract_span({'|||', _, _, _, _, SPAN}) ->
	SPAN;
extract_span({skip, SPAN}) ->
	SPAN.

same_span(SPAN, Track, Current) ->
	try 
		{Current,{_,SPANTrack}} = 
		 	digraph:vertex(Track, Current),
		SPANTrack =:= SPAN
	catch 
		_:_ ->
			false
	end.
	
