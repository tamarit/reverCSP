-module(csp_process_interactive).

-export([
	start/1, start_from_track/2, 
	reverse_options/1, remove_from_track/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO
% - Undo
% - Padding for options when there is more than 10 options

start(FirstProcess) -> 
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	start_from_expr(FirstProcess, FirstExp).

start_from_expr(FirstProcess, FirstExp) -> 
	ResultExe = 
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
	case ResultExe of 
		finish -> 
			InfoGraph;
		reverse -> 
			{{_,_,_}, {NodesDigraph, EdgesDigraph}}
				= InfoGraph,
			% io:format("EdgesDigraph: ~p\n", [EdgesDigraph]),
			Digraph = 
				csp_tracker:build_digraph(NodesDigraph, EdgesDigraph),
			csp_tracker:print_from_digraph(Digraph, "current2", [], false),
			register_printer(),
			start_reverse_mode(FirstProcess, start_from_track(FirstProcess, Digraph))
	end.

start_reverse_mode(FirstProcess, {InfoTrack = {{_,_,Trace}, DigraphContent}, ResExp}) ->
	io:format(
		"\n\nCurrent expression:\n~s\n\n", 
		[csp_expression_printer:csp2string(ResExp)]),
	{NodesDigraph, EdgesDigraph} = 
		DigraphContent,
	Digraph = 
		csp_tracker:build_digraph(NodesDigraph, EdgesDigraph),
	PrintCurrentTrack = 
		fun () -> 
			csp_tracker:print_from_digraph(Digraph, "current", [], false)
		end,
	% csp_tracker:print_from_digraph(Digraph, "track_from_track", [], false),
	% io:format("\n*********** Trace from track ************\n\n~s\n******************************\n",[Trace]),
	ReverseOptions = 
		reverse_options(Digraph),
	io:format("Reverse options: ~w\n", [ReverseOptions]),
	% io:get_line(standard_io, "PRESS INTRO TO CONTINUE..."),
	case ReverseOptions of 
		[] ->
			io:format("The track is empty, so there is nothing to reverse.\n"),
			case 
				ask_questions(
					[{e, "Forward evaluation."}, {f, "Finish."}], 
					fun process_answer_reverse/3, 
					[])
			of 
				finish -> 
					digraph:delete(Digraph),
					InfoTrack;
				forward -> 
					register_printer(),
					start_from_track_continue_user(FirstProcess, Digraph)
			end;
		[_|_] ->
			ReverseOptionsReady = 
				prepare_questions_reverse(FirstProcess, ReverseOptions, Digraph),
			AdditionalOptions = 
				[
					{t, "See current trace."},
					{c, "Print current track."},
					{e, "Forward evaluation."},
					{f, "Finish."}
				],
			case ask_questions(
					ReverseOptionsReady ++ AdditionalOptions, 
					fun process_answer_reverse/3, 
					{Trace, PrintCurrentTrack}) 
			of 
				finish -> 
					digraph:delete(Digraph),
					InfoTrack;
				forward -> 
					register_printer(),
					start_from_track_continue_user(FirstProcess, Digraph);
				NEvalInfo -> 
					digraph:delete(Digraph),
					start_reverse_mode(FirstProcess, NEvalInfo)
			end
	end.

prepare_questions_reverse(FirstProcess, [H | T], G) ->
	NG = copy_digraph(G),
	Ns = 
		case is_list(H) of 
			true -> 
				H;
			false -> 
				[H]
		end,
	NodeStr = 
		string:join(
			lists:map(
				fun node2str/1, 
				[element(2, digraph:vertex(G, N)) 
				|| N <- Ns]),
			","),
	csp_process_interactive:remove_from_track(H, NG),
	register_printer(),
	Result = {_, ResExp} = 
		start_from_track(FirstProcess, NG),
	digraph:delete(NG),
	Printed = 
			NodeStr ++ "\n\t\\__ "  
		++ 	csp_expression_printer:csp2string(ResExp),
	[ 	{Result, Printed} 
	| 	prepare_questions_reverse(FirstProcess, T, G)];
prepare_questions_reverse(_, [], _) ->
	[].

process_answer_reverse(t, RC, {Trace,_}) ->
	io:format("\n*********** Trace ************\n\n~s\n******************************\n",[Trace]), 
	RC();
process_answer_reverse(c, RC, {_,PT}) ->
	PT(),
	io:format("Current track available at current.pdf\n"),
	RC();
process_answer_reverse(e, _, _) ->
	forward;
process_answer_reverse(f, _, _) ->
	finish;
process_answer_reverse(Other, _, _) ->
	Other.

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
			% TODO: Should ask before if the user wants to undo or reverse.
			io:format("This CSP expression cannot be further evaluated.\n"),
			ask_questions(
				[{t, "See current trace."}, {c, "Print current track."}, 
				 {r, "Reverse evaluation."}, {f, "Finish evaluation."}],
				fun process_answer_exe/3,
				[]);
		_ ->
			AdditionalOptions = 
				[
					{t, "See current trace."},
					{c, "Print current track."},
					{r, "Reverse evaluation."},
					{f, "Finish evaluation."}
				],
			case ask_questions(
					build_str_tuples(Questions) ++ AdditionalOptions, 
					fun process_answer_exe/3,
					[]) 
			of 
				finish ->
					finish;
				reverse ->
					reverse;
				Answer  ->
					io:format("Answer: ~p\n", [Answer]),
					{NExp, NNodes} = 
						process_answer(Exp, Answer, Parent),
					case Answer of 
						[_|_] -> 
							build_sync_edges(NNodes),
							% io:format("~p\n", [lists:seq(1, length(NNodes) - 1)]),
							[ begin
								send_message2regprocess(
									printer,
									{unprint_last, get_self()}),
								receive
									unprinted_last ->
										ok
								end
							 end
							|| _ <- lists:seq(1, length(NNodes) - 1)];
						_ ->
							ok
					end,
					execute_csp(NExp, [])
			end
	end.
% execute_csp(Par1, Par2) ->
% 	io:format("Par1: ~p\nPar2: ~p\n", [Par1, Par2]).

build_sync_edges([H|T]) ->
	[
		begin 
			send_message2regprocess(
				printer,
				{print_sync, H, E, get_self()}),
			receive
				{printed_sync, H, E} ->
					ok
			end
		end 
	|| E <- T],
	build_sync_edges(T);
build_sync_edges(_) ->
	ok.

process_answer_exe(t, RC, _) ->
	send_message2regprocess(printer,{get_trace, get_self()}),
	InfoGraph = 
		receive 
			{trace, Trace} ->
				io:format("\n*********** Trace ************\n\n~s\n******************************\n",[Trace])
		end, 
	RC();
process_answer_exe(c, RC, _) ->
	send_message2regprocess(printer,{info_graph_no_stop, get_self()}),
	InfoGraph = 
		receive 
			{info_graph, {{_,_,_}, {NodesDigraph, EdgesDigraph}}} ->
				Digraph = 
					csp_tracker:build_digraph(NodesDigraph, EdgesDigraph),
				csp_tracker:print_from_digraph(Digraph, "current", [], false),
				io:format("Current track available at current.pdf\n")
		end, 
	RC();
process_answer_exe(r, _, _) ->
	reverse;
process_answer_exe(f, _, _) ->
	finish;
process_answer_exe(Other, _, _) ->
	Other.

build_str_tuples(List) ->
	[{E, csp_expression_printer:csp2string(E)} || E <- List].

ask_questions(List, ProcessAnswer, Data) ->
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
        	[0 | lists:seq(1, length(Ans))]),
    ProcessAnswer(
    	dict:fetch(Answer, AnsDict), 
    	fun() -> ask_questions(List, ProcessAnswer, Data) end, 
    	Data).

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
get_questions({sharing, {closure, Events}, PA, PB, SPAN}, Renamings) ->
	Res = get_questions(PA, Renamings) ++ get_questions(PB, Renamings),
	% io:format("Res: ~p\n", [Res]),
	Res;
get_questions({sharing, {closure, Events}, PA, PB, _, _, SPAN}, Renamings) ->
	OptA = 
		get_questions(PA, Renamings),
	OptB = 
		get_questions(PB, Renamings),
	FunIsPref = 
		fun
			(Pref = {prefix, _, _, Event, _, _}, {NotPref, IsPref}) ->
				{NotPref, [{Event, Pref} | IsPref]};
			(Prefs = [{prefix, _, _, Event, _, _}|_], {NotPref, IsPref}) ->
				{NotPref, [{Event, Prefs} | IsPref]};
			(Other, {NotPref, IsPref}) -> 
				{[Other | NotPref], IsPref}
		end,
	{NotPrefA, IsPrefA} = 
		lists:foldl(FunIsPref, {[], []}, OptA),
	{NotPrefB, IsPrefB} = 
		lists:foldl(FunIsPref, {[], []}, OptB),
	NotPref = 
		NotPrefA ++ NotPrefB, 
	PrefNotSync = 
		[{Event, Pref} 
		 || {Event, Pref} <- (IsPrefA ++ IsPrefB), 
		 	not(lists:member(Event, Events))],
	PrefSyncA = 
		IsPrefA -- PrefNotSync,
	PrefSyncB = 
		IsPrefB -- PrefNotSync,
	PrefSync = 
		lists:foldl(
			fun({EventA, PrefA}, Acc) ->  
					Acc ++
						[case {PrefA, PrefB} of 
							{[_|_], [_|_]} -> 
								PrefA ++ PrefB;
							{[_|_], _} -> 
								[PrefB | PrefA];
							{_, [_|_]} -> 
								[PrefA | PrefB];
							_ -> 
								[PrefA, PrefB]
						 end
						 || {EventB, PrefB} <- PrefSyncB, EventA == EventB]  
			end,
			[],
			PrefSyncA),
	Opts = NotPref ++ [Pref || {_, Pref} <- PrefNotSync] ++ PrefSync,
	% io:format("Opts: ~p\n", [Opts]),
	Opts;
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

process_answer(P = {prefix, SPAN1, Channels, Event, ProcessPrefixing, SPAN}, L = [_|_], Parent) ->
	case lists:member(P, L) of 
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
					{NExp, NNodes} = 
						process_answer(ProcessPrefixing, P, NParent),
					{NExp, [(NParent - 1)| NNodes]}
			end;
		false -> 
			{{P, Parent}, []}
	end;
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
			{NExp, NNodes} = 
				process_answer(ProcessPrefixing, P, NParent),
			{NExp, [(NParent - 1) | NNodes]}
	end;
process_answer(P = {'|~|', PA, PB, SPAN}, P, Parent) ->
	Selected = 
		ask_questions(
			build_str_tuples([PA, PB]), 
			fun process_answer_exe/3, 
			[]),
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
			{{Selected, NParent}, []}
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
			{{NCode, NParent}, []}
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
	{{NPA, ParentA}, NNodesA} = 
		process_answer(PA, P, NParent),
	{{NPB, ParentB}, NNodesB} = 
		process_answer(PB, P, NParent),
	{
		{{'|||', 
			NPA, 
			NPB, 
			ParentA,
			ParentB,
			SPAN}, 
		 NParent},
		NNodesA ++ NNodesB
	};
process_answer({'|||', PA, PB, ParentA, ParentB, SPAN}, P, Parent) ->
	{{NPA, NParentA}, NNodesA} = 
		process_answer(PA, P, ParentA),
	{{NPB, NParentB}, NNodesB} = 
		process_answer(PB, P, ParentB),
	{
		{{'|||', 
			NPA, 
			NPB, 
			NParentA,
			NParentB,
			SPAN}, 
		 Parent},
		NNodesA ++ NNodesB
	 };
process_answer(IL = {sharing, {closure, Events}, PA, PB, SPAN}, P, Parent) ->
	send_message2regprocess(
		printer,
		{create_graph, IL, Parent, get_self()}),
	NParent = 
		receive
			{created, NParent0} ->
				NParent0
		end,
	{{NPA, ParentA}, NNodesA} = 
		process_answer(PA, P, NParent),
	{{NPB, ParentB}, NNodesB} = 
		process_answer(PB, P, NParent),
	{
		{{sharing, 
			{closure, Events}, 
			NPA, 
			NPB,
			ParentA,
			ParentB,
			SPAN},
		NParent},
		NNodesA ++ NNodesB
	};
process_answer({sharing, {closure, Events}, PA, PB, ParentA, ParentB, SPAN}, P, Parent) ->
	{{NPA, NParentA}, NNodesA} = 
		process_answer(PA, P, ParentA),
	{{NPB, NParentB}, NNodesB} = 
		process_answer(PB, P, ParentB),
	{
		{{sharing, 
			{closure, Events}, 
			NPA, 
			NPB,
			NParentA,
			NParentB,
			SPAN},
		Parent},
		NNodesA ++ NNodesB
	};
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
			{{{finished_skip, SPAN, NParent}, NParent}, []}
	end;
process_answer(P, Ans, Parent) ->
	{{P, Parent}, []}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Process Execution (based on a track)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_from_track_continue_user(FirstProcess, Track) -> 
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	case digraph:vertices(Track) of 
		[_|_] ->
			{NExp, NNodes} = 
				execute_csp_from_track(
					{FirstExp, -1}, 
					Track, 
					0, 
					get_max_vertex(Track) + 1),
			% io:format("FINAL NNodes1: ~w\n", [NNodes]),
			start_from_expr(FirstProcess, NExp);
		[] ->
			start_from_expr(FirstProcess, FirstExp)
	end.

start_from_track(FirstProcess, Track) -> 
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	case digraph:vertices(Track) of 
		[_|_] ->
			% io:format("INIT ~p\n", [get_max_vertex(Track) + 1]),
			{NExp, NNodes} = 
				execute_csp_from_track({FirstExp, -1}, Track, 0, get_max_vertex(Track) + 1),
			% io:format("FINAL NNodes2: ~w\n", [NNodes]),
			send_message2regprocess(printer,{info_graph,get_self()}),
			InfoGraph = 
				receive 
					{info_graph, InfoGraph_} ->
						% io:format("~p\n", [InfoGraph_]),
						InfoGraph_
				after 
					1000 -> 
						{{{{0, 0, 0, 0}, 0, []}, {[], []}}, FirstExp}
				end,
			send_message2regprocess(printer,stop),
			{InfoGraph, NExp};
		[] ->
			{{{{0, 0, 0, 0}, 0, []}, {[], []}}, FirstExp}
	end.

execute_csp_from_track({Exp, Parent}, Track, Current, Top) ->
	% io:format("Current: ~p\n", [Current]),
	{NExpParent = {NExp,_}, NCurrent, NNodes} = 
		process_answer_from_track(Exp, Parent, Track, Current),
	case NCurrent of 
		Top ->
			{NExp, NNodes};
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
					{Nexp, NCurrent, NNodes} = 
						process_answer_from_track(
							ProcessPrefixing, 
							NParent, 
							Track, 
							Current + 2),
					% io:format("Add ~w to NNodes for event ~p (~p)\n", [[(NParent - 1) | NNodes], Event, SPAN1]),
					{Nexp, NCurrent, [(NParent - 1) | NNodes]}
			end;
		false ->
			{{P, Parent}, Current, []}
	end;
process_answer_from_track(P = {'|~|', PA, PB, SPAN}, Parent, Track, Current) ->
	case same_span(SPAN, Track, Current) of 
		true ->
			case digraph:out_neighbours(Track, Current) of 
				[] -> 
					{{P, Parent}, Current + 1, []};
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
							process_answer_from_track(
								Selected, 
								NParent, 
								Track, 
								Current + 1)
					end
			end;
		false ->
			{{P, Parent}, Current, []}
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
			{{P, Parent}, Current, []}
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
			{{NPA, ParentA}, CurrentA, NNodesA} =
				process_answer_from_track(PA, NParent, Track, Current + 1), 
			{{NPB, ParentB}, CurrentB, NNodesB} = 
				process_answer_from_track(PB, NParent, Track, Current + 1),
			{
				{{'|||', 
					NPA, 
					NPB, 
					ParentA,
					ParentB,
					SPAN}, 
				 NParent},
				max(CurrentA, CurrentB),
				NNodesA ++ NNodesB
			};
		false -> 
			{{IL, Parent}, Current, []}
	end;
process_answer_from_track(P = {'|||', PA, PB, ParentA, ParentB, SPAN}, Parent, Track, Current) ->
	{{NPA, NParentA}, CurrentA, NNodesA}  = 
		process_answer_from_track(PA, ParentA, Track, Current), 
	{{NPB, NParentB}, CurrentB, NNodesB} = 
		process_answer_from_track(PB, ParentB, Track, Current), 
	{
		{{'|||', 
			NPA, 
			NPB, 
			NParentA,
			NParentB,
			SPAN}, 
		 Parent},
		max(CurrentA, CurrentB),
		NNodesA ++ NNodesB
	};
process_answer_from_track(IL = {sharing, {closure, Events}, PA, PB, SPAN}, Parent, Track, Current) ->
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
			{{NPA, ParentA}, CurrentA, NNodesA} =
				process_answer_from_track(PA, NParent, Track, Current + 1), 
			{{NPB, ParentB}, CurrentB, NNodesB} = 
				process_answer_from_track(PB, NParent, Track, Current + 1),
			% io:format("NNodesA_0: ~w, NNodesB_0 ~w\n", [NNodesA, NNodesB]),
			{
				{{sharing, 
					{closure, Events},
					NPA, 
					NPB, 
					ParentA,
					ParentB,
					SPAN}, 
				 NParent},
				max(CurrentA, CurrentB),
				NNodesA ++ NNodesB
			};
		false -> 
			{{IL, Parent}, Current, []}
	end;
process_answer_from_track(IL = {sharing, {closure, Events}, PA, PB, ParentA, ParentB, SPAN}, Parent, Track, Current) ->
	{{NPA, NParentA}, CurrentA, NNodesA}  = 
		process_answer_from_track(PA, ParentA, Track, Current), 
	SyncEventsFun = 
		fun(Nodes) -> 
			lists:usort(
				[begin 
					% io:format("~p\n", [digraph:vertex(Track, N)]),
					case digraph:vertex(Track, N) of 
						{Node, {NodeTerm,_}} ->  
							case lists:member(list_to_atom(NodeTerm), Events) of 
								true -> 
									Node;
								false -> 
									none 
							end;
						% TODO: Should not be happening
						false -> 
							none 
					end
				end 
				|| N <- Nodes])
		end,
	SyncEventsA = 
		[E || E <- SyncEventsFun(NNodesA), E /= none],
	CurrentB0 =
		case SyncEventsA of 
			[] -> 
				Current;
			_ -> 
				CurrentA
		end,
	{{NPB, NParentB}, CurrentB, NNodesB} = 
		process_answer_from_track(PB, ParentB, Track, CurrentB0), 
	SyncEventsB = 
		[E || E <- SyncEventsFun(NNodesB), E /= none],
	build_sync_edges(SyncEventsA ++ SyncEventsB),
	case length(SyncEventsA ++ SyncEventsB) > 0 of 
		true -> 
			[ begin
				send_message2regprocess(
					printer,
					{unprint_last_from, Events, get_self()}),
				receive
					unprinted_last ->
						ok
				end
			 end
			|| _ <- lists:seq(1, length(SyncEventsA ++ SyncEventsB) - 1)];
		false -> 
			ok
	end,
	% io:format("NNodesA: ~w, NNodesB ~w, Current: ~p\n", [NNodesA, NNodesB, max(CurrentA, CurrentB)]),
	{
		{{sharing,
			{closure, Events}, 
			NPA, 
			NPB, 
			NParentA,
			NParentB,
			SPAN}, 
		 Parent},
		max(CurrentA, CurrentB),
		NNodesA ++ NNodesB
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
					{{{finished_skip, SPAN, NParent}, NParent}, Current + 1, []}
			end;
		false ->
			{{P, Parent}, Current, []}
	end;
process_answer_from_track(P = {finished_skip, SPAN, NodeSkip}, Parent, Track, Current) ->
	{{P, Parent}, Current, []}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Extract reverse options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reverse_options(Track) ->
	reverse_options_list(get_leaves(Track), Track, []).
	
reverse_options_list([H | T], Track, Acc) ->
	% io:format("List: ~w, Acc: ~w\n", [[H | T], Acc]),
	{NT, NAcc} =
		case is_option(H, Track) of 
			true ->
				SyncNodes0 = 
					[begin
						% io:format("E: ~p, H:\n")
						{E, From, To, Type} = 
							digraph:edge(Track, E),
						SyncNode = 
							case From of 
								H -> 
									To; 
								_ -> 
								 	From 
							end,
						case {Type, SyncNode /= H} of 
							{"sync", true} -> 
								[From];
							_ -> 
								[] 
						end
					end
					|| E <- (digraph:in_edges(Track, H) ++ digraph:out_edges(Track, H))],	
				SyncNodes = 
					lists:usort([H | lists:concat(SyncNodes0)]),
				ReachableFromSyncNodes = 
					digraph_utils:reachable(SyncNodes, Track),
				% io:format("NT: ~w, NACC: ~w\n", [T -- SyncNodes, [ SyncNodes | Acc]]),
				{T -- ReachableFromSyncNodes, [ SyncNodes | Acc]};
			false ->
				InNeigbs = 
					digraph:in_neighbours(Track, H),
				{InNeigbs ++  T, Acc}
		end,
	reverse_options_list(NT, Track, NAcc);
reverse_options_list([], _, Acc) ->
	lists:reverse(Acc).

is_option(N, Track) ->
	try 
		{N, {Label, _}} = 
			digraph:vertex(Track, N), 
		NAcc = 
			case Label of 
				"SKIP" ->
					false;
				"STOP" ->
					false;
				_ ->
					lists:member(hd(Label), lists:seq($a,$z)) 
					or
					lists:member(hd(Label), lists:seq($A,$Z))
			end
	catch 
		_:_ ->
			false
	end.

remove_from_track(N, Track) ->
	Ns = 
		case is_list(N) of 
			true -> 
				N;
			false -> 
				[N]
		end,
	digraph:del_vertices(
		Track, 
		digraph_utils:reachable(Ns, Track)),
	ok.

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

get_leaves(G) ->
	[V || V <- digraph:vertices(G), digraph:out_degree(G, V) == 0].

% search_root(G) ->
% 	hd([V || V <- digraph:vertices(G), digraph:in_degree(G, V) == 0]).

get_max_vertex(G) ->
	% io:format("~p\n", [digraph:vertices(G)]),
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
		{Current, {_, SPANTrack}} = 
		 	digraph:vertex(Track, Current),
		SPANTrack =:= SPAN
	catch 
		_:_ ->
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
	     	register(printer, 
	        spawn(printer,loop,
	            [all, false]))
	end.