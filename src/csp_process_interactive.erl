-module(csp_process_interactive).

-export([
	start/1, start_from_track/2, 
	reverse_options/1, remove_from_track/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start(FirstProcess) -> 
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	start_from_expr(FirstProcess, FirstExp, []).

start_from_expr(FirstProcess, FirstExp, Previous) ->
	register_printer(),
	ResultExe = 
		execute_csp({FirstExp, -1}, Previous),
	send_message2regprocess(printer,{info_graph_no_stop,get_self()}),
	InfoGraph = 
		receive 
			{info_graph, InfoGraph_} ->
				% io:format("~p\n", [InfoGraph_]),
				InfoGraph_
		after 
			1000 -> 
				{{{0,0,0,now()},"",""},{[],[]}}
		end,
		send_message2regprocess(printer,{stop, self()}),
	receive 
		stopped -> 
			ok
	end,
	case ResultExe of 
		finish -> 
			InfoGraph;
		{reverse, Pending} -> 
			{{_,_,_}, {NodesDigraph, EdgesDigraph}}
				= InfoGraph,
			% io:format("EdgesDigraph: ~p\n", [EdgesDigraph]),
			Digraph = 
				csp_tracker:build_digraph(NodesDigraph, EdgesDigraph),
			% csp_tracker:print_from_digraph(Digraph, "current1_1", [], false),
			EvalInfo = 
				start_from_track(FirstProcess, Digraph),
			start_reverse_mode(
				FirstProcess, 
				EvalInfo,
				Pending);
		{forward_reverse, Steps} -> 
			random_forward_reverse_action_from_forward(
				FirstProcess, 
				InfoGraph, 
				Steps) 
	end.

random_forward_reverse_action_from_forward(FirstProcess, InfoGraph, Steps) -> 
	EvalOrder = 
		decide_eval_order(),
	% io:format("Selected ~p mode.", [EvalOrder]),
	{{_,_,_}, {NodesDigraph, EdgesDigraph}}
		= InfoGraph,
	Digraph = 
		csp_tracker:build_digraph(NodesDigraph, EdgesDigraph),
	case EvalOrder of 
		reverse -> 
			EvalInfo = 
				start_from_track(FirstProcess, Digraph),
			start_reverse_mode(
				FirstProcess, 
				EvalInfo,
				{forward_reverse, Steps});
		forward -> 
			start_from_track_continue_user(
				FirstProcess, 
				Digraph, 
				{forward_reverse, Steps})
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Reverse Execution (interactive)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_reverse_mode(FirstProcess, EvalInfo = {InfoTrack = {{_,_,Trace}, DigraphContent}, ResExp}, Previous) ->
	% io:format("Previous: ~p\n", [Previous]),
	{NodesDigraph, EdgesDigraph} = 
		DigraphContent,
	% io:format("DigraphContent: ~p\n", [DigraphContent]),
	Digraph = 
		csp_tracker:build_digraph(NodesDigraph, EdgesDigraph),
	FunPrintCurrentTrack = 
		fun () -> 
			csp_tracker:print_from_digraph(Digraph, "current", [], false)
		end,
	% csp_tracker:print_from_digraph(Digraph, "current2", [], false),
	% process_answer_reverse(t, fun() -> ok end, {Trace,0}),
	% csp_tracker:print_from_digraph(Digraph, "track_from_track", [], false),
	% io:format("\n*********** Trace from track ************\n\n~s\n******************************\n",[Trace]),
	ReverseOptions = 
		reverse_options(Digraph),
	% error("algo\n"),
	% io:format("Reverse options: ~w\n", [ReverseOptions]),
	% io:get_line(standard_io, "PRESS INTRO TO CONTINUE..."),
	case Previous of 
		{forward_reverse, N} when is_integer(N) andalso N > 0 -> 
			case ReverseOptions of 
				[] -> 
					start_from_track_continue_user(
						FirstProcess, 
						Digraph, 
						{forward_reverse, N});
				_ -> 
					io:format(
						"\n\nCurrent expression:\n~s\n\n", 
						[csp_expression_printer:csp2string(ResExp)]),
					ReverseOptionsReady = 
						prepare_questions_reverse(FirstProcess, ReverseOptions, Digraph),
					{NEvalInfo, Printed} = 
						lists:nth(
							rand:uniform(length(ReverseOptionsReady)), 
							ReverseOptionsReady),
					io:format("\nReverse evaluation. Randomly selected:\n~s\n", [Printed]),
					digraph:delete(Digraph),
					case decide_eval_order() of 
						reverse -> 
							start_reverse_mode(
								FirstProcess, 
								NEvalInfo, 
								{forward_reverse, N - 1});
						forward -> 
							{{_, NDigraphContent}, _} = 
								NEvalInfo,
							{NNodesDigraph, NEdgesDigraph} = 
								NDigraphContent,
							% io:format("NDigraphContent: ~p\n", [NDigraphContent]),
							NDigraph = 
								csp_tracker:build_digraph(NNodesDigraph, NEdgesDigraph),
							start_from_track_continue_user(
								FirstProcess, 
								NDigraph, 
								{forward_reverse, N - 1})
					end		
			end;
		_ -> 
			io:format(
				"\n\nCurrent expression:\n~s\n\n", 
				[csp_expression_printer:csp2string(ResExp)]),
			case ReverseOptions of 
				[] ->
					io:format("The track is empty, so there is nothing to reverse.\n"),
					case 
						ask_questions(
							[
								% Track and trace are empty so these options has no sense here.
								% {t, "See current trace."},
								% {c, "Print current track."},
								{e, "Forward evaluation."}, 
								{f, "Finish evaluation."}
							], 
							fun process_answer_reverse/3, 
							[])
					of 
						finish -> 
							digraph:delete(Digraph),
							InfoTrack;
						forward -> 
							start_from_track_continue_user(FirstProcess, Digraph, [])
					end;
				[_|_] ->
					ReverseOptionsReady = 
						prepare_questions_reverse(FirstProcess, ReverseOptions, Digraph),
					AdditionalOptions = 
						[
							{rr, "Random choice."},
							{rfr, "Random forward-reverse choice."},
							{t, "See current trace."},
							{c, "Print current track."},
							{e, "Forward evaluation."},
							{f, "Finish evaluation."}
						],
					case Previous of 
						0 -> 
							digraph:delete(Digraph),
							start_reverse_mode(FirstProcess, {InfoTrack, ResExp}, []);
						N when is_integer(N) -> 
							start_reverse_mode_random(FirstProcess, ReverseOptionsReady, Digraph, Previous);
						_ -> 
							case ask_questions(
									ReverseOptionsReady ++ AdditionalOptions, 
									fun process_answer_reverse/3, 
									{Trace, FunPrintCurrentTrack}) 
							of 
								finish -> 
									digraph:delete(Digraph),
									InfoTrack;
								forward -> 
									start_from_track_continue_user(FirstProcess, Digraph, []);
								random_reverse ->
									Steps = 
								        get_answer(
								        	"\nHow many steps?\n[1..1000]: ", 
								        	lists:seq(1, 1000)),
									start_reverse_mode_random(FirstProcess, ReverseOptionsReady, Digraph, Steps);				
								random_forward_reverse -> 
									Steps = 
								        get_answer(
								        	"\nHow many steps?\n[1..1000]: ", 
								        	lists:seq(1, 1000)),
									case decide_eval_order() of 
										reverse -> 
											digraph:delete(Digraph),
											start_reverse_mode(
												FirstProcess, 
												EvalInfo, 
												{forward_reverse, Steps});
										forward -> 
											start_from_track_continue_user(
												FirstProcess, 
												Digraph, 
												{forward_reverse, Steps})
									end;
								NEvalInfo -> 
									digraph:delete(Digraph),
									start_reverse_mode(FirstProcess, NEvalInfo, Previous)
							end
					end
			end
	end.

start_reverse_mode_random(FirstProcess, Options, Digraph, Steps) -> 
	{NEvalInfo, Printed} = 
		lists:nth(rand:uniform(length(Options)), Options),
	io:format("\nRandomly selected:\n~s\n", [Printed]),
	digraph:delete(Digraph),
	start_reverse_mode(FirstProcess, NEvalInfo, Steps - 1).

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
	% io:format("H: ~w\n", [H]),
	% csp_tracker:print_from_digraph(NG, "current" ++ integer_to_list(length([H | T])), [], false),
	Result = {_, ResExp} = 
		start_from_track(FirstProcess, NG),
	% io:format("ResExp: ~p\n", [ResExp]),
	Printed = 
			NodeStr ++ "\n\t\\__ "  
		++ 	csp_expression_printer:csp2string(ResExp),
	[ 	{Result, Printed} 
	| 	prepare_questions_reverse(FirstProcess, T, G)];
prepare_questions_reverse(_, [], _) ->
	[].

process_answer_reverse(t, RC, {Trace,_}) ->
	io:format(
		"\n*********** Trace ************\n\n~s\n******************************\n",
		[Trace]), 
	RC();
process_answer_reverse(c, RC, {_,PT}) ->
	PT(),
	io:format("Current track available at current.pdf\n"),
	RC();
process_answer_reverse(e, _, _) ->
	forward;
process_answer_reverse(f, _, _) ->
	finish;
process_answer_reverse(rr, _, _) ->
	random_reverse;
process_answer_reverse(rfr, _, _) ->
	random_forward_reverse;
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
	case Previous of 
		{forward_reverse, N} when is_integer(N) andalso N > 0 -> 
			case Questions of 
				[] -> 
					{reverse, {forward_reverse, N}};
				_ -> 
					io:format(
						"\n\nCurrent expression:\n~s\n\n", 
						[csp_expression_printer:csp2string(Exp)]),
					Answer = 
						lists:nth(rand:uniform(length(Questions)), Questions),
					io:format("\nForward evaluation. Randomly selected:\n~s\n", [csp_expression_printer:csp2string(Answer)]),
					put(random_choice, true),
					csp_process_option_processing(Exp, Answer, Parent),
					put(random_choice, false),
					% io:format("PASA1\n"),
					{forward_reverse, N - 1}
			end;
		_ -> 
			io:format(
				"\n\nCurrent expression:\n~s\n\n", 
				[csp_expression_printer:csp2string(Exp)]),
			case Questions of 
				[] ->
					% TODO: Should ask before if the user wants to undo or reverse.
					io:format("This CSP expression cannot be further evaluated.\n"),
					ask_questions(
						[
							{t, "See current trace."}, 
							{c, "Print current track."}, 
						 	{r, "Reverse evaluation."}, 
						 	{f, "Finish evaluation."}
						],
						fun process_answer_exe/3,
						[]);
				_ ->
					case Previous of 
						0 -> 
							execute_csp({Exp, Parent}, []);
						N when is_integer(N) -> 
							execute_csp_random({Exp, Parent}, Questions, Previous);
						_ -> 
							AdditionalOptions = 
								[
									{rf, "Random choice."},
									{rfr, "Random forward-reverse choice."},
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
								{reverse, []} ->
									{reverse, []};
								random_forward -> 
									Steps = 
								        get_answer(
								        	"\nHow many steps?\n[1..1000]: ", 
								        	lists:seq(1, 1000)),
									execute_csp_random({Exp, Parent}, Questions, Steps);
								random_forward_reverse -> 
									Steps = 
								        get_answer(
								        	"\nHow many steps?\n[1..1000]: ", 
								        	lists:seq(1, 1000)),
									{forward_reverse, Steps};
								Answer  ->
									% io:format("Answer: ~p\n", [Answer]),
									NExp = 
										csp_process_option_processing(
											Exp, 
											Answer, 
											Parent),
									execute_csp(NExp, Previous)
							end
					end
			end
	end.

csp_process_option_processing(Exp, Answer, Parent) ->
	put(in_parallelism, false),
	{NExp, NNodes} = 
		process_answer(Exp, Answer, Parent),
	case Answer of 
		[_|_] -> 
			build_sync_edges([N || {N, _} <- NNodes]);
			% io:format("~p\n", [lists:seq(1, length(NNodes) - 1)]),
			% [ begin
			% 	send_message2regprocess(
			% 		printer,
			% 		{unprint_last, get_self()}),
			% 	receive
			% 		unprinted_last ->
			% 			ok
			% 	end
			%  end
			% || _ <- lists:seq(1, length(NNodes) - 1)];
		_ ->
			ok
	end,
	NExp.

execute_csp_random({Exp, Parent}, Options, Steps) -> 
	Answer = 
		lists:nth(rand:uniform(length(Options)), Options),
	io:format(
		"\nRandomly selected:\n~s\n", 
		[csp_expression_printer:csp2string(Answer)]),
	put(random_choice, true),
	NExp = 
		csp_process_option_processing(Exp, Answer, Parent),
	put(random_choice, false),
	execute_csp(NExp, Steps - 1).

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
	{reverse, []};
process_answer_exe(f, _, _) ->
	finish;
process_answer_exe(rf, _, _) ->
	random_forward;
process_answer_exe(rfr, _, _) ->
	random_forward_reverse;
process_answer_exe(Other, _, _) ->
	Other.

build_str_tuples(List) ->
	[{E, csp_expression_printer:csp2string(E)} || E <- List].

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
get_questions({';', PA, _, _}, Renamings) -> 
	get_questions(PA, Renamings);
get_questions({skip, SPAN}, Renamings) ->
	[];
get_questions({finished_skip, _, _}, Renamings) ->
	[].

process_answer(P = {prefix, SPAN1, Channels, Event, ProcessPrefixing, SPAN}, L = [_|_], Parent) ->
	case lists:member(P, L) of 
		true ->
			case get(in_parallelism) of 
				false -> 
					print_event(Event);
				_ -> 
					ok
			end,
			send_message2regprocess(
				printer,
				{create_graph, P, Parent, get_self()}),
			receive
				{created, NParent} ->
					{NExp, NNodes} = 
						process_answer(ProcessPrefixing, P, NParent),
					{NExp, NNodes ++ [{(NParent - 1), Event}]}
			end;
		false -> 
			{{P, Parent}, []}
	end;
process_answer(P = {prefix, SPAN1, Channels, Event, ProcessPrefixing, SPAN}, P, Parent) ->
	case get(in_parallelism) of 
		false -> 
			print_event(Event);
		_ -> 
			ok
	end,
	send_message2regprocess(
		printer,
		{create_graph, P, Parent, get_self()}),
	receive
		{created, NParent} ->
			{NExp, NNodes} = 
				process_answer(ProcessPrefixing, P, NParent),
			{NExp, NNodes ++ [{(NParent - 1), Event}] }
	end;
process_answer(P = {'|~|', PA, PB, SPAN}, P, Parent) ->
	Selected = 
		case get(random_choice) of 
			true -> 
				lists:nth(rand:uniform(2), [PA, PB]);
			_ ->
				ask_questions(
					build_str_tuples([PA, PB]), 
					fun process_answer_exe/3, 
					[])
		end,
	SelectedAtom = 
		case Selected of 
			PA -> 
				left;
			PB -> 
				right
		end,
	Event = 
		list_to_atom("   tau -> Internal Choice. Branch: "
			++ csp_expression_printer:csp2string(Selected)),
	print_event(Event),
	send_message2regprocess(
		printer,
		{create_graph, {'|~|', PA, PB, SelectedAtom, SPAN}, Parent, get_self()}),
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
	print_event(Event),
	send_message2regprocess(
		printer,
		{create_graph, P, Parent, get_self()}),
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
	process_answer_interleaving(
		{PA, NParent}, 
		{PB, NParent}, 
		P, 
		NParent,
		SPAN);
process_answer({'|||', PA, PB, ParentA, ParentB, SPAN}, P, Parent) ->
	process_answer_interleaving(
		{PA, ParentA}, 
		{PB, ParentB}, 
		P, 
		Parent,
		SPAN);
process_answer(IL = {sharing, {closure, Events}, PA, PB, SPAN}, P, Parent) ->
	send_message2regprocess(
		printer,
		{create_graph, IL, Parent, get_self()}),
	NParent = 
		receive
			{created, NParent0} ->
				NParent0
		end,
	process_answer_sharing(
		{PA, NParent}, 
		{PB, NParent}, 
		P, 
		NParent,
		SPAN,
		Events);
process_answer({sharing, {closure, Events}, PA, PB, ParentA, ParentB, SPAN}, P, Parent) ->
	process_answer_sharing(
		{PA, ParentA}, 
		{PB, ParentB}, 
		P, 
		Parent,
		SPAN,
		Events);
% process_answer({procRenaming, ListRenamings, Proc, SPAN}, P) ->
% 	{procRenaming, ListRenamings, process_answer(Proc, P), SPAN};
process_answer({';', PA, PB, SPAN}, P, Parent) ->
	{{NPA, NParentA}, NNodesA} = 
		process_answer(PA, P, Parent),
	NSC_NParent = 
		case NPA of 
			{finished_skip, _, NodesSkipA} ->
				send_message2regprocess(
					printer, 
					{create_graph, 
						{';', NodesSkipA, SPAN}, -1, get_self()}),
				receive
					{created, NParent0} ->
						{PB, NParent0}
				end;
			_ -> 
				{{';', NPA, PB, SPAN}, NParentA}
		end,
	{NSC_NParent, NNodesA};
process_answer(P = {skip, SPAN}, _, Parent) ->
	Event = 
		'   tau (SKIP)',
	print_event(Event),
	send_message2regprocess(
		printer,
		{create_graph, P, Parent, get_self()}),
	receive
		{created, NParent} ->
			{{{finished_skip, SPAN, [NParent]}, NParent}, []}
	end;
process_answer(P, Ans, Parent) ->
	{{P, Parent}, []}.

process_answer_interleaving({PA, ParentA}, {PB, ParentB}, P, Parent, SPAN) -> 
	{{NPA, NParentA}, NNodesA} = 
		process_answer(PA, P, ParentA),
	{{NPB, NParentB}, NNodesB} =
		case {not(is_list(P)), NPA /= PA} of 
			{true, true} ->
				{{PB, ParentB}, []};
			_ ->   
				process_answer(PB, P, ParentB)
		end,
	NProcess = 
		case {NPA, NPB} of 
			{{finished_skip, _, NodesSkipA}, {finished_skip, _, NodesSkipB}} -> 
				{finished_skip, SPAN, NodesSkipA ++ NodesSkipB};
			_ -> 
				{'|||', 
					NPA, 
					NPB, 
					NParentA,
					NParentB,
					SPAN}
		end,
	{
		{NProcess, Parent}, 
		NNodesA ++ NNodesB
	}.

process_answer_sharing({PA, ParentA}, {PB, ParentB}, P, Parent, SPAN, Events) -> 
	IsInParallel = 
		get(in_parallelism),
	put(in_parallelism, [true | IsInParallel]),
	{{NPA, NParentA}, NNodesA} = 
		process_answer(PA, P, ParentA),
	{{NPB, NParentB}, NNodesB} =
		case {not(is_list(P)), NPA /= PA} of 
			{true, true} ->
				{{PB, ParentB}, []};
			_ ->   
				process_answer(PB, P, ParentB)
		end,
	put(in_parallelism, IsInParallel),
	case IsInParallel of 
		false -> 
			case {NPA /= PA, NPB /= PB} of 
				{true, true} ->  
					% io:format("A: ~p\nB: ~p\n", [{PA, NPA}, {PB, NPB}]),
					% io:format("~p\n", [NNodesA ++ NNodesB]),
					EventToPrint = 
						element(2, hd(NNodesA ++ NNodesB)),
					print_event(EventToPrint);
				_ -> 
					EventsToPrint = 
						[E || {_,E} <- lists:sort(NNodesA ++ NNodesB)],
					[print_event(Event) || Event <- EventsToPrint]
			end;
		_ -> 
			ok
	end,
	NProcess = 
		case {NPA, NPB} of 
			{{finished_skip, _, NodesSkipA}, {finished_skip, _, NodesSkipB}} -> 
				{finished_skip, SPAN, NodesSkipA ++ NodesSkipB};
			_ -> 
				{sharing, 
					{closure, Events}, 
					NPA, 
					NPB,
					NParentA,
					NParentB,
					SPAN}
		end,
	{
		{NProcess, Parent}, 
		NNodesA ++ NNodesB
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Process Execution (based on a track)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_from_track_continue_user(FirstProcess, Track, Previous) -> 
	% io:format("ENTRA1\n"),
	register_printer(),
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	try digraph:vertices(Track) of 
		[_|_] ->
			{NExp, NNodes} = 
				execute_csp_from_track(
					{FirstExp, -1}, 
					Track, 
					0, 
					get_max_vertex(Track) + 1,
					[]),
			% io:format("FINAL NNodes1: ~w\n", [NNodes]),
			digraph:delete(Track),
			start_from_expr(FirstProcess, NExp, Previous);
		[] ->
			digraph:delete(Track),
			start_from_expr(FirstProcess, FirstExp, Previous)
	catch 
		_:_ -> 
			digraph:delete(Track),
			start_from_expr(FirstProcess, FirstExp, Previous)
	end.

start_from_track(FirstProcess, Track) -> 
	% io:format("ENTRA2\n"),
	register_printer(),
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	NState = 
		case digraph:vertices(Track) of 
			[_|_] ->
				% io:format("INIT ~p\n", [get_max_vertex(Track) + 1]),
				{NExp, NNodes} = 
					execute_csp_from_track({FirstExp, -1}, Track, 0, get_max_vertex(Track) + 1, []),
				% io:format("FINAL NNodes2: ~w\n", [NNodes]),
				send_message2regprocess(printer,{info_graph_no_stop,get_self()}),
				InfoGraph = 
					receive 
						{info_graph, InfoGraph_} ->
							% io:format("~p\n", [InfoGraph_]),
							InfoGraph_
					after 
						1000 -> 
							{{{{0, 0, 0, 0}, 0, []}, {[], []}}, FirstExp}
					end,
				{InfoGraph, NExp};
			[] ->
				{{{{0, 0, 0, 0}, 0, []}, {[], []}}, FirstExp}
		end,
	digraph:delete(Track),
	send_message2regprocess(printer, {stop, self()}),
	receive 
		stopped -> 
			ok
	end,
	NState.

execute_csp_from_track({Exp, Parent}, Track, Current, Top, Dict) ->
	% io:format("Current: ~p\n", [Current]),
	% io:format("\n++++++\nExp: ~s\n++++++\n", [csp_expression_printer:csp2string(Exp)]),
	put(top, Top),
	put(in_parallelism, false),
	{NExpParent = {NExp,_}, NCurrent, NNodes, NDict} = 
		process_answer_from_track(Exp, Parent, Track, Dict, Current),
	% io:format("NCurrent: ~p\n", [NCurrent]),
	% io:format("\n++++++\nNExp: ~s\n++++++\n", [csp_expression_printer:csp2string(NExp)]),
	case NCurrent of 
		Top ->
			% io:format("NDict: ~p\n", [NDict]),
			{NExp, NNodes};
		Current ->
			execute_csp_from_track(NExpParent, Track, Current + 1, Top, NDict);
		_ ->
			execute_csp_from_track(NExpParent, Track, NCurrent, Top, NDict)
	end.

process_answer_from_track(P = {prefix, SPAN1, Channels, Event, ProcessPrefixing, SPAN}, Parent, Track, Dict, Current) ->
	% io:format("Entra: ~p\n", [{Current, P}]),
	% case (same_span(SPAN1, Track, Current) or same_span(SPAN, Track, Current)) of 
	case (same_span(SPAN1, Track, Current, Parent, Dict)) of 
		true -> 
			% io:format("The span are the same: ~p\n", [{Current, P}]),
			case get(in_parallelism) of 
				false -> 
					print_event(Event);
				_ -> 
					ok
			end,
			send_message2regprocess(
				printer,
				{create_graph, P, Parent, get_self()}),
			receive
				{created, NParent} ->
					{Nexp, NCurrent, NNodes, NDict} = 
						process_answer_from_track(
							ProcessPrefixing, 
							NParent, 
							Track,
							[{Current, (NParent - 1)}, {Current + 1, NParent}  | Dict],
							Current + 2),
					{Nexp, NCurrent, NNodes ++ [{Current, (NParent - 1), Event}], NDict}
					% {NCurrent, CurrentNode} = 
					% 	% case same_span(SPAN1, Track, Current) of 
					% 	% 	true -> 
					% 			{Current + 2, Current}, %;
					% 	% 	false -> 
					% 	% 		{Current + 1, Current - 1}
					% 	% end,
					% NNodes = 
					% 	[{CurrentNode, (NParent - 1), Event}],
					% {{ProcessPrefixing, NParent}, NCurrent, NNodes, [{CurrentNode, (NParent - 1)}, {CurrentNode + 1, NParent}  | Dict]}
			end;
		false ->
			% io:format("The span are not the same: ~p\n", [{Current, P}]),
			{{P, Parent}, Current, [], Dict}
	end;
process_answer_from_track(P = {'|~|', PA, PB, SPAN}, Parent, Track, Dict, Current) ->
	case same_span(SPAN, Track, Current, Parent, Dict) of 
		true ->
			% case digraph:out_neighbours(Track, Current) of 
			% 	[] -> 
			% 		{{P, Parent}, Current + 1, []};
			% 	[V_CHILD] -> 
			% 		{_,{_,SPAN_CHILD}} = 
			% 			digraph:vertex(Track, V_CHILD),
			% 		Selected = 
			% 			case extract_span(PA) of 
			% 				SPAN_CHILD -> 
			% 					PA; 
			% 				_ -> 
			% 					PB 
			% 			end,
					{Current, {[$|,$~,$|,$. | SelectedStr], _}} = 
		 				digraph:vertex(Track, Current),
		 			Selected = 
			 			case SelectedStr of 
			 				"left" -> 
			 					PA;
			 				"right" -> 
			 					PB
			 			end,
					Event = 
						list_to_atom("   tau -> Internal Choice. Branch: "
							++ csp_expression_printer:csp2string(Selected)),
					case get(in_parallelism) of 
						false -> 
							print_event(Event);
						_ -> 
							ok
					end,
					send_message2regprocess(
						printer,
						{create_graph, {'|~|', PA, PB, list_to_atom(SelectedStr), SPAN}, Parent, get_self()}),
					receive
						{created, NParent} ->
							{Nexp, NCurrent, NNodes, NDict} = 	
								process_answer_from_track(
									Selected, 
									NParent, 
									Track, 
									[{Current, NParent} | Dict],
									Current + 1),
							{Nexp, NCurrent, NNodes ++ [{Current, NParent, Event}], NDict}
							% {{Selected, NParent}, Current + 1, [{Current, NParent, Event}], [{Current, NParent} | Dict]}
					end;
			% end;
		false ->
			{{P, Parent}, Current, [], Dict}
	end;
process_answer_from_track(P = {agent_call, SPAN, ProcessName, Arguments}, Parent, Track, Dict, Current) ->
	case same_span(SPAN, Track, Current, Parent, Dict) of 
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
			case get(in_parallelism) of 
				false -> 
					print_event(Event);
				_ -> 
					ok
			end,
			send_message2regprocess(
				printer,
				{create_graph, P, Parent, get_self()}),
			receive
				{created, NParent} ->
					{Nexp, NCurrent, NNodes, NDict} = 
						process_answer_from_track(
							NCode, 
							NParent, 
							Track, 
							[{Current, NParent} | Dict],
							Current + 1),
					{Nexp, NCurrent, NNodes ++ [{Current, NParent, Event}], NDict}
					% {{NCode, NParent}, Current + 1, [{Current, NParent, Event}],  [{Current, NParent} | Dict]}
			end;
		false -> 
			{{P, Parent}, Current, [], Dict}
	end;
process_answer_from_track(IL = {'|||', PA, PB, SPAN}, Parent, Track, Dict, Current) ->
	case same_span(SPAN, Track, Current, Parent, Dict) of 
		true -> 
			send_message2regprocess(
				printer,
				{create_graph, IL, Parent, get_self()}),
			NParent = 
				receive
					{created, NParent0} ->
						NParent0
				end,
			process_answer_from_track_interleaving(
				{PA, NParent}, 
				{PB, NParent}, 
				Track, 
				Current + 1,
				NParent,
				SPAN,
				[{Current, NParent} | Dict]);
		false -> 
			{{IL, Parent}, Current, [], Dict}
	end;
process_answer_from_track(P = {'|||', PA, PB, ParentA, ParentB, SPAN}, Parent, Track, Dict, Current) ->
	process_answer_from_track_interleaving(
		{PA, ParentA}, 
		{PB, ParentB}, 
		Track, 
		Current,
		Parent,
		SPAN,
		Dict);
process_answer_from_track(IL = {sharing, {closure, Events}, PA, PB, SPAN}, Parent, Track, Dict, Current) ->
	case same_span(SPAN, Track, Current, Parent, Dict) of 
		true -> 
			send_message2regprocess(
				printer,
				{create_graph, IL, Parent, get_self()}),
			NParent = 
				receive
					{created, NParent0} ->
						NParent0
				end,
			process_answer_from_track_sharing(
				{PA, NParent}, 
				{PB, NParent}, 
				Track, 
				Current + 1,
				NParent,
				SPAN,
				Events,
				[{Current, NParent} | Dict]);
		false -> 
			{{IL, Parent}, Current, [], Dict}
	end;
process_answer_from_track(IL = {sharing, {closure, Events}, PA, PB, ParentA, ParentB, SPAN}, Parent, Track, Dict, Current) ->
	process_answer_from_track_sharing(
		{PA, ParentA}, 
		{PB, ParentB}, 
		Track, 
		Current,
		Parent,
		SPAN,
		Events,
		Dict);
process_answer_from_track(P = {';', PA, PB, SPAN}, Parent, Track, Dict, Current) ->
	{{NPA, NParentA}, NCurrent0, NNodesA, NDict0}  = 
		process_answer_from_track(PA, Parent, Track, Dict, Current), 
	{NSC_NParent, NCurrent, NDict} = 
		case NPA of 
			{finished_skip, _, NodesSkipA} ->
				send_message2regprocess(
					printer, 
					{create_graph, 
						{';', NodesSkipA, SPAN}, -1, get_self()}),
				receive
					{created, NParent0} ->
						{{PB, NParent0}, NCurrent0 + 1, [{NCurrent0, NParent0} | NDict0]}
				end;
			_ -> 
				{{{';', NPA, PB, SPAN}, NParentA}, NCurrent0, NDict0}
		end,
	{NSC_NParent, NCurrent, NNodesA, NDict};
process_answer_from_track(P = {skip, SPAN}, Parent, Track, Dict, Current) ->
	case same_span(SPAN, Track, Current, Parent, Dict) of 
		true ->
			Event = 
				'   tau (SKIP)',
			case get(in_parallelism) of 
				false -> 
					print_event(Event);
				_ -> 
					ok
			end,
			send_message2regprocess(
				printer,
				{create_graph, P, Parent, get_self()}),
			receive
				{created, NParent} ->
					{{{finished_skip, SPAN, [NParent]}, NParent}, Current + 1, [{Current, NParent, Event}], [{Current, NParent} | Dict]}
			end;
		false ->
			{{P, Parent}, Current, [], Dict}
	end;
process_answer_from_track(P = {finished_skip, _, _}, Parent, _, Dict, Current) ->
	{{P, Parent}, Current, [], Dict}.

process_answer_from_track_interleaving(
		{PA, ParentA}, 
		{PB, ParentB}, 
		Track, Current, 
		Parent,
		SPAN,
		Dict
	) -> 
	{{NPA, NParentA}, CurrentA, NNodesA, DictA}  = 
		process_answer_from_track(PA, ParentA, Track, Dict, Current), 
	{{NPB, NParentB}, CurrentB, NNodesB, DictB} = 
		process_answer_from_track(PB, ParentB, Track, DictA, Current), 
	NProcess = 
		case {NPA, NPB} of 
			{{finished_skip, _, NodesSkipA}, {finished_skip, _, NodesSkipB}} -> 
				{finished_skip, SPAN, NodesSkipA ++ NodesSkipB};
			_ -> 
				{'|||', 
					NPA, 
					NPB, 
					NParentA,
					NParentB,
					SPAN}
		end,
	{
		{NProcess, Parent}, 
		max(CurrentA, CurrentB), 
		NNodesA ++ NNodesB,
		DictB
	}.

process_answer_from_track_sharing(
		{PA, ParentA}, 
		{PB, ParentB}, 
		Track, 
		Current, 
		Parent, 
		SPAN, 
		Events,
		Dict
	) ->
	% io:format("Current: ~w\n", [Current]),
	% io:format("~p\n~p\n*****************\n", [SPAN, {{PA, ParentA}, {PB, ParentB}, Current}]),
	IsInParallel = 
		get(in_parallelism),
	put(in_parallelism, [true | IsInParallel]),
	% io:format("{Current, PA}: ~w\n", [{Current, ParentA, PA}]),
	{{NPA0, NParentA0}, CurrentA0, NNodesA0, DictA0}  = 
		process_answer_from_track(PA, ParentA, Track, Dict, Current), 
	% io:format("NNodesA0: ~p\n", [NNodesA0]),
	SyncEventsFun = 
		fun(Nodes) -> 
			lists:flatten(
					[NewN || {Old, NewN, Event} <- Nodes, Old /= sync, lists:member(Event, Events)] 
				++ 	[NodesSync || {sync, Event, NodesSync} <- Nodes, lists:member(Event, Events)]) 
		end,
	SyncEventsFullFun = 
		fun(Nodes) -> 
			lists:flatten(
					[N || N = {Old, _, Event} <- Nodes, Old /= sync, lists:member(Event, Events)] 
				++ 	[N || N = {sync, Event, _} <- Nodes, lists:member(Event, Events)]) 
		end,
	NotSyncEventsFun = 
		fun(Nodes) -> 
			lists:flatten(
					[N || N = {Old, _, Event} <- Nodes, Old /= sync, not(lists:member(Event, Events))] 
				++ 	[N|| N = {sync, Event, _} <- Nodes, not(lists:member(Event, Events))]) 
		end,
	EventsFun = 
		fun(Nodes) -> 
			% io:format("EventsFun: ~p\n", [Nodes]),
			Res0 = 
				[ {Event, Old} || {Old, _, Event} <- Nodes, Old /= sync] 
			++ 	[ {Event, lists:max(NodesSync)} || {sync, Event, NodesSync} <- Nodes],
			Res = 
				% [E || E <- Res0, is_atom(E)],
				Res0,
			% io:format("ResEventsFun: ~p\n", [Res]),
			Res
		end,
	SyncEventsA0 = 
		SyncEventsFun(NNodesA0),
	CurrentB0 =
		% CurrentA0,
		case SyncEventsA0 of 
			[] -> 
				Current;
			_ -> 
				CurrentA0
		end,
	{{NPB, NParentB}, CurrentB, NNodesB, DictB} = 
		case (CurrentB0 >= get(top)) of 
			true -> 
				{{PB, ParentB}, CurrentB0, [], DictA0};
			false -> 
				% io:format("{Current, ParentB, PB}: ~w\n", [{CurrentB0, ParentB, PB}]),
				process_answer_from_track(PB, ParentB, Track, DictA0, CurrentB0)
		end, 
	% io:format("NNodesB: ~p\n", [NNodesB]),
	SyncEventsB = 
		SyncEventsFun(NNodesB),
	{{NPA, NParentA}, CurrentA, NNodesA, DictA}  = 
		case {SyncEventsA0, SyncEventsB} of 
			{[], [_|_]} -> 
				case (CurrentB >= get(top)) of
					true -> 
						{{NPA0, NParentA0}, CurrentA0, NNodesA0, DictB};
					false -> 
						% io:format("SECOND TRY: {Current, ParentA, PA}: ~w\n", [{CurrentB, ParentA, PA}]),
						process_answer_from_track(PA, ParentA, Track, DictB, CurrentB)
				end;
			_ -> 
				{{NPA0, NParentA0}, CurrentA0, NNodesA0, DictB}
		end,
	SyncEventsA = 
		SyncEventsFun(NNodesA),
	put(in_parallelism, IsInParallel),
	% io:format("Current: ~w\n", [Current]),
	% io:format("NNodesA: ~w\n", [NNodesA]),
	% io:format("SyncEventsA: ~w\n", [SyncEventsA]),
	% io:format("CurrentA: ~w\n", [CurrentA]),
	% io:format("NNodesB: ~w\n", [NNodesB]),
	% io:format("SyncEventsB: ~w\n", [SyncEventsB]),
	% io:format("CurrentB: ~w\n", [CurrentB]),
	SyncEvents = 
		SyncEventsA ++ SyncEventsB,
	NNodesANNodesB = 
		NNodesA ++ NNodesB,
	NNodes = 
		case IsInParallel of 
			false -> 
				build_sync_edges(SyncEvents),
				case length(SyncEvents) > 0 of 
					true -> 
						% io:format("SYNC: ~w\n", [SyncEvents]),
						% io:format("NODES: ~w\n", [NNodesANNodesB]),
						% io:format("EVENTS_SYNC: ~w\n", [SyncEventsFullFun(NNodesANNodesB)]),
						EventToPrint = 
							hd(EventsFun(
								[hd(SyncEventsFullFun(NNodesANNodesB))])),
						% io:format("NODES TRUE: ~w\n", [NNodesA ++ NNodesB]),
						% io:format("EventToPrint TRUE: ~w\n", [EventToPrint]),
						% print_event(EventToPrint),
						NoSyncNodes = 
							NotSyncEventsFun(NNodesANNodesB),
						EventsToPrint = 
							EventsFun(NoSyncNodes),
						EventsToPrintSorted = 
							lists:sort(
								fun({_, A}, {_, B}) -> 
									A =< B
								end, 
								[EventToPrint | EventsToPrint]),
						[print_event(Event) || {Event, _} <- EventsToPrintSorted],
						% io:format("Events: ~p\n", [Events]),
						% io:format("Is not in parallel: ~w\n", [[{sync, EventToPrint, SyncEvents}] ++ NoSyncNodes]),
						[{sync, EventToPrint, SyncEvents}] ++ NoSyncNodes;
					false -> 
						EventsToPrint = 
							EventsFun(NNodesANNodesB),
						% io:format("NODES FALSE: ~w\n", [NNodesA ++ NNodesB]),
						% io:format("EventToPrint FALSE: ~p\n", [EventsToPrint]),
						EventsToPrintSorted = 
							lists:sort(
								fun({_, A}, {_, B}) -> 
									A =< B
								end, 
								EventsToPrint),
						[print_event(Event) || {Event, _} <- EventsToPrintSorted],
						NNodesANNodesB
				end;
			_ -> 
				case length(SyncEvents) > 0 of 
					true ->
						{EventToPrint,_} = 
							hd(EventsFun(
								[hd(SyncEventsFullFun(NNodesANNodesB))])),
						NoSyncNodes = 
							NotSyncEventsFun(NNodesANNodesB),
						% io:format("NODES *: ~w\n", [NNodesA ++ NNodesB]),
						% io:format("EventToPrint *: ~w\n", [EventToPrint]),
						% io:format("Is in parallel: ~w\n", [[{sync, EventToPrint, SyncEvents}] ++ NoSyncNodes]),
						[{sync, EventToPrint, SyncEvents}] ++ NoSyncNodes;
					false -> 
						NNodesANNodesB
				end
		end,
	% io:format("NNodes: ~p\n", [NNodes]),
	% io:format("SyncEventsA: ~p\n", [SyncEventsA]),
	% io:format("NNodesA: ~w, NNodesB ~w, Current: ~p\n", [NNodesA, NNodesB, max(CurrentA, CurrentB)]),
	NProcess = 
		case {NPA, NPB} of 
			{{finished_skip, _, NodesSkipA}, {finished_skip, _, NodesSkipB}} -> 
				{finished_skip, SPAN, NodesSkipA ++ NodesSkipB};
			_ -> 
				{sharing,
					{closure, Events}, 
					NPA, 
					NPB, 
					NParentA,
					NParentB,
					SPAN}
		end,
	{
		{NProcess, Parent}, 
		max(CurrentA, CurrentB), 
		NNodes,
		DictA
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Extract reverse options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reverse_options(Track) ->
	reverse_options_list(get_leaves(Track), Track, [], []).
	
reverse_options_list([H | T], Track, PendingSync, Acc) ->
	% io:format("List: ~w, Acc: ~w\n", [[H | T], Acc]),
	{NT, NPendingSync, NAcc} =
		case is_option(H, Track) of 
			true ->
				SyncNodes0 = 
					[begin
						{E, From, To, Type} = 
							digraph:edge(Track, E),
						% io:format("From: ~p, To:~p, Type: ~p\n", [From, To, Type]),
						SyncNode = 
							case From of 
								H -> 
									To; 
								_ -> 
								 	From 
							end,
						case {Type, SyncNode /= H} of 
							{"sync", true} -> 
								[SyncNode];
							_ -> 
								[] 
						end
					end
					|| E <- (digraph:in_edges(Track, H) ++ digraph:out_edges(Track, H))],	
				SyncNodes = 
					lists:usort([H | lists:concat(SyncNodes0)]),
				NPendingSync0 = 
					[H | PendingSync],
				SetCurrentPending = 
					sets:from_list(NPendingSync0),
				SetSyncNodes = 
					sets:from_list(SyncNodes),
				% io:format("{NPendingSync0, SyncNodes} =\n~p\n~p\n", [lists:sort(NPendingSync0), lists:sort(SyncNodes)]),
				{ReachableFromSyncNodes, NPendingSync1, NAcc0} = 
					case lists:sort(sets:to_list(sets:intersection(SetCurrentPending, SetSyncNodes))) of 
						SyncNodes -> 
							% io:format("ENTRA: ~w\n", [a]),
							% All sync nodes are options 
							{
								digraph_utils:reachable(SyncNodes, Track), 
								sets:to_list(
									sets:subtract(SetCurrentPending, SetSyncNodes)),
								[ SyncNodes | Acc]
							};
						_ -> 
							% io:format("NO ENTRA: ~w\n", [lists:sort(sets:to_list(sets:intersection(SetCurrentPending, SetSyncNodes)))]),
							{
								[],
								NPendingSync0,
								Acc
							}
					end,
				% io:format("NT: ~w, NACC: ~w\n", [T -- SyncNodes, [ SyncNodes | Acc]]),
				{T -- ReachableFromSyncNodes, NPendingSync1, NAcc0};
			false ->
				InNeigbs = 
					digraph:in_neighbours(Track, H),
				{InNeigbs ++  T, PendingSync, Acc}
		end,
	reverse_options_list(NT, Track, NPendingSync, NAcc);
reverse_options_list([], _, _, Acc) ->
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
	% io:format("\tremove_from_track N: ~p\n", [N]),
	Ns = 
		case is_list(N) of 
			true -> 
				N;
			false -> 
				[N]
		end,
	% io:format("\tremove_from_track Reachable: ~p\n", [digraph_utils:reachable(Ns, Track)]),
	digraph:del_vertices(
		Track, 
		digraph_utils:reachable(Ns, Track)),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Other functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
extract_span({';', _, _, SPAN}) ->
	SPAN;
extract_span({skip, SPAN}) ->
	SPAN.

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
