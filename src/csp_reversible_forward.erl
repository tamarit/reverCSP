-module(csp_reversible_forward).

-export([
			start/1,
			start_from_expr/3
			% reverse_options/1, 
			% remove_from_track/2
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start(FirstProcess) -> 
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	start_from_expr(FirstProcess, FirstExp, []).

start_from_expr(FirstProcess, FirstExp, Previous) ->
	csp_reversible_lib:register_printer(),
	ResultExe = 
		execute_csp({FirstExp, -1}, Previous),
	csp_reversible_lib:send_message2regprocess(printer,{info_graph_no_stop,csp_reversible_lib:get_self()}),
	InfoGraph = 
		receive 
			{info_graph, InfoGraph_} ->
				% io:format("~p\n", [InfoGraph_]),
				InfoGraph_
		after 
			1000 -> 
				{{{0,0,0,now()},"",""},{[],[]}}
		end,
		csp_reversible_lib:send_message2regprocess(printer,{stop, csp_reversible_lib:get_self()}),
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
				csp_reversible_backward:start_from_track(FirstProcess, Digraph),
			csp_reversible_backward:start_reverse_mode(
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
		csp_reversible_lib:decide_eval_order(),
	% io:format("Selected ~p mode.", [EvalOrder]),
	{{_,_,_}, {NodesDigraph, EdgesDigraph}}
		= InfoGraph,
	Digraph = 
		csp_tracker:build_digraph(NodesDigraph, EdgesDigraph),
	case EvalOrder of 
		reverse -> 
			EvalInfo = 
				csp_reversible_backward:start_from_track(FirstProcess, Digraph),
			csp_reversible_backward:start_reverse_mode(
				FirstProcess, 
				EvalInfo,
				{forward_reverse, Steps});
		forward -> 
			csp_reversible_backward:start_from_track_continue_user(
				FirstProcess, 
				Digraph, 
				{forward_reverse, Steps})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Process Execution (interactive)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute_csp({Exp, Parent}, Previous) ->
	Questions = get_questions(Exp, []),
	% io:get_line(standard_io, format("****State****\n~s\n****Options****\n~s\n************\n", [csp_reversible_lib:csp2string(Exp), lists:join("\n", [ csp_reversible_lib:csp2string(Q) || Q <- Questions])])),
	case Previous of 
		{forward_reverse, N} when is_integer(N) andalso N > 0 -> 
			case Questions of 
				[] -> 
					{reverse, {forward_reverse, N}};
				_ -> 
					io:format(
						"\n\nCurrent expression:\n~s\n\n", 
						[csp_reversible_lib:csp2string(Exp)]),
					{_, NSteps} = 
						select_random_option({Exp, Parent}, Questions, "Forward evaluation. ", N),
					% io:format("PASA1\n"),
					{forward_reverse, NSteps}
			end;
		_ -> 
			io:format(
				"\n\nCurrent expression:\n~s\n\n", 
				[csp_reversible_lib:csp2string(Exp)]),
			case Questions of 
				[] ->
					% TODO: Should ask before if the user wants to undo or reverse.
					io:format("This CSP expression cannot be further evaluated.\n"),
					csp_reversible_lib:ask_questions(
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
							case csp_reversible_lib:ask_questions(
									csp_reversible_lib:build_str_tuples(Questions) ++ AdditionalOptions, 
									fun process_answer_exe/3,
									[]) 
							of 
								finish ->
									finish;
								{reverse, []} ->
									{reverse, []};
								random_forward -> 
									Steps = 
								        csp_reversible_lib:get_answer(
								        	"\nHow many steps?\n[1..1000]: ", 
								        	lists:seq(1, 1000)),
									execute_csp_random({Exp, Parent}, Questions, Steps);
								random_forward_reverse -> 
									Steps = 
								        csp_reversible_lib:get_answer(
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
	% io:format("{Exp, Answer, Parent}: ~p\n", [{Exp, Answer, Parent}]),
	% io:format("{NExp, NNodes}: ~p\n", [{NExp, NNodes}]),
	case Answer of 
		[_|_] -> 
			csp_reversible_lib:build_sync_edges(lists:flatten([N || {N, _} <- NNodes]));
			% io:format("~p\n", [lists:seq(1, length(NNodes) - 1)]),
			% [ begin
			% 	csp_reversible_lib:send_message2regprocess(
			% 		printer,
			% 		{unprint_last, csp_reversible_lib:get_self()}),
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
	{NExp, NSteps} = 
		select_random_option({Exp, Parent}, Options, "", Steps),
	execute_csp(NExp, NSteps).

select_random_option({Exp, Parent}, Options, AdditionalInfo, Steps) -> 
	FunHasBP = 
		fun(List) -> 
			[bp || {prefix, _, _, bp, _, _} <- List]
		end,
	HasBP = 
			FunHasBP(Options) 
		++ 	lists:concat([FunHasBP(O) || O <- Options, is_list(O)]),
	case HasBP of 
		[] -> 
			Answer = 
				lists:nth(rand:uniform(length(Options)), Options),
			io:format(
				"\n~sRandomly selected:\n~s\n", 
				[AdditionalInfo, csp_reversible_lib:csp2string(Answer)]),
			put(random_choice, true),
			NExp = 
				csp_process_option_processing(Exp, Answer, Parent),
			put(random_choice, false),
			{NExp, Steps - 1};
		_ ->
			{{Exp, Parent}, []}
	end.


process_answer_exe(t, RC, _) ->
	csp_reversible_lib:send_message2regprocess(printer,{get_trace, csp_reversible_lib:get_self()}),
	InfoGraph = 
		receive 
			{trace, Trace} ->
				csp_reversible_lib:print_trace(Trace) 
		end, 
	RC();
process_answer_exe(c, RC, _) ->
	csp_reversible_lib:send_message2regprocess(printer,{info_graph_no_stop, csp_reversible_lib:get_self()}),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Options available
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	get_questions_sharing({Events, PA, PB}, Renamings);
get_questions({sharing, {closure, Events}, PA, PB, _, _, SPAN}, Renamings) ->
	get_questions_sharing({Events, PA, PB}, Renamings);
get_questions({';', PA, _, _}, Renamings) -> 
	get_questions(PA, Renamings);
get_questions({skip, SPAN}, Renamings) ->
	[];
get_questions({finished_skip, _, _}, Renamings) ->
	[].

get_questions_sharing({Events, PA, PB}, Renamings) -> 
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
	% io:format("PrefSync: ~w\n", [PrefSync]),
	% io:format("PrefNotSync: ~w\n", [PrefNotSync]),
	Opts = NotPref ++ [Pref || {_, Pref} <- PrefNotSync] ++ PrefSync,
	% io:format("Opts: ~p\n", [Opts]),
	Opts.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Execution from an answer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_answer(P = {prefix, SPAN1, Channels, Event, ProcessPrefixing, SPAN}, L = [_|_], Parent) ->
	case lists:member(P, L) of 
		true ->
			case get(in_parallelism) of 
				false -> 
					csp_reversible_lib:print_event(Event);
				_ -> 
					ok
			end,
			csp_reversible_lib:send_message2regprocess(
				printer,
				{create_graph, P, Parent, csp_reversible_lib:get_self()}),
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
			csp_reversible_lib:print_event(Event);
		_ -> 
			ok
	end,
	csp_reversible_lib:send_message2regprocess(
		printer,
		{create_graph, P, Parent, csp_reversible_lib:get_self()}),
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
				csp_reversible_lib:ask_questions(
					csp_reversible_lib:build_str_tuples([PA, PB]), 
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
			++ csp_reversible_lib:csp2string(Selected)),
	csp_reversible_lib:print_event(Event),
	csp_reversible_lib:send_message2regprocess(
		printer,
		{create_graph, {'|~|', PA, PB, SelectedAtom, SPAN}, Parent, csp_reversible_lib:get_self()}),
	receive
		{created, NParent} ->
			{NExp, NNodes} = 
				process_answer(Selected, P, NParent),
			{NExp, NNodes}
	end;
process_answer(P = {agent_call, SPAN, ProcessName, Arguments}, P, Parent) ->
	csp_reversible_lib:send_message2regprocess(codeserver, {ask_code, ProcessName, Arguments, csp_reversible_lib:get_self()}),
	NCode = 
		receive
			{code_reply, Code} -> 
				Code
		end,
	Event = 
		list_to_atom("   tau -> Call to process "
			++ atom_to_list(ProcessName)
			++ printer:string_arguments(Arguments)),
	csp_reversible_lib:print_event(Event),
	csp_reversible_lib:send_message2regprocess(
		printer,
		{create_graph, P, Parent, csp_reversible_lib:get_self()}),
	receive
		{created, NParent} ->
			{NExp, NNodes} = 
				case NCode of 
					{sharing, _, _, _, _} -> 
						process_answer(NCode, P, NParent);
					{'|||', _, _, _} -> 
						process_answer(NCode, P, NParent);
					_ -> 
						{{NCode, NParent}, []}
				end,
			{NExp, NNodes}
	end;
process_answer(IL = {'|||', PA, PB, SPAN}, P, Parent) ->
	csp_reversible_lib:send_message2regprocess(
		printer,
		{create_graph, IL, Parent, csp_reversible_lib:get_self()}),
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
	csp_reversible_lib:send_message2regprocess(
		printer,
		{create_graph, IL, Parent, csp_reversible_lib:get_self()}),
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
				csp_reversible_lib:send_message2regprocess(
					printer, 
					{create_graph, 
						{';', NodesSkipA, SPAN}, -1, csp_reversible_lib:get_self()}),
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
	csp_reversible_lib:print_event(Event),
	csp_reversible_lib:send_message2regprocess(
		printer,
		{create_graph, P, Parent, csp_reversible_lib:get_self()}),
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
		% case {not(is_list(P)), ParentA /= NParentA} of
			{true, true} ->
				% case {PA, NPA} of 
				% 	{{sharing, _, _, _, _}, {sharing, _, _, _, _, _, _} } -> 
				% 		process_answer(PB, P, ParentB);
				% 	_ -> 		
						{{PB, ParentB}, []};
				% end;
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
		% case {not(is_list(P)), ParentA /= NParentA} of 
			{true, true} ->
				% case {PA, NPA} of 
					% {{sharing, _, _, _, _}, {sharing, _, _, _, _, _, _} } -> 
					% 	process_answer(PB, P, ParentB);
					% _ -> 		
						{{PB, ParentB}, []};
				% end;
			_ ->   
				process_answer(PB, P, ParentB)
		end,
	put(in_parallelism, IsInParallel),
	NNodes = 
		case IsInParallel of 
			false -> 
				case {NPA /= PA, NPB /= PB} of 
					{true, true} ->  
						% io:format("A: ~p\nB: ~p\n", [{PA, NPA}, {PB, NPB}]),
						% io:format("DIFF: ~p\n", [NNodesA ++ NNodesB]),
						case (NNodesA ++ NNodesB) of 
							[] -> 
								ok;
							_ -> 
								EventToPrint = 
									element(2, hd(NNodesA ++ NNodesB)),
								csp_reversible_lib:print_event(EventToPrint)
						end;
					_ -> 
						% io:format("A: ~p\nB: ~p\n", [{PA, NPA}, {PB, NPB}]),
						% io:format("SAME: ~p\n", [NNodesA ++ NNodesB]),
						EventsToPrint = 
							[E || {_, E} <- lists:sort(NNodesA ++ NNodesB)],
						[csp_reversible_lib:print_event(Event) || Event <- EventsToPrint]
				end,
				NNodesA ++ NNodesB;
			_ -> 
				case {NPA /= PA, NPB /= PB} of 
					{true, true} -> 
						{_, CommonEvent} =  
							hd(NNodesA ++ NNodesB),
						NodesAB = 
							[N || {N, _} <- NNodesA ++ NNodesB],
						[{NodesAB, CommonEvent}];
					_ -> 
						NNodesA ++ NNodesB
				end
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
		NNodes
	}.


