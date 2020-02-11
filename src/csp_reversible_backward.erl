-module(csp_reversible_backward).

-export([
			start_reverse_mode/3,
			start_from_track/2,
			start_from_track_continue_user/3,
			reverse_options/1,
			prepare_questions_reverse/3
		]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Reverse Execution (interactive)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_reverse_mode(FirstProcess, EvalInfo = {InfoTrack = {{_,_,Trace}, DigraphContent}, ResExp}, Previous) ->
	{NodesDigraph, EdgesDigraph} =
		DigraphContent,
	Digraph =
		csp_tracker:build_digraph(NodesDigraph, EdgesDigraph),
	FunPrintCurrentTrack = 
		fun () -> 
			csp_tracker:print_from_digraph(Digraph, "current", [], false),
			csp_reversible_lib:move("current.pdf", "output/current.pdf"),
			csp_reversible_lib:move("current.dot", "output/current.dot")
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
						[csp_reversible_lib:csp2string(ResExp)]),
					ReverseOptionsReady = 
						prepare_questions_reverse(FirstProcess, ReverseOptions, Digraph),
					% {NEvalInfo, Printed} = 
					% 	lists:nth(
					% 		rand:uniform(length(ReverseOptionsReady)), 
					% 		ReverseOptionsReady),
					% io:format("\nReverse evaluation. Randomly selected:\n~s\n", [Printed]),
					% digraph:delete(Digraph),
					{NEvalInfo, NSteps} = 
						select_random_option(ReverseOptionsReady, EvalInfo, "Reverse evaluation. ", Digraph, N),
					case csp_reversible_lib:decide_eval_order() of 
						reverse -> 
							start_reverse_mode(
								FirstProcess, 
								NEvalInfo, 
								{forward_reverse, NSteps});
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
								{forward_reverse, NSteps})
					end		
			end;
		_ -> 
			io:format(
				"\n\nCurrent expression:\n~s\n\n", 
				[csp_reversible_lib:csp2string(ResExp)]),
			case ReverseOptions of 
				[] ->
					io:format("The track is empty, so there is nothing to reverse.\n"),
					case 
						csp_reversible_lib:ask_questions(
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
							start_reverse_mode_random(FirstProcess, EvalInfo, ReverseOptionsReady, Digraph, Previous);
						_ -> 
							case csp_reversible_lib:ask_questions(
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
								        csp_reversible_lib:get_answer(
								        	"\nHow many steps?\n[1..1000]: ", 
								        	lists:seq(1, 1000)),
									start_reverse_mode_random(FirstProcess, EvalInfo, ReverseOptionsReady, Digraph, Steps);				
								random_forward_reverse -> 
									Steps = 
								        csp_reversible_lib:get_answer(
								        	"\nHow many steps?\n[1..1000]: ", 
								        	lists:seq(1, 1000)),
									case csp_reversible_lib:decide_eval_order() of 
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

start_reverse_mode_random(FirstProcess, EvalInfo, Options, Digraph, Steps) -> 
	% io:format("Options: ~p\n", [Options]),
	{NEvalInfo, NSteps} = 
		select_random_option(Options, EvalInfo, "", Digraph, Steps),
	start_reverse_mode(FirstProcess, NEvalInfo, NSteps).

select_random_option(Options, EvalInfo, AdditionalInfo, Digraph, Steps) -> 
	HasBP = 
		lists:flatten(
			[case Printed of 
				[$b, $p | _] -> 
					Printed;
				_ -> 
					[]
			end
			|| {_, Printed} <- Options]),
	{NEvalInfo, NSteps} = 
		case HasBP of 
			[] -> 
				{NEvalInfo0, Printed} = 
					lists:nth(rand:uniform(length(Options)), Options),
				io:format(
					"\n~sRandomly selected:\n~s\n", 
					[AdditionalInfo, Printed]),
				{NEvalInfo0, Steps - 1};
			_ ->
				{EvalInfo, []}
		end,
	digraph:delete(Digraph),
	{NEvalInfo, NSteps}.

prepare_questions_reverse(FirstProcess, [H | T], G) ->
	NG = csp_reversible_lib:copy_digraph(G),
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
				fun csp_reversible_lib:node2str/1, 
				[element(2, digraph:vertex(G, N)) 
				|| N <- Ns]),
			","),
	% io:format("ARRIBA\n"),
	remove_from_track(H, NG),
	% io:format("ARRIBA2\n"),
	% io:format("H: ~w\n", [H]),
	% csp_tracker:print_from_digraph(NG, "current" ++ integer_to_list(length([H | T])), [], false),
	Result = {_, ResExp} = 
		start_from_track(FirstProcess, NG),
	% io:format("ARRIBA3\n"),
	% io:format("ResExp: ~p\n", [ResExp]),
	Printed = 
			NodeStr ++ "\n\t\\__ "  
		++ 	csp_reversible_lib:csp2string(ResExp),
	[ 	{Result, Printed} 
	| 	prepare_questions_reverse(FirstProcess, T, G)];
prepare_questions_reverse(_, [], _) ->
	[].

process_answer_reverse(t, RC, {Trace,_}) ->
	csp_reversible_lib:print_trace(Trace), 
	RC();
process_answer_reverse(c, RC, {_,PT}) ->
	PT(),
	io:format("Current track available at output/current.pdf\n"),
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
%   Process Execution (based on a track)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_from_track_continue_user(FirstProcess, Track, Previous) -> 
	% io:format("ENTRA_1\n"),
	csp_reversible_lib:register_printer(),
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	try digraph:vertices(Track) of 
		[_|_] ->
			{NExp, _NNodes} =
				execute_csp_from_track(
					{FirstExp, -1}, 
					Track, 
					0, 
					csp_reversible_lib:get_max_vertex(Track) + 1,
					[]),
			% io:format("FINAL NNodes1: ~w\n", [NNodes]),
			digraph:delete(Track),
			csp_reversible_forward:start_from_expr(FirstProcess, NExp, Previous);
		[] ->
			digraph:delete(Track),
			csp_reversible_forward:start_from_expr(FirstProcess, FirstExp, Previous)
	catch 
		_:_ -> 
			digraph:delete(Track),
			csp_reversible_forward:start_from_expr(FirstProcess, FirstExp, Previous)
	end.

start_from_track(FirstProcess, Track) -> 
	% io:format("ENTRA_2\n"),
	csp_reversible_lib:register_printer(),
	FirstExp = 
		{agent_call,{src_span,0,0,0,0,0,0},FirstProcess,[]},
	NState = 
		case digraph:vertices(Track) of 
			[_|_] ->
				% io:format("INIT ~p\n", [csp_reversible_lib:get_max_vertex(Track) + 1]),
				{NExp, _NNodes} =
					execute_csp_from_track({FirstExp, -1}, Track, 0, csp_reversible_lib:get_max_vertex(Track) + 1, []),
				% io:format("FINAL NNodes2: ~w\n", [NNodes]),
				csp_reversible_lib:send_message2regprocess(printer, {info_graph_no_stop, csp_reversible_lib:get_self()}),
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
	csp_reversible_lib:send_message2regprocess(printer, {stop, csp_reversible_lib:get_self()}),
	receive 
		stopped -> 
			ok
	end,
	NState.

execute_csp_from_track({Exp, Parent}, Track, Current, Top, Dict) ->
	% io:format("Current: ~p\n", [Current]),
	% io:format("\n++++++\nExp: ~s\n++++++\n", [csp_reversible_lib:csp2string(Exp)]),
	put(top, Top),
	put(in_parallelism, false),
	{NExpParent = {NExp,_}, NCurrent, NNodes, NDict} = 
		process_answer_from_track(Exp, Parent, Track, Dict, Current),
	% io:format("NCurrent: ~p\n", [NCurrent]),
	% io:format("\n++++++\nNExp: ~s\n++++++\n", [csp_reversible_lib:csp2string(NExp)]),
	case NCurrent of 
		Top ->
			% io:format("NDict: ~p\n", [NDict]),
			{NExp, NNodes};
		Current ->
			execute_csp_from_track(NExpParent, Track, Current + 1, Top, NDict);
		_ ->
			execute_csp_from_track(NExpParent, Track, NCurrent, Top, NDict)
	end.

process_answer_from_track(P = {prefix, SPAN1, _Channels, Event, ProcessPrefixing, _SPAN}, Parent, Track, Dict, Current) ->
	% io:format("Entra: ~p\n", [{Current, P}]),
	% case (csp_reversible_lib:same_span(SPAN1, Track, Current) or csp_reversible_lib:same_span(SPAN, Track, Current)) of 
	case (csp_reversible_lib:same_span(SPAN1, Track, Current, Parent, Dict)) of 
		true -> 
			% io:format("The span are the same: ~p\n", [{Current, P}]),
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
					{Nexp, NCurrent, NNodes, NDict} = 
						process_answer_from_track(
							ProcessPrefixing, 
							NParent, 
							Track,
							[{Current, (NParent - 1)}, {Current + 1, NParent}  | Dict],
							Current + 2),
					{Nexp, NCurrent, NNodes ++ [{Current, (NParent - 1), Event}], NDict}
					% {NCurrent, CurrentNode} = 
					% 	% case csp_reversible_lib:same_span(SPAN1, Track, Current) of 
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
	case csp_reversible_lib:same_span(SPAN, Track, Current, Parent, Dict) of 
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
							++ csp_reversible_lib:csp2string(Selected)),
					case get(in_parallelism) of 
						false -> 
							csp_reversible_lib:print_event(Event);
						_ -> 
							ok
					end,
					csp_reversible_lib:send_message2regprocess(
						printer,
						{create_graph, {'|~|', PA, PB, list_to_atom(SelectedStr), SPAN}, Parent, csp_reversible_lib:get_self()}),
					receive
						{created, NParent} ->
							{Nexp, NCurrent, NNodes, NDict} = 	
								process_answer_from_track(
									Selected, 
									NParent, 
									Track, 
									[{Current, NParent} | Dict],
									Current + 1),
							{Nexp, NCurrent, NNodes ++ [{Current, NParent, Event}], NDict}
							% {{Selected, NParent}, Current + 1, [{Current, NParent, Event}], [{Current, NParent} | Dict]}
					end;
			% end;
		false ->
			{{P, Parent}, Current, [], Dict}
	end;
process_answer_from_track(P = {agent_call, SPAN, ProcessName, Arguments}, Parent, Track, Dict, Current) ->
	case csp_reversible_lib:same_span(SPAN, Track, Current, Parent, Dict) of 
		true -> 
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
					{Nexp, NCurrent, NNodes, NDict} = 
						process_answer_from_track(
							NCode, 
							NParent, 
							Track, 
							[{Current, NParent} | Dict],
							Current + 1),
					{Nexp, NCurrent, NNodes ++ [{Current, NParent, Event}], NDict}
					% {{NCode, NParent}, Current + 1, [{Current, NParent, Event}],  [{Current, NParent} | Dict]}
			end;
		false -> 
			{{P, Parent}, Current, [], Dict}
	end;
process_answer_from_track(IL = {'|||', PA, PB, SPAN}, Parent, Track, Dict, Current) ->
	case csp_reversible_lib:same_span(SPAN, Track, Current, Parent, Dict) of 
		true -> 
			csp_reversible_lib:send_message2regprocess(
				printer,
				{create_graph, IL, Parent, csp_reversible_lib:get_self()}),
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
				[{Current, NParent} | Dict]);
		false -> 
			{{IL, Parent}, Current, [], Dict}
	end;
process_answer_from_track(_P = {'|||', PA, PB, ParentA, ParentB, SPAN}, Parent, Track, Dict, Current) ->
	process_answer_from_track_interleaving(
		{PA, ParentA}, 
		{PB, ParentB}, 
		Track, 
		Current,
		Parent,
		SPAN,
		Dict);
process_answer_from_track(IL = {sharing, {closure, Events}, PA, PB, SPAN}, Parent, Track, Dict, Current) ->
	case csp_reversible_lib:same_span(SPAN, Track, Current, Parent, Dict) of 
		true -> 
			csp_reversible_lib:send_message2regprocess(
				printer,
				{create_graph, IL, Parent, csp_reversible_lib:get_self()}),
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
				[{Current, NParent} | Dict]);
		false -> 
			{{IL, Parent}, Current, [], Dict}
	end;
process_answer_from_track(_IL = {sharing, {closure, Events}, PA, PB, ParentA, ParentB, SPAN}, Parent, Track, Dict, Current) ->
	process_answer_from_track_sharing(
		{PA, ParentA}, 
		{PB, ParentB}, 
		Track, 
		Current,
		Parent,
		SPAN,
		Events,
		Dict);
process_answer_from_track(_P = {';', PA, PB, SPAN}, Parent, Track, Dict, Current) ->
	{{NPA, NParentA}, NCurrent0, NNodesA, NDict0}  = 
		process_answer_from_track(PA, Parent, Track, Dict, Current), 
	{NSC_NParent, NCurrent, NDict} = 
		case NPA of 
			{finished_skip, _, NodesSkipA} ->
				csp_reversible_lib:send_message2regprocess(
					printer, 
					{create_graph, 
						{';', NodesSkipA, SPAN}, -1, csp_reversible_lib:get_self()}),
				receive
					{created, NParent0} ->
						{{PB, NParent0}, NCurrent0 + 1, [{NCurrent0, NParent0} | NDict0]}
				end;
			_ -> 
				{{{';', NPA, PB, SPAN}, NParentA}, NCurrent0, NDict0}
		end,
	{NSC_NParent, NCurrent, NNodesA, NDict};
process_answer_from_track(P = {skip, SPAN}, Parent, Track, Dict, Current) ->
	case csp_reversible_lib:same_span(SPAN, Track, Current, Parent, Dict) of 
		true ->
			Event = 
				'   tau (SKIP)',
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
	put(in_parallelism, [true | IsInParallel]),
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
				csp_reversible_lib:build_sync_edges(SyncEvents),
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
						% csp_reversible_lib:print_event(EventToPrint),
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
						[csp_reversible_lib:print_event(Event) || {Event, _} <- EventsToPrintSorted],
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
						[csp_reversible_lib:print_event(Event) || {Event, _} <- EventsToPrintSorted],
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
	reverse_options_list(csp_reversible_lib:get_leaves(Track), Track, [], []).
	
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
					[H | PendingSync],
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
		_NAcc =
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


