-module(csp_reversible).

-export([run/1, run/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(File) -> 
	run(File,'MAIN').

run(File, FirstProcess) -> 
	io:format("~s\n", [File]),
	OutputFile = 
		File ++ "_temp",
	csp_tracker:rewrite_renamings(File, OutputFile),
	AbsOutputFile = 
		filename:absname(OutputFile),
	% io:format("s\n", [AbsOutputFile]),
	OutputConversion = 
		os:cmd("cd csp_tracker;./createoutput.sh " ++ AbsOutputFile),
	io:format("~s\n",[OutputConversion]),
	csp_tracker:preprocess_variables("csp_tracker/"),
	case file:consult("csp_tracker/output_rewritten.txt") of
		{error,{_,_,InfoError}} ->
			io:format(
				"Error reading Erlang translation:\n~s\n",
				[lists:flatten(erl_parse:format_error(InfoError))]),
			io:format("Correct the syntax error before to proceed\n"),
			ok;
		{ok, ProcessList} ->
			% io:format("~p\n", [ProcessList])
			case ProcessList of
			     [[]|_] -> 
			     	io:format("Correct the syntax error before to proceed\n"),
			     	% result_for_error();
			     	ok;
			     [_|_] -> 
					file:write_file("track.dot", list_to_binary("digraph csp_track {\n}")),
					Processes = ets:new(processes,[bag]),
					csp_tracker:insert_processes(hd(ProcessList),Processes),
					ChannelInfo_ = csp_tracker:read_channels_info("csp_tracker/"),
					ChannelInfo = 
						[{Channel, csp_parsing:extract_type(Type)} 
						 || {Channel, Type} <- ChannelInfo_],
					% io:format("~p\n",[ChannelInfo]),
					csp_tracker:insert_processes(ChannelInfo,Processes),
			%		io:format("Processes: ~p\n",
			%		          [[ PN || {PN,_} <- ets:tab2list(Processes)]--
			%		           [ PN || {PN,_} <- ets:tab2list(Processes),
			%		                   csp_parsing:fake_process_name(atom_to_list(PN))]]),
					case lists:member(codeserver,registered()) of
					     true -> ok;
					     false -> 
					     	register(codeserver, spawn(codeserver,loop,[Processes]))
					end,
					Timeout = 
					  infinity,
					case lists:member(printer,registered()) of
					     true -> ok;
					     false -> 
					     	register(printer, 
					         spawn(printer,loop,
					            [all, false]))
					end,					
					%io:format("Timout: ~p\n",[Timeout]),
					% TimeBeforeExecuting = now(),
					{{{N,E,S,TimeAfterExecuting},_G,Trace}, DigraphContent} = 
						csp_process_interactive:start(FirstProcess),
					{NodesDigraph, EdgesDigraph} = DigraphContent,
					% TimeBeforeTrack = now(),
					Digraph = csp_tracker:build_digraph(NodesDigraph, EdgesDigraph),
					% TimeAfterTrack = now(),
					% io:format("Total of time generate track:\t~p ms\n",[timer:now_diff(TimeAfterTrack - TimeBeforeTrack)]),
					%TimeAfterExecuting = now(),
					% case Timeout of
					% 	infinity -> 
					% 		ok;
					% 	_ -> 
					% 		% printer:add_to_file(G,NoOutput),
							csp_tracker:print_from_digraph(Digraph, "track", [], false),
					% 		case NoOutput of 
					% 			false ->
									io:format("\n*********** Trace ************\n\n~s\n******************************\n",[Trace]),
					% 			true ->
					% 				ok 
					% 		end
					% end,
					% TimeExecuting = timer:now_diff(TimeAfterExecuting, TimeBeforeExecuting),
					% SizeFile = filelib:file_size("track.dot"), 
					% io:format("~p.\n~p.\n", [
					% 	[digraph:vertex(Digraph, V)  || V <- digraph:vertices(Digraph)], 
					% 	[digraph:edge(Digraph, E)  || E <- digraph:edges(Digraph)]]),
					TrackStr = 
						io_lib:format("~p.\n~p.\n", [NodesDigraph, EdgesDigraph]),
					file:write_file("track.txt", list_to_binary(TrackStr)),



					case lists:member(printer,registered()) of
					     true -> ok;
					     false -> 
					     	register(printer, 
					         spawn(printer,loop,
					            [all, false]))
					end,
					{{_,_,TraceTRACK}, DigraphContentTRACK} = 
						csp_process_interactive:start_from_track(FirstProcess, Digraph),
					{NodesDigraphTRACK, EdgesDigraphTRACK} = 
						DigraphContentTRACK,
					DigraphTRACK = 
						csp_tracker:build_digraph(NodesDigraphTRACK, EdgesDigraphTRACK),
					csp_tracker:print_from_digraph(DigraphTRACK, "track_from_track", [], false),
					io:format("\n*********** Trace from track ************\n\n~s\n******************************\n",[TraceTRACK]),


					% [_,{memory,Words},_] = digraph:info(Digraph),
					% Result1 = 
					% 	case NoOutput of 
					% 		false ->
					% 			io:format("\n********** Results ************\n"),
					% 			io:format("Total of time converting:\t~p ms\n",[TimeConversion/1000]),
					% 			io:format("Total of time executing:\t~p ms\n",[TimeExecuting/1000]),
					% 			% io:format("Total of time generate track:\t~p ms\n",[timer:now_diff(TimeAfterTrack, TimeBeforeTrack) / 1000]),
					% 			io:format("Total of time:\t~p ms\n",[(TimeExecuting + TimeConversion)/1000]),
					% 			io:format("Total of node:\t~p nodes\n",[N]),
					% 			io:format("Total of control edges:\t~p edges\n",[E]),
					% 			io:format("Total of synchronization edges:\t~p edges\n",[S]),
					% 			io:format("Total of edges:\t~p edges\n",[E + S]),
					% 			io:format("Size of DOT file:\t~p bytes\n",[SizeFile]),
					% 			io:format("Track size in memory:\t~p bytes\n", [Words * erlang:system_info(wordsize)]),
					% 			io:format("******************************\n");
					% 		true ->
					% 			{{N,E,S},TimeConversion,TimeExecuting,TimeConversion + TimeExecuting,SizeFile}
					% 	end,
					% DigraphComplete = 
					% 	csp_tracker:build_digraph(NodesDigraph, EdgesDigraph),
					% TotalSlice = csp_slicer:get_total_slices(DigraphComplete),
					% Result2 = 
					% 	case NoOutput of 
					% 		false ->
					% 			io:format("\n*********** Slice ************\n"),
					% 			case TotalSlice of 
					% 				0 ->
					% 					io:format("Slicing criterion not executed.\n");
					% 				_ ->
					% 					io:format("The slicing criterion was executed " 
					% 						++ integer_to_list(TotalSlice) ++ " times.\n"),
					% 					FunAnswer(DigraphComplete, TotalSlice)
					% 			end,
					% 			io:format("*******************************\n"),
					% 			io:format("~p\n", [TotalSlice]);
					% 		true -> 
					% 			case TotalSlice of 
					% 				0 ->
					% 					0;
					% 				_ ->
					% 					{Slice, TimeCal} = get_slices_from_digraph(DigraphComplete, 1),
					% 					remove_slice_nodes(DigraphComplete),
					% 					Lines = read_lines_file(File),
					% 					{_, TimeGen} = slice_output(Slice, FirstProcess, DigraphComplete, Lines),
					% 					TimeCal + TimeGen
					% 			end
					% 	end,
					csp_process:send_message2regprocess(codeserver, stop),
					ok
				% 	{Result1, Result2};
				% _ ->
				% 	result_for_error()
			end
	end.