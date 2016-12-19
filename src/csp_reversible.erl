-module(csp_reversible).

-export([run/1, run/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(File) -> 
	run(File,"MAIN").

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
			io:format("~p\n", [ProcessList])
	end.