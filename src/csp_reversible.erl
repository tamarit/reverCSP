-module(csp_reversible).

-export([run/1, run/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(File) -> 
	run(File,'MAIN').

run(File, FirstProcess) -> 
	ok.