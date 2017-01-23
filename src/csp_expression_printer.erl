-module(csp_expression_printer).

-export([csp2string/1]).

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
csp2string({skip, SPAN}) ->
	"SKIP";
csp2string({finished_skip, _, _}) ->
	"T".

brackets(S) ->
	"(" ++ S ++ ")".