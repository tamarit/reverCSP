ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
ERLC_DIR = $(shell which erlc)
ERLC_PATH = $(shell dirname $(lastword $(ERLC_DIR)))

compile:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -o ebin src/*.erl 
	@cd csp_tracker; make

track:
	@cd csp_tracker
	@make

clean:
	@rm -Rf ebin
	@rm -f *.txt

run1:
	erl -pa ebin csp_tracker/ebin -run csp_reversible run ex1.csp -noshell -s erlang halt

run2:
	erl -pa ebin csp_tracker/ebin -run csp_reversible run ex2.csp -noshell -s erlang halt

run3:
	erl -pa ebin csp_tracker/ebin -run csp_reversible run ex3.csp -noshell -s erlang halt

run4:
	erl -pa ebin csp_tracker/ebin -run csp_reversible run ex4.csp -noshell -s erlang halt

run5:
	erl -pa ebin csp_tracker/ebin -run csp_reversible run ex5.csp -noshell -s erlang halt

run6:
	erl -pa ebin csp_tracker/ebin -run csp_reversible run ex6.csp -noshell -s erlang halt

run7:
	erl -pa ebin csp_tracker/ebin -run csp_reversible run ex7.csp -noshell -s erlang halt

run8:
	erl -pa ebin csp_tracker/ebin -run csp_reversible run ex8.csp -noshell -s erlang halt

run_paper:
	erl -pa ebin csp_tracker/ebin -run csp_reversible run ex_paper.csp -noshell -s erlang halt

update_sm:
	@cd csp_tracker; git checkout master && git pull
	@git add csp_tracker
	@git commit -m "updating csp_tracker to latest"
	@git push

# install:
# 	@erl -pa ebin -run make_script from_path $(ROOT_DIR)  -noshell -s erlang halt
# 	@chmod +x pn_suite_temp
# 	@mv -f pn_suite_temp $(ERLC_PATH)/pn_suite


