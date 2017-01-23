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

run:
	erl -pa ebin csp_tracker/ebin -run csp_reversible run ex.csp -noshell -s erlang halt

update_sm:
	@cd csp_tracker; git checkout master && git pull
	@git add csp_tracker
	@git commit -m "updating csp_tracker to latest"
	@git push

# install:
# 	@erl -pa ebin -run make_script from_path $(ROOT_DIR)  -noshell -s erlang halt
# 	@chmod +x pn_suite_temp
# 	@mv -f pn_suite_temp $(ERLC_PATH)/pn_suite


