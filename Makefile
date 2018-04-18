REBAR := ./rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3
ERL       ?= erl
DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt

.PHONY: compile test

all: compile

compile: $(REBAR)
	$(REBAR) compile

shell: $(REBAR)
	$(REBAR) shell

test: $(REBAR)
	$(REBAR) eunit -v
	# $(REBAR) ct --config rebar.test.config

clean: $(REBAR)
	$(REBAR) clean

./rebar3:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar3"}])' \
	  -s init stop
	chmod +x ./rebar3

$(DEPSOLVER_PLT):
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
		--apps erts kernel stdlib crypto public_key -r deps

dialyzer: $(DEPSOLVER_PLT)
	dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src
