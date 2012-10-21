rebuild: clean compile generate

generate:
	rm -rf ./rel/search_engine
	./rebar generate

clean:
	./rebar clean

compile:
	./rebar compile
