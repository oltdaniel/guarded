# Define default task
default: run

# Execute server
run:
	./rebar3 run

# Simplify rebar3 download
load-rebar3:
	wget -nc https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

# Clean up the project
clean:
	rm -r _build

.PHONY: clean
