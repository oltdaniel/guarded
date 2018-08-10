STYLE_SRC := priv/src/style.sass
STYLE_DIST := priv/assets/style.css

# Define default task
default: run

# Execute server
run:
	./rebar3 run

# Simplify rebar3 download
load-rebar3:
	wget -nc https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

# Compile sass
sass.compile:
	sass $(STYLE_SRC):$(STYLE_DIST) --style compressed

# Watch and compile sass
sass.watch:
	sass --watch $(STYLE_SRC):$(STYLE_DIST) --style compressed

# Clean up the project
clean:
	rm -r _build

.PHONY: clean
