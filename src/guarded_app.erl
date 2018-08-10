%
%% Define module properties
%
-module(guarded_app).
-behaviour(application).

%
%% Make functions public
%
-export([start/2, stop/1]).

%
%% Entry function for the application
%
start(_Type, _Args) ->
  % Construct a new cowboy router
  Router = cowboy_router:compile([ {
    '_',
    [
     {"/", cowboy_static, {priv_file, guarded, "index.html"} },
     {"/assets/[...]", cowboy_static, {priv_dir, guarded, "assets/" } }
    ]
   } ]),
  % Fire up a new cowboy http instance
  {ok, _} = cowboy:start_clear(guarded_http, [ {port, 1234} ], #{
    env => #{ dispatch => Router}
  }),
  % Start application supervisor
  guarded_sup:start_link().

%
%% Placeholder for application stop
%
stop(_State) ->
  % Nothing needs to be shut down securly
  ok.
