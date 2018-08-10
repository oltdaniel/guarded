%
%% Define module properties
%
-module(guarded_sup).
-behaviour(supervisor).

%
%% Make functions public
%
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%
%% Start the supervisor for the application
%
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%
%% Initialize placeholder
%
init([]) ->
  {ok, { {one_for_all, 0, 1}, []} }.
