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

%
%% Start the supervisor for the application
%
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%
%% Initialize placeholder
%
init([]) ->
  {ok, { {one_for_one, 5, 10}, [
    {guarded_messenger, {
      guarded_messenger, start_link, []
    }, permanent, 5000, worker, []}
  ]} }.
