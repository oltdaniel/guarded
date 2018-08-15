%
%% Define module properties
%
-module(guarded_messenger).
-behaviour(gen_server).

%
%% Make functions public
%
-export([start_link/0, connect/2, disconnect/1, message/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%
%% External api functions
%
connect(Uid, Pid) ->
  gen_server:cast(?MODULE, {connect, Uid, Pid}).

disconnect(Uid) ->
  gen_server:cast(?MODULE, {disconnect, Uid}).

message(Uid, ReceiverUid, Msg) ->
  gen_server:cast(?MODULE, {message, Uid, ReceiverUid, Msg}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, dict:new()}.

%
%% Gen server callbacks
%
handle_cast({connect, Uid, Pid}, Users) ->
  NewUsers = dict:append(Uid, Pid, Users),
  {noreply, NewUsers};

handle_cast({disconnect, Uid}, Users) ->
  NewUsers = dict:erase(Uid, Users),
  {noreply, NewUsers};

handle_cast({message, Uid, ReceiverUid, Msg}, Users) ->
  UserTemp = dict:find(ReceiverUid, Users),
  case UserTemp of
    {ok, [User|_]} ->
      User ! {message, Uid, Msg};
    _ ->
      ok
  end,
  {noreply, Users};

%
%% Gen server placeholders
%
handle_cast(_Msg, State) -> {noreply, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
