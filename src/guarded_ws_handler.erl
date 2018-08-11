%
%% Define module properties
%
-module(guarded_ws_handler).

%
%% Make functions public
%
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

%
%% Entry function for a new socket
%
init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

%
%% Mix up socket on connection
%
websocket_init(State) ->
  {ok, State}.

%
%% Handle text messages
%
websocket_handle({text, Msg}, State) ->
  {reply, {text, << "message: ", Msg/binary >>}, State};

%
%% Handle any other data
%
websocket_handle(_Data, State) ->
  {ok, State}.

%
%% Handle timeout info message
%
websocket_info({timeout, _Ref, Msg}, State) ->
  {reply, {text, Msg}, State};

%
%% Handle other info messages
%
websocket_info(_Info, State) ->
  {ok, State}.
