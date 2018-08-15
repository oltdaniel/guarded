%
%% Define module properties
%
-module(guarded_ws_handler).
-behaviour(cowboy_http_websocket_handler).

%
%% Make functions public
%
-export([
  init/2, websocket_init/1, websocket_handle/2, websocket_info/2,
  terminate/2
]).

%
%% Entry function for a new socket
%
init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

%
%% Mix up socket on connection
%
websocket_init(State) ->
  Uid = generate_uid(),
  guarded_messenger:connect(Uid, self()),
  {reply, {text, << "uid:", Uid/binary>>}, State}.

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

%
%% Handle termination callback
%
terminate(_Reason, _State) ->
  guarded_messenger:disconnect(self()),
  ok.

%
%% Generate random user id
%
generate_uid() ->
  base64:encode(crypto:strong_rand_bytes(8)).
