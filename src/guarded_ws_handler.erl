%
%% Define module properties
%
-module(guarded_ws_handler).
% -behaviour(cowboy_http_websocket_handler).

-record(state, {uid, receiver_uid, public_key}).

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
init(Req, _Opts) ->
  Uid = generate_uid(),
  {cowboy_websocket, Req, #state{uid=Uid}, #{timeout=>120000}}.

%
%% Mix up socket on connection
%
websocket_init(State) ->
  Uid = State#state.uid,
  guarded_messenger:connect(Uid, self()),
  {reply, {text, << "uid:", Uid/binary>>}, State}.

%
%% Handle text messages
%
websocket_handle({text, <<"/p ", Msg/binary>>}, State) ->
  Uid = State#state.uid,
  ReceiverUid = State#state.receiver_uid,
  guarded_messenger:message(Uid, ReceiverUid, Msg),
  {ok, State};

websocket_handle({text, <<"/o ", Msg/binary>>}, State) ->
  State2 = State#state{receiver_uid = Msg},
  {reply, {text, <<"server:speaking to ", Msg/binary>>}, State2};

websocket_handle({text, <<"/k ", Msg/binary>>}, State) ->
  State2 = State#state{public_key = Msg},
  {ok, State2};

websocket_handle({text, <<"/kr ", Msg/binary>>}, State) ->
  Uid = State#state.uid,
  guarded_messenger:key_request(Uid, Msg),
  {ok, State};

websocket_handle({text, <<"ping", _Msg/binary>>}, State) ->
  {ok, State};

websocket_handle({text, _Msg}, State) ->
  {reply, {text, <<"server:I do not understand your message">>}, State};

%
%% Handle any other data
%
websocket_handle(_Data, State) ->
  {ok, State}.

%
%% Handle message info message
%
websocket_info({message, Uid, Msg}, State) ->
  {reply, {text, << Uid/binary, ": ", Msg/binary >>}, State};

%
%% Handle RSA public key request
%
websocket_info({key_request,  ReceiverUid}, State) ->
  Uid = State#state.uid,
  PublicKey = State#state.public_key,
  guarded_messenger:message(<<"key">>, ReceiverUid, << Uid/binary, " ", PublicKey/binary >>),
  {ok, State};

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
