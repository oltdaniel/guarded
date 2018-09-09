%
%% Define module properties
%
-module(guarded_ws_handler).
% -behaviour(cowboy_http_websocket_handler).

-record(state, {uid, receiver_uid, public_key, shared_key, secret, prime, generator}).

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
  Prime = 50053,
  Generator = crypto:bytes_to_integer(crypto:strong_rand_bytes(1)),
  Secret = crypto:bytes_to_integer(crypto:strong_rand_bytes(2)),
  SharedKey = crypto:bytes_to_integer(crypto:mod_pow(Generator, Secret, Prime)),
  State = #state{shared_key=SharedKey, secret=Secret, prime=Prime, generator=Generator},
  crypto:start(),
  {cowboy_websocket, Req, State}.

%
%% Mix up socket on connection
%
websocket_init(State) ->
  SharedKey = State#state.shared_key,
  Prime = State#state.prime,
  Generator = State#state.generator,
  Msg = lists:concat(["shared:", SharedKey, " ", Prime, " ", Generator]),
  {reply, {text, Msg}, State}.

%
%% Handle text messages
%
websocket_handle({parsed, <<"/s", _Msg/binary>>}, State) ->
  guarded_messenger:disconnect(State#state.uid),
  Uid = generate_uid(),
  State2 = State#state{uid=Uid},
  guarded_messenger:connect(Uid, self()),
  websocket_info({message, <<"uid">>, Uid}, State2);

websocket_handle({parsed, <<"/p ", Msg/binary>>}, State) ->
  Uid = State#state.uid,
  ReceiverUid = State#state.receiver_uid,
  guarded_messenger:message(Uid, ReceiverUid, Msg),
  {ok, State};

websocket_handle({parsed, <<"/o ", Msg/binary>>}, State) ->
  State2 = State#state{receiver_uid = Msg},
  websocket_info({message, <<"server">>, <<"speaking to ", Msg/binary>>}, State2);

websocket_handle({parsed, <<"/k ", Msg/binary>>}, State) ->
  State2 = State#state{public_key = Msg},
  {ok, State2};

websocket_handle({parsed, <<"/kr ", Msg/binary>>}, State) ->
  Uid = State#state.uid,
  guarded_messenger:key_request(Uid, Msg),
  {ok, State};

websocket_handle({text, <<"ping", _Msg/binary>>}, State) ->
  {ok, State};

websocket_handle({text, <<"/d ", Msg/binary>>}, State) ->
  SharedKey = crypto:bytes_to_integer(crypto:mod_pow(binary_to_integer(Msg), State#state.secret, State#state.prime)),
  SharedKey2 = crypto:hash(sha256, integer_to_list(SharedKey, 16)),
  State2 = State#state{shared_key=SharedKey2},
  {ok, State2};

websocket_handle({text, Msg}, State) ->
  Decoded = base64:decode(<<Msg/binary>>),
  Msg2 = binary_to_list(Decoded),
  IVec = list_to_binary(lists:sublist(Msg2, 16)),
  Msg3 = list_to_binary(lists:sublist(Msg2, 17, byte_size(Decoded))),
  CryptoStream = crypto:stream_init(aes_ctr, State#state.shared_key, IVec),
  {_, Msg4} = crypto:stream_decrypt(CryptoStream, Msg3),
  websocket_handle({parsed, Msg4}, State);

websocket_handle(_Msg, State) ->
  websocket_info({message, <<"server">>, <<"undefined command">>}, State),
  {ok, State};

%
%% Handle any other data
%
websocket_handle(_Data, State) ->
  {ok, State}.

%
%% Handle message info message
%
websocket_info({message, Uid, Msg}, State) ->
  Msg2 = << Uid/binary, ": ", Msg/binary >>,
  IVec = crypto:strong_rand_bytes(16),
  CryptoStream = crypto:stream_init(aes_ctr, State#state.shared_key, IVec),
  {_, Msg3} = crypto:stream_encrypt(CryptoStream, Msg2),
  Msg4 = base64:encode(<<IVec/binary, Msg3/binary>>),
  {reply, {text, <<Msg4/binary>>}, State};

%
%% Handle RSA public key request
%
websocket_info({key_request, ReceiverUid}, State) ->
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
terminate(_Reason, State) ->
  guarded_messenger:disconnect(State#state.uid),
  ok.

%
%% Generate random user id
%
generate_uid() ->
  base64:encode(crypto:strong_rand_bytes(8)).
