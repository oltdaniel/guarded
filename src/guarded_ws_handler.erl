%
%% Define module properties
%
-module(guarded_ws_handler).

%
%% Additional user socket information
%
-record(state, {
  uid,          % Store the socket identified
  receiver_uid, % Current session partner
  public_key,   % Public key of the socket
  shared_key,   % The shared key of the key exchange
  secret,       % Server secret selected for the key exchange
  generator     % Generator number selected for the key exchange
}).

%
%% Some crypto settings
%
-define(SHARED_PRIME, 50053).    % The prime selected for the key exchange
-define(STREAM_CIPHER, aes_ctr). % The cipher used for the stream encryption

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
  Generator = crypto:bytes_to_integer(crypto:strong_rand_bytes(1)),
  Secret = crypto:bytes_to_integer(crypto:strong_rand_bytes(2)),
  SharedKey = crypto:bytes_to_integer(crypto:mod_pow(Generator, Secret, ?SHARED_PRIME)),
  State = #state{shared_key=SharedKey, secret=Secret, generator=Generator},
  crypto:start(),
  {cowboy_websocket, Req, State}.

%
%% Initialize the socket
%
websocket_init(State) ->
  SharedKey = State#state.shared_key,
  Generator = State#state.generator,
  Msg = lists:concat(["shared:", SharedKey, " ", ?SHARED_PRIME, " ", Generator]),
  {reply, {text, Msg}, State}.

%
%% Handle new user id request
%
websocket_handle({parsed, <<"/s", _Msg/binary>>}, State) ->
  guarded_messenger:disconnect(State#state.uid),
  Uid = generate_uid(),
  State2 = State#state{uid=Uid},
  guarded_messenger:connect(Uid, self()),
  websocket_info({message, <<"uid">>, Uid}, State2);

%
%% Handle send message
%
websocket_handle({parsed, <<"/p ", Msg/binary>>}, State) ->
  Uid = State#state.uid,
  ReceiverUid = State#state.receiver_uid,
  guarded_messenger:message(Uid, ReceiverUid, Msg),
  {ok, State};

%
%% Handle new partner selection
%
websocket_handle({parsed, <<"/o ", Msg/binary>>}, State) ->
  State2 = State#state{receiver_uid = Msg},
  websocket_info({message, <<"server">>, <<"speaking to ", Msg/binary>>}, State2);

%
%% Handle key publish
%
websocket_handle({parsed, <<"/k ", Msg/binary>>}, State) ->
  State2 = State#state{public_key = Msg},
  {ok, State2};

%
%% Handle key request message
%
websocket_handle({parsed, <<"/kr ", Msg/binary>>}, State) ->
  Uid = State#state.uid,
  guarded_messenger:key_request(Uid, Msg),
  {ok, State};

%
%% handle ping message
%
websocket_handle({text, <<"ping", _Msg/binary>>}, State) ->
  {ok, State};

%
%% Handle Key Exchange message
%
websocket_handle({text, <<"/d ", Msg/binary>>}, State) ->
  ClientPart = binary_to_integer(Msg),
  SharedKey = crypto:bytes_to_integer(crypto:mod_pow(ClientPart, State#state.secret, ?SHARED_PRIME)),
  SharedKey2 = integer_to_list(SharedKey, 16),
  SharedKey3 = crypto:hash(sha256, SharedKey2),
  State2 = State#state{shared_key=SharedKey3},
  {ok, State2};

%
%% Handle general text messages
%
websocket_handle({text, Msg}, State) ->
  Decoded = base64:decode(<<Msg/binary>>),
  Msg2 = binary_to_list(Decoded),
  IVec = list_to_binary(lists:sublist(Msg2, 16)),
  Msg3 = list_to_binary(lists:sublist(Msg2, 17, byte_size(Decoded))),
  CryptoStream = crypto:stream_init(aes_ctr, State#state.shared_key, IVec),
  {_, Msg4} = crypto:stream_decrypt(CryptoStream, Msg3),
  websocket_handle({parsed, Msg4}, State);

%
%% Handle undefined messages
%
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
  Msg = << Uid/binary, " ", PublicKey/binary >>,
  guarded_messenger:message(<<"key">>, ReceiverUid, Msg),
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
