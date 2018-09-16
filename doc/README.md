# guarded :lock: - Documentation

## Security

The `guarded` message protocol is based on to encryption layers: **_1)_ shared key** and **_2)_ public-key based**.
Both layers will ensure the security from external attackers, as well as of internal attackers. The first layer
will be constructed between each client and the server. This will protect against external attackers, as they
won't be able to extract any data that is sent. The second one ensures the users privacy, by protecting against
the server _(a possible internal attacker)_, by constructing an secure messaging channel between both users. An
possible attack of the internal attacker could be hard, as the users sign their messages in order to guarantee
an unmodified message.

## Protocol

The protocol for `guarded` is mainly focused on encryption techniques. It only allows the user to select
the message receiver and send messages. Other commands should be and are mostly handled by the application
itself, e.g. shared key exchanges, key publishing nor securing the actual message traffic. An example
communication between e.g. Alice and Bob could look like the following.

```
# Key exchange
-> receive the key exchange part, prime and generator of the server
<- send own key part with `/d [num]` to complete key exchange

# Start a new session
<- send `/s` to receive an uid from the server

# Publish key
<- send public-key to the server with `/k [pub] [n]`

# Open new session to the user
<- open a new session to bob with `/o bob`
<- send `/kr bob` to the server to request bob's public key
-> receive public key of bob

# Send message to bob
-> send encrypted, signed message encrypted with the shared
   key to the server by using `/p Hello bob`
```

## Commands

| command | description |
| - | - |
| `/s` | initialize the current socket with a new uid |
| `/d [num]` | publish the client part for the shared key |
| `/o [uid]` | select a new message receiver and request the public key to send further messages |
| `/p [msg]` | send a message to the previous selected message receiver |
| `/k [pub] [n]` | publish the public key and modulus parameter to the server |
| `/kr [uid]` | request the public key and modulus parameter of a specific user |
| `ping` | ping the server to keep the connection alive |

## Issues

- [ ] The used prime number for the shared key defined in the server code [`src/guarded_ws_handle.erl`](https://github.com/oltdaniel/guarded/blob/2d78e38b525ea8a0bf6911d2945a35ee41584986/src/guarded_ws_handler.erl#L21) should be generated for each connection and shouldn't be static
- [ ] The used numbers for the calculations are to small in order to be secure against bruteforce attacks, see [`src/guarded_ws_handler.erl`](https://github.com/oltdaniel/guarded/blob/2d78e38b525ea8a0bf6911d2945a35ee41584986/src/guarded_ws_handler.erl#L36) and [`priv/assets/script.js`](https://github.com/oltdaniel/guarded/blob/2d78e38b525ea8a0bf6911d2945a35ee41584986/priv/assets/script.js#L197)
