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
-> send prime, generator and key exchange part to the server
<- receive the key exchange part of the server

# Publish key
-> send public-key to the server

# Open new session to the user
/o bob
-> send `/kr bob` to the server to request bob's public key
<- receive public key of bob

# Send message to bob
/p Hello bob
-> send encrypted, signed message encrypted with the shared
   key to the server
```

## Commands

| command | description |
| - | - |
| `/o [uid]` | select a new message receiver and request the public key to send further messages |
| `/p [msg]` | send a message to the previous selected message receiver |
| `/k [pub] [n]` | publish the public key and modulus parameter to the server |
| `/kr [uid]` | request the public key and modulus parameter of a specific user |
| `ping` | ping the server to keep the connection alive |

## Issues

###### TODO: List issues
