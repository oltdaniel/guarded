# guarded :lock:

visualize a secure messaging protocol

## About

Encryption is an important part in todays world, as everyone cares about their
privacy _(some more than others)_. In order to visualize how a basic messaging
protocol could look like, this project had been started. It will show and
explain each mathmatical step that will be executed in order to encrypt nor
decrypt your messages.

###### This project is part of a school project

## Protocol

In order to secure the messaging protocol from external attackers _(this excludes the attack of the server owner)_, a Diffie-Hellman Key Exchange will be executed before any further messages will be send. The public parameters will be calculated by the client itself to allow full flexibility in the number selection. When this process finished, a secure tunnel has been established between the client and server, so on, an attack from the outside of this chain is not possible.


Privacy is another aspect this protocol will take care of, by using the RSA algorithm to encrypt and decrypt the messages between the users. This will allow the users to communicate privately, but not completely anonymous as the server owner can log the amount of messages sent from one specific user to another _(however, this is not the goal of this project)_.

## Documentation

The detailed documentation on the protocol can be found [here in the `/doc` directory](https://github.com/oltdaniel/guarded/tree/master/doc/README.md).

## Usage

Use the following commands in order to execute this software:

```shell
# Clone the code
$ git clone https://github.com/oltdaniel/guarded.git
$ cd guarded
# Load rebar3
$ make load-rebar3
# Start the server
$ make run
```

By visiting [http://localhost:1234/](http://127.0.0.1:1234) you will have
access to the application.

## License

_Just do what you'd like to_

[![license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/oltdaniel/guarded/blob/master/LICENSE)

#### Credit

[Daniel Oltmanns](https://github.com/oltdaniel) - creator
