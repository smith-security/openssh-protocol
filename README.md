# openssh-protocol

Haskell implementation of openssh protocol primitives.

The openssh primitives are defined in [RFC4251](https://www.ietf.org/rfc/rfc4251.txt).

They are used by various parts of the openssh toolchain:
 - [ssh](https://github.com/openssh/openssh-portable/blob/master/PROTOCOL)
 - [certificates](https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.certkey)
 - [agent](https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.agent)
 - [certkeys](https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.certkeys)
 - [key](https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.key)
 - [krl](https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.krl)
 - [mux](https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.mux)

### Stability

This library should be considered stable. Primitives will
be added but will not removed.

### Future

Right now, these are the building blocks required to implement the
higher level protocol components. They are useful, but the end goal
is to produce public implementations of all the protocol,
most importantly:
 - Key pair encode/decode.
 - Ceritificate encode/decode + signing.
 - Agent message protocol.

### Builds

[![CircleCI](https://circleci.com/gh/smith-security/openssh-protocol.svg?style=svg)](https://circleci.com/gh/smith-security/openssh-protocol)
