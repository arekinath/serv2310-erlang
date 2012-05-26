Basic Erlang implementation of the csse2310 (2012) trivia 'serv' application.

## Building/using

First, install Erlang, then:

    $ make
    $ ./serv
    Usage: serv round_time minplayers maxplayers port qfile

Use it the way the spec says, mostly.

## Spec deviations/notes

Currently known spec deviations or corner cases:

 * We don't run through the question file linearly every time. Games are a set number of questions long (see src/app.erl) and questions are chosen randomly.
 * Disconnected clients are not shown in the "C"/"correct?" list
 * Long player names cause a '$' error
 * Failing to supply a player name in 15sec after connecting causes a '$' error
 * No attempt is made to support multi-port mode
