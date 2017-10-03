## Requirements

You need to install `stack` first. 

## Starting the server

Simply run `./start.sh`. This command will compile and start your project.

## Starting the client

There is no client :stuck_out_tongue:. But we have telnet:

```bash
franco@xinaiu:~/tmp/ping-pong-hs$ telnet localhost 3500
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
read

set:hello
hello
read
hello
set:world
world
read
world
clear

read

set:persistent
persistent
quit
Connection closed by foreign host.
franco@xinaiu:~/tmp/ping-pong-hs$ telnet localhost 3500
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
read
persistent
```

As you can see, data is persistent thanks to `STM` and `IO`

## The goal

Our server is quite limited, it is a table of just one unnamed slot. We want to extend it so that it works with multiple slots and has richer operations:

```bash
franco@xinaiu:~/tmp/ping-pong-hs$ telnet localhost 3500
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
schema:name:surname:house
<OK>
read:name

set:name:jon
jon
set:lastname:snow
snow
set:house:stark
stark
clear:lastname

set:lastname:spoiler
spoiler
clear_all
read:lastname

read:name

read:house

set:name:arya
arya
reverse:name
ayra
upcase:name
AYRA
```

The goal of this project is to extend this code so that the previous commands work.

## Tips

* Read this: https://www.reddit.com/r/haskell/comments/25xyts/how_about_an_stm_hash_table_implementation/. 
* You can implement the table as an inmutable list of string keys and (mutable) `TVar` values.
* The `schema` operation may destroy all data.
* don't try to change `Main` and `Server` alot. Intead, focus on the `Rehs` module, which declares its transactional primitive functions, and `Commands` module, which parses strings into transactions.
* notice that `Rehs.Server` and `Rehs.IO` actually deal with `IO`, but other modules just work with `STM` and pure functions. If you start to write many `IO` functions, you are probably doing it wrong.

## Going further

Make the table schema-less. You can use use `TArray`'s if you want.


