# Erlbank Transactions Self Contained System (SCS) 

Erlbank Future System

## Build

```
$ rebar3 compile
```


## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

The web-frontend is served at http://localhost:8000/

## Standalone Docker setup

```
cd .devcontainer
docker build -t flex-arvato-transactions .
```

Then start a shell in the the container with `run-docker.sh` or:

```
docker run --rm -p 8000:8000 -v PWD:/transactions -w /transactions -i -t flex-arvato-transactions bash
```

Replace `PWD` by the full path of the `monolithic` directory.


Windows:
docker run --rm -p 8000:8000 -v /mnt/d/Workspaces/Training/FLEX/flex-arvato/flex-arvato-transactions:/flex-arvato-transactions -w /flex-arvato-transactions -i -t flex-arvato-transactions bash


## Wie gestalten wir unsere Events?

### Eventstructur

siehe data.htl => record(transaction_event,

### Schnittstellen

* "Feed": get mechanismus für liste von transaction events mit start id. Required, um den Fall zu behandeln, dass der Server mal nicht da ist.

  gen_server, call (synchron) oder cast (asynchron)? => "Nicht die kriegsentscheidende Frage"

** Was ist, wenn Anfrage nicht zurückkommt?  
** Synch

* subscribe-mechanismus für transaction_event (mit callback-adresse)

  In Erlang:

** Niels schickt uns Nachricht ("subscribe", "transaction_event", "Niels_service"|Pid)

### Implementierung

Annahmen:

* Server läuft single threaded
* Namen des Consumenten brauchen wir nicht, nur Pid
* Unsubscribe wird vorerst ignoriert

TODO:

* Test accounts löschen
* 