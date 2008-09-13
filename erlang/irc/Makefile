.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W +debug_info $<

ERL = erl -boot start_clean

MODS = gui client room chat make utils bict tests

LOCAL_ERL = ${ERL} -pa '~/Code/erlang/irc'

SERVER = 'gandalf@localhost'

all: doc test

local_test: all
	${LOCAL_ERL} -s make local_test

remote_test : all start_server start_test_clients

kill_server : all
	${LOCAL_ERL} -noinput -sname balrog -s make kill ${SERVER} -s init stop &

start_server : kill_server
	${LOCAL_ERL} -noinput -sname ${SERVER} -s make start_server &

start_test_clients : all
	${LOCAL_ERL} -sname 'bilbo@localhost' -s make start_test_clients ${SERVER}

doc: compile
	${LOCAL_ERL} -noshell -s make doc -s init stop

test: compile
	${LOCAL_ERL} -noshell -s tests test -s init stop

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump

