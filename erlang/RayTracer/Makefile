.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W +debug_info $<

ERL = erl -boot start_clean

MODS = colors image vectors polynoms raytracer_impl raytracer tests make

LOCAL_ERL = ${ERL} -pa '~/Code/erlang/RayTracer'

all: test

test_draw: all
	${LOCAL_ERL} -noshell -s tests test_draw -s init stop

test: doc
	${LOCAL_ERL} -noshell -s tests test -s init stop

doc: compile
	${LOCAL_ERL} -noshell -s make doc -s init stop

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump

