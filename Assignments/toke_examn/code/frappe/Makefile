# Generally about makefiles: https://opensource.com/article/18/8/what-how-makefile
# Erlang makefile example: https://gist.github.com/leandrosilva/866072/87c1bf000f877db06edcad04b42e7797d6107f72

compile:
	erlc -Wall -o src src/*.erl

compile_test:
	erlc -Wall -o tests src/*.erl
	erlc -Wall -o tests tests/*.erl

run_test: compile_test
	erl -noshell -pa tests -run test_frappe test_all

clean:
	@find . -type f -name "*.beam" -delete