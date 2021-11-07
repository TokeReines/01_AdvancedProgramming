# Generally about makefiles: https://opensource.com/article/18/8/what-how-makefile
# Erlang makefile example: https://gist.github.com/leandrosilva/866072/87c1bf000f877db06edcad04b42e7797d6107f72

compile:
	erlc -Wall -o part2/src part2/src/*.erl

compile_test:
	erlc -Wall -o part2/tests part2/src/*.erl
	erlc -Wall -o part2/tests part2/tests/*.erl

run_test: compile_test
	erl -noshell -pa part2/tests -run test_emoji test_all

clean:
	@find . -type f -name "*.beam" -delete