OPTS = -L../header -lheader --header-file=../header/libc.h ${options}
case = *
#TEST = tests/test${case}.c
TEST = test-recv-system/test-system-recv-1.c


quarantine : *.ml
		bapbuild quarantine.plugin -pkg bin_prot.syntax

header : ../header/*.ml
		make -C ../header

test : quarantine header test.ml
		@TEST_USER_OPTIONS="${OPTS}" bapbuild test.native -- quarantine $(TEST)

bap: quarantine header
		bap ${OPTS} --use-ida -lquarantine ${binary}

clean:
		bapbuild -clean
		make -C ../header clean
