rm -rf analysis-test-system-*

# bap --use-ida --dump-symbols=/tmp/bap/tests/tests/test1.syms -dbir ../minos/binary/test-system/test1 > /tmp/bap/tests/tests/test1.bir
# bap --no-byteweight -lminos.plugin binary/test-system/test1 --minos-check=system --minos-out_dir=analysis-test-system-1 --minos-srcs=configs/n-1-@system/srcs.txt --minos-sinks=configs/n-1-@system/sinks.txt --syms=/tmp/bap/tests/tests/test1.syms

# bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/test4 --minos-check=system --minos-out_dir=analysis-test-system-1 --minos-srcs=configs/n-1-@system/srcs.txt --minos-sinks=configs/n-1-@system/sinks.txt
# bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/test8 --minos-check=system --minos-out_dir=analysis-test-system-1 --minos-srcs=configs/n-1-@system/srcs.txt --minos-sinks=configs/n-1-@system/sinks.txt
# bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/test2 --minos-check=system --minos-out_dir=analysis-test-system-2 --minos-srcs=configs/n-1-@system/srcs.txt --minos-sinks=configs/n-1-@system/sinks.txt
# bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/test3 --minos-check=system --minos-out_dir=analysis-test-system-3 --minos-srcs=configs/n-1-@system/srcs.txt --minos-sinks=configs/n-1-@system/sinks.txt
# bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/test4 --minos-check=system --minos-out_dir=analysis-test-system-4 --minos-srcs=configs/n-1-@system/srcs.txt --minos-sinks=configs/n-1-@system/sinks.txt
# bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/test5 --minos-check=system --minos-out_dir=analysis-test-system-5 --minos-srcs=configs/n-1-@system/srcs.txt --minos-sinks=configs/n-1-@system/sinks.txt


# reverse test
bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/test6 --minos-check=system --minos-out_dir=analysis-test-system-6 --minos-srcs=configs/n-1-@system/srcs.txt --minos-sinks=configs/n-1-@system/sinks.txt
