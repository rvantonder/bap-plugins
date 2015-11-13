rm -rf analysis-test-system-*

# bap --use-ida --dump-symbols=/tmp/bap/tests/tests/test1.syms -dbir ../minos/binary/test-system/test1 > /tmp/bap/tests/tests/test1.bir
# bap --no-byteweight -ldriver.plugin binary/test-system/test1 --driver-check=system --driver-out_dir=analysis-test-system-1 --driver-srcs=configs/n-1-@system/srcs.txt --driver-sinks=configs/n-1-@system/sinks.txt --syms=/tmp/bap/tests/tests/test1.syms

# bap --use-ida idaq64 --no-byteweight -ldriver.plugin binary/test-system/test4 --driver-check=system --driver-out_dir=analysis-test-system-1 --driver-srcs=configs/n-1-@system/srcs.txt --driver-sinks=configs/n-1-@system/sinks.txt
bap --use-ida idaq64 --no-byteweight -ldriver.plugin binary/test-system/test8 --driver-check=system --driver-out_dir=analysis-test-system-1 --driver-srcs=configs/n-1-@system/srcs.txt --driver-sinks=configs/n-1-@system/sinks.txt
# bap --use-ida idaq64 --no-byteweight -ldriver.plugin binary/test-system/test2 --driver-check=system --driver-out_dir=analysis-test-system-2 --driver-srcs=configs/n-1-@system/srcs.txt --driver-sinks=configs/n-1-@system/sinks.txt
# bap --use-ida idaq64 --no-byteweight -ldriver.plugin binary/test-system/test3 --driver-check=system --driver-out_dir=analysis-test-system-3 --driver-srcs=configs/n-1-@system/srcs.txt --driver-sinks=configs/n-1-@system/sinks.txt
# bap --use-ida idaq64 --no-byteweight -ldriver.plugin binary/test-system/test4 --driver-check=system --driver-out_dir=analysis-test-system-4 --driver-srcs=configs/n-1-@system/srcs.txt --driver-sinks=configs/n-1-@system/sinks.txt
# bap --use-ida idaq64 --no-byteweight -ldriver.plugin binary/test-system/test5 --driver-check=system --driver-out_dir=analysis-test-system-5 --driver-srcs=configs/n-1-@system/srcs.txt --driver-sinks=configs/n-1-@system/sinks.txt
