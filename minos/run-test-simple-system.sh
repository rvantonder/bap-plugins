rm -rf analysis-test-system-*

# reverse test
bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/test6 \
  --minos-check=system_simple \
  --minos-out_dir=analysis-test-system-6 \
  --minos-srcs=configs/n-1-@system/srcs.txt \
  --minos-sinks=configs/n-1-@system/sinks.txt

# easy conf test
# bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/easyconf \
#   --minos-check=system_simple \
#   --minos-out_dir=analysis-test-system-easyconf \
#   --minos-srcs=configs/n-1-@system/srcs.txt \
#   --minos-sinks=configs/n-1-@system/sinks.txt
