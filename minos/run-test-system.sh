rm -rf analysis-test-system-*

# reverse test
bap --use-ida idaq64 --no-byteweight -lminos.plugin binary/test-system/easyconf \
  --minos-check=system \
  --minos-out_dir=analysis-test-system-easyconf \
  --minos-srcs=configs/n-1-@system/srcs.txt \
  --minos-sinks=configs/n-1-@system/sinks.txt
