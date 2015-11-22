make

mkdir -p /tmp/bap/TEST
DUMP_DIR=/tmp/bap/TEST
BINARY=$1

bap --use-ida idaq64 --dump-symbols=$DUMP_DIR/$1.syms -dbir > \
$DUMP_DIR/$1.bir $BINARY

# copy the binary there too
cp $BINARY $DUMP_DIR
