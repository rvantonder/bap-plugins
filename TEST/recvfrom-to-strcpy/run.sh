TEST_DIR=/tmp/bap/TEST
BINARY=$1

# With highlighting
# bap --emit-attr={fore,back}ground -dbir -lheader \
# --header-file=libc.h -lquarantine \
# $TEST_DIR/$BINARY \
# --syms=$TEST_DIR/$BINARY.syms


# No highlighting
bap -lheader \
--header-file=libc.h -lquarantine \
$TEST_DIR/$BINARY \
--syms=$TEST_DIR/$BINARY.syms
