TESTS
=====

1. server1.c calls system with recv's buffer
2. server2.c calls system on a constant string
3. server3.c calls system with recv's buffer after it passes through snprintf

Why doesn't taint continue through snprintf in server3? The argument to
snprintf is the same as the tainted one on the stack that is loaded for
printf/sendto. The argument to snprintf should at least be tainted (like
the strcpy example below).

4. server4.c calls system with recv's buffer after it passes through strdup
5. server5.c calls system with recv's buffer after it goes through strcpy

For strcpy, it's the same as snprintf: we don't see taint continue after
being copied to the destination buffer. However, we do see the argument passed
to strcpy is tainted (which we don't see for snprintf)
