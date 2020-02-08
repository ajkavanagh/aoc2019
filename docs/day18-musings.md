# Trying to work out how to get it working

Using the shortest path algorithm (Floyd Warshall) this gives things such as

aoCb 38 -- path aoCb cost of 38.

But we can't use that path as it as a C and we don't have a c yet.  So if we
split it at C then we have two paths ao and Cb.  But we know that we need to
get to c, so a strategy could be:

1. split aoCb to ao Cb
2. keep oa and make a path to o->c
3. Let's say this path a-c is oOjc
4. We now have a path aoOjc
5. So now we need shortest paths from c.
6. Select the shortest, and proceed again to check it.

While exploring the path, we have to track the best cost for a path so far.
Can we do something with storing paths?  This is so we can cull things
that we've already explored.  We CAN cull all the paths that go to a door that
we don't have a key for.  e.g.

abDc and abDf can be culled to abD as we have we will have ab and then all the
shortest paths from b.  i.e. we ignore all the other paths from D as we can't
reach them.

