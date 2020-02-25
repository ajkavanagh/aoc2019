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

    #################
    #i.G..c...e..H.p#
    ########.########
    #j.A..b...f..D.o#
    ########@########
    #k.E..a...g..B.n#
    ########.########
    #l.F..d...h..C.m#
    #################

The problem is that every new key causes a fork;  e.g. at 'f' one goes on
towards 'D', the other goes to 'say' b.

* One keeps going on the way it was going
* The other goes back

Why not have the strategy of keeping all the forks but only pushing one path
forward from that key combination?  We need to control memory by only allowing
ONE item to continue at each fork, but remember the fork.  And check that on
each subsequent move whether we've just ended up where one of the 'saves' is.
If we get back to a save; we abandon the save and continue on with that one
instead.

We could do this by storing the *other* forks in the Partial:

```haskell
data Partial = Partial { _path  :: !String  -- path is reversed
                       , _pathWord :: !Word64 -- path as bits
                       , _cost  :: !Int
                       , _coord :: !Coord
                       , _traversal :: H.HashMap Coord Cost
                       , _partials :: [Partial]
                       } deriving (Eq)
```


The only problem is if we get consumed (i.e. the new paths are different) then
we have to extract the next partial from the existing one and continue at that
point.

So we have a `Partial` and we get the next set of candidates?

Then we will get a list either with the same path or a different path (keyCode).

1. Items with the same path: sort by cost, take the first one as the head; any
   remaining: blank their `_partials` and add them to the head's `_partials`.
2. Items with a different path the current item: black their `_partials`; group
   them by their keyCode (path), sort by cost, and create a head partial and
   `_partials` by keyCode.

(that sounds hard to do). let's try an alternative.

define `HashMap KeyCode (IntPSQ Int Partial)`  -- call this the 'spares' in the
state.


If there is ONE
then we need to check to see if the path has changed.  If it has then we extract
any internal partials, and convert hat list into a new partial that 

Or some more thinking:

Main queue is a `IntPSQ Int Tracker  -- p:cost k:keycode v:Tracker`

Then `HashMap Coord (HashMap Int Int) -- k:keycode v:cost`

A Tracker is:

```haskell
data Tracker = Tracker { _keyCode :: !Int
                       , _partials :: ![Partial]  -- not sorted
		       } deriving Show
```

So we grab the lowest cost tracker.  It has a keycode and a list of partials.
The head of the partials is the cost.  We check the `Coord -> keycode -> cost`
lookup.  If that's cheaper we discard the head, and try the next one in the
list.  We repeat until this head is valid (i.e. cheaper or same as the one in
the `Coord -> keycode -> cost`.  Now we get the candidates (around this
partial).

If we have the same keyCodes in the partial, we update the `_partials` in the
tracker, get the lowest cost, update the `Coord -> keycode -> cost` map, and
then updat the Tracker on the PQ with a (potentially new cost) and the
`_partials`.

Then for the new keyCodes we need to grab the trackers (or make them) from the
PQ, and update them as well. (the seems like a lot of work!)

The idea with a tracker is to collect together the list of partials that form
the head of searching with that keyCode (path).  We only actually move one
tracker forward (the least cost) -- (option: do we keep the list sorted by
least cost?)

i.e.

1. We get the next lower cost from the main queue -> `p:cost k:keycode v:(x,y)`
2. We then look up the that key at the (x,y).  This should be the lowest cost
   Partial for that key at the location.
3. We do a set of candidates (more on how to cull them)

Then:

If we get ONE candidate back: (it'll have an XY, cost, etc, path)
A.4. get the partial at xy, keycode and compare it with our candidate; the
    lowest cost wins and gets stored back.
A.5. Add the cost, keycode, (x,y) to the main priority queue.

So, we implemented a reasonable thing and it solves the big maze in around 30
seconds.  Not brilliant, but not bad either.

# Part 2: 4 Mazes, 4 robots, shared keys

So the problem with part 2 is that it uses 4 shared mazes.

e.g.

```
#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba@#@BcIJ#
#############
#nK.L@#@G...#
#M###N#H###.#
#o#m..#i#jk.#
#############
```

i.e. there are 4 entrances with a common set of keys.  We have to find the
shortest path using all the robots.  The steps appear to be:

1. Find the 4 entrances; these become the starting positions.
2. Look at any/all of the robots for their next partial.
3. Somehow, workout which of the robots we move next, and how we track the
   4 robots as a set.  Without running out of memory. Or taking forever.

We need to alter the `Partial` to hold 4 robots; there is still the idea of
least cost, which means we only move the partial with the lowest cost AND the
robot with the lowest cost (if it can move).  If no robot can move then the
`Partial` is finished?

## Failure (again) on Part 2

So the attempt to migrate the part 1 solution (which worked, but took about 30
seconds) to part 2 with 4 robots failed.

So, I'm going to try a new approach with a graph, but this time the graph will
be at the decision points where the robot has to make a choice about which
direction to go in.

Therefore, the Nodes will be:

  * the entrance
  * a floor location where the path splits
  * a key

Doors are not stored, but are requirements to traverse an edge.  An edge (or
vertex) maps between two nodes, and the constraint is that a key is required.

So, we'll scan the map from the four locations and build up a Node/Edge map of
each corner of the map.  Then we "just" have to solve graph.  I wonder how that
will go.

Update:

So the scanning to a graph and eliminating dead-ends and corner node (nodes that
are a '.' and with only two edges) has simplified the graph to something that
looks like it is solvable.

The main issue is picking the key for the Priority Queue (PQ).  I think the best
option is to use a 5 tuple of 4 robot nodes and the keycode fo those positions.
The priority can then be the cost at that position, and then the value is the
robots positions and their histories/key-codes.  We then proceed by always
taking the cheapest cost item and moving it forwards.  We then always insert the
cheapest cost fo that keycode/node set and do the search on that basis.  The
only issue is capping the search space, which is an issue. And I'm not sure how
to do that.

## So it finally worked; but it isn't that great

The main issue is that solver for Day 18 Part 2 still does too much work at
generating options and doesn't cull the options early enough; thus it repeats
searching paths again and again.  I'll do some research on that and try to
solve it.  Because, it still takes 5 minutes on the laptop to solve it; and
I know it's been done in less than 5 seconds.
