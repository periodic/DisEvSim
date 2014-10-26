# DisEvSim

A simple discrete event simulator.

NOTE: This repository is currently under development for Version 2.  The
original Version 1 is available under the tag "v1".

## Summary

DisEvSim is a very simple discrete event simulator.  It keeps a queue of events
tagged by time and executes them in order until a predetermined limit is
reached or the queue is empty (as an empty queue will remain empty until the
end of time.  It is more efficient than simulations that run on time ticks for
sparse events.  It can also be more precise as events happen to within the
precision of a double (though you will always have a problem processing
something ever 1/3rd or a second).

## Main Concept

The simulation occurs in the Sim monad.  The user must provide a data type for
the state of the world as well as possible events.  The world state will need
to have an initial state and then may change as the simulation runs.  The event
list is simply a list of events, though an initial event must be supplied to
occur at time 0.

Event handlers are functions of an event which may have some impact on the
world.  They may query the current time, alter the state of the world, create
more events, or create or destroy handlers.  Handlers must have a unique id so
that they can be updated and removed.

The simulator will repeatedly take the next soonest event and then run each
event handler with that event.  The order that event handlers are run in is not
specified.

## Example

For example, you could run the following super-simple simulation of a counter.

```haskell
tick :: () -> Sim Integer ()
tick _ = after 1 ()

count :: () -> Sim Integer ()
count _ = modifyWorld (+1)

initialize :: Sim Integer ()
initialize = do
  _ <- registerHandler () tick
  _ <- registerHandler () count
  after 1 () -- Start it off.

runCounter :: Time -> (Time, Integer)
runCounter tMax =
    let config = defaultConfig { maxTime = Just tMax }
        (t, _, w) = simulate config 0 initialize
    in (t, w)
```

Breaking this down:

* simulate runs a simulation
* defaultConfig is configuration to pass in to the simulator, indicating that
  we want to stop after tMax time units.
* 0 is the initial state of the simulation data (also called world).
* initialize starts up the simulation by registering two handlers and creating
  an initial event.

In this case the ev type is `()` and the world type is `Int`.

    *Counter> runCounter 100
    (100.0, 100)

In the ouput you see that 100.0 is the final time and 100 is the value of the
world.  Note that there is also a log of events that can be generated, but that
is currently being ignored.

## Final Remarks

It is a very rough system, but it serves its purpose.  Please report any bugs
or feature requests through the GitHub issue tracker.
