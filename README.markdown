# DisEvSim

A simple discrete event simulator.

## Summary

DisEvSim is a very simple discrete event simulator.  It keeps a queue of events tagged by time and executes them in order until a predetermined limit is reached or the queue is empty (as an empty queue will remain empty until the end of time.  It is more efficient than simulations that run on time ticks for sparse events.  It can also be more precise as events happen to within the precision of a double (though you will always have a problem processing something ever 1/3rd or a second).

## Main Concept

The simulation occurs in the Sim monad.  The user must provide a data type for the state of the world as well as possible events.  The world state will need to have an initial state and then may change as the simulation runs.  The event list is simply a list of events, though an initial event must be supplied to occur at time 0.

Event handlers are functions of an event which may have some impact on the world.  They may query the current time, alter the state of the world, create more events, or create or destroy handlers.  Handlers must have a unique id so that they can be updated and destroyed.

The simulator will repeatedly take the next soonest event and then run each event handler with that event.  The order that event handlers are run in is not specified.

## Final Remarks

It is a very rough system, but it serves its purpose.  Please report any bugs or feature requests to drew@havenisms.com.
