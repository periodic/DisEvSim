# DisEvSim

A simple discrete event simulator.

## Summary

DisEvSim is a very simple discrete event simulator.  It keeps a queue of events tagged by time and executes them in order until a predetermined limit is reached or the queue is empty (as an empty queue will remain empty until the end of time.  It is more efficient than simulations that run on time ticks for sparse events.  It can also be more precise as events happen to within the precision of a double (though you will always have a problem processing something ever 1/3rd or a second).

## Main Concept

The simulation occurs in the Sim monad.  The user must provide a data type for the state of the world as well as possible events.  The world state will need to have an initial state and then may change as the simulation runs.  The event list is simply a list of events, though an initial event must be supplied to occur at time 0.

Event handlers are functions of an event which may have some impact on the world.  They may query the current time, alter the state of the world, create more events, or create or destroy handlers.  Handlers must have a unique id so that they can be updated and destroyed.

The simulator will repeatedly take the next soonest event and then run each event handler with that event.  The order that event handlers are run in is not specified.

## Example

For example, you could run the following super-simple simulation of a counter.

    simulate defaultConfig 0 [("tick", \_ -> after 1 ()), ("counter", \_ -> modW (+1))] () 100

Breaking this down:
* simulate runs a simulation
* defaultConfig is configuration to pass in to the simulator, which currently just includes whether to turn logging of all events on or off (useful for debugging)
* 0 is the initial state of the world
* The next argument is a list of handlers.  The first one is named "tick" and on each event it generates a new event in 1 time-unit (In this simulation our events are the unit-type ()).  The second one is named "counter" and simply changes the state of the world by increasing it by one every time an event occurs.
* Then we supply an initial event, in this case (), but I often have a StartSim event.
* And finally the maximum amount of time to run (since this simulation never terminates).

    *DisEvSim> simulate defaultConfig 0 [("tick", \_ -> after 1 ()), ("counter", \_ -> modW (+1))] () 100
    (100.0,[(0.0,()),(1.0,()),(2.0,()),(3.0,()),(4.0,()),(5.0,()),(6.0,()),(7.0,()),(8.0,()),(9.0,()),(10.0,()),(11.0,()),(12.0,()),(13.0,()),(14.0,()),(15.0,()),(16.0,()),(17.0,()),(18.0,()),(19.0,()),(20.0,()),(21.0,()),(22.0,()),(23.0,()),(24.0,()),(25.0,()),(26.0,()),(27.0,()),(28.0,()),(29.0,()),(30.0,()),(31.0,()),(32.0,()),(33.0,()),(34.0,()),(35.0,()),(36.0,()),(37.0,()),(38.0,()),(39.0,()),(40.0,()),(41.0,()),(42.0,()),(43.0,()),(44.0,()),(45.0,()),(46.0,()),(47.0,()),(48.0,()),(49.0,()),(50.0,()),(51.0,()),(52.0,()),(53.0,()),(54.0,()),(55.0,()),(56.0,()),(57.0,()),(58.0,()),(59.0,()),(60.0,()),(61.0,()),(62.0,()),(63.0,()),(64.0,()),(65.0,()),(66.0,()),(67.0,()),(68.0,()),(69.0,()),(70.0,()),(71.0,()),(72.0,()),(73.0,()),(74.0,()),(75.0,()),(76.0,()),(77.0,()),(78.0,()),(79.0,()),(80.0,()),(81.0,()),(82.0,()),(83.0,()),(84.0,()),(85.0,()),(86.0,()),(87.0,()),(88.0,()),(89.0,()),(90.0,()),(91.0,()),(92.0,()),(93.0,()),(94.0,()),(95.0,()),(96.0,()),(97.0,()),(98.0,()),(99.0,()),(100.0,())],101)

    *DisEvSim> simulate defaultConfig{enableLog = False} 0 [("tick", \_ -> after 1 ()), ("counter", \_ -> modW (+1))] () 100
    (100.0,[],101)

In the ouput you see that 100.0 is the final time, [] is the log of events, and 101 is the value of the world.

## Final Remarks

It is a very rough system, but it serves its purpose.  Please report any bugs or feature requests to drew.haven@gmail.com
