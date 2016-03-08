<!-- -*- coding: utf-8-unix; -*- -->
% Life Beyond Relational Database
% Capital Match Team
% 2016-03-10

## Agenda

* Introduction & Motivation
* Slideware
* Software
* Future works

# Relational Model

## What's good with Relational Model?

* Really great for querying $\longrightarrow$ *SQL Rocks!*
* Conceptually simple to understand: *Everything is a Table*
* Ubiquitous

## What's wrong with Relational Model?

* Writes/updates are complex
* *Impedance Mismatch*: Lot of data is more tree-ish or graph-ish
* One single Database for everything $\longrightarrow$ *SPOF*

## Mutable State!

![One Ring to Rule Them All](images/one-ring.jpg)

# Event Sourcing

## State vs. Transitions

* RDBMS stores the **state** of the model at some point in time...
* ... But we are also interested in the **transitions** ...
* ... And deterministic state can always be reconstructed from a *sequence of transitions*.

## The Event Sourcing Model

> Event Sourcing ensures that all changes to application state are stored as a sequence of events. Not just can we query these
> events, we can also use the event log to reconstruct past states, and as a foundation to automatically adjust the state to cope
> with retroactive changes. 

> [Martin Fowler](http://martinfowler.com/eaaDev/EventSourcing.html)

## Events makes it easier to...

* Audit current state and what lead to it
* Implement global undo/redo mechanism
* Run simulations with different hypothesis over live data
* Cope with data format migrations
* Handle potentially conflicting changes[^1]

## Events & Business Model

* Events are what makes a model dynamic: What affects it, how it reacts to outside world...
* Provide foundation for [Domain Driven Design]() techniques $\longrightarrow$ Better business models, Ubiquitous language
* Lead to [Event Storming]() technique for "requirements" elicitation and business domain modelling[^2]

# In Practice

## Overview

![](images/event-sourcing.png)

## Pure Business Models

* Each model delimits a *Bounded Context*: It is responsible for a single cohesive part of the domain
* Models are **pure** immutable data structures
* Distinguish *Commands* from *Events*

## Pure Business Models (2)

* Commands compute Event from State
    ```
    act :: Command -> Model -> Event
    ```
    
* Events modify model
    ```
    apply :: Event -> Model -> Model
    ```

## Effectful Services

> Services are used to orchestrate interaction between one or more business models and the outside world

* Services are functions operating across several contexts
* They can be synchronous or asynchronous (we use mostly synchronous)[^3]
* There are no *distributed transactions*: Service has to cope with failures from each context

## Effectful Services (2)

* We have a monad to express effects and sequencing on each context: `WebStateM`
```
newtype WebStateM g l m a = WebStateM { runWebM :: TVar g -> l -> m a }
```
* `g` is a "global" Model which can be accessed concurrently $\longrightarrow$ protected in STM
* `l` is local data, contextual to a single service execution
* `m` is underlying monad, usually `IO`

## Events Storage

```
data StoredEvent s = StoredEvent { eventVersion :: EventVersion
                                 , eventType    :: EventType s 
                                 , eventDate    :: Date        
                                 , eventUser    :: UserId      
                                 , eventRequest :: Encoded Hex 
                                 , eventSHA1    :: Encoded Hex 
                                 , event        :: ByteString  
                                 }
```

## Events Storage (2)

* We use a simple Append-only file store, writing serialized events (mostly JSON) packed with metadata
* Each event has a (monotonically increasing) version which is used for proper deserialization
* Events carry useful information for troubleshooting and auditing: User who initiated the request, request id itself, SHA1
  representing version of appplication
* Events Store serializes concurrent writes 

# Software

## Code Review

![In Practice](images/workshop.jpg)

## Demo

* Anatomy of a complete business model
    * Web layer w/ servant
    * Service layer (w/ Free monads...)
    * Business model
    * Migration code
    * Standalone service
* Using Haskell scripts for operational queries and updates

# Future Works

##

![](images/monolith-2001.jpg)

## Implement Better CQRS

* Separate *Read Model* from *Write Model*
* *Write Model*: Append-only linear data store per context, very fast, minimize locking/write time
* *Read model*: Optimized for specific querying, may be relational if needed in order to make it more user-friendly

## Make models resilient

* Resilience of models $\longrightarrow$ *Replication* 
* Use [Raft](http://raft.github.io/) to maintain strong consistency of models: [several](https://github.com/cartazio/haver-raft) [implementations](https://github.com/NicolasT/kontiki) [in](https://github.com/chrisnc/tangaroa) Haskell
* Started implementation of practical cluster based on Raft, called [raptr](https://github.com/capital-match/raptr)

## Make models secure

* Turn event stream into a *source of truth* $\longrightarrow$ Blockchain[^4] and beyond...
* Juno: [Smart contracts](https://github.com/buckie/juno) over Raft cluster
  * Uses cryptographically signed events to ensure history cannot be tampered with
  * Turns journal into a "legally binding ledger"?

# Questions

## 

![](images/omg_wtf.jpg)

# Credits

* [HAL-9000](http://observationdeck.kinja.com/the-monoliths-have-faces-interstellar-answers-2001-a-1659091453)

[^1]: That's the way RDBMS handle transactional isolation: Record a *log* of all operations on data then reconcile when transactions are committed

[^2]: I never know how many `l`s modelling takes...

[^3]: Synchronicity is a property of the business domain, e.g. depends on what client expects from the service and whether or not he
wants to "pay" for synchronous confirmation

[^4]: Blockchain is all rage in the FinTech ecosystem those days, although early implementation like Bitcoins or Dogecoins failed to
deliver all their promises.
