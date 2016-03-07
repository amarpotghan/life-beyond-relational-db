<!-- -*- coding: utf-8-unix; -*- -->
% Life Beyond Relational Database
% Capital Match Team
% 2016-03-10

# Agenda

* Introduction/Motivation
* Slideware
* Software
* Future works

# What's wrong with Relational Model?

* Impedance Mismatch
* One single DB for everything
* Stores *state* of the model
* Writes/updates are complex

# What's good with Relational Model?

* Really great for querying
* Conceptually simple to understand
* Ubiquitous

# State vs. Transitions

* DB stores the *state* of the model at some point in time
* But we are also interested in the *transitions*
    * Auditability
    * Undo/redo
    * Simulation, alternate world

# Business Models

* Start from the business domain, aka. Ubiquitous Language
* External actions are Commands
* Result of actions are Events

# Separation of Concerns

* Bounded Context: Each business model is responsible for one part of the domain
* Commands and Events are local to each bounded context

# Orchestration with Services

* Services are functions operating across several contexts
* They can synchronous or asynchronous (we are synchronous)
* There is no distributed transactions: Service has to cope with failures from each context

# Business Models are Pure

* Commands compute Event from State
    ```
    act :: Command -> Model -> Event
    ```
* Events modify model
    ```
    apply :: Event -> Model -> Model
    ```

# Services are Effectful

* We have a monad to express effects and sequencing on each context: WebStateM 
* Contains global Model which can be updated by several threads 

# Events Data Store

* Plain Old Append-Only File 

# CQRS

* Separate Read Model from Write Model
* Write Model: Append-only linear data store per context, very fast, minimize locking/write time
* Read model: Optimized for specific querying -> maybe relational if needed

# Make models resilient

* Resilience of models  => *Replication* 
* Use [Raft](http://raft.github.io/) to maintain strong consistency of models: [several](https://github.com/cartazio/haver-raft) [implementations](https://github.com/NicolasT/kontiki) [in](https://github.com/chrisnc/tangaroa) Haskell
* Started implementation of practical cluster based on Raft, called [raptr](https://github.com/capital-match/raptr)

# Make models secure

* Turn event stream into a *source of truth* => Blockchain and beyond...
    * Juno: [Smart contracts](https://github.com/buckie/juno) over Raft cluster
    * Uses cryptographically signed events to ensure history cannot be tampered with
    * Turns journal into a "legally binding ledger"?

# Credits

* [HAL-9000](http://observationdeck.kinja.com/the-monoliths-have-faces-interstellar-answers-2001-a-1659091453)
