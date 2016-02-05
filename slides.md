<!-- -*- coding: utf-8-unix; -*- -->
% Life Beyond Relational Database
% Capital Match Team
% 2016-03-10


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

* 

# Services are Effectful

* We have a monad to express effects and sequencing on each context

# Events Data Store

* Plain Old Append-Only File 

# CQRS

* Separate Read Model from Write Model
* Write Model: Append-only linear data store per context, very fast, minimize locking/write time
* Read model: Optimized for specific querying -> maybe relational if needed

