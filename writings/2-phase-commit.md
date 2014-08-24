2 Phase Commit
==============

It's a simple protocol to ensure that the database is consistent in all replicas.

### Actors

* Client: Sends the transaction to the coordinator
* Coordinator: One of the selected servers acts as a coordinator
* Transaction Manager(TM): Each replica has a transaction manager that is responsible for commiting the data

### Why?
If all the replicas commit independently then there are chances that the data on some replicas are different, in case one(or few) replicas fails. For consistency the transactions commits need to be coordinated. Failure on one replica should abort the transaction for all the replicas.

### Protocol
It's a simple state machine where the Coordinator sends a Ready to commit message to all Transaction Managers. TM's run the transaction to the point before commit, and see if there are failures(issues with storage/FS too busy or whatever).
* If there are no failures each TM will send a ready message to the coordinator. And Coordinator will ask all TM's to commit if all TM's are ready.
* If even one TM responds with a failure(or timeouts) the Coordinator will issue an abort transaction to the TM's.

States at coordinator
* Working - Client issues a commit and Coordinator issues a ready to all replicas
* Prepared - Receives responses from TMs or timesout
* Commited - If all TM respond with success
* Aborted - If even one TM responds with a failure(or timeout)


###Problems
* It is a synchronous/blocking protocl. All replicas need to respond and transaction cannot be commited till all the replicas respond(or timeout)
* Failure of a single node can abort the protocol
* Coordinator failure is a SPOF. and leads to strange anomolies.(though there are ways around it)


### Alternatives.
* Paxos Commit
