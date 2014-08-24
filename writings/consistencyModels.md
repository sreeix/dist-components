There are different consistency models that databases provide. It is possible to provide different consistency models to different clients from the same database. Also not all clients need strong consistencies in an application, so it'd be nice to support a system that provides that sort of a thing configurable at the client level.


Consistency varies in the continueum between Strong Consistency  and Eventual Consistency. Strong consistency make guarantees about how consistent the replicas will be vs. Eventual consistency where there are few guarantees about when the replicas will be consistent and what the final values will be.

Assumption:
* There are replicas
* Clients can go to either replica to request a read/write
*

Following are possible consistency settings that we can possibly have.

* Strong Consistency: All completed writes are visible for all the reads. this means that all replicas must be in sync before reads can happen or writes ensure all replicas are in sync before they complete. A lot of datastores are read heavy and may optimize for reads and choose latter scheme.
* Consistent Prefix: This guarantees that an consistently ordered sequence of writes are visible to the reader. In this case it is possible that some writes to the master are not synced to the replicas and may get older values, but the sequence of writes are always correct. So at no time users will see a score that actually did not exist and some time in the past.(Snapshot isoloation)
* Bounded staleness: This provides a guarantee that the replicas can be stale by specified number of time/number of operations. Otherwise similar to Consistent Prefix.
* Monotonic reads: Also called 'Session Guarantees'. It means that application will get to see the same values or later values. Note that this does not make any guarantees about weather the sequence is right. It like Eventually consistent, but once it's retuned a result unless new writes to the master happen and are synched same result will be returned.

* Read My writes: This guarantees that if effect of all the clients writes are visible to the client(or updates by other client, but definitely my writes).

* Eventual Consistency: Only guarantee that the replicas will be consistent in some future time. Any replica can return any value it had in the past without regard for sequence of operations.





There are different tradeoffs between the different consistency models, performance characteristics and Availability requirements. For example with Strong Consistency it is hard to make it highly available (but not impossible as CAP specifies)

Strong consistency is generally bad on performance and bad on availability.

Bounded staleness also is poor on availability because it needs to do the same protocols as Strong except that it can be delayed and it means that the avai
