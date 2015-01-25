Gossip Protocols
===================

Push: Node A sends all the {Key, Value, Version} tuples to Node B, Node B then updates it's state based on the latest values.

Pull: A digest of {Key, Version} tuples are sent to Node B, which computes the reconcile function and send back the {Key, Value, Version} for the keys that Node B has higher version.

Push Pull : Same as Pull except on Response from NodeB NodeA sends the


Also important is to fit inside an MTU so not many redundant packets are sent.

Anti Entropy vs. Rumor mongering.
---------------------------------
Anti entropy are typically used for reliable info sharing and will gossip when new information is available
Rumour Mongering gen

A key exists in multiple replicas, now the master wants to sync the state of the key between all the replica, it gossips with all the peers that have this key to reconcile the value of this key.

But there are a lot of keys that need to be reconciled by gossip. there are a few problems

* Large message sizes for transmission across to the nodes
* Large number of nodes/computations that needs to be made for sending the right keys to right notes


[http://bitsavers.informatik.uni-stuttgart.de/pdf/xerox/parc/techReports/CSL-89-1_Epidemic_Algorithms_for_Replicated_Database_Maintenance.pdf](Epidemic Algorithms for Database maintenance)
