-----------------------------------------------------------------------------------------------
# Clusters

A _cluster_ is a set of computers that generally operate as if they were one computer. 

It is assumed that none of the systems of any cluster will have a console (screen display,
keyboard, pointing device, sound and speakers, etc.), since that would defeat the purpose of
clustering. 

.....




-----------------------------------------------------------------------------------------------
## Loose and Tight {#loti}

In AdaOS, cluster can be one of two kinds: a _loose cluster_; a _tight cluster_. 


### Effective System

In AdaOS, an [effective system](../intro/intro.md#effsys) can be one of:

 * a single system (in essence, one computer); 

 * a loose cluster of one or more effective systems (none of which can itself be a loose
   cluster); 

 * a tight cluster of one or more systems (individual computers). 

The systems or effective systems that make up a cluster are the cluster's _members_. 

All the members of a cluster share the same set of services. If the service resides on
the same system, it will be accessed directly, otherwise it will be accessed indirectly by
means of a [service proxy](../nyota/proxies.md). 

Thus, every service will have a proxy for it on every member except the one which hosts that
service. 


### Loose Cluster

A _loose cluster_ is a set of effective systems which can generally be used as if it were one
effective system. 

A member of a loose cluster cannot be another loose cluster (but it can be a tight cluster). In
other words, a loose cluster cannot contain other loose clusters, but it can contain tight
clusters. 

The purpose of a loose cluster is administrative convenience. 

Most of the time, users and application software is not interested in the internal structure or
resource divisions of a loose cluster, but instead can simply refer to the loose cluster as a
single entity. 

.....


### Tight Cluster

A tight cluster comprises one or more systems (individual computers) that all have compatible
architecture with respect to each other. 

Any two systems have _compatible architecture_ if: 

 * they have the same processor model (or, at least, models that have the exact same set of
   architectural features, instruction set, registers, and so on, so that any machine code that
   runs on one will run on the other); 

 * the same memory, in terms of cache structure and sizes, main memory size, and (preferably)
   the same speed at every level (so that software running on one computer can run on the other
   without a risk of failing due to differences in capacity or speed); 

 * they all have the same [system configuration](?????); 

 * they are able to access the same set of services, directly or indirectly (so that software
   running on one can run on the other accessing all the things it needs via services). 

The purpose of a tight cluster is _load balancing_. 

.....












-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}










