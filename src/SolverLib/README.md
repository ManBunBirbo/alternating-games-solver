# Dependency Graph Library 

A F# library for the dependency graph class. 

A dependency graph is implemented with a lot of inspiration from LiuSmolka
paper. 


## Implementation: Object-oriented and iterative 

Object-oriented and iterative concepts are used in this library. This is most
clearly seen with arrays being used in stead of maps. There are two
reasons for this: 

- Space constraints: Arrays are very compact and large graphs can thus be
  represented. 
- Run time: Arrays have very fast look-up and insert speeds. 

The arrays work like so: For a graph with n vertices, each vertex must have a
unique integer ID between 0 and (n-1). The hyper edges from a given node are
listed in the array at that index. 

For example: For a graph G, the node v's hyper edges can be found at G.[v].  

A hyper edge is a simple linked list. The list of all hyper edges for a given
node is a linked list of hyper edges. 
