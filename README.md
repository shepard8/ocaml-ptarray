# ocaml-ptarray

A persistent tree-array is an immutable indexed data-structure internally represented as a tree. As with B-Trees, each non-leaf node can have a given number of children (say k), and store k values.

![alt Example of ptarray](https://cloud.githubusercontent.com/assets/5731430/21565339/d6b2e106-ce96-11e6-8eb9-93b0b875c9ba.png)

This structure is intended to be used as an array with functional updates. This means it is not possible to add elements and each operation (set) creates a new array. Here are some of the complexities achieved with persistent tree-arrays:

Operation | Complexity
----------|-----------
Getting an element by its index | O(log^2_k(n)) <= O(k^2)
Changing value at given index | O(log^2_k(n)) <= O(k^2)
Creating from a list/array | O(n)
Creating from a function | O(n) *
Transforming to a list/array | O(n)
Iter/Map/Fold | O(n) *
Predicate (exists/for_all) | O(n) *
Sub array (not yet implemented) | O(log^2_k(n)) <= O(k^2)

Of course, These complexities have to be multiplied by the complexity of the functions applied when appropriate (marked with a star).

The parameter n is always the number of elements in the persistent tree-array. The parameter k is fixed at array creation according to the number of elements to store. The rule is to ensure that the height of the subsequent tree is always lower than the order chosen.

For example, with order 3, we do not want to store more than 26 values : 2 at the first floor, 6 at the second and 18 at the third. In more generality, we can store up to k^k values in a persistent tree-array of order k. That is, n <= k^k, which explains the simplified complexities in the table. In order to store 10 billions minus one values, we only need order 10.




