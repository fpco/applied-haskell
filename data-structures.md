# Data Structures

*You mean Haskell has things besides lists???*

It's important to focus on data structures, since getting your data
right is step 1 to great Haskell code.

## Generic data structures

There are three basic flavors to the data structures

* __Sequences__: list, vector, ByteString, Text, and Seq (not covered)
    * Order of values matters
    * Can have multiple copies of the same value
* __Maps__: Map, HashMap, IntMap
    * Map a key to a value
    * Aka, dictionary
    * Each key only appears once
* __Sets__: Set, HashSet, IntSet
    * Map without values
    * Like a sequence, but (1) order undefined and (2) no duplicates

## API design

* Each of these families has very similar APIs (though not quite
  identical).
* Learning one will help you master the others.
    * Yes, this means you already know most of the vector, ByteString,
      and Text APIs.
* The Map and Set APIs are also very similar.
* APIs are designed to be imported qualified (lots of conflicting
  names)
* Later: we'll look at mono-traversable for typeclasses to unify these APIs.

## Laziness

* Lists are fully lazy
* Lazy `ByteString` and `Text` still have strict chunks
* Boxed vectors are spine strict
* `Map` has strict and lazy modules, different impacts on values
* Unboxed and storable vectors are fullhy strict.

Impacts: undefined, infinite structures, performance, memory usage.

## Some caveats

* Unlike the other sequences, ByteString and Text are monomorphic
* There are three common flavors of Vector, we'll cover the
  differences later
* IntMap and IntSet requires Int keys
* Some of the structures have constraints on keys or values

There are also ByteString and Text builders for efficient
construction.

### Quiz: Pick the data structure

* Names of all students in a class
* Names and grades of all students in a class
* All the prime numbers
* The first 1000 prime numbers
* Whitelist of allowed email addresses to access a resource

We'll come back to this quiz after we explore the data types in more
detail.
