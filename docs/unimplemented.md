# Unimplemented features

This document lists features that are:
1) Not implemented currently.
2) Unplanned for unknown period of time.

For planned features, look at github project pages,
it contains several roadmaps.

* Complex numbers
* Reflection and `unsafe`
* Struct field tags (field associated strings)
* [1] Channels (along with `close`, `select` and other related features)
* [1] `go` statements
* Init functions
* [Interface-to-interface type assertions](https://github.com/Quasilyte/goism/issues/110)

[1] See [concurrency and multithreading](https://github.com/Quasilyte/goism/issues/52).

Features described here *may* be implemented one day,
but that day may be very far away from today.

If you really miss something from that list,
you can do the following to fix that:
1. Create an issue (if it is not already exist);
2. Explain why do you need it that badly. Code examples are appreciated;
3. Propose possible implementation;
4. Write tests for it. They should pass along with existing tests;

The process details may change in future, but the main thing will remain the same:
the implementation is up to you.
