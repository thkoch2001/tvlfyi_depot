These tests are disabeld primarily because the DummyStore used for
tests does not interact with real files on disk at the moment, but the
tests expect it to.

Once we have a solution for this (potentially just reading & hashing
the files, but not writing them anywhere) these tests will be enabled
again.
