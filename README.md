Incomplete Common Lisp bindings for libovr 0.7

Expect APIs and data formats to change as parts are tested.

May require patched CFFI to avoid infinite loops on some function calls
(see https://github.com/cffi/cffi/pull/72 ), and doesn't seem to work
with the `mingw64/mingw-w64-x86_64-libffi 3.2.1-3 (mingw-w64-x86_64)`
from msys2, which segfaults after some calls (3.1-2 works).
