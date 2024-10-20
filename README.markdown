Ordinals
=========

This small work-in-progress library provides a type, `Ordinal`, which
represents finite and infinite ordinal numbers up to, and excluding, phi(2,0),
that is, those ordinals that can be expressed using natural numbers, omega, and
epsilon. The ordinals are represented using
[Cantor Normal Form](https://en.wikipedia.org/wiki/Ordinal_arithmetic#Cantor_normal_form)
(CNF).

It is directly inspired by David Madore's work on explaining and implenting
those same ordinals. In particular, I am taking a lot of conceptual inspiration
from the code of his
[ordinal browser](http://www.madore.org/~david/math/drawordinals.html#?v=e),
whose source code is available in the public domain in the source code of the
above link.
