# Scheme2Smalltalk-Translator

This is a collection of archived files from a Scheme->Smalltalk translator which last eas used in 2002 under Squeak-3.2.

At the time, the translator translated R5RS Scheme into Squeak Smalltalk.

The translator was able to translate itself.

The code, with two minor exceptions, passed the R5RS test suite, including the Call/CC cases.

Almost all code was written in Scheme, including a full R5RS reader and complex number support (see directory 'scm') with a small amount of glue code in Squeak.

The kernel/glue code is in file "SmallScheme.16.cs".

Files from directory 'scm' are translated in directory 'st'.

There is a bunch of cruft from various areas (e.g. .sts files from Dave Simmons' SmallScript Smalltalk implementation.
