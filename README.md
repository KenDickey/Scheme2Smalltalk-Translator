# Scheme2Smalltalk-Translator

This is a collection of archived files from a Scheme->Smalltalk translator which last used in 2002 under Squeak-3.2.

At the time, the translator translated R5RS Scheme into Squeak Smalltalk.

The translator was able to translate itself.

The code, with two minor exceptions, passed the R5RS test suite, including the Call/CC cases.

Almost all code was written in Scheme, including a full R5RS reader and complex number support (see directory 'scm') with a small amount of glue code in Squeak.

The kernel/glue code is in file "SmallScheme.16.cs".

Files from directory 'scm' are translated in directory 'st'.

There is a bunch of cruft from various areas (e.g. .sts files from Dave Simmons' SmallScript Smalltalk implementation).


NOTES:

The basic strategy is to have a Scheme global environment with closures/blocks bound to transliterated Scheme identifiers.

E.g. from "scm/booleans.scm"


````Scheme
(define (boolean? obj) (: obj "isKindOf:" ($ "Boolean")))

(define (not b) 
  ;; (if (eq? b #f) #t #f) 
  ;; -- bummed for speed
  (if (: b == #f) #t #f))
````

There are two "loophole functions" here:  $ and :

(: "mumble") means "pass this through as is"

($ "mumble") means "use the Smalltalk value of this here"

````Smalltalk

SmallScheme define:  #'booleanX3F'    "boolean?"
  as: ( 
       [ :obj |  ( obj isKindOf: Boolean)] )
 .


 SmallScheme define:  #'not'
  as: ( 
       [ :b |  ((( ( b == false)) == false) not)
               	ifTrue: [ true]
               	ifFalse: [ false]] )
 .
````
Aside: the odd looking definition for #'not' is because in Scheme anything not #f is true and the translator knows this.


Also, Scheme identifiers use many more characters than allowable is Smalltalk, so characters like ? translate into X and a hex value for Smalltalk.
