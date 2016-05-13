
'2/16/2002 9:39:34 am'!

'#
# This file is encoded in (so called) ASCII,
# line delimitor is CR.
#
SmallScheme
SmallScheme code for Squeak 3.0

"Translated from file D:/SmallScript/ProtoScheme/SCM/booleans.scm"
'!

!SmallScheme class methodsFor: 'initialization'!
initBooleans
	"(define (boolean? obj)
		(: obj ""isKindOf:"" ($ ""Boolean"")))"

	SmallScheme
		define: #'boolean?'
		as: [:obj | obj isKindOf: Boolean].

	"(define (not b) (if (: b == #f) #t #f))"
	SmallScheme
		define: #'not'
		as: [:b | b == false
				ifTrue:  [true]
				ifFalse: [false]]
! !

