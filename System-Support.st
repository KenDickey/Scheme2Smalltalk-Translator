!classDefinition: #SystemChangeNotifier category: #'System-Support'!
ActiveModel subclass: #SystemChangeNotifier
	instanceVariableNames: 'silenceLevel'
	classVariableNames: 'UniqueInstance'
	poolDictionaries: ''
	category: 'System-Support'!

!SystemChangeNotifier methodsFor: 'private' stamp: 'jmv 3/17/2012 22:50'!
triggerEvent: anEventSelector

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector ]! !

!SystemChangeNotifier methodsFor: 'private' stamp: 'jmv 3/22/2012 11:58'!
triggerEvent: anEventSelector withArguments: anArgumentList

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector withArguments: anArgumentList ]! !


!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:44'!
classAdded: aClass inCategory: aCategoryName

	self
		triggerEvent: #classAdded
		withArguments: { aClass . aCategoryName }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
classCommented: aClass
	"A class with the given name was commented in the system."

	self
		triggerEvent: #classCommented
		with: aClass! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
classDefinitionChangedFrom: oldClass to: newClass

	self
		triggerEvent: #classDefinitionChanged
		withArguments: { oldClass . newClass }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
classRecategorized: aClass from: oldCategory to: newCategory

	self
		triggerEvent: #classRecategorized
		withArguments: { aClass . oldCategory . newCategory }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
classRemoved: aClass fromCategory: aCategoryName

	self
		triggerEvent: #classRemoved
		withArguments: { aClass . aCategoryName }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self
		triggerEvent: #classRenamed
		withArguments: { aClass . oldClassName . newClassName . aCategoryName }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
classReorganized: aClass

	self triggerEvent: #classReorganized with: aClass! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
evaluated: expression context: aContext

	self
		triggerEvent: #evaluated
		withArguments: { expression . aContext }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor
	"A method with the given selector was added to aClass, but not put in a protocol."

	self
		triggerEvent: #methodAdded
		withArguments: { aMethod . aSymbol . aClass . requestor }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor
	"A method with the given selector was added to aClass in protocol aCategoryName."

	self
		triggerEvent: #methodAddedInProtocol
		withArguments: { aMethod . aSymbol . aCategoryName . aClass . requestor }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	self
		triggerEvent: #methodChanged
		withArguments: { oldMethod . newMethod . aSymbol . aClass . requestor }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:45'!
methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass 
	"A method with the given selector was removed from the class."

	self
		triggerEvent: #methodRemoved
		withArguments: { aMethod . aSymbol . protocol . aClass }! !

!SystemChangeNotifier methodsFor: 'system triggers' stamp: 'jmv 3/19/2012 08:46'!
selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

	self
		triggerEvent: #selectorRecategorized
		withArguments: { selector . oldCategory . newCategory . aClass }! !


!SystemChangeNotifier methodsFor: 'public' stamp: 'eem 6/11/2008 18:17'!
doSilently: aBlock
	"Perform the block, and ensure that no system notification are broadcasted while doing so."

	silenceLevel := silenceLevel + 1.
	^aBlock ensure: [silenceLevel > 0 ifTrue: [silenceLevel := silenceLevel - 1]]! !

!SystemChangeNotifier methodsFor: 'public' stamp: 'NS 1/26/2004 20:41'!
isBroadcasting

	^ silenceLevel = 0! !


!SystemChangeNotifier methodsFor: 'initialization' stamp: 'jmv 9/28/2009 09:52'!
initialize

	silenceLevel _ 0! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SystemChangeNotifier class' category: #'System-Support'!
SystemChangeNotifier class
	instanceVariableNames: ''!

!SystemChangeNotifier class methodsFor: 'instance creation' stamp: 'jmv 3/19/2012 08:52'!
new

	^self error: 'This is a singleton implementation, so you are not allowed to create instances yourself. Use #uniqueInstance to access the instance.'! !


!SystemChangeNotifier class methodsFor: 'public' stamp: 'jmv 3/19/2012 08:52'!
uniqueInstance

	UniqueInstance ifNil: [ UniqueInstance _ self  basicNew initialize ].
	^UniqueInstance! !


!classDefinition: #SystemOrganizer category: #'System-Support'!
Categorizer subclass: #SystemOrganizer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!SystemOrganizer commentStamp: '<historical>' prior: 0!
My instances provide an organization for the classes in the system, just as a ClassOrganizer organizes the messages within a class. The only difference is the methods for fileIn/Out.!


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:52'!
fileOut  "SystemOrganization fileOut"

	(FileStream newFileNamed:
		(FileDirectory default nextNameFor: 'SystemOrganization' extension: 'st'))
			nextPutAll: 'SystemOrganization changeFromCategorySpecs: #(';
			newLine;
			print: SystemOrganization;  "ends with a cr"
			nextPutAll: ')!!';
			newLine;
			close.! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 3/5/2009 10:22'!
fileOutAllCategories
	"
	Cursor write showWhile: [
		SystemOrganization fileOutAllCategories ]
	"
	| fileStream imName |
	imName _ FileDirectory baseNameFor:
		(FileDirectory localNameFor: Smalltalk imageName).
	fileStream _ FileStream newFileNamed: imName, '-AllCode', FileDirectory dot, 'st'.
	[
		self categories do: [ :category |
			self fileOutCategoryNoPoolsNoInit: category on: fileStream]
	] ensure: [ fileStream close ]! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 11/27/2008 15:23'!
fileOutCategory: category
	"FileOut all the classes in the named system category."
	| fileStream |
	fileStream _ FileStream newFileNamed: category asFileName , '.st'.
	self fileOutCategory: category on: fileStream initializing: true.
	fileStream close! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'ar 12/22/1999 17:28'!
fileOutCategory: category on: aFileStream 
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."
	^self fileOutCategory: category on: aFileStream initializing: true! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 6/11/2013 14:16'!
fileOutCategory: category on: aFileStream initializing: aBool
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first poolSet tempClass classes |
	classes _ (self superclassOrderIn: category).
	poolSet _ Set new.
	classes do: 
		[:class | class sharedPools do: [:eachPool | poolSet add: eachPool]].
	poolSet size > 0 ifTrue:
		[tempClass _ Class new.
		tempClass shouldFileOutPools ifTrue:
			[poolSet _ poolSet select: [:aPool | tempClass shouldFileOutPool: (Smalltalk keyAtIdentityValue: aPool)].
			poolSet do: [:aPool | tempClass fileOutPool: aPool onFileStream: aFileStream]]].
	first _ true.
	classes do: 
		[:class | 
		first
			ifTrue: [first _ false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false].
	aBool ifTrue:[classes do:[:cls| cls fileOutInitializerOn: aFileStream]].! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 6/11/2013 14:16'!
fileOutCategoryNoPoolsNoInit: category on: aFileStream
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first classes |
	classes _ self superclassOrderIn: category.
	first _ true.
	classes do: [ :class | 
		first
			ifTrue: [first _ false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false]! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'tk 9/28/2000 15:50'!
objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a path to me in the other system instead."

self == SystemOrganization ifTrue: [
	dp _ DiskProxy global: #SystemOrganization selector: #yourself args: #().
	refStrm replace: self with: dp.
	^ dp].
^ self
! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 6/11/2013 16:18'!
superclassOrderIn: category 
	"Answer an OrderedCollection containing references to the classes in the 
	category whose name is the argument, category (a string). The classes 
	are ordered with superclasses first so they can be filed in."

	| list |
	list _ 
		(self listAtCategoryNamed: category asSymbol) 
			collect: [:title | Smalltalk at: title].
	^Array streamContents: [ :strm |
		Smalltalk hierarchySorted: list do: [ :cls | strm nextPut: cls ]].! !


!SystemOrganizer methodsFor: 'remove' stamp: 'di 3/3/2001 16:07'!
categoriesMatching: matchString
	"Return all matching categories"
	^ self categories select: [:c | matchString match: c]! !

!SystemOrganizer methodsFor: 'remove' stamp: 'di 3/3/2001 16:08'!
removeCategoriesMatching: matchString
	"Remove all matching categories with their classes"
	(self categoriesMatching: matchString) do:
		[:c | self removeSystemCategory: c]! !

!SystemOrganizer methodsFor: 'remove' stamp: 'jm 5/20/1998 19:38'!
removeMissingClasses
	"Remove any class names that are no longer in the Smalltalk dictionary. Used for cleaning up after garbage collecting user-generated classes."
	"SystemOrganization removeMissingClasses"

	elementArray copy do: [:el |
		(Smalltalk includesKey: el) ifFalse: [self removeElement: el]].
! !

!SystemOrganizer methodsFor: 'remove' stamp: 'jmv 6/11/2013 14:16'!
removeSystemCategory: category
	"remove all the classes associated with the category"

	(self superclassOrderIn: category) reverseDo: [ :class | class removeFromSystem].

	self removeCategory: category.
! !


!classDefinition: #Clipboard category: #'System-Support'!
Object subclass: #Clipboard
	instanceVariableNames: 'contents recent'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'System-Support'!
!Clipboard commentStamp: '<historical>' prior: 0!
The Clipboard class implements a basic buffering scheme for text. The currently selected text is also exported to the OS so that text can be copied from and to other applications. Commonly only a single instance is used (the default clipboard) but applications are free to use other than the default clipboard if necessary.!


!Clipboard methodsFor: 'accessing' stamp: 'jmv 3/13/2012 21:55'!
chooseRecentClipping
	"
	Clipboard chooseRecentClipping
	"
	"Choose by menu from among the recent clippings"
	recent isEmpty ifTrue: [ ^ nil ].
	^ (SelectionMenu
		labelList:
			(recent collect: [ :txt |
				((txt asString contractTo: 50)
					withLineEndings: '\') withBlanksCondensed ])
		selections: recent) startUp! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 11/14/2011 16:31'!
retrieveMorph
	"Answer a Morph or nil."
	| answer |
	
	answer _ self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer is: #Morph) ifTrue: [
		^ answer ].

	"If clipboard contains a Form, prepare a Morph including it."
	(answer is: #Form) ifTrue: [
		^ ImageMorph new image: answer ].

	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^ (TextModelMorph withText: answer asText)
			embeddedInMorphicWindowLabeled: 'Text editor' ].
	
	"No useful contents"
	^nil! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 11/10/2013 10:01'!
retrieveObject
	"Answer whatever was last stored in the clipboard"
	| stringOrNil |

	"If the OS clipboard has the id for our contents, or the same characters, then answer the richer Smalltalk object.
	Note: if the (extended) clipboard contains a serialized object, it shouldn't contain an id, so
	it is deserialized even if ivar contents contains the object. This is done to guarantee consistency with pasting
	from another Cuis image."
	stringOrNil _ self retrieveIdOrStringFromOS.
	(stringOrNil = (self idFor: contents) or: [ stringOrNil = contents asString])
		ifTrue: [
			"We copy the object, because the result of each paste operation could be modified independently of the others afterwards
			(and the same clipboard contents might be pasted many times)"
			^contents copyForClipboard ].

	"If we have the ExtendedClipboardInterface, try to get an RTF or Form"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface retrieveObject ifNotNil: [ :object | ^object ]].

	"Otherwise answer the string brought by clipboard primitives,
	but if they are not present or fail, use the internal clipboard."
	^stringOrNil ifNil: [ contents copyForClipboard ]! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 3/18/2011 16:09'!
retrieveStringOrText
	"Answer a Text or a String. Appropriate for a Text Editor."
	| answer |
	
	answer _ self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^answer ].
	
	"If clipboard contains a Morph, answer a Text incuding its drawing..."
	(answer is: #Morph) ifTrue: [
		answer _ (answer imageForm:32) offset: 0@0 ].
	
	"If clipboard contains a Form, prepare a Text including it."
	(answer is: #Form) ifTrue: [
		^Text withForm: answer ].
	
	"Answer some string..."
	^answer ifNil: [ '' ] ifNotNil: [ answer printString ]! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 11/10/2013 10:02'!
storeObject: anObject
	"Set new contents on the clipboard.  Also export to OS.
	anObject can be a:
		String
		Text
		Form
		Morph
		Object.
	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."

	| primitiveFormat id |

	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.
	Another copy must be made again when pasting, as the same object could be pasted many times."
	contents _ (anObject isString or: [ anObject is: #Text])
		ifTrue: [ anObject withCuisLineEndings ]
		ifFalse: [ anObject copyForClipboard ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			id _ self idFor: contents.
			contents isString
				ifTrue: [ ^interface storeString: contents id: id ].
			(contents is: #Text)
				ifTrue: [ ^interface storeText: contents id: id ].
			(contents is: #Form)
				ifTrue: [ ^interface storeForm: contents id: id ].
			(contents is: #Morph)
				ifTrue: [ ^interface storeForm: (contents imageForm: 32) id: id ].
			^interface storeString: contents asString id: id ]].

	"Otherwise use the clipboard primitives in the VM"
	"The VM uses UTF-8 for clipboard"
	primitiveFormat _ (self stringOrIdFor: contents) asUtf8: true.
	self primitiveClipboardString: primitiveFormat! !


!Clipboard methodsFor: 'primitives' stamp: 'jmv 1/18/2011 22:36'!
primitiveClipboardString
	"Get the current clipboard string. Return the empty string if the primitive fails."
	<primitive: 141>
	^ ''! !

!Clipboard methodsFor: 'primitives' stamp: 'jmv 1/18/2011 22:36'!
primitiveClipboardString: aString
	"Set the current clipboard to the given string."

	<primitive: 141>
	"don't fail if the primitive is not implemented"! !


!Clipboard methodsFor: 'private' stamp: 'jmv 11/10/2013 09:56'!
extendedClipboardInterface
	"Answer a subinstance of ExtendedClipboardInterface, if present and operational.
	Clipboard default extendedClipboardInterface
	"
	| interface |
	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface |
		interface _ clipboardInterface current.
		interface isOperational ifTrue: [
			^ interface ]].
	"No operational extended clipboard."
	^nil! !

!Clipboard methodsFor: 'private' stamp: 'jmv 1/19/2011 15:03'!
idFor: anObject
	"
	Clipboard default stringOrIdFor: Smalltalk
	"
	^{anObject class. anObject hash} printString! !

!Clipboard methodsFor: 'private' stamp: 'jmv 1/19/2011 14:03'!
noteRecentClipping: text
	"Keep most recent clippings in a queue for pasteRecent (paste... command)"
	(text isString or: [text is: #Text]) ifFalse: [^self ].
	text isEmpty ifTrue: [^ self].
	text size > 50000 ifTrue: [^ self].
	(recent includes: text) ifTrue: [^ self].
	recent addFirst: text.
	[recent size > 5] whileTrue: [recent removeLast].
! !

!Clipboard methodsFor: 'private' stamp: 'jmv 11/10/2013 10:00'!
retrieveIdOrStringFromOS
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| primitiveFormat |

	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].
		
	primitiveFormat _ self primitiveClipboardString.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard.
	We prefer nil"
	primitiveFormat isEmpty ifTrue: [ ^nil ].

	"The VM uses UTF-8 for clipboard"
	^(String fromUtf8: primitiveFormat hex: false trimLastNull: true) withCuisLineEndings! !

!Clipboard methodsFor: 'private' stamp: 'jmv 1/10/2014 10:25'!
stringOrIdFor: anObject
	"
	Clipboard default stringOrIdFor: Smalltalk
	"
	anObject isString ifTrue: [ ^anObject ].
	(anObject is: #Text) ifTrue: [ ^anObject string ].
	^self idFor: anObject! !


!Clipboard methodsFor: 'initialization' stamp: 'jmv 1/18/2011 22:22'!
initialize
	contents _ nil.
	recent _ OrderedCollection new.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Clipboard class' category: #'System-Support'!
Clipboard class
	instanceVariableNames: ''!

!Clipboard class methodsFor: 'accessing' stamp: 'ar 1/15/2001 18:45'!
chooseRecentClipping  "Clipboard chooseRecentClipping"
	"Choose by menu from among the recent clippings"
	^self default chooseRecentClipping! !

!Clipboard class methodsFor: 'accessing' stamp: 'ar 1/15/2001 18:33'!
default
	^Default ifNil:[Default _ self new].! !

!Clipboard class methodsFor: 'accessing' stamp: 'ar 1/15/2001 18:48'!
default: aClipboard
	"So that clients can switch between different default clipboards"
	Default _ aClipboard.! !


!Clipboard class methodsFor: 'default clipboard' stamp: 'jmv 1/19/2011 14:05'!
retrieveMorph
	"Answer a Morph"
	^self default retrieveMorph! !

!Clipboard class methodsFor: 'default clipboard' stamp: 'jmv 1/18/2011 23:40'!
retrieveObject
	"Answer whatever was last stored in the clipboard"
	^self default retrieveObject! !

!Clipboard class methodsFor: 'default clipboard' stamp: 'jmv 1/18/2011 23:39'!
retrieveStringOrText
	"Answer a Text or a String. Appropriate for a Text Editor."
	^self default retrieveStringOrText! !

!Clipboard class methodsFor: 'default clipboard' stamp: 'jmv 1/18/2011 23:40'!
storeObject: aTextOrStringOrFormOrObject
	"Set text currently on the clipboard.  Also export to OS"
	^self default storeObject: aTextOrStringOrFormOrObject! !


!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames'
	classVariableNames: 'EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp'
	poolDictionaries: ''
	category: 'System-Support'!
!SystemDictionary commentStamp: '<historical>' prior: 0!
I represent a special dictionary that supports protocol for asking questions about the structure of the system. Other than class names, I contain (print this)...
	Smalltalk keys select: [:k | ((Smalltalk at: k) isKindOf: Class) not]
			thenCollect: [:k | k -> (Smalltalk at: k) class]
!


!SystemDictionary methodsFor: 'accessing' stamp: 'dkh 4/23/2012 21:06'!
classes

	^self classNames collect: [:each | self at: each ]! !

!SystemDictionary methodsFor: 'accessing' stamp: 'ar 7/11/1999 21:56'!
organization
	"Return the organizer for the receiver"
	^SystemOrganization! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 12/16/2013 22:59'!
classNamed: className 
	"className is either a class name or a class name followed by ' class'.
	Answer the class or metaclass it names"
	"
	Smalltalk classNamed: 'Point'
	Smalltalk classNamed: 'Point class'
	Smalltalk classNamed: 'BogusClassName'
	Smalltalk classNamed: 'BogusClassName class'
	"
.
	Smalltalk at: className asSymbol ifPresent: [ :found | ^ found ].
	
	^(className withoutSuffix: ' class') ifNotNil: [ :baseName |
		(Smalltalk at: baseName asSymbol ifAbsent: [ ^nil ]) class ]! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 12/28/2010 10:55'!
fillCaches
	"Fill cachedClassNames and cachedNonClassNames. Return an array with the calculated values."

	| classNames nonClassNames |
	classNames _ IdentitySet new: self size.
	nonClassNames _ IdentitySet new.
	self keysAndValuesDo: [ :key :value |
		value isInMemory ifTrue: [
			"The key == value name test below addresses two separate issues:
				1) Obsolete classes, where key = #Foo and value name = 'AnObsoleteFoo'
				2) Aliases, i.e., Smalltalk at: #OtherName put: aClass"
			((value isKindOf: Class) and: [ key == value name ])
				ifTrue: [ classNames add: key ]
				ifFalse: [ nonClassNames add: key ] ] ].
	classNames _ classNames asArray sort.
	nonClassNames _ nonClassNames asArray sort.
	cachedClassNames _ classNames.
	cachedNonClassNames _ nonClassNames.
	^{ classNames. nonClassNames }! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 12/28/2010 10:53'!
flushClassNameCache
	"Smalltalk flushClassNameCache"
	"Forse recomputation of the cached list of class names."

	cachedClassNames _ nil.
	cachedNonClassNames _ nil! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/2/2010 10:11'!
forgetClass: aClass logged: aBool 
	"Delete the class, aClass, from the system.
	Note that this doesn't do everything required to dispose of a class - to do that use Class>>removeFromSystem."

	aBool ifTrue: [SystemChangeNotifier uniqueInstance classRemoved: aClass fromCategory: aClass category].		
	SystemOrganization removeElement: aClass name.
	self removeFromStartUpList: aClass.
	self removeFromShutDownList: aClass.
	self removeKey: aClass name ifAbsent: nil.
	self flushClassNameCache! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/2/2010 10:11'!
hasClassNamed: aString
	"Answer whether there is a class of the given name, but don't intern aString if it's not alrady interned.  4/29/96 sw"

	Symbol hasInterned: aString ifTrue: [ :aSymbol | 
		^ (self at: aSymbol ifAbsent: nil) isKindOf: Class].
	^ false! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/22/2012 22:32'!
removeClassNamed: aName
	"Invoked from fileouts:  if there is currently a class in the system named aName, then remove it.  If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	oldClass _ self at: aName asSymbol ifAbsent: [
		Transcript newLine; show: 'Removal of class named ', aName, ' ignored because ', aName, ' does not exist.'.
		^ self].

	oldClass removeFromSystem! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/23/2012 18:45'!
removeClassNamedIfInBaseSystem: aName
	"Invoked from fileouts:  if there is currently a class in the system named aName, and it is not part of a package, then remove it.
	If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	oldClass _ self at: aName asSymbol ifAbsent: [
"		Transcript newLine; show: 'Removal of class named ', aName, ' ignored because ', aName, ' does not exist.'."
		^ self].

	CodePackage
		packageOfClass: oldClass
		ifNone: [
			"If remove is actually done, then include it in the current change set for the base system,
			as a regular remove."
			oldClass removeFromSystem ]! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/29/2009 16:15'!
renameAndUpdateReferences: oldName as: newName
	"Dangerous. Simple search and replace could break stuff.
	Use only to generate change sets to later checked with care
	
OldMorph withAllSubclasses do: [ :c |
	n _ c name.
	(n beginsWith: 'Old') ifTrue: [
		Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]]

OldBorderStyle withAllSubclasses do: [ :c |
	n _ c name.
	(n beginsWith: 'Old') ifTrue: [
		Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]]

#(#OldKeyboardBuffer #OldLayoutCell #OldLayoutFrame #OldLayoutPolicy #OldLayoutProperties #OldMorphExtension #OldMouseClickState #OldProportionalLayout #OldTableLayout #OldTableLayoutProperties #OldTextAnchor #OldTextComposer #OldTextContainer #OldTextLine #OldTheWorldMenu #OldWorldState #OldWorldTest) do: [ :n |
	Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]
	"

	self renameClassNamed: oldName as: newName.
	(Smalltalk allCallsOn: (Smalltalk associationAt: newName)) do: [ :methodRef |
		methodRef updateReferencesTo: oldName toBe: newName ]! !

!SystemDictionary methodsFor: 'class names' stamp: 'rr 3/11/2004 15:18'!
renameClass: aClass as: newName 
	"Rename the class, aClass, to have the title newName."
	| oldref i oldName category |
	oldName := aClass name.
	category := aClass category.
	SystemOrganization classify: newName under: aClass category.
	SystemOrganization removeElement: aClass name.
	oldref _ self associationAt: aClass name.
	self removeKey: aClass name.
	oldref key: newName.
	self add: oldref.  "Old association preserves old refs"
	(Array with: StartUpList with: ShutDownList) do:
		[:list |  i _ list indexOf: aClass name ifAbsent: [0].
		i > 0 ifTrue: [list at: i put: newName]].
	self flushClassNameCache.
	SystemChangeNotifier uniqueInstance classRenamed: aClass from: oldName to: newName inCategory: category! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/13/2012 12:50'!
renameClassNamed: oldName as: newName
	"Invoked from fileouts:  if there is currently a class in the system named oldName, then rename it to newName.  If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	(oldClass _ self at: oldName asSymbol ifAbsent: nil)
		ifNil: [
			Transcript newLine; show: 'Class-rename for ', oldName, ' ignored because ', oldName, ' does not exist.'.
			^ self].

	oldClass rename: newName! !


!SystemDictionary methodsFor: 'dictionary access' stamp: 'jmv 5/24/2014 11:05'!
associationOrUndeclaredAt: key 
	"return an association or install in undeclared. Used for mating up ImageSegments."

	^ self associationAt: key ifAbsent: [
		Undeclared at: key put: nil.
		Undeclared associationAt: key ]! !

!SystemDictionary methodsFor: 'dictionary access'!
at: aKey put: anObject 
	"Override from Dictionary to check Undeclared and fix up
	references to undeclared variables."
	| index element |
	(self includesKey: aKey) ifFalse: 
		[self declare: aKey from: Undeclared.
		self flushClassNameCache].
	super at: aKey put: anObject.
	^ anObject! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 11/16/2010 08:44'!
browseEqEqSentToSmallIntegerConstants
	"
	Smalltalk browseEqEqSentToSmallIntegerConstants
	"
	| hasMatch visitor |
	hasMatch _ false.
	visitor _ ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isConstantNumber or: [ node arguments first isConstantNumber ]]]) ifTrue: [
						hasMatch _ true ]]
		select: [ :node |
			hasMatch not ].
	Smalltalk browseAllSelect: [ :method |
		hasMatch _ false.
		method decompile accept: visitor.
		hasMatch ].! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'ar 8/6/2001 22:50'!
browseObsoleteMethodReferences
	"Open a browser on all referenced behaviors that are obsolete"
	"Smalltalk browseObsoleteMethodReferences"
	| list |
	list _ self obsoleteMethodReferences.
	self browseMessageList: list name:'Method referencing obsoletes' autoSelect: nil! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/12/2012 10:37'!
browseUndeclaredReferences
	"
	Smalltalk browseUndeclaredReferences
	"
	| anythingToShow |
	Smalltalk cleanOutUndeclared.
	anythingToShow _ false.
	Undeclared keys do: [ :k |
		anythingToShow _ true.
		self
			browseMessageList: (Smalltalk allCallsOn: (Undeclared associationAt: k))
			name: 'References to Undeclared: ', k printString ].

	"undeclared not in Undeclared - "
	"This happened in Pharo. See http://lists.gforge.inria.fr/pipermail/pharo-project/2012-March/061270.html
	Maybe do something like this in Cuis too???
	lostUndeclared _ Smalltalk allSelect: [:m|
		m literals anySatisfy: [:l|
			l isVariableBinding
				and: [l key isSymbol ""avoid class-side methodClass literals""
				and: [(m methodClass bindingOf: l key) isNil
				and: [(Undeclared includesAssociation: l) not]]]]].
	"

	anythingToShow ifFalse: [
		 (PopUpMenu labels: ' OK ')
				startUpWithCaption: 'There are no Undeclared at all' ]! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/9/2010 13:51'!
cleanCompactObsoleteClasses

	| cct |
	cct _ Smalltalk compactClassesArray.
	cct do: [ :c |
		c ifNotNil: [
			c isObsolete ifTrue: [ 
				cct at: c indexIfCompact put: nil ]]]! !

!SystemDictionary methodsFor: 'housekeeping'!
cleanOutUndeclared 
	Undeclared removeUnreferencedKeys! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 7/29/2013 23:36'!
condenseChanges	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseChanges"

	| f oldChanges classCount |
	f _ FileStream fileNamed: 'ST80.temp'.
	f timeStamp.
'Condensing Changes File...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: Smalltalk classNames size
	during:
		[:bar | classCount _ 0.
		Smalltalk allClassesDo:
			[:class | bar value: (classCount _ classCount + 1).
			class moveChangesTo: f.
			class putClassCommentToCondensedChangesFile: f.
			class class moveChangesTo: f]].
	LastQuitLogPosition _ f position.
	f close.

	CompiledMethod allInstancesDo:
		[ : e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges _ SourceFiles at: 2.
	oldChanges close.
	FileDirectory default 
		deleteFileNamed: oldChanges name , '.old';
		rename: oldChanges name toBe: oldChanges name , '.old';
		rename: f name toBe: oldChanges name.
	self setMacFileInfoOn: oldChanges name.
	SourceFiles at: 2
			put: (FileStream oldFileNamed: oldChanges name).

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 2/21/2013 22:04'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| f classCount dir newVersionString oldChangesName newChangesName newSourcesName |
	newVersionString _ FillInTheBlankMorph request: 'Please name the new sources file' initialAnswer: SourceFileVersionString.
	newVersionString ifNil: [^ self].
	newVersionString = SourceFileVersionString ifTrue: [
		^ self error: 'The new source file must not be the same as the old.'].
	SourceFileVersionString _ newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName _ self defaultSourcesName.
	f _ FileStream newFileNamed: newSourcesName.
	f timeStamp.
	'Condensing Sources File...'
		displayProgressAt: Sensor mousePoint
		from: 0 to: Smalltalk classNames size
		during: [ :bar |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				bar value: (classCount _ classCount + 1).
				class fileOutOn: f moveSource: true toFile: 1]].
	f close.

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChangesName _ self currentChangesName.
	self closeSourceFiles.
	oldChangesName ifNotNil: [
		dir _ FileDirectory default.
		dir rename: oldChangesName toBe: oldChangesName, '.old' ].
	newChangesName _ self defaultChangesName.
	(FileStream newFileNamed: newChangesName)
		timeStamp; close.
	LastQuitLogPosition _ 0.

	self setMacFileInfoOn: newChangesName.
	self setMacFileInfoOn: newSourcesName.
	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/27/2014 13:00'!
fixSourceCodeLineEndings
	"
	Smalltalk fixSourceCodeLineEndings
	"
	"Scan all methods for source code with Carriage Returns.
	Replaces all occurrences of<CR> or  <CR><LF> by <LF>."

	| oldCodeString n newCodeString oldStamp oldCategory m oldClassComment newClassComment c o stamp |
'Scanning sources for old Line Ending conventions.
This will take a few moments...'
	displayProgressAt: Sensor mousePoint
	from: 0
	to: CompiledMethod instanceCount
	during: [ :bar |
		n _ 0.
		m _ 0.
		c _ 0.
		Smalltalk allBehaviorsDo: [ :cls | 
			cls selectors do: [ :selector | 
				(n _ n+1) \\ 100 = 0 ifTrue: [ bar value: n ].
				oldCodeString _ (cls sourceCodeAt: selector) asString.
				newCodeString _ oldCodeString withCuisLineEndings.
				newCodeString = oldCodeString ifFalse: [
					oldStamp _ (cls compiledMethodAt: selector) timeStamp.
					oldCategory _ cls whichCategoryIncludesSelector: selector.
					cls compile: newCodeString classified: oldCategory withStamp: oldStamp notifying: nil.
					m _ m + 1].
				cls isMeta ifFalse: [
					o _ cls organization.
					oldClassComment _ o classComment.
					stamp _ o commentStamp.
					newClassComment _  oldClassComment withCuisLineEndings.
					newClassComment = oldClassComment ifFalse: [
						cls classComment: newClassComment stamp: stamp.
						c _ c + 1 ]]
				]].
	].
	Transcript newLine; show: m printString , ' methods were fixed.'.
	Transcript newLine; show: c printString , ' text class comments were fixed.'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/29/2014 00:59'!
macroBenchmark1    "Smalltalk macroBenchmark1"
	"Decompiles and prettyPrints the source for every method in the system (or less depending on the *FILTER*, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same.  Because it never installs the new method, it should not cause any flusing of the method cache."
	 | methodNode oldMethod newMethod badOnes oldCodeString n classes |
	classes _ Smalltalk allClasses select: [:c | c name < 'B3'].
	badOnes _ OrderedCollection new.
'Decompiling and recompiling...'
displayProgressAt: Sensor mousePoint
from: 0 to: (classes detectSum: [:c | c selectors size])
during: [:bar | n _ 0.
	classes do:
		[:cls | 
		"Transcript cr; show: cls name."
		cls selectors do:
			[:selector | bar value: (n _ n+1).
			oldMethod _ cls compiledMethodAt: selector.
			oldCodeString _ (Decompiler new
								decompile: selector in: cls method: oldMethod)
							decompileString.
			methodNode _ Compiler new
						compile: oldCodeString
						in: cls notifying: nil ifFail: nil.
			newMethod _ methodNode generate: #(0 0 0 0).
			oldCodeString = (Decompiler new
								decompile: selector in: cls method: newMethod)
							decompileString ifFalse: [badOnes add: cls name , ' ' , selector]]].
].
	^ badOnes size! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 4/18/2011 14:41'!
macroBenchmark3   "Smalltalk macroBenchmark3"
	| testBlock tallies prev receiver |
	"Runs the stepping simulator with the messageTally tree (like tallySends)."
	testBlock _
		['Running the context step simulator'
			displayProgressAt: Sensor mousePoint
			from: 0 to: 200
			during:
				[:bar |
				1 to: 200 do:
				[:x | bar value: x.
				Float pi printString.
				15 factorial printString]]].
	tallies _ MessageTally new class: testBlock receiver class
							method: testBlock method.
	receiver _ nil.
	prev _ testBlock.
	thisContext sender
		runSimulated: testBlock
		contextAtEachStep:
			[:current |
			current == prev ifFalse: [
				"call or return"
				prev sender ifNotNil: [
					"call only"
					(receiver == nil or: [current receiver == receiver])
						ifTrue: [tallies tally: current by: 1]].
				prev _ current]].
! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'sd 7/2/2003 22:11'!
makeInternalRelease		"Smalltalk makeInternalRelease"
	(self confirm: SystemVersion current version , '
Is this the correct version designation?
If not, choose no, and fix it.') ifFalse: [^ self].
	(Object classPool at: #DependentsFields) size > 1 ifTrue: [self halt].
	Browser initialize.
	Undeclared isEmpty ifFalse: [self halt].
	Smalltalk garbageCollect.
	self obsoleteClasses isEmpty ifFalse: [self halt].
	Symbol rehash.
	self halt: 'Ready to condense changes'.
	Smalltalk condenseChanges! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'ar 7/15/1999 16:05'!
obsoleteBehaviors   "Smalltalk obsoleteBehaviors inspect"
	"Find all obsolete behaviors including meta classes"
	| obs |
	obs _ OrderedCollection new.
	Smalltalk garbageCollect.
	self allObjectsDo:[:cl|
		(cl isBehavior and:[cl isObsolete]) ifTrue:[obs add: cl]].
	^ obs asArray! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/1/2010 14:08'!
obsoleteClasses   "Smalltalk obsoleteClasses inspect"
	"NOTE:  Also try inspecting comments below"
	| obs c |
	obs _ OrderedCollection new.  Smalltalk garbageCollect.
	Metaclass allInstancesDo:
		[:m | c _ m soleInstance.
		(c notNil and: ['AnOb*' match: c name asString])
			ifTrue: [obs add: c]].
	^ obs asArray

"Likely in a ClassDict or Pool...
(Association allInstances select: [:a | (a value isKindOf: Class) and: ['AnOb*' match: a value name]]) asArray
"
"Obsolete class refs or super pointer in last lit of a method...
| n l found |
Smalltalk browseAllSelect:
	[:m | found _ false.
	1 to: m numLiterals do:
		[:i | (((l _ m literalAt: i) isMemberOf: Association)
				and: [(l value isKindOf: Behavior)
				and: ['AnOb*' match: l value name]])
			ifTrue: [found _ true]].
	found]
"! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 9/2/2013 10:25'!
obsoleteMethodReferences
	"Smalltalk obsoleteMethodReferences"
	"Smalltalk browseObsoleteMethodReferences"
	"Open a browser on all referenced behaviors that are obsolete"
	| obsClasses obsRefs references |
	references _ WriteStream on: Array new.
	obsClasses _ self obsoleteBehaviors.
	'Scanning for methods referencing obsolete classes' displayProgressAt: Sensor mousePoint
		from: 1 to: obsClasses size during:[:bar|
	obsClasses keysAndValuesDo:[:index :each|
		bar value: index.
		obsRefs _ self pointersTo: each except: obsClasses.
		obsRefs do:[:ref|
			"Figure out if it may be a global"
			((ref isVariableBinding) and:[ ref key isString "or Symbol" ]) ifTrue:[
				(self pointersTo: ref) do:[:meth|
					(meth is: #CompiledMethod) ifTrue:[
						meth methodReference ifNotNil: [ :mref|
							references nextPut: mref]]]]]].
	].
	^references contents! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/13/2012 12:50'!
recompileAllFrom: firstName 
	"Recompile all classes, starting with given name."

	self allClassesDo: [ :class |
		class name >= firstName
			ifTrue: [
				Transcript show: class name; newLine.
				class compileAll]]

	"Smalltalk recompileAllFrom: 'AAABodyShop'."
! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'di 6/8/1999 15:47'!
removeEmptyMessageCategories
	"Smalltalk removeEmptyMessageCategories"
	Smalltalk garbageCollect.
	(ClassOrganizer allInstances copyWith: SystemOrganization) do:
		[:org | org removeEmptyCategories]! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/27/2014 13:00'!
removeTextCode    "Smalltalk removeTextCode"
	"Scan all methods for source code that is Text (i.e. with attributes)
	Replace it with simpleStrings"
	 | oldCodeString n newCodeString oldStamp oldCategory m classComment c o stamp |
	'Scanning sources for Text.
This will take a few moments...'
		displayProgressAt: Sensor mousePoint
		from: 0 
		to: CompiledMethod instanceCount
		during: [ :bar | 
			n _ 0.
			m _ 0.
			c _ 0.
			Smalltalk allBehaviorsDo: [ :cls | 
				cls selectors do: [ :selector  | 
					(n _ n+1) \\ 100 = 0 ifTrue: [bar value: n].
					oldCodeString _ cls sourceCodeAt: selector.
					oldCodeString class = String ifFalse: [
						newCodeString _ oldCodeString asString.
						oldStamp _ (cls compiledMethodAt: selector) timeStamp.
						oldCategory _ cls whichCategoryIncludesSelector: selector.
						cls compile: newCodeString classified: oldCategory withStamp: oldStamp notifying: nil.
						m _ m + 1]].
				cls isMeta ifFalse: [
					o _ cls organization.
					classComment _ o classComment.
					stamp _ o commentStamp.
					classComment class == String ifFalse: [
						classComment hasAnyAttribute ifTrue: [
							self halt "review" ].
						cls classComment: classComment asString stamp: stamp.
						c _ c + 1 ]].
			].
		].
		Transcript newLine; show: m printString , ' text methods converted to strings.'.
		Transcript newLine; show: c printString , ' text class comments converted to strings.'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/29/2014 08:50'!
someCleanup
	"
	Smalltalk someCleanup
	Sometimes ueful, for example, prior to saving an image.
	"
	WorldState allInstancesDo: [ :a | a convertStepList; convertAlarms ].
	Delay startTimerEventLoop.
	Smalltalk garbageCollect! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/29/2014 00:59'!
testDecompiler
	"Smalltalk testDecompiler"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	| methodNode oldMethod newMethod badOnes oldCodeString n |
	badOnes _ OrderedCollection new.
	'Decompiling all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :bar |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].
					oldMethod _ cls compiledMethodAt: selector.
					oldCodeString _ (Decompiler new
						decompile: selector
						in: cls
						method: oldMethod) decompileString.
					methodNode _ Compiler new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldCodeString =
						(Decompiler new
							decompile: selector
							in: cls
							method: newMethod) decompileString ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Decompiler Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/29/2014 00:58'!
testFormatter
	"Smalltalk testFormatter"
	"Reformats the source for every method in the system, and then compiles that source and verifies that it generates identical code.
	The formatting used will be classic monochrome."
	| newCodeString methodNode oldMethod newMethod badOnes n |
	badOnes _ OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :bar |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].
					newCodeString _ Compiler new
						format: (cls sourceCodeAt: selector)
						in: cls
						notifying: nil.
					methodNode _ Compiler new
						compile: newCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldMethod _ cls compiledMethodAt: selector.
					oldMethod = newMethod ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/29/2014 00:58'!
testFormatter2
	"Smalltalk testFormatter2"
	"Reformats the source for every method in the system, and then verifies that the order of source tokens is unchanged.
	The formatting used will be classic monochrome"
	| newCodeString badOnes n oldCodeString oldTokens newTokens |
	badOnes _ OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :bar |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].
					oldCodeString _ (cls sourceCodeAt: selector) asString.
					newCodeString _ Compiler new
						format: oldCodeString
						in: cls
						notifying: nil.
					oldTokens _ oldCodeString findTokens: Character separators.
					newTokens _ newCodeString findTokens: Character separators.
					oldTokens = newTokens ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping'!
verifyChanges		"Smalltalk verifyChanges"
	"Recompile all methods in the changes file."
	Smalltalk allBehaviorsDo: [:class | class recompileChanges].
! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 4/12/2013 11:31'!
alternativeSourcesName
	"Answer the alternative full path to the sources file.
	If Sources not found at #defaultSourcesName (image folder), seek for them here (vm folder)."
	"
	Smalltalk alternativeSourcesName
	"
	"Answer the default full path to the version-stable source code"
	^ self vmPath , SourceFileVersionString , FileDirectory dot , FileDirectory sourceSuffix! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'MPH 10/23/2000 13:31'!
changeImageNameTo: aString
	self imageName: aString.
	LastImageName _ self imageName! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/21/2013 22:14'!
currentChangesName
	"Answer the full path to the version-stable source code currently in use.
	Answer nil if not a file (i.e. if internalized with #internalizeChangeLog or #internalizeSources, or closed with #closeSourceFiles)"
	| changes |
	changes _ SourceFiles second.
	^(changes isKindOf: FileStream)
		ifTrue: [ changes name ]! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/21/2013 22:13'!
currentSourcesName
	"Answer the full path to the version-stable source code currently in use
	Answer nil if not a file (i.e. if internalized with #internalizeSources, or closed with #closeSourceFiles)"
	| sources |
	sources _ SourceFiles first.
	^(sources isKindOf: FileStream)
		ifTrue: [ sources name ]! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/20/2013 22:36'!
defaultChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultChangesName
	"
	^(FileDirectory baseNameFor: self imageName), FileDirectory dot, FileDirectory changeSuffix! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/20/2013 22:37'!
defaultSourcesName
	"Answer the default full path to the sources file corresponding to the image file name."
	"
	Smalltalk defaultSourcesName
	"
	^ self imagePath, FileDirectory slash, SourceFileVersionString, FileDirectory dot, FileDirectory sourceSuffix! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/14/2008 00:29'!
fullNameForChangesNamed: aName
	| newName |
	newName _ FileDirectory baseNameFor: (FileDirectory default fullNameFor: aName).
	^newName , FileDirectory dot, FileDirectory changeSuffix! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/14/2008 00:30'!
fullNameForImageNamed: aName
	| newName |
	newName _ FileDirectory baseNameFor: (FileDirectory default fullNameFor: aName).
	^newName , FileDirectory dot, FileDirectory imageSuffix! !

!SystemDictionary methodsFor: 'image, changes name'!
imageName
	"Answer the full path name for the current image."
	"Smalltalk imageName"

	<primitive: 121>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'image, changes name'!
imageName: newName
	"Set the the full path name for the current image.  All further snapshots will use this."

	<primitive: 121>
	^ self primitiveFailed! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jm 12/4/97 22:25'!
imagePath
	"Answer the path for the directory containing the image file."
	"Smalltalk imagePath"

	^ FileDirectory dirPathFor: self imageName
! !

!SystemDictionary methodsFor: 'image, changes name'!
vmPath
	"Answer the path for the directory containing the Smalltalk virtual machine. Return the empty string if this primitive is not implemented."
	"Smalltalk vmPath"

	<primitive: 142>
	^ ''! !


!SystemDictionary methodsFor: 'memory space'!
bytesLeft
	"Answer the number of bytes of space available. Does a full garbage collection."

	^ self garbageCollect
! !

!SystemDictionary methodsFor: 'memory space' stamp: 'ar 2/25/2001 17:55'!
bytesLeft: aBool
	"Return the amount of available space. If aBool is true, include possibly available swap space. If aBool is false, include possibly available physical memory. For a report on the largest free block currently availabe within Squeak memory but not counting extra memory use #primBytesLeft."
	<primitive: 112>
	^self primBytesLeft! !

!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 1/7/2014 21:40'!
bytesLeftString
	"Return a string describing the amount of memory available"
	| availInternal availPhysical availTotal |
	self garbageCollect.
	availInternal _ self primBytesLeft.
	availPhysical _ self bytesLeft: false.
	availTotal _ self bytesLeft: true.
	(availTotal > (availInternal + 10000)) "compensate for mini allocations inbetween"
		ifFalse:[^availInternal printStringWithCommas, ' bytes available'].
	^String streamContents:[:s|
		s nextPutAll: availInternal printStringWithCommas, 	' bytes (internal) '; newLine.
		s nextPutAll: availPhysical printStringWithCommas,	' bytes (physical) '; newLine.
		s nextPutAll: availTotal printStringWithCommas, 	' bytes (total)     '].! !

!SystemDictionary methodsFor: 'memory space'!
createStackOverflow
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; createStackOverflow"

	self createStackOverflow.  "infinite recursion"! !

!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 6/2/2011 09:29'!
garbageCollect
	"Primitive. Reclaims all garbage and answers the number of bytes of available space."
	ActiveModel flushEventSystem.
	^self primitiveGarbageCollect! !

!SystemDictionary methodsFor: 'memory space'!
garbageCollectMost
	"Primitive. Reclaims recently created garbage (which is usually most of it) fairly quickly and answers the number of bytes of available space."

	<primitive: 131>
	^ self primBytesLeft! !

!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 10/8/2012 22:03'!
installLowSpaceWatcher
	"Start a process to watch for low-space conditions."
	"Smalltalk installLowSpaceWatcher"

	self primSignalAtBytesLeft: 0.  "disable low-space interrupts"
	LowSpaceProcess ifNotNil: [LowSpaceProcess terminate].
	LowSpaceProcess _ [self lowSpaceWatcher] newProcess.
	LowSpaceProcess priority: Processor lowIOPriority.
	LowSpaceProcess name: 'Low Space Watcher'.
	LowSpaceProcess resume.

! !

!SystemDictionary methodsFor: 'memory space' stamp: 'di 8/18/2000 16:49'!
lowSpaceThreshold 
	"Return the low space threshold. When the amount of free memory (after garbage collection) falls below this limit, the system is in serious danger of completely exhausting memory and crashing. This limit should be made high enough to allow the user open a debugger to diagnose a problem or to save the image."

	thisContext isPseudoContext
		ifTrue: [^ 400000  "Enough for JIT compiler"]
		ifFalse: [^ 200000  "Enough for interpreter"]! !

!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 5/25/2014 15:56'!
lowSpaceWatcher
	"Wait until the low space semaphore is signalled, then take appropriate actions."

	self garbageCollectMost <= self lowSpaceThreshold ifTrue: [
		self garbageCollect <= self lowSpaceThreshold ifTrue: [
			"free space must be above threshold before starting low space watcher"
			^ Smalltalk primitiveBeep ]].

	LowSpaceSemaphore _ Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.  "enable low space interrupts"

	LowSpaceSemaphore wait.  "wait for a low space condition..."

	self primSignalAtBytesLeft: 0.  "disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess _ nil.
	"Note: user now unprotected until the low space watcher is re-installed"

	ProjectX currentInterruptNameX: 'Space is low'! !

!SystemDictionary methodsFor: 'memory space' stamp: 'nk 10/28/2000 20:37'!
lowSpaceWatcherProcess
	^LowSpaceProcess! !

!SystemDictionary methodsFor: 'memory space'!
okayToProceedEvenIfSpaceIsLow
	"Return true if either there is enough memory to do so safely or if the user gives permission after being given fair warning."

	self garbageCollectMost > self lowSpaceThreshold ifTrue: [^ true].  "quick"
	self garbageCollect > self lowSpaceThreshold ifTrue: [^ true].  "work harder"

	^ self confirm:
'WARNING: There is not enough space to start the low space watcher.
If you proceed, you will not be warned again, and the system may
run out of memory and crash. If you do proceed, you can start the
low space notifier when more space becomes available simply by
opening and then closing a debugger (e.g., by hitting Cmd-period.)
Do you want to proceed?'
! !

!SystemDictionary methodsFor: 'memory space'!
primBytesLeft
	"Primitive. Answer the number of bytes available for new object data.
	Not accurate unless preceded by
		Smalltalk garbageCollectMost (for reasonable accuracy), or
		Smalltalk garbageCollect (for real accuracy).
	See Object documentation whatIsAPrimitive."

	<primitive: 112>
	^ 0! !

!SystemDictionary methodsFor: 'memory space'!
primLowSpaceSemaphore: aSemaphore
	"Primitive. Register the given Semaphore to be signalled when the
	number of free bytes drops below some threshold. Disable low-space
	interrupts if the argument is nil."

	<primitive: 124>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'memory space'!
primSignalAtBytesLeft: numBytes
	"Tell the interpreter the low-space threshold in bytes. When the free
	space falls below this threshold, the interpreter will signal the low-space
	semaphore, if one has been registered.  Disable low-space interrupts if the
	argument is zero.  Fail if numBytes is not an Integer."

	<primitive: 125>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'memory space' stamp: 'ar 2/11/2001 02:16'!
primitiveGarbageCollect
	"Primitive. Reclaims all garbage and answers the number of bytes of available space."

	<primitive: 130>
	^ self primBytesLeft! !

!SystemDictionary methodsFor: 'memory space'!
signalLowSpace
	"Signal the low-space semaphore to alert the user that space is running low."

	LowSpaceSemaphore signal.! !

!SystemDictionary methodsFor: 'memory space' stamp: 'apb 10/3/2000 16:40'!
useUpMemory
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemory"

	| lst |
	lst _ nil.
	[true] whileTrue: [
		lst _ Link nextLink: lst.
	].! !

!SystemDictionary methodsFor: 'memory space' stamp: 'di 8/18/2000 21:15'!
useUpMemoryWithArrays 
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemoryWithArrays"

	| b |  "First use up most of memory."
	b _ String new: self bytesLeft - self lowSpaceThreshold - 100000.
	b _ b.  "Avoid unused value warning"
	(1 to: 10000) collect: [:i | Array new: 10000]! !

!SystemDictionary methodsFor: 'memory space' stamp: 'di 8/18/2000 16:49'!
useUpMemoryWithContexts 
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemoryWithContexts"

	self useUpMemoryWithContexts! !

!SystemDictionary methodsFor: 'memory space' stamp: 'di 8/18/2000 16:50'!
useUpMemoryWithTinyObjects 
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemoryWithTinyObjects"

	| b |  "First use up most of memory."
	b _ String new: self bytesLeft - self lowSpaceThreshold - 100000.
	b _ b.  "Avoid unused value warning"
	(1 to: 10000) collect: [:i | BitBlt new]! !


!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 1/4/2013 00:46'!
allContributors
"
	Smalltalk allContributors
"
	| answer author |
	answer _ Set new.
	Smalltalk allBehaviorsDo: [ :behavior |
		behavior methodsDo: [ :compiledMethod |
			author _ compiledMethod author.
			author notEmpty ifTrue: [
				answer add: author ]]].
	^answer! !

!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 12/29/2009 10:44'!
contributionsOf: aString
	"
	Smalltalk contributionsOf: 'JMV'
	"
	| author answer |
	answer _ OrderedCollection new.
	Smalltalk allBehaviorsDo: [ :behavior |
		behavior methodsDo: [ :compiledMethod |
			author _ compiledMethod author.
			aString = author ifTrue: [
				answer add: {compiledMethod methodClass. compiledMethod selector}]]].
	^answer! !

!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 12/9/2013 17:36'!
knownInitialsAndNames
	"This list could include people who hasn't contributed code to the Cuis image, but some optional package."
"
| all ok |
all _ Smalltalk allContributors asSet.
ok _ (Smalltalk contributorInitialsAndNames collect: [ :pair | pair first ]) asSet.
self assert: all = ok

initials         name"
^ #(
	#('ab' 					'Alexandre Bergel')
	#('abc' 					'Colin Putney')
	#('acg' 					'Andrew C. Greenberg')
	#('ads' 					'Adam Spitz')
	#('AFi' 					'Alain Fischer')
	#('ajh' 					'Anthony Hannan')
	#('al' 					'Adrian Lienhard')
	#('aoy' 					'Andres Otaduy')
	#('apb' 					'Andrew P. Black')
	#('ar' 					'Andreas Raab')
	#('asm' 				'Alejandro Magistrello')
	#('avi' 					'Avi Bryant')
	#('bf' 					'Bert Freudenberg')
	#('BG' 					'Boris Gaertner')
	#('BJP' 					'Bijan Parsia')
	#('bkv' 					'Brent Vukmer')
	#('bolot' 				'Bolot Kerimbaev')
	#('bp' 					'Bernhard Pieber')
	#('BP' 					'Brent Pinkney') 
	#('brp' 					'Brent Pinkney')
	#('cbc' 					'Chris Cunningham')
	#('cbr'					'Casey Ransberger')
	#('ccn' 					'Chris Norton')
	#('cmm' 				'Chris Muller')
	#('crl' 					'Craig Latta')
	#('cwp' 				'Colin Putney')
	#('das' 					'David A Smith')
	#('dc' 					'Damien Cassou')
	#('dew' 				'Doug Way')
	#('dgd' 				'Diego Gomez Deck')
	#('dkh'					'Dale Henrichs')
	#('dhhi' 				'Dan Ingalls')
	#('di' 					'Dan Ingalls')
	#('djp' 					'David J. Pennell')
	#('DKL'					'Daniel K Lyons ')
	#('DSM' 				'Duane Maxwell')
	#('DSG'					'David Graham')
	#('dtl' 					'Dave Lewis')
	#('dvf' 					'Daniel Vainsencher')
	#('eat' 					'Eric Arseneau Tremblay')
	#('eem'					'Eliot Emilio Miranda')
	#('efc' 					'Eddie Cottongim')
	#('em' 					'Ernest Micklei?')
	#('emm' 				'Ernest Micklei')
	#('fbs' 					'Frank Shearar')
	#('FBS' 					'Frank Shearar')
	#('fc' 					'Frank Caggiano')
	#('fcs' 					'Frank Sergeant')
	#('FernandoOlivero' 	'Fernando Olivero')
	#('FernanodOlivero' 	'Fernando Olivero')
	#('GabrielOmarCotelli' 	'Gabriel Omar Cotelli')
	#('gh' 					'Goran Krampe (nee Hultgren)')
	#('gk' 					'Goran Krampe (nee Hultgren)')
	#('gm' 					'German Morales')
	#('go' 					'Georg Gollmann')
	#('gsa' 					'German Arduino')
	#('hjh' 					'Hannes Hirzel')
	#('hmm' 				'Hans-Martin Mosner')
	#('hsj' 					'Henrik Sperre Johansen')
	#('Igor.Stasenko' 		'Igor Stasenko')
	#('ikp' 					'Ian Piumarta')
	#('Jb' 					'Jean Baptiste Arnaud')
	#('jcg' 					'Joshua Gargus')
	#('jdr' 					'Javier Diaz-Reinoso')
	#('je' 					'Joern Eyrich')
	#('jf' 					'Julian Fitzell')
	#('JF' 					'Julian Fitzell')
	#('jhm' 					'John Maloney')
	#('jlb' 					'Jim Benson')
	#('jm' '					John Maloney')
	#('jmb' 					'Hans Baveco')
	#('JMG'					'Jeff Gonis')
	#('JMM' 				'John McIntosh')
	#('jmv' 					'Juan Vuletich')
	#('JMV' 					'Juan Vuletich')
	#('jp' 					'Joseph Pelrine')
	#('jrm' 					'John-Reed Maffeo')
	#('jrp' 					'John Pierce')
	#('jsp' 					'Jeff Pierce')
	#('KenD' 				'Ken Dickey')
	#('kfr' 					'Karl Ramberg')
	#('KLC'			 		'Ken Causey')
	#('kph'					'Keith Hodges')
	#('KTT' 				'Kurt Thams')
	#('laza' 				'Alexander Lazarevic')
	#('LC' 					'Leandro Caniglia')
	#('len' 					'Luciano Esteban Notarfrancesco')
	#('lr' 					'Lukas Renggli')
	#('Lukas Renggli' 		'Lukas Renggli')
	#('ls' 					'Lex Spoon')
	#('md' 					'Marcus Denker')
	#('MarcusDenker' 		'Marcus Denker')
	#('marcus.denker' 		'Marcus Denker')
	#('mdr' 				'Mike Rutenberg')
	#('mga' 				'Markus Galli')
	#('mha' 				'Michael Haupt')
	#('mir' 					'Michael Rueger')
	#('mjg' 					'Mark Guzdial')
	#('mk' 					'Matej Kosik')
	#('MPH' 				'Michael Hewner')
	#('mpw' 				'Marcel Weiher')
	#('MPW' 				'Marcel Weiher')
	#('mrm' 				'Martin McClure')
	#('mtf' 					'Matthew Fulmer')
	#('mu' 					'Masashi Umezawa')
	#('nb' 					'Naala Brewer')
	#('nice'				 	'Nicolas Cellier')
	#('nk' 					'Ned Konz')
	#('nop' 					'Jay Carlson')
	#('NS' 					'Nathanael Schaerli')
	#('panda' 				'Michael Rueger')
	#('pb'					'Phil Bellalouna')
	#('PHK' 				'Peter Keeler')
	#('Pmm' 				'Philippe Marschall')
	#('pnm' 				'Paul McDonough')
	#('r++' 				'Gerardo Richarte')
	#('raa' 					'Bob Arning')
	#('RAA' 					'Bob Arning')
	#('raok' 				'Richard A. O''Keefe')
	#('rca' 					'Russell Allen')
	#('reThink'			 	'Paul McDonough')
	#('rew' 					'Roger Whitney')
	#('rhi' 					'Robert Hirschfeld')
	#('rr' 					'Romain Robbes')
	#('rss' 					'Ron Spengler')
	#('rw' 					'Robert Withers')
	#('rww' 				'Robert Withers')
	#('Sames' 				'Samuel S. Shuster')
	#('sbw' 				'Stephan B. Wessels')
	#('sd' 					'Stephane Ducasse')
	#('SD' 					'Stephane Ducasse')
	#('sge' 					'Steve Elkins')
	#('sma' 				'Stefan Matthias Aust')
	#('sps' 					'Steven Swerling')
	#('SqR' 					'Andres Valloud')
	#('sqr' 					'Andres Valloud')
	#('sr' 					'Stephan Rudlof')
	#('SSS' 				'Samuel S. Shuster')
	#('stephane.ducasse' 	'Stephane Ducasse')
	#('stephaneducasse' 	'Stephane Ducasse')
	#('stp' 					'Stephen Travis Pope')
	#('sumim' 				'Masato Sumi')
	#('svp' 					'Stephen Vincent Pair')
	#('sw' 					'Scott Wallace')
	#('TAG' 				'Travis Griggs')
	#('tak' 					'Takashi Yamamiya')
	#('tao' 					'Tim Olson')
	#('TBn' 					'Torsten Bergmann')
	#('tfei' 					'The Fourth Estate, Inc.')
	#('tfel' 					'Tim Felgentreff')
	#('th' 					'Torge Husfeldt')
	#('tk' 					'Ted Kaehler')
	#('tlk' 					'Tom Koenig')
	#('tpr' 					'Tim Rowledge')
	#('TPR' 					'Tim Rowledge')
	#('tween' 				'Andy Tween')
	#('ul' 					'Levente Uzonyi')
	#('vb' 					'Vassili Bykov')
	#('ward' 				'Ward Cunningham')
	#('wiz' 					'Jerome Peace')
	#('wod' 				'Bill Dargel')
	#('yo' 					'Yoshiki Ohshima')
	#('zz' 					'Serge Stinckwich'))! !

!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 5/25/2012 11:34'!
unknownContributors
	"Answer a collection of authorInitials for whom there is code in the system 
	(either in core or in loaded packages), but we don't knwo their full name.
	Smalltalk unknownContributors
	"

	| all ok |
	all _ Smalltalk allContributors asSet.
	ok _ (Smalltalk knownInitialsAndNames collect: [ :pair | pair first ]) asSet.
	^(all difference: ok) asArray sort! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 2/28/2011 18:59'!
cogitClass
	^self getSystemAttribute: 1008! !

!SystemDictionary methodsFor: 'miscellaneous'!
exitToDebugger
	"Primitive. Enter the machine language debugger, if one exists. Essential.
	See Object documentation whatIsAPrimitive."

	<primitive: 114>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jm 7/10/1999 08:10'!
extraVMMemory
	"Answer the current setting of the 'extraVMMemory' VM parameter. See the comment in extraVMMemory: for details."

	^ Smalltalk vmParameterAt: 23
! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jm 7/10/1999 08:26'!
extraVMMemory: extraBytesToReserve
	"Request that the given amount of extra memory be reserved for use by the virtual machine to leave extra C heap space available for things like plugins, network and file buffers, and so on. This request is stored when the image is saved and honored when the image is next started up. Answer the previous value of this parameter."

	extraBytesToReserve < 0
		ifTrue: [self error: 'VM memory reservation must be non-negative'].
	^ Smalltalk vmParameterAt: 23 put: extraBytesToReserve
! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 11/18/2011 22:47'!
getSystemAttribute: attributeID
  	"Optional. Answer the string for the system attribute with the given 
  	integer ID. Answer nil if the given attribute is not defined on this 
  	platform. On platforms that support invoking programs from command 
  	lines (e.g., Unix), this mechanism can be used to pass command line 
  	arguments to programs written in Squeak.
  
  	By convention, the first command line argument that is not a VM
  	configuration option is considered a 'document' to be filed in. Such a
  	document can add methods and classes, can contain a serialized object,
  	can include code to be executed, or any combination of these.
  
  	Currently defined attributes include: 
  	-1000	1000th command line argument that specify VM options
  	...
  	-1		first command line argument that specify VM options
  	0		the full path name for currently executing VM
  			(or, on some platforms, just the path name of the VM's directory) 
  	1		full path name of this image (better use primImageName instead)
  	2		a Squeak document to open, if any 
  	3		first command line argument for Squeak programs
  	...
  	1000	1000th command line argument for Squeak programs
  	1001	this platform's operating system 'Mac OS', 'Win32', 'unix', ...
  	1002	operating system version
  	1003	this platform's processor type
  	1004	vm version
  	1005	window system name
  	1006	vm build id
  	1007	Interpreter class (Cog VM only)
  	1008	Cogit class (Cog VM only)
 	1009	Platform source version (Cog VM only?)
  	1201	max filename length (Mac OS only)
  	1202	file last error (Mac OS only)
  	10001	hardware details (Win32 only)
  	10002	operating system details (Win32 only)
  	10003	graphics hardware details (Win32 only)
  	"

	<primitive: 149>
	^ nil! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'ikp 1/2/98 20:29'!
getVMParameters	"Smalltalk getVMParameters"
	"Answer an Array containing the current values of the VM's internal
	parameter/metric registers.  Each value is stored in the array at the
	index corresponding to its VM register.  (See #vmParameterAt: and
	#vmParameterAt:put:.)"

	<primitive: 254>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 6/15/2009 21:18'!
handleUserInterrupt
	Preferences cmdDotEnabled ifTrue: [
		[ProjectX currentInterruptNameX: 'User Interrupt'] fork]! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 2/28/2011 18:59'!
interpreterClass
	^self getSystemAttribute: 1007! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'ar 5/16/2000 00:54'!
listBuiltinModule: index
	"Return the name of the n-th builtin module.
	This list is not sorted!!"
	<primitive: 572>
	^self primitiveFailed! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'ar 6/5/2000 18:44'!
listBuiltinModules
	"Smalltalk listBuiltinModules"
	"Return a list of all builtin modules (e.g., plugins). Builtin plugins are those that are compiled with the VM directly, as opposed to plugins residing in an external shared library. The list will include all builtin plugins regardless of whether they are currently loaded or not. Note that the list returned is not sorted!!"
	| modules index name |
	modules _ WriteStream on: Array new.
	index _ 1.
	[true] whileTrue:[
		name _ self listBuiltinModule: index.
		name ifNil:[^modules contents].
		modules nextPut: name.
		index _ index + 1.
	].! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'TPR 5/16/2000 16:56'!
listLoadedModule: index
	"Return the name of the n-th loaded module.
	This list is not sorted!!"
	<primitive: 573>
	^self primitiveFailed! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'ar 6/5/2000 18:43'!
listLoadedModules
	"Smalltalk listLoadedModules"
	"Return a list of all currently loaded modules (e.g., plugins). Loaded modules are those that currently in use (e.g., active). The list returned will contain all currently active modules regardless of whether they're builtin (that is compiled with the VM) or external (e.g., residing in some external shared library). Note that the returned list is not sorted!!"
	| modules index name |
	modules _ WriteStream on: Array new.
	index _ 1.
	[true] whileTrue:[
		name _ self listLoadedModule: index.
		name ifNil:[^modules contents].
		modules nextPut: name.
		index _ index + 1.
	].! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 3/13/2012 12:50'!
logError: errMsg inContext: aContext to: aFilename
	"Log the error message and a stack trace to the given file."

	| ff |
	FileDirectory default deleteFileNamed: aFilename ifAbsent: nil.
	(ff _ FileStream fileNamed: aFilename) ifNil: [^ self "avoid recursive errors"].

  	ff nextPutAll: errMsg; newLine.
	aContext errorReportOn: ff.
	ff close.! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/31/2011 08:29'!
maxExternalSemaphores
	"The size of table where external semaphores are registered. Only in Cog"
	self isRunningCog ifFalse: [^nil].
	^self vmParameterAt: 49! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/31/2011 08:32'!
maxExternalSemaphores: aSize
	"Changes the size of table where external semaphores are registered. 
	The size can only grow, and will always be the next power of two larger than the parameter.
	
	Setting this at any time other than start-up can potentially lose requests.
	 i.e. during the realloc new storage is allocated, t
	he old contents are copied and then pointers are switched. 
	 Requests occurring during copying won't be seen if they occur to indices already copied. 
	The intended use is to set the table to some adequate maximum at start-up"
	
	self isRunningCog ifFalse: [^0].
	"The vm-header field is a short, maximum 64k entries. Well, on most platforms anyways "
	(aSize < 0 or: [aSize > 16rFFFF]) ifTrue: [^DomainError signal: 'Must be in the range (0 to: 16rFFFF)'].
	^self vmParameterAt: 49 put: aSize! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'JMM 5/29/2001 11:55'!
osVersion
	"Return the version number string of the platform we're running on"
	^(self getSystemAttribute: 1002) asString! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'ar 2/1/2000 15:39'!
platformName
	"Return the name of the platform we're running on"
	^self getSystemAttribute: 1001! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'JMM 5/29/2001 11:56'!
platformSubtype
	"Return the subType of the platform we're running on"
	^self getSystemAttribute: 1003! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 1/13/2011 16:18'!
primVmFileName
	"Answer the absolute file name of the the Smalltalk virtual machine"

	^ self getSystemAttribute: 0! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'MPH 10/24/2000 14:27'!
setMacFileInfoOn: aString
	"On Mac, set the file type and creator (noop on other platforms)"
	FileDirectory default
		setMacFileNamed: aString
		type: 'STch'
		creator: 'FAST'.! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'ar 3/10/2000 17:32'!
unloadModule: aString
	"Primitive. Unload the given module.
	This primitive is intended for development only since some
	platform do not implement unloading of DLL's accordingly.
	Also, the mechanism for unloading may not be supported
	on all platforms."
	<primitive: 571>
	^self primitiveFailed! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 1/13/2011 16:18'!
vmFileName
	"Answer the absolute file name of the the Smalltalk virtual machine"
	"
	Smalltalk vmFileName
	"

	^ self primVmFileName! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 12/4/2012 22:55'!
vmParameterAt: parameterIndex
	"parameterIndex is a positive integer corresponding to one of the VM's internal
	parameter/metric registers.  Answer with the current value of that register.
	Fail if parameterIndex has no corresponding register.
	VM parameters are numbered as follows:
		1	end of old-space (0-based, read-only)
		2	end of young-space (read-only)
		3	end of memory (read-only)
		4	allocationCount (read-only; nil in Cog VMs)
		5	allocations between GCs (read-write; nil in Cog VMs)
		6	survivor count tenuring threshold (read-write)
		7	full GCs since startup (read-only)
		8	total milliseconds in full GCs since startup (read-only)
		9	incremental GCs since startup (read-only)
		10	total milliseconds in incremental GCs since startup (read-only)
		11	tenures of surving objects since startup (read-only)
		12-20 specific to the translating VM
		21	root table size (read-only)
		22	root table overflows since startup (read-only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc.
		24	memory threshold above whichto shrink object memory (read-write)
		25	memory headroom when growing object memory (read-write)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
		27	number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking
		28	number of times sweep loop iterated for current IGC/FGC (read-only)
		29	number of times make forward loop iterated for current IGC/FGC (read-only)
		30	number of times compact move loop iterated for current IGC/FGC (read-only)
		31	number of grow memory requests (read-only)
		32	number of shrink memory requests (read-only)
		33	number of root table entries used for current IGC/FGC (read-only)
		34	number of allocations done before current IGC/FGC (read-only)
		35	number of survivor objects after current IGC/FGC (read-only)
		36	millisecond clock when current IGC/FGC completed (read-only)
		37	number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)
		38	milliseconds taken by current IGC (read-only)
		39	Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)
		40	BytesPerWord for this image
		41	imageFormatVersion for the VM
		42	number of stack pages in use (Cog Stack VM only, otherwise nil)
		43	desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
		44	size of eden, in bytes (Cog VMs only, otherwise nil)
		45	desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
		46	size of machine code zone, in bytes (stored in image file header; Cog JIT VM only, otherwise nil)
		47	desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
		48	various properties of the Cog VM as an integer encoding an array of bit flags.
			 Bit 0: implies the image's Process class has threadId as its 3rd inst var (zero relative)
		49	the size of the external semaphore table (read-write; Cog VMs only)
		50-55 reserved for VM parameters that persist in the image (such as eden above)
		56	number of process switches since startup (read-only)
		57	number of ioProcessEvents calls since startup (read-only)
		58	number of ForceInterruptCheck (Cog VMs) or quickCheckInterruptCalls (non-Cog VMs) calls since startup (read-only)
		59	number of check event calls since startup (read-only)
		60	number of stack page overflows since startup (read-only; Cog VMs only)
		61	number of stack page divorces since startup (read-only; Cog VMs only)
		62	number of machine code zone compactions since startup (read-only; Cog VMs only)
		63	milliseconds taken by machine code zone compactions since startup (read-only; Cog VMs only)
		64	current number of machine code methods (read-only; Cog VMs only)
		65	true if the VM supports multiple bytecode sets;  (read-only; Cog VMs only; nil in older Cog VMs)
		70	the value of VM_PROXY_MAJOR (the interpreterProxy major version number)
		71	the value of VM_PROXY_MINOR (the interpreterProxy minor version number)"

	<primitive: 254>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 12/4/2012 22:55'!
vmParameterAt: parameterIndex put: newValue
	"parameterIndex is a positive integer corresponding to one of the VM's internal
	parameter/metric registers.  Store newValue (a positive integer) into that
	register and answer with the previous value that was stored there.
	Fail if newValue is out of range, if parameterIndex has no corresponding
	register, or if the corresponding register is read-only.

	As of 2011-ish the parameters which can be set are
		5	allocations between GCs (read-write; nil in Cog VMs)
		6	survivor count tenuring threshold (read-write)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc.
		24	memory threshold above whichto shrink object memory (read-write)
		25	memory headroom when growing object memory (read-write)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
		43	desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
		45	desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
		47	desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
		48	various properties of the Cog VM as an integer encoding an array of bit flags.
			 Bit 0: implies the image's Process class has threadId as its 3rd inst var (zero relative)
		49	the size of the external semaphore table (read-write; Cog VMs only)"

	<primitive: 254>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'sma 6/25/2000 09:26'!
vmVersion	"Smalltalk vmVersion"
	"Return a string identifying the interpreter version"
	^self getSystemAttribute: 1004! !


!SystemDictionary methodsFor: 'object serialization' stamp: 'tk 9/28/2000 15:50'!
objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a reference to Smalltalk instead."

	dp _ DiskProxy global: #Smalltalk selector: #yourself
			args: #().
	refStrm replace: self with: dp.
	^ dp! !

!SystemDictionary methodsFor: 'object serialization' stamp: 'tk 3/7/2000 18:40'!
storeDataOn: aDataStream
	"I don't get stored.  Use a DiskProxy"

	self error: 'use a DiskProxy to store me'! !


!SystemDictionary methodsFor: 'printing' stamp: 'sma 6/1/2000 09:53'!
printElementsOn: aStream
	aStream nextPutAll:'(lots of globals)'! !

!SystemDictionary methodsFor: 'printing' stamp: 'jmv 9/3/2012 18:04'!
printOn: aStream
	self == Smalltalk
		ifTrue: [ aStream nextPutAll: 'Smalltalk' ]
		ifFalse: [ super printOn: aStream ]! !


!SystemDictionary methodsFor: 'profiling'!
clearProfile
	"Clear the profile database."

	<primitive: 250>
! !

!SystemDictionary methodsFor: 'profiling'!
dumpProfile
	"Dump the profile database to a file."

	<primitive: 251>
! !

!SystemDictionary methodsFor: 'profiling'!
profile: aBlock
	"Make a virtual machine profile of the given block."
	"Note: Profiling support is provided so that VM implementors
	 can better understand and improve the efficiency of the virtual
	 machine. To use it, you must be running a version of the
	 virtual machine compiled with profiling enabled (which
	 makes it much slower than normal even when not profiling).
	 You will also need the CodeWarrior profile reader application."

	self stopProfiling.
	self clearProfile.
	self startProfiling.
	aBlock value.
	self stopProfiling.
	self dumpProfile.! !

!SystemDictionary methodsFor: 'profiling'!
startProfiling
	"Start profiling the virtual machine."

	<primitive: 252>
! !

!SystemDictionary methodsFor: 'profiling'!
stopProfiling
	"Stop profiling the virtual machine."

	<primitive: 253>
! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 8/31/2011 22:34'!
allBehaviorsDo: aBlock 
	"Evaluate the argument, aBlock, for each kind of Behavior in the system 
	(that is, Object and its subclasses).
	ar 7/15/1999: The code below will not enumerate any obsolete or anonymous
	behaviors for which the following should be executed:

		Smalltalk allObjectsDo:[:obj| obj isBehavior ifTrue:[aBlock value: obj]].

	but what follows is way faster than enumerating all objects."

	Smalltalk do: [ :root |
		(root isBehavior and: [root superclass isNil]) ifTrue: [	"Grab ProtoObject and any other alike"
			root withAllSubclassesDo: [ :class |
				class isMeta ifFalse: [ "The metaclasses are rooted at Class; don't include them twice."
					aBlock
						value: class;
						value: class class ]]]]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:54'!
allCallsOn: aLiteral   "Smalltalk browseAllCallsOn: #open:label:."
	"Answer a Collection of all the methods that call on aLiteral."
	| aCollection special aList byte |

	#(23 48 'fred' (new open:label:)) size.
"Example above should find #open:label:, though it is deeply embedded here."

	aCollection _ OrderedCollection new.
	special _ self hasSpecialSelector: aLiteral ifTrueSetByte: [:b | byte _ b ].
	self allBehaviorsDo: [:class |
		aList _ class whichSelectorsReferTo: aLiteral special: special byte: byte.
		aList do: [ :sel |
					
			"For special selectors, look for the literal in the source code.
			Otherwise, for example, searching for senders of #== will include senders of #ifNil:"
			(byte isNil or: [
				((class sourceCodeAt: sel)
					findString: aLiteral) > 0]) ifTrue: [

				aCollection add: (
					MethodReference new
						setStandardClass: class 
						methodSymbol: sel
				)
			]
		]
	].
	^ aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:55'!
allCallsOn: firstLiteral and: secondLiteral
	"Answer a SortedCollection of all the methods that call on both aLiteral 
	and secondLiteral."

	| aCollection secondArray firstSpecial secondSpecial firstByte secondByte |
	aCollection _ SortedCollection new.
	firstSpecial _ self hasSpecialSelector: firstLiteral ifTrueSetByte: [:b | firstByte _ b].
	secondSpecial _ self hasSpecialSelector: secondLiteral ifTrueSetByte: [:b | secondByte _ b].
	self allBehaviorsDo: [ :class |
		secondArray _ class 
			whichSelectorsReferTo: secondLiteral
			special: secondSpecial
			byte: secondByte.
		((class whichSelectorsReferTo: firstLiteral special: firstSpecial byte: firstByte) select: [ :aSel |
			(secondArray includes: aSel)]) do: [ :sel | 
				aCollection add: (
					MethodReference new
						setStandardClass: class 
						methodSymbol: sel )]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 6/29/2012 16:32'!
allClasses  
	"Return all the class defines in the Smalltalk SystemDictionary"
	"
	Smalltalk allClasses
	"

	^ self classNames collect: [:name | self at: name]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'sd 4/17/2003 21:18'!
allClassesDo: aBlock
	"Evaluate the argument, aBlock, for each class in the system."

	(self classNames collect: [:name | self at: name]) do: aBlock! !

!SystemDictionary methodsFor: 'retrieving'!
allClassesImplementing: aSelector  
	"Answer an Array of all classes that implement the message aSelector."

	| aCollection |
	aCollection _ ReadWriteStream on: Array new.
	self allBehaviorsDo:
		[:class | (class includesSelector: aSelector)
			ifTrue: [aCollection nextPut: class]].
	^ aCollection contents! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 6/8/2011 08:16'!
allGlobalRefs
	"Answer a set of symbols that may be refs to Global names.
	Warning: Will not include references to a class from its own methods"

	^ self allGlobalRefsWithout: #(#() #())! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:18'!
allGlobalRefsWithout: classesAndMessagesPair
	"Answer a set of symbols that may be refs to Global names. 
	This method computes its result in the absence of specified classes and messages.
	Does not include references from a class to itself"

	| globalRefs absentClasses absentSelectors |
	globalRefs _ IdentitySet new: CompiledMethod instanceCount.
	absentClasses _ classesAndMessagesPair first.
	absentSelectors _ classesAndMessagesPair second.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
					ifTrue: [#()]
					ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl | 
			(absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				"Include all capitalized symbols for good measure"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m | 
					((m isMemberOf: Symbol) and: [ m size > 0] and: [m first isUppercase]) ifTrue: [
						m = cl name ifFalse: [
							globalRefs add: m]].
					(m isVariableBinding) ifTrue:
						[m key ifNotNil: [
							m key = cl name ifFalse: [
								globalRefs add: m key]]]]]]].
	^ globalRefs! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 6/8/2011 08:16'!
allImplementedMessages
	"Answer a Set of all the messages that are implemented in the system."

	^ self allImplementedMessagesWithout:  #(#() #())! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:19'!
allImplementedMessagesWithout: classesAndMessagesPair
	"Answer a Set of all the messages that are implemented in the system, computed in the absence of the supplied classes and messages.  Note this reports messages that are in the absent selectors set."
	| messages absentClasses |
	messages _ IdentitySet new: CompiledMethod instanceCount.
	absentClasses _ classesAndMessagesPair first.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
			ifTrue: [#()]
			ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
				messages addAll: cl selectors]].
	^ messages! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:56'!
allImplementorsOf: aSelector  
	"Answer a SortedCollection of all the methods that implement the message 
	aSelector."

	| aCollection |

	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :class |
		(class includesSelector: aSelector) ifTrue: [
			aCollection add: (
				MethodReference new
					setStandardClass: class 
					methodSymbol: aSelector )]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:57'!
allImplementorsOf: aSelector  localTo: aClass
	"Answer a sorted Collection of all the methods that implement the message 
	aSelector in, above, or below the given class."

	| aSet cls |
	aSet _ Set new.
	cls _ aClass theNonMetaClass.
	cls withAllSuperAndSubclassesDoGently: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [aSet add: class name, ' ', aSelector]].
	cls class withAllSuperAndSubclassesDoGently: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [aSet add: class name, ' ', aSelector]].
	^aSet asArray sort! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:57'!
allMethodsInCategory: category 
	| aCollection |
	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :x |
		(x organization listAtCategoryNamed: category) do: [ :sel |
			aCollection add: x name , ' ' , sel]].
	^aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 8/30/2011 08:22'!
allMethodsWithSourceString: aString matchCase: caseSensitive
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"
	| list classCount adder |
	list _ Set new.
	adder _ [ :mrClass :mrSel |
	list add:
		(MethodReference new
			setStandardClass: mrClass
			methodSymbol: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size
		during: [ :bar |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				bar value: (classCount _ classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						((cl sourceCodeAt: sel)
							findString: aString
							startingAt: 1
							caseSensitive: caseSensitive) > 0 ifTrue: [
							adder
								value: cl
								value: sel ]].
					(cl organization classComment asString
						findString: aString
						startingAt: 1
						caseSensitive: caseSensitive) > 0 ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 15:58'!
allMethodsWithString: aString
	"Answer a sorted Collection of all the methods that contain, in a string literal, aString as a substring.  2/1/96 sw.  The search is case-sensitive, and does not dive into complex literals, confining itself to string constants.
	5/2/96 sw: fixed so that duplicate occurrences of aString in the same method don't result in duplicated entries in the browser"
	| aStringSize list |
	aStringSize _ aString size.
	list _ Set new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel |
			(class compiledMethodAt: sel) literalsDo: [ :aLiteral |
				((aLiteral isMemberOf: String) and: [ aLiteral size >= aStringSize ]) ifTrue: [
					(aLiteral
						findString: aString
						startingAt: 1) > 0 ifTrue: [ list add: class name , ' ' , sel ]]]]].
	^ list asArray sort! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'ul 1/11/2014 16:18'!
allObjectsDo: aBlock 
	"Evaluate the argument, aBlock, for each object in the system
	 excluding SmallIntegers."

	| object lastObject |
	object _ self someObject.
	lastObject _ Object new.
	[ lastObject == object ]
		whileFalse: [
			| nextObject |
			nextObject := object nextObject.
			aBlock value: object.
			object := nextObject ]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'di 10/28/2001 10:51'!
allObjectsSelect: aBlock 
	"Evaluate the argument, aBlock, for each object in the system excluding SmallIntegers.
	Return a collection af all objects for whom the value is true."

	^ Array streamContents:
		[:s | self allObjectsDo: [:object | (aBlock value: object) ifTrue: [s nextPut: object]]]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:20'!
allPrimitiveMessages
	"Answer an OrderedCollection of all the methods that are implemented by 
	primitives."

	| aColl method | 
	aColl _ OrderedCollection new: 200.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel | 
			method _ class compiledMethodAt: sel.
			method primitive ~= 0 ifTrue: [
				aColl addLast: class name , ' ' , sel 
					, ' ' , method primitive printString]]].
	^aColl! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:21'!
allPrimitiveMethodsInCategories: aList
	"Answer an OrderedCollection of all the methods that are implemented by primitives in the given categories.  1/26/96 sw"

	| aColl method | 
	aColl _ OrderedCollection new: 200.
	self allBehaviorsDo: [ :aClass |
		(aList includes: (SystemOrganization categoryOfElement: aClass theNonMetaClass name asString) asString)
			ifTrue: [
				aClass selectorsDo: [ :sel | 
					method _ aClass compiledMethodAt: sel.
					method primitive ~= 0
						ifTrue: [
							aColl addLast: aClass name , ' ' , sel 
								, ' ' , method primitive printString]]]].
	^ aColl

"Smalltalk allPrimitiveMethodsInCategories: #('Collections-Streams' 'Files-Streams' 'Files-Abstract' 'Files-Macintosh')"! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:21'!
allSelect: aBlock
	"Answer a SortedCollection of each method that, when used as the block 
	argument to aBlock, gives a true result."
	| aCollection |
	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel |
			(aBlock value: (class compiledMethodAt: sel)) ifTrue: [
				aCollection add: (
					MethodReference new
						setStandardClass: class 
						methodSymbol: sel)
			]]].
	^ aCollection! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 6/8/2011 08:16'!
allSentMessages
	"Answer the set of selectors which are sent somewhere in the system."

	^ self allSentMessagesWithout: #(#() #())! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:24'!
allSentMessagesWithout: classesAndMessagesPair
	"Answer the set of selectors which are sent somewhere in the system, 
	computed in the absence of the supplied classes and messages."

	| sent absentClasses absentSelectors |
	sent _ IdentitySet new: CompiledMethod instanceCount.
	absentClasses _ classesAndMessagesPair first.
	absentSelectors _ classesAndMessagesPair second.
	self classNames do: [ :cName | 
		((absentClasses includes: cName)
				ifTrue: [#()]
				ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
			 (absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				 "Include all sels, but not if sent by self"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m |
					m == sel ifFalse: [sent add: m] ]]]].
		"The following may be sent without being in any literal frame"
		1 to: self specialSelectorSize do: [ :index | 
			sent add: (self specialSelectorAt: index)].
	Smalltalk presumedSentMessages do: [ :sel | sent add: sel].
	^ sent! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 6/8/2011 08:17'!
allUnSentMessages   "Smalltalk allUnSentMessages"
	"Answer the set of selectors that are implemented by some object in the system but not sent by any."

	^ self allUnSentMessagesWithout: #(#() #())! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'di 2/25/2001 21:51'!
allUnSentMessagesIn: selectorSet
	"Answer the subset of selectorSet which are not sent anywhere in the system."

	^ selectorSet copyWithoutAll: self allSentMessages! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'di 2/25/2001 23:17'!
allUnSentMessagesWithout: classesAndMessagesPair
	"Answer the set of selectors that are implemented but not sent, computed in the absence of the supplied classes and messages."

	^ (self allImplementedMessagesWithout: classesAndMessagesPair)
		copyWithoutAll: (self allSentMessagesWithout: classesAndMessagesPair)! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:25'!
allUnimplementedCalls 
	"Answer an Array of each message that is sent by an expression in a 
	method but is not implemented by any object in the system."

	| aStream secondStream all  |
	all _ self allImplementedMessages.
	aStream _ WriteStream on: (Array new: 50).
	self allBehaviorsDo: [ :cl |
		 cl selectorsDo: [ :sel |
			 secondStream _ WriteStream on: (String new: 5).
			(cl compiledMethodAt: sel) messages do: [ :m |
				(all includes: m) ifFalse: [secondStream nextPutAll: m; space]].
			secondStream position = 0 ifFalse: [
				aStream nextPut: cl name , ' ' , sel , ' calls: ', secondStream contents ]]].
	^aStream contents! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 3/2/2010 10:11'!
allUnusedClassesWithout: classesAndMessagesPair
	"Enumerates all classes in the system and returns a list of those that are apparently unused. A class is considered in use if it (a) has subclasses or (b) is referred to by some method or (c) has its name in use as a literal (but not in the same class) or: (d) some instance is a global."
	"Smalltalk unusedClasses"

	| unused cl |
	Smalltalk garbageCollect.
	unused _ Smalltalk classNames asIdentitySet
				copyWithoutAll: (self allGlobalRefsWithout: classesAndMessagesPair).
				
	Smalltalk do: [ :global |
		unused remove: global class name ifAbsent: nil].
	
	^ unused reject:
		[:cName | cl _ Smalltalk at: cName.
		cl subclasses notEmpty
			"or: [ cl someInstance notNil ]"
			or: [cl inheritsFrom: FileDirectory]]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:00'!
isThereAReferenceTo: aLiteral
	"Answer a Collection of all the methods that call on aLiteral."
	"
	Smalltalk isThereAReferenceTo: #open:label:
	"
	| special byte |

	#(23 48 'fred' (new open:label:)) size.
"Example above should find #open:label:, though it is deeply embedded here."

	special _ self hasSpecialSelector: aLiteral ifTrueSetByte: [:b | byte _ b ].
	self allBehaviorsDo: [:class |
		(class whichSelectorsReferTo: aLiteral special: special byte: byte) do: [ :sel |
			^true ]].
	^ false! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'sw 7/27/2001 18:39'!
isThereAnImplementorOf: aSelector  
	"Answer true if there is at least one implementor of the selector found in the system, false if there are no implementors"

	self allBehaviorsDo:
		[:class |
			(class includesSelector: aSelector)
				ifTrue: [^ true]].
	^ false
"
Smalltalk isThereAnImplementorOf: #contents.
Smalltalk isThereAnImplementorOf: #nobodyImplementsThis.
"! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 4/19/2014 18:43'!
isThereAnImplementorOf: aSelector exceptFor: aCollectionOfBehaviors
	"Answer whether if there is at least some other implementor of the selector besides aBehavior."

	self allBehaviorsDo: [ :class |
		((aCollectionOfBehaviors includes: class) not and: [class includesSelector: aSelector])
			ifTrue: [^ true]].
	^ false
"
Smalltalk isThereAnImplementorOf: #contents.
Smalltalk isThereAnImplementorOf: #nobodyImplementsThis.
"! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'sw 1/8/2001 11:31'!
numberOfImplementorsOf: aSelector  
	"Answer a count of the implementors of the given selector found in the system"

	| aCount |
	aCount _ 0.
	self allBehaviorsDo:
		[:class |
			(class includesSelector: aSelector)
				ifTrue: [aCount _ aCount + 1]].
	^ aCount
"
Smalltalk numberOfImplementorsOf: #contents.
Smalltalk numberOfImplementorsOf: #nobodyImplementsThis. 
Smalltalk numberOfimplementorsOf: #numberOfImplementorsOf:.
"! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:00'!
numberOfSendersOf: aSymbol
	"Answer the count of all the methods that call on aLiteral.
	[ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun
	"
	| count specialFlag specialByte |

	count _ 0.
	specialFlag _ self hasSpecialSelector: aSymbol ifTrueSetByte: [ :b | specialByte _ b ].
	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :method |
			((method hasLiteral: aSymbol) or: [specialFlag and: [method scanFor: specialByte]])
				ifTrue: [ count _ count + 1 ]]].
	^ count! !

!SystemDictionary methodsFor: 'retrieving'!
pointersTo: anObject
	"Find all occurrences in the system of pointers to the argument anObject."
	"(Smalltalk pointersTo: Browser) inspect."

	^ self pointersTo: anObject except: #()
! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 1/13/2011 15:55'!
pointersTo: anObject except: objectsToExclude 
	"Find all occurrences in the system of pointers to the argument
	anObject. Remove objects in the exclusion list from the
	results. "
	^ anObject inboundPointersExcluding: objectsToExclude! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/25/2011 19:39'!
poolUsers
	"Answer a dictionary of pool name -> classes that refer to it. Also includes any globally know dictionaries (such as Smalltalk, Undeclared etc) which although not strictly accurate is potentially useful information "
	"Smalltalk poolUsers"
	| poolUsers pool refs |
	poolUsers _ Dictionary new.
	Smalltalk keys
		do: [ :k |
			 (((pool _ Smalltalk at: k) isKindOf: Dictionary)
					or: [pool isKindOf: SharedPool class])
				ifTrue: [refs _ Smalltalk allClasses
								select: [:c | c sharedPools identityIncludes: pool]
								thenCollect: [:c | c name].
					refs _ refs asOrderedCollection.
					refs
						add: (Smalltalk
								allCallsOn: (Smalltalk associationAt: k)).
					poolUsers at: k put: refs]].
	^ poolUsers! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 5/26/2013 16:26'!
unimplemented
	"Answer an Array of each message that is sent by an expression in a method but is not implemented by any object in the system."

	| all unimplemented entry |
	all _ IdentitySet new: Symbol instanceCount * 2.
	self allBehaviorsDo: [ :cl | cl selectorsDo: [ :aSelector | all add: aSelector]].

	unimplemented _ IdentityDictionary new.
	self allBehaviorsDo: [:cl |
		 cl selectorsDo: [:sel |
			(cl compiledMethodAt: sel) messages do: [ :m |
				(all includes: m) ifFalse: [
					entry _ unimplemented at: m ifAbsent: [Array new].
					entry _ entry copyWith: (cl name, '>', sel).
					unimplemented at: m put: entry]]]].

	"remove some clutter from the result:"
	#(doPrimitive: primitiveFail success:) do: [ :sel |
		unimplemented removeKey: sel ifAbsent: nil].

	^ unimplemented! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 3/5/2013 11:06'!
abandonSources
	"
	Smalltalk abandonSources
	"
	 | m bTotal bCount |
	(self confirm:
'This method will detach the image fom source code.
A fresh changes file will be created to record further changes.
-- CAUTION --
If you have backed up your system and
are prepared to face the consequences of
abandoning source code files, choose Yes.
If you have any doubts, you may choose No
to back out with no harm done.')
		==  true ifFalse: [^ self inform: 'Okay - no harm done'].
	bTotal _ 0.  bCount _ 0.
	Smalltalk allBehaviorsDo: [: b | bTotal _ bTotal + 1].
'Doing #destroySourcePointer ...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: bTotal
	during: [ :bar |
		Smalltalk allBehaviorsDo: [ :cl |
		"for testing"
		"{ EllipseMorph } do: [ :cl |"
			bar value: (bCount _ bCount + 1).
			cl selectors do: [:selector |
				m _ cl compiledMethodAt: selector.
				m destroySourcePointer ]]].
	Smalltalk allBehaviorsDo: [:b | b zapOrganization].
	Smalltalk closeSourceFiles.
	Preferences disable: #warnIfNoChangesFile.
	Preferences disable: #warnIfNoSourcesFile! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/25/2013 23:21'!
presumedSentMessages
	| sent |
	"
	In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent:
	Smalltalk presumedSentMessages
	"

	"The following should be preserved for doIts, etc"
	sent _ IdentitySet new.
	#( rehashWithoutBecome compactSymbolTable
		browseAllSelect:  lastRemoval
		vScrollBarValue: hScrollBarValue: 
		to: removeClassNamed:
		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib
		newDepth: restoreAfter: zapAllMethods obsoleteClasses
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		zapOrganization condenseSources condenseChanges browseObsoleteReferences
		subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:
		startTimerEventLoop unusedClasses allClasses
		unimplemented
		reduceCuis
		variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		printSpaceAnalysis:on:) do: [ :sel |
			sent add: sel].
	"The following may be sent by perform: in dispatchOnChar..."
	Editor withAllSubclassesDo: [ :c |
		c shortcuts asSet do: [ :sel | sent add: sel ].
		c cmdShortcuts asSet do: [ :sel | sent add: sel ]].
	#(beReadOnlyBinding beReadWriteBinding) do: [ :sel |
		sent add: sel].
	^ sent! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 9/3/2013 12:00'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	PasteUpMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	FormCanvas clearFormsCache.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
"	ColorPickerMorph class removeSelector: #buildEyedropperIcon."
	Theme removeSelector: #miscellaneousIcons.
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont removeMostFonts.
	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally).
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 4/18/2011 14:41'!
removeAllUnSentMessages
	"Smalltalk removeAllUnSentMessages"
	"[Smalltalk unusedClasses do: [:c | (Smalltalk at: c) removeFromSystem]. 
	Smalltalk removeAllUnSentMessages > 0] whileTrue."
	"Remove all implementations of unsent messages."
	| sels n |
	sels _ self allUnSentMessages.
	self presumedSentMessages
		do: [:sel | sels
				remove: sel
				ifAbsent: nil].
	sels size = 0
		ifTrue: [^ 0].
	n _ 0.
	Smalltalk
		allBehaviorsDo: [:x | n _ n + 1].
	'Removing ' , sels size printString , ' messages . . .'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: n
		during: [:bar | 
			n _ 0.
			self
				allBehaviorsDo: [:class | 
					bar value: (n _ n + 1).
					sels
						do: [:sel | class removeSelector: sel]]].
	^ sels size! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'sma 6/18/2000 11:34'!
removeSelector: descriptor
	"Safely remove a selector from a class (or metaclass). If the class
	or the method doesn't exist anymore, never mind and answer nil.
	This method should be used instead of 'Class removeSelector: #method'
	to omit global class references."

	| class sel |
	class _ Smalltalk at: descriptor first ifAbsent: [^ nil].
	(descriptor size > 2 and: [descriptor second == #class])
		ifTrue:
			[class _ class class.
			sel _ descriptor third]
		ifFalse: [sel _ descriptor second].
	^ class removeSelector: sel! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 4/7/2009 14:24'!
removedUnusedClassesAndMethods
	[
		#hereWeGo print.
		Smalltalk unusedClasses do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		Smalltalk removeAllUnSentMessages > 0 or: [ Smalltalk unusedClasses notEmpty ]] whileTrue.! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/8/2011 08:17'!
reportClassAndMethodRemovalsFor: collectionOfClassNames
	| initialClassesAndMethods finalClassesAndMethods |
	"Smalltalk reportClassAndMethodRemovalsFor: #(Celeste Scamper MailMessage)"

	initialClassesAndMethods _ self unusedClassesAndMethodsWithout: #(#() #()).
	finalClassesAndMethods _ self unusedClassesAndMethodsWithout: {collectionOfClassNames. #()}.
	^ {finalClassesAndMethods first copyWithoutAll: initialClassesAndMethods first.
		finalClassesAndMethods second copyWithoutAll: initialClassesAndMethods second}! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/8/2011 08:17'!
unusedClasses
	"Enumerates all classes in the system and returns a list of those that are apparently unused. A class is considered in use if it (a) has subclasses or (b) is referred to by some method or (c) has its name in use as a literal (but not in the same class) or (d) some instance is a global."
	"Smalltalk unusedClasses asSortedCollection"

	^ self allUnusedClassesWithout: #(#() #())! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 3/13/2012 17:02'!
unusedClassesAndMethodsWithout: classesAndMessagesPair
	| classRemovals messageRemovals nClasses nMessages |
	"Accepts and returns a pair: {set of class names. set of selectors}.
	It is expected these results will be diff'd with the normally unused results."

	(classRemovals _ IdentitySet new) addAll: classesAndMessagesPair first.
	(messageRemovals _ IdentitySet new) addAll: classesAndMessagesPair second.
	nClasses _ nMessages _ -1.
	["As long as we keep making progress..."
	classRemovals size > nClasses or: [messageRemovals size > nMessages]]
		whileTrue:
			["...keep trying for bigger sets of unused classes and selectors."
			nClasses _ classRemovals size.
			nMessages _ messageRemovals size.
			Utilities informUser: 'Iterating removals ' ,
					(classesAndMessagesPair first isEmpty
						ifTrue: ['for baseline...']
						ifFalse: ['for ', classesAndMessagesPair first first, ' etc...']) , String newLineString ,
					nClasses printString , ' classes, ' , nMessages printString , ' messages.
|
|' "spacers move menu off cursor"
				during:
				[classRemovals addAll: (self allUnusedClassesWithout: {classRemovals. messageRemovals}).
				messageRemovals addAll: (self allUnSentMessagesWithout: {classRemovals. messageRemovals})]].
	^ {classRemovals. self allUnSentMessagesWithout: {classRemovals. messageRemovals}}! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/2/2010 10:11'!
add: aClass toList: startUpOrShutDownList after: predecessor
	"Add the name of aClass to the startUp or shutDown list.
	Add it after the name of predecessor, or at the end if predecessor is nil."

	| name earlierName |
	name _ aClass name.
	(self at: name ifAbsent: nil) == aClass ifFalse: [
		self error: name , ' cannot be found in Smalltalk dictionary.'].
	predecessor
		ifNil: ["No-op if alredy in the list."
				(startUpOrShutDownList includes: name) ifFalse:
					[startUpOrShutDownList == StartUpList
						ifTrue: ["Add to end of startUp list"
								startUpOrShutDownList addLast: name]
						ifFalse: ["Add to front of shutDown list"
								startUpOrShutDownList addFirst: name]]]
		ifNotNil: ["Add after predecessor, moving it if already there."
				earlierName _ predecessor name.
				(self at: earlierName) == predecessor ifFalse: [
					self error: earlierName , ' cannot be found in Smalltalk dictionary.'].
				(startUpOrShutDownList includes: earlierName) ifFalse: [
					self error: earlierName , ' cannot be found in the list.'].
				startUpOrShutDownList remove: name ifAbsent: nil.
				startUpOrShutDownList add: name after: earlierName]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'di 2/4/1999 15:38'!
addToShutDownList: aClass
	"This will add a ref to this class at the BEGINNING of the shutDown list."

	self addToShutDownList: aClass after: nil! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'di 2/3/1999 22:04'!
addToShutDownList: aClass after: predecessor

	self add: aClass toList: ShutDownList after: predecessor! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'di 2/4/1999 15:37'!
addToStartUpList: aClass
	"This will add a ref to this class at the END of the startUp list."

	self addToStartUpList: aClass after: nil! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'di 2/3/1999 22:04'!
addToStartUpList: aClass after: predecessor

	self add: aClass toList: StartUpList after: predecessor! !

!SystemDictionary methodsFor: 'snapshot and quit'!
lastQuitLogPosition
	^ LastQuitLogPosition! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/22/2013 21:13'!
okayToDiscardUnsavedCode
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCode
	"

	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSet allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges and: [ any isEmpty not ]]].
	dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ].

	baseCSdirty & dirtyPackages ifTrue: [
		^self confirm: 'There are both unsaved Packages', String newLineString,
			'and unsaved Changes to Cuis core.', String newLineString,
			'If you continue, they will all be lost.', String newLineString,
			'Continue?' ].
	baseCSdirty ifTrue: [
		^self confirm: 'Some ChangeSet for Cuis core might have unsaved changes.', String newLineString,
			'If you continue, they would be lost.', String newLineString,
			'Continue?' ].
	dirtyPackages ifTrue: [
		^self confirm: 'There are unsaved Packages.', String newLineString,
			'If you continue, they will all be lost.', String newLineString,
			'Continue?' ].

	^true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/22/2013 21:12'!
okayToSave
	"Answer true unless the user cancels saving because of some warning given."

	| wasCog isCog |
	isCog _ Smalltalk isRunningCog.
	[ wasCog _ self imageFormatVersionFromFile allMask: 1 ]
		on: Error
		do: [ :ignore |
			"probably save-as to non-existing file"
			^ true ].

	(isCog and: [wasCog not]) ifTrue: [
		(self confirm: 'You''re running with a Cog VM.', String newLineString,
			'Non-Cog VMs might not be able to open images saved under Cog!!', String newLineString,
			'(If you choose "YES", you might only use this image under Cog VMs.)', String newLineString,
			'(If you choose "NO", you might save your work in some other way, and later exit Cuis without saving).', String newLineString,
			'Really save?')
				ifFalse: [ ^false ]].
		
	^ true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/1/2010 23:55'!
processShutDownList: quitting
	"Send #shutDown to each class that needs to wrap up before a snapshot."

	self send: #shutDown: toClassesNamedIn: ShutDownList with: quitting.
	EndianCache _ nil! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/1/2010 23:55'!
processStartUpList: resuming
	"Send #startUp to each class that needs to run initialization after a snapshot."

	EndianCache _ self calcEndianness.
	self send: #startUp: toClassesNamedIn: StartUpList with: resuming! !

!SystemDictionary methodsFor: 'snapshot and quit'!
quitPrimitive
	"Primitive. Exit to another operating system on the host machine, if one
	exists. All state changes in the object space since the last snapshot are lost.
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 113>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'ar 4/24/2001 16:38'!
readDocumentFile
	"No longer used. Everything is now done in ProjectLauncher."
	StartupStamp _ '----STARTUP----', Time dateAndTimeNow printString, ' as ', Smalltalk imageName.
! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/2/2010 10:12'!
removeFromShutDownList: aClass

	ShutDownList remove: aClass name ifAbsent: nil! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/2/2010 10:12'!
removeFromStartUpList: aClass

	StartUpList remove: aClass name ifAbsent: nil! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/21/2013 14:30'!
saveAs
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	| newName |
	newName _ self getFileNameFromUser.
	newName ifNil: [^ self].
 	self okayToSave ifFalse: [^self].
	self saveAs: newName! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/21/2013 22:11'!
saveAs: newName
	"Save the image  under a new name."

	self currentChangesName ifNotNil: [ :oldChangesName | | newChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileDirectory default 
			copyFileWithoutOverwriteConfirmationNamed: oldChangesName
			toFileNamed: newChangesName.
		self setMacFileInfoOn: newChangesName ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: false! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/24/2012 19:48'!
saveAsEmbeddedImage
	"Save the current state of the system as an embedded image"

	| dir newName newImageName |
	dir _ FileDirectory default.
	newName _ FillInTheBlankMorph
		request: 'Select existing VM file'
		initialAnswer: (FileDirectory localNameFor: '').
	newName = '' ifTrue: [^ self].
	newName _ FileDirectory baseNameFor: newName asFileName.

	newImageName _ newName.
	(dir includesKey: newImageName) ifFalse:
		[^ self inform: 'Unable to find name ', newName, ' Please choose another name.'].

	self logChange: '----SAVEAS (EMBEDDED) ', newName, '----', Date dateAndTimeNow printString.
	self imageName: (dir fullNameFor: newImageName).
	LastImageName _ self imageName.
	self closeSourceFiles.
	self snapshot: true andQuit: true embedded: true
! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/27/2014 22:32'!
saveAsNewVersion
	"Save the image/changes using the next available version number."
	"
	Smalltalk saveAsNewVersion
	"
	| fileName newName changesName systemVersion |
	self okayToSave ifFalse: [ ^ self ].
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate ].
	newName _ fileName, FileDirectory dot,  FileDirectory imageSuffix.
	(FileDirectory default includesKey: newName) ifTrue: [
		newName _ FileDirectory default
			nextNameFor: fileName
			extension: FileDirectory imageSuffix ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	(FileDirectory default includesKey: changesName) ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', newName, '
curiously already present, even though there is
no corresponding .image file.   Please remedy
manually and then repeat your request.' ].
	self saveAs: newName.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'sw 2/8/1999 12:37'!
saveSession
	self snapshot: true andQuit: false! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/8/2013 15:17'!
send: startUpOrShutDown toClassesNamedIn: startUpOrShutDownList with: argument
	"Send the message #startUp: or #shutDown: to each class named in the list.
	The argument indicates if the system is about to quit (for #shutDown:) or if
	the image is resuming (for #startUp:).
	If any name cannot be found, then remove it from the list."

	| removals class |
	removals _ OrderedCollection new.
	startUpOrShutDownList do:
		[:name |
		class _ self at: name ifAbsent: nil.
		class
			ifNil: [removals add: name]
			ifNotNil: [
				class isInMemory ifTrue: [
					class perform: startUpOrShutDown with: argument]]].

	"Remove any obsolete entries, but after the iteration"
	"Well, not. Better just ignore them. Maybe it is stuff, like SoundPlayer, that was moved to optional packages, and can be loaded again anytime."
	"startUpOrShutDownList removeAll: removals"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jm 6/7/1999 21:33'!
setGCParameters
	"Adjust the VM's default GC parameters to avoid premature tenuring."

	Smalltalk vmParameterAt: 5 put: 4000.  "do an incremental GC after this many allocations"
	Smalltalk vmParameterAt: 6 put: 2000.  "tenure when more than this many objects survive the GC"
! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'ar 2/2/2001 14:08'!
setPlatformPreferences
	"Set some platform specific preferences on system startup"
	| platform specs |
	Preferences automaticPlatformSettings ifFalse:[^self].
	platform _ self platformName.
	specs _ 	#(	
					(soundStopWhenDone false)
					(soundQuickStart false)
			).
	platform = 'Win32' ifTrue:[
		specs _ #(	
					(soundStopWhenDone true)
					(soundQuickStart false)
				)].
	platform = 'Mac OS' ifTrue:[
		specs _ #(	
					(soundStopWhenDone false)
					(soundQuickStart true)
				)].
	specs do:[:tuple|
		Preferences setPreference: tuple first toValue: (tuple last == #true).
	].
! !

!SystemDictionary methodsFor: 'snapshot and quit'!
shutDown
	^ self closeSourceFiles! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/22/2013 21:14'!
snapshot: save andQuit: quit
	save
		ifTrue: [
			self okayToSave ifFalse: [ ^ self ].
			ChangeSet zapAllChangeSets ]
		ifFalse: [
			quit ifTrue: [
				self okayToDiscardUnsavedCode ifFalse: [ ^ self ]]].
	^ self
		snapshot: save
		andQuit: quit
		embedded: false! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'JMM 11/21/2000 21:02'!
snapshotEmbeddedPrimitive
	<primitive: 247>
	^nil "indicates error writing embedded image file"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'ar 7/22/2000 14:34'!
snapshotPrimitive
	"Primitive. Write the current state of the object memory on a file in the
	same format as the Smalltalk-80 release. The file can later be resumed,
	returning you to this exact state. Return normally after writing the file.
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 97>
	^nil "indicates error writing image file"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'ar 3/26/2000 16:12'!
unbindExternalPrimitives
	"Primitive. Force all external primitives to be looked up again afterwards. Since external primitives that have not found are bound for fast failure this method will force the lookup of all primitives again so that after adding some plugin the primitives may be found."
	<primitive: 570>
	"Do nothing if the primitive fails for compatibility with older VMs"! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'sw 2/15/1999 16:18'!
aboutThisSystem 
	"Identify software version"

	^ self inform: self systemInformationString! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/13/2012 12:48'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile |
	StartupStamp ifNil: [^ self].
	(SourceFiles isNil or: [(changesFile _ SourceFiles at: 2) == nil]) ifTrue: [^ self].
	changesFile isReadOnly ifTrue:[^self].
	changesFile setToEnd; newLine; newLine.
	changesFile nextChunkPut: StartupStamp asString; newLine.
	StartupStamp _ nil.
	self forceChangesToDisk! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 9/1/2010 23:54'!
calcEndianness
	| bytes word blt |
	"What endian-ness is the current hardware?  The String '1234' will be stored into a machine word.  On BigEndian machines (the Mac), $1 will be the high byte if the word.  On LittleEndian machines (the PC), $4 will be the high byte."
	"Smalltalk endianness"

	bytes _ ByteArray withAll: #(0 0 0 0).  "(1 2 3 4) or (4 3 2 1)"
	word _ WordArray with: 16r01020304.
	blt _ (BitBlt toForm: (Form new hackBits: bytes)) 
				sourceForm: (Form new hackBits: word).
	blt combinationRule: Form over.  "store"
	blt sourceY: 0; destY: 0; height: 1; width: 4.
	blt sourceX: 0; destX: 0.
	blt copyBits.  "paste the word into the bytes"
	bytes first = 1 ifTrue: [^ #big].
	bytes first = 4 ifTrue: [^ #little].
	self error: 'Ted is confused'.! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/19/2012 08:18'!
classRemoved: aClass fromCategory: aCategoryName

	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			self logChange: 'Smalltalk removeClassNamed: #' , aClass name ]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/21/2013 21:58'!
closeSourceFiles
	"Shut down the source files if appropriate.  1/29/96 sw: changed so that the closing and nilification only take place if the entry was a FileStream, thus allowing stringified sources to remain in the saved image file"

	1 to: 2 do: [:i |
		((SourceFiles at: i) isKindOf: FileStream)
			ifTrue: [
				(SourceFiles at: i) close.
				SourceFiles at: i put: nil]]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'mir 5/1/2001 18:20'!
datedVersion
	"Answer the version of this release."

	^SystemVersion current datedVersion! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 9/1/2010 23:58'!
endianness
	"Endianness is cached. If it happens to be nil, we are in the midst of either shutdown or startup. In such cases, compute endiannes but DO NOT cache it. I.e. do not do lazy initialization."
	^EndianCache ifNil: [ self calcEndianness ]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/19/2012 08:21'!
evaluated: expression context: aContext

	self logChange: expression! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/20/2013 22:55'!
externalizeSources   
	"Write the sources and changes streams onto external files."
 	"Smalltalk externalizeSources"

	| sourcesName changesName aFile |
	sourcesName _ self defaultSourcesName.
	(FileDirectory default fileExists: sourcesName)
		ifTrue: [^ self inform:
'Sorry, you must first move or remove the
file named ', sourcesName].
	changesName _ self defaultChangesName.
	(FileDirectory default fileExists: changesName)
		ifTrue: [^ self inform:
'Sorry, you must first move or remove the
file named ', changesName].

	aFile _ FileStream newFileNamed: sourcesName.
	aFile nextPutAll: SourceFiles first originalContents.
	aFile close.
	self setMacFileInfoOn: sourcesName.
	SourceFiles at: 1 put: (FileStream readOnlyFileNamed: sourcesName).

	aFile _ FileStream newFileNamed: changesName.
	aFile nextPutAll: SourceFiles last contents.
	aFile close.
	"On Mac, set the file type and creator (noop on other platforms)"
	FileDirectory default
		setMacFileNamed: changesName
		type: 'STch'
		creator: 'FAST'.
	SourceFiles at: 2 put: (FileStream oldFileNamed: changesName).

	self inform: 'Sources successfully externalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 10/18/2006 22:27'!
forceChangesToDisk
	"Ensure that the changes file has been fully written to disk by closing and re-opening it. This makes the system more robust in the face of a power failure or hard-reboot."

	| changesFile |
	changesFile _ SourceFiles at: 2.
	(changesFile isKindOf: FileStream) ifTrue: [
		changesFile flush.
		changesFile close.
		changesFile open: changesFile name forWrite: true.
		changesFile setToEnd.
	].
! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/21/2013 22:17'!
internalizeChangeLog    
		"
		Smalltalk internalizeChangeLog
		"
	"Bring the changes file into a memory-resident filestream, for faster access and freedom from external file system.  1/31/96 sw"

	| reply aFile |
	reply _ self confirm:  'CAUTION -- do not undertake this lightly!!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	(aFile _ SourceFiles second) ifNil: [
		^ self halt: 'Cannot locate Changes file so cannot proceed.'].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, changes file internalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/21/2013 22:16'!
internalizeSources
		"
		Smalltalk internalizeSources
		"
	"Bring the sources and changes files into memory-resident filestreams, for faster access and freedom from file-system interface.  1/29/96 sw"

	| reply aFile |
	reply _ self confirm:  'CAUTION -- do not undertake this lightly!!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	(aFile _ SourceFiles first) ifNil: [
		^ self halt: 'Cannot locate Sources file so cannot proceed.'].
	SourceFiles at: 1 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	(aFile _ SourceFiles last) ifNil: [
		^ self halt: 'Cannot locate Changes so cannot proceed.'].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, sources internalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'ar 2/27/2001 15:05'!
isBigEndian
	^self endianness == #big! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'ar 2/27/2001 15:05'!
isLittleEndian
	^self endianness == #little! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'mir 5/1/2001 18:02'!
lastUpdateString
	"Smalltalk lastUpdateString"
	^'latest update: #', SystemVersion current highestUpdate printString! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/13/2012 12:50'!
logChange: aStringOrText 
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	(SourceFiles isNil or: [(SourceFiles at: 2) == nil]) ifTrue: [^ self].
	self assureStartupStampLogged.

	aString _ aStringOrText asString.
	(aString findFirst: [:char | char isSeparator not]) = 0
		ifTrue: [^ self].  "null doits confuse replay"
	(changesFile _ SourceFiles at: 2).
	changesFile isReadOnly ifTrue:[^self].
	changesFile setToEnd; newLine; newLine.
	changesFile nextChunkPut: aString.
		"If want style changes in DoIt, use nextChunkPutWithStyle:, and allow Texts to get here"
	self forceChangesToDisk.! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/13/2012 12:50'!
logChange: aStringOrText preamble: preamble
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	(SourceFiles isNil or: [(SourceFiles at: 2) == nil]) ifTrue: [^ self].
	self assureStartupStampLogged.

	aString _ aStringOrText asString.
	(aString findFirst: [:char | char isSeparator not]) = 0
		ifTrue: [^ self].  "null doits confuse replay"
	(changesFile _ SourceFiles at: 2).
	changesFile isReadOnly ifTrue:[^self].
	changesFile setToEnd; newLine; newLine.
	changesFile nextPut: $!!; nextChunkPut: preamble; newLine.
	changesFile nextChunkPut: aString.
		"If want style changes in DoIt, use nextChunkPutWithStyle:, and allow Texts to get here"
	self forceChangesToDisk.! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/19/2012 08:22'!
methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			self logChange: aClass name , ' removeSelector: #' , aSymbol ]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/29/2013 22:31'!
openSourceFiles
	self imageName = LastImageName ifFalse: [
		LastImageName _ self imageName.
		"Reset the author initials to blank when the image gets moved"
		Utilities clearAuthor.
		"And clear any absolute path that is most likely invalid now."
		CodePackage clearPaths ].
	"Warning: Do open the source files only if nil.
	If not nil, it is because they are internalized and the files should not be opened"
	self openSourcesAndChanges.
	CuisSourceFileArray install! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/29/2013 23:37'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sourcesPathName changesPathName sources changes msg wmsg |
	msg _ 'Cuis cannot locate 
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		sourcesPathName _ Smalltalk defaultSourcesName.
		sources _ FileStream oldFileOrNoneNamed: sourcesPathName.
		sources ifNil: [
			sourcesPathName _ Smalltalk alternativeSourcesName.
			sources _ FileStream oldFileOrNoneNamed: sourcesPathName ]].
	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , sourcesPathName) ].

	changesPathName _ Smalltalk defaultChangesName.
	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ FileStream oldFileNamed: changesPathName ].
	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (msg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

	((Preferences valueOfFlag: #warnIfNoChangesFile) and: [changes notNil]) ifTrue: [
		changes isReadOnly ifTrue: [
			self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

		((changes next: 200) includesSubString: String crlfString) ifTrue: [
			self inform: 'The changes file named ' , changesPathName , '
has been injured by an unpacking utility.  Lfs were changed to CrLfs.
Please set the preferences in your decompressing program to 
"do not convert text files" and unpack the system again.']].

	SourceFiles _ Array with: sources with: changes! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/19/2012 16:14'!
systemInformationString
	"Identify software version"
	^ SystemVersion current version, String newLineString, self lastUpdateString! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'mir 5/1/2001 18:19'!
timeStamp: aStream 
	"Writes system version and current time on stream aStream."

	| dateTime |
	dateTime _ Time dateAndTimeNow.
	aStream nextPutAll: 'From ', Smalltalk datedVersion, ' [', Smalltalk lastUpdateString, '] on ', (dateTime at: 1) printString,
						' at ', (dateTime at: 2) printString! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'mir 5/1/2001 18:17'!
version
	"Answer the version of this release."

	^SystemVersion current version! !


!SystemDictionary methodsFor: 'special objects' stamp: 'JMM 6/6/2000 20:36'!
clearExternalObjects
	"Clear the array of objects that have been registered for use in non-Smalltalk code."
	"Smalltalk clearExternalObjects"

	ExternalSemaphoreTable clearExternalObjects
! !

!SystemDictionary methodsFor: 'special objects'!
compactClassesArray  "Smalltalk compactClassesArray"
	"Return the array of 31 classes whose instances may be
	represented compactly"
	^ Smalltalk specialObjectsArray at: 29! !

!SystemDictionary methodsFor: 'special objects' stamp: 'JMM 6/6/2000 21:01'!
externalObjects
	"Return an array of objects that have been registered for use in non-Smalltalk code. Smalltalk objects should be referrenced by external code only via indirection through this array, thus allowing the objects to move during compaction. This array can be cleared when the VM re-starts, since variables in external code do not survive snapshots. Note that external code should not attempt to access a Smalltalk object, even via this mechanism, while garbage collection is in progress."
	"Smalltalk externalObjects"

	^ ExternalSemaphoreTable externalObjects
! !

!SystemDictionary methodsFor: 'special objects'!
hasSpecialSelector: aLiteral ifTrueSetByte: aBlock

	1 to: self specialSelectorSize do:
		[:index | 
		(self specialSelectorAt: index) == aLiteral
			ifTrue: [aBlock value: index + 16rAF. ^true]].
	^false! !

!SystemDictionary methodsFor: 'special objects' stamp: 'eem 10/29/2009 10:52'!
primitiveErrorTable
	"Smalltalk primitiveErrorTable"
	^self specialObjectsArray at: 52! !

!SystemDictionary methodsFor: 'special objects' stamp: 'jmv 4/2/2013 14:01'!
recreateSpecialObjectsArray
	"Smalltalk recreateSpecialObjectsArray"
	
	"To external package developers:
	**** DO NOT OVERRIDE THIS METHOD.  *****
	If you are writing a plugin and need additional special object(s) for your own use, 
	use addGCRoot() function and use own, separate special objects registry "
	
	"The Special Objects Array is an array of objects used by the Squeak virtual machine.
	 Its contents are critical and accesses to it by the VM are unchecked, so don't even
	 think of playing here unless you know what you are doing."
	| newArray |
	newArray := Array new: 56.
	"Nil false and true get used throughout the interpreter"
	newArray at: 1 put: nil.
	newArray at: 2 put: false.
	newArray at: 3 put: true.
	"This association holds the active process (a ProcessScheduler)"
	newArray at: 4 put: (self associationAt: #Processor).
	"Numerous classes below used for type checking and instantiation"
	newArray at: 5 put: Bitmap.
	newArray at: 6 put: SmallInteger.
	newArray at: 7 put: String.
	newArray at: 8 put: Array.
	newArray at: 9 put: Smalltalk.
	newArray at: 10 put: Float.
	newArray at: 11 put: MethodContext.
	newArray at: 12 put: BlockContext.
	newArray at: 13 put: Point.
	newArray at: 14 put: LargePositiveInteger.
	newArray at: 15 put: Display.
	newArray at: 16 put: Message.
	newArray at: 17 put: CompiledMethod.
	newArray at: 18 put: (self specialObjectsArray at: 18).
	"(low space Semaphore)"
	newArray at: 19 put: Semaphore.
	newArray at: 20 put: Character.
	newArray at: 21 put: #doesNotUnderstand:.
	newArray at: 22 put: #cannotReturn:.
	newArray at: 23 put: nil. "This is the process signalling low space."
	"An array of the 32 selectors that are compiled as special bytecodes,
	 paired alternately with the number of arguments each takes."
	newArray at: 24 put: #(	#+ 1 #- 1 #< 1 #> 1 #<= 1 #>= 1 #= 1 #~= 1
							#* 1 #/ 1 #\\ 1 #@ 1 #bitShift: 1 #// 1 #bitAnd: 1 #bitOr: 1
							#at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #== 1 #class 0
							#blockCopy: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).
	"An array of the 255 Characters in ascii order.
	 Cog inlines table into machine code at: prim so do not regenerate it."
"	newArray at: 25 put: ((0 to: 255) collect: [:ascii | Character value: ascii])."
	newArray at: 25 put: (self specialObjectsArray at: 25).
	newArray at: 26 put: #mustBeBoolean.
	newArray at: 27 put: ByteArray.
	newArray at: 28 put: Process.
	"An array of up to 31 classes whose instances will have compact headers"
	newArray at: 29 put: self compactClassesArray.
	newArray at: 30 put: (self specialObjectsArray at: 30).
	"(delay Semaphore)"
	newArray at: 31 put: (self specialObjectsArray at: 31).
	"(user interrupt Semaphore)"
	"Prototype instances that can be copied for fast initialization"
	newArray at: 32 put: Float new.
	newArray at: 33 put: (LargePositiveInteger new: 4).
	newArray at: 34 put: Point new.
	newArray at: 35 put: #cannotInterpret:.
	"Note: This must be fixed once we start using context prototypes (yeah, right)"
	"(MethodContext new: CompiledMethod fullFrameSize)."
	newArray at: 36 put: (self specialObjectsArray at: 36). "Is the prototype MethodContext (unused by the VM)"
	newArray at: 37 put: BlockClosure.
	"(BlockContext new: CompiledMethod fullFrameSize)."
	newArray at: 38 put: (self specialObjectsArray at: 38). "Is the prototype BlockContext (unused by the VM)"
	"array of objects referred to by external code"
	newArray at: 39 put: (self specialObjectsArray at: 39).	"preserve external semaphores"
	newArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	newArray at: 41 put: nil. "Reserved for a LinkedList instance for overlapped calls in CogMT"
	"finalization Semaphore"
	newArray at: 42 put: ((self specialObjectsArray at: 42) ifNil: [Semaphore new]).
	newArray at: 43 put: LargeNegativeInteger.
	"External objects for callout.
	 Note: Written so that one can actually completely remove the FFI."
	newArray at: 44 put: (self at: #ExternalAddress ifAbsent: []).
	newArray at: 45 put: (self at: #ExternalStructure ifAbsent: []).
	newArray at: 46 put: (self at: #ExternalData ifAbsent: []).
	newArray at: 47 put: (self at: #ExternalFunction ifAbsent: []).
	newArray at: 48 put: (self at: #ExternalLibrary ifAbsent: []).
	newArray at: 49 put: #aboutToReturn:through:.
	newArray at: 50 put: #run:with:in:.
	"51 reserved for immutability message"
	"newArray at: 51 put: #attemptToAssign:withIndex:."
	newArray at: 52 put: #(nil "nil => generic error" #'bad receiver'
							#'bad argument' #'bad index'
							#'bad number of arguments'
							#'inappropriate operation'  #'unsupported operation'
							#'no modification' #'insufficient object memory'
							#'insufficient C memory' #'not found' #'bad method'
							#'internal error in named primitive machinery'
							#'object may move').
	"53 to 55 are for Alien"
	newArray at: 53 put: (self at: #Alien ifAbsent: []).
	newArray at: 54 put: #invokeCallback:stack:registers:jmpbuf:.
	newArray at: 55 put: (self at: #UnsafeAlien ifAbsent: []).

	"Weak reference finalization"
	newArray at: 56 put: (self at: #WeakFinalizer ifAbsent: []).

	"Now replace the interpreter's reference in one atomic operation"
	self specialObjectsArray become: newArray
	! !

!SystemDictionary methodsFor: 'special objects' stamp: 'JMM 6/6/2000 20:39'!
registerExternalObject: anObject
	"Register the given object in the external objects array and return its index. If it is already there, just return its index."

	^ExternalSemaphoreTable registerExternalObject: anObject! !

!SystemDictionary methodsFor: 'special objects'!
specialNargsAt: anInteger 
	"Answer the number of arguments for the special selector at: anInteger."

	^ (self specialObjectsArray at: 24) at: anInteger * 2! !

!SystemDictionary methodsFor: 'special objects'!
specialObjectsArray  "Smalltalk specialObjectsArray at: 1"
	<primitive: 129>
	^ self primitiveFailed! !

!SystemDictionary methodsFor: 'special objects'!
specialSelectorAt: anInteger 
	"Answer the special message selector from the interleaved specialSelectors array."

	^ (self specialObjectsArray at: 24) at: anInteger * 2 - 1! !

!SystemDictionary methodsFor: 'special objects'!
specialSelectorSize
	"Answer the number of special selectors in the system."

	^ (self specialObjectsArray at: 24) size // 2! !

!SystemDictionary methodsFor: 'special objects'!
specialSelectors
	"Used by SystemTracer only."

	^SpecialSelectors! !

!SystemDictionary methodsFor: 'special objects' stamp: 'JMM 6/6/2000 20:40'!
unregisterExternalObject: anObject
	"Unregister the given object in the external objects array. Do nothing if it isn't registered."

	ExternalSemaphoreTable unregisterExternalObject: anObject! !


!SystemDictionary methodsFor: 'toDeprecate' stamp: 'jmv 9/24/2012 19:48'!
getFileNameFromUser

	| newName |
	newName _ FillInTheBlankMorph
		request: 'New File Name?'
		initialAnswer: (FileDirectory localNameFor: self imageName).
	newName isEmpty ifTrue: [^nil].
	((FileDirectory default fileOrDirectoryExists: (self fullNameForImageNamed: newName)) or:
	 [FileDirectory default fileOrDirectoryExists: (self fullNameForChangesNamed: newName)]) ifTrue: [
		(self confirm: ('{1} already exists. Overwrite?' format: {newName})) ifFalse: [^nil]].
	^newName
! !

!SystemDictionary methodsFor: 'toDeprecate' stamp: 'jmv 3/29/2012 12:27'!
snapshot: save andQuit: quit embedded: embeddedFlag
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
	| resuming msg |
	ActiveModel flushEventSystem.
	(SourceFiles at: 2) ifNotNil: [
		msg _ String streamContents: [ :s |
			s
				nextPutAll: '----';
				nextPutAll:
				(save
					ifTrue: [
						quit
							ifTrue: [ 'QUIT' ]
							ifFalse: [ 'SNAPSHOT' ]]
					ifFalse: [
						quit
							ifTrue: [ 'QUIT/NOSAVE' ]
							ifFalse: [ 'NOP' ]]);
				nextPutAll: '----';
				print: Date dateAndTimeNow;
				space;
				nextPutAll: (FileDirectory default localNameFor: self imageName);
				nextPutAll: ' priorSource: ';
				print: LastQuitLogPosition ].
		self assureStartupStampLogged.
		save ifTrue: [
			LastQuitLogPosition _ (SourceFiles at: 2)
				 setToEnd;
				 position ].
		self logChange: msg.
		Transcript
			 newLine;
			 show: msg;
			 newLine ].
	self processShutDownList: quit.
	Cursor write show.
	save
		ifTrue: [
			resuming _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ resuming _ false ].
	quit & (resuming == false) ifTrue: [ self quitPrimitive ].
	Cursor normal show.
	self setGCParameters.
	resuming == true ifTrue: [ self clearExternalObjects ].
	self processStartUpList: resuming == true.
	resuming == true ifTrue: [
		self setPlatformPreferences.
		self readDocumentFile ].
	"Now it's time to raise an error"
	resuming ifNil: [ self error: 'Failed to write image file (disk full?)' ].
	^ resuming! !


!SystemDictionary methodsFor: 'ui' stamp: 'jmv 5/25/2014 15:54'!
beep
	"
	Smalltalk beep
	"
	Preferences soundsEnabled ifTrue: [
		Smalltalk
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]
			ifAbsent: [ self primitiveBeep ]]! !

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 12/8/2008 15:49'!
confirmRemovalOf: aSelector on: aClass 
	"Determine if it is okay to remove the given selector. Answer 1 if it  
	should be removed, 2 if it should be removed followed by a senders  
	browse, and 3 if it should not be removed."
	| count aMenu answer caption allCalls |
	allCalls _ self allCallsOn: aSelector.
	(count _ allCalls size) = 0
		ifTrue: [^ 1].
	"no senders -- let the removal happen without warning"
	count = 1
		ifTrue: [(allCalls first actualClass == aClass
					and: [allCalls first methodSymbol == aSelector])
				ifTrue: [^ 1]].
	"only sender is itself"
	aMenu _ PopUpMenu labels: 'Remove it
Remove, then browse senders
Don''t remove, but show me those senders
Forget it -- do nothing -- sorry I asked'.
	caption _ 'This message has ' , count printString , ' sender'.
	count > 1
		ifTrue: [caption _ caption copyWith: $s].
	answer _ aMenu startUpWithCaption: caption.
	answer = 3
		ifTrue: [self
				browseMessageList: allCalls
				name: 'Senders of ' , aSelector
				autoSelect: aSelector keywords first].
	answer = 0
		ifTrue: [answer _ 3].
	"If user didn't answer, treat it as cancel"
	^ answer min: 3! !

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 1/17/2011 00:24'!
inspectGlobals
	"Smalltalk  inspectGlobals"
	
	| associations aDict |
	associations _ ((self  keys select: [:aKey | ((self  at: aKey) isKindOf: Class) not]) collect:[:aKey | self associationAt: aKey]).
	aDict _ IdentityDictionary new.
	associations do: [:as | aDict add: as].
	aDict inspectWithLabel: 'The Globals'! !

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 5/25/2014 15:49'!
primitiveBeep
	"Make a primitive beep. Not to be called directly."
	"
	Smalltalk primitiveBeep
	"

	<primitive: 140>
	self primitiveFailed! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/26/2013 15:58'!
browseAllAccessesTo: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods 
	or any methods of a subclass/superclass that refer to the instance variable name."

	"self new browseAllAccessesTo: 'contents' from: Collection."
	
	| coll |
	coll _ OrderedCollection new.
	aClass withAllSubAndSuperclassesDo: [:class | 
		(class whichSelectorsAccess: instVarName) do: [:sel |
			coll add: (
				MethodReference new
					setStandardClass: class 
					methodSymbol: sel)]].
	^ self 
		browseMessageList: coll 
		name: 'Accesses to ' , instVarName 
		autoSelect: instVarName! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/23/2010 09:50'!
browseAllCallsOn: aLiteral
	"Create and schedule a message browser on each method that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (self allCallsOn: aLiteral) asArray sort
				name: 'Users of ' , aLiteral key
				autoSelect: aLiteral key ]
		ifFalse: [
			self
				browseMessageList: (self allCallsOn: aLiteral) asArray sort
				name: 'Senders of ' , aLiteral
				autoSelect: aLiteral keywords first ].! !

!SystemDictionary methodsFor: 'browsing'!
browseAllCallsOn: literal1 and: literal2 
	"Create and schedule a message browser on each method that calls on the 
	two Symbols, literal1 and literal2. For example, Smalltalk 
	browseAllCallsOn: #at: and: #at:put:."

	^self 
		browseMessageList: (self allCallsOn: literal1 and: literal2)
		name: literal1 printString , ' -and- ' , literal2 printString! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/23/2010 09:51'!
browseAllCallsOn: aLiteral localTo: aClass
	"Create and schedule a message browser on each method in or below the given class that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	aClass ifNil: [ ^ self inform: 'no selected class' ].
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral) asArray sort
				name: 'Users of ' , aLiteral key , ' local to ' , aClass name
				autoSelect: aLiteral key ]
		ifFalse: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral) asArray sort
				name: 'Senders of ' , aLiteral , ' local to ' , aClass name
				autoSelect: aLiteral keywords first ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/23/2010 09:51'!
browseAllCallsOnClass: aClass
	"Create and schedule a message browser on each method that refers to 
	aClass. For example, SystemNavigation new browseAllCallsOnClass: Object."
	self
		browseMessageList: aClass allCallsOn asArray sort
		name: 'Users of class ' , aClass theNonMetaClass name
		autoSelect: aClass theNonMetaClass name.! !

!SystemDictionary methodsFor: 'browsing'!
browseAllImplementorsOf: selector
	"Create and schedule a message browser on each method that implements 
	the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put:."

	^self browseMessageList: (self allImplementorsOf: selector) name: 'Implementors of ' , selector! !

!SystemDictionary methodsFor: 'browsing' stamp: 'nk 4/10/2001 07:55'!
browseAllImplementorsOf: selector localTo: aClass
	"Create and schedule a message browser on each method in or below the given class
	that implements the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put: localTo: Dictionary."

	aClass ifNil: [ ^self inform: 'no class selected' ].
	^self browseMessageList: (self allImplementorsOf: selector localTo: aClass)
		name: 'Implementors of ' , selector, ' local to ', aClass name! !

!SystemDictionary methodsFor: 'browsing'!
browseAllImplementorsOfList: selectorList
	"Create and schedule a message browser on each method that implements 
	the message whose selector is in the argument selectorList. For example, 
	Smalltalk browseAllImplementorsOf: #(at:put: size).
	1/16/96 sw: defer to the titled version"

	self browseAllImplementorsOfList: selectorList title: 'Implementors of all'! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/26/2013 16:32'!
browseAllImplementorsOfList: selectorList title: aTitle
	"Create and schedule a message browser on each method that implements the message whose selector is in the argument selectorList. For example,  Smalltalk browseAllImplementorsOf: #(at:put: size)."

	| implementorLists flattenedList |

	implementorLists _ selectorList collect: [:each | self allImplementorsOf: each].
	flattenedList _ Array streamContents: [ :stream |
		implementorLists do: [ :each | stream nextPutAll: each]].
	flattenedList sort.
	^ self browseMessageList: flattenedList name: aTitle! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 12/28/2011 19:28'!
browseAllMethodsInCategory: aSymbol 
	^self
		browseMessageList: (self allMethodsInCategory: aSymbol)
		name: aSymbol! !

!SystemDictionary methodsFor: 'browsing'!
browseAllSelect: aBlock
	"Create and schedule a message browser on each method that, when used 
	as the block argument to aBlock gives a true result. For example, 
	Smalltalk browseAllSelect: [:method | method numLiterals > 10]."

	^self browseMessageList: (self allSelect: aBlock) name: 'selected messages'! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/30/2011 08:21'!
browseAllSelect: aBlock name: aName autoSelect: autoSelectString
	"Create and schedule a message browser on each method that, when used 
	as the block argument to aBlock gives a true result.   Do not return an #DoIt traces."

	"Smalltalk browseAllSelect: [:method | method numLiterals > 10] name: 'Methods with more than 10 literals' autoSelect: 'isDigit'"

	^ self browseMessageList: (self allSelect: aBlock) name: aName autoSelect: autoSelectString! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/26/2013 15:58'!
browseAllStoresInto: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods 
	or any methods of a subclass/superclass that refer to the instance variable name."
	
	"self new browseAllStoresInto: 'contents' from: Collection."

	| coll |
	coll _ OrderedCollection new.
	aClass withAllSubAndSuperclassesDo: [:class | 
		(class whichSelectorsStoreInto: instVarName) do: [:sel |
			coll add: (
				MethodReference new
					setStandardClass: class 
					methodSymbol: sel)]].
	^ self
		browseMessageList: coll 
		name: 'Stores into ' , instVarName 
		autoSelect: instVarName! !

!SystemDictionary methodsFor: 'browsing' stamp: 'sw 1/16/2002 21:03'!
browseAllUnSentMessages
	"Create and schedule a message browser on each method whose message is  not sent in any method in the system."
	
	self browseAllImplementorsOfList: self allUnSentMessages title: 'Messages implemented but not sent'

"Smalltalk browseAllUnSentMessages"! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 6/6/2012 11:58'!
browseAllUnimplementedCalls
	"Create and schedule a message browser on each method that includes a 
	message that is not implemented in any object in the system.
	Smalltalk browseAllUnimplementedCalls
	"

	^self browseMessageList: self allUnimplementedCalls name: 'Unimplemented calls'! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/26/2013 15:59'!
browseClassCommentsWithString: aString
	"Smalltalk browseClassCommentsWithString: 'my instances' "
	"Launch a message list browser on all class comments containing aString as a substring."
	| caseSensitive suffix list |
	suffix _ (caseSensitive _ Sensor shiftPressed)
		ifTrue: [ ' (case-sensitive)' ]
		ifFalse: [ ' (use shift for case-sensitive)' ].
	list _ Set new.
	Smalltalk allClassesDo: [ :class |
		(class organization classComment asString
			findString: aString
			startingAt: 1
			caseSensitive: caseSensitive) > 0 ifTrue: [
			list add:
				(MethodReference new
					setStandardClass: class
					methodSymbol: #Comment) ]].
	^ self
		browseMessageList: list asArray sort
		name: 'Class comments containing ', aString printString, suffix
		autoSelect: aString.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/26/2013 15:59'!
browseClassesWithNamesContaining: aString caseSensitive: caseSensitive 
	"Smalltalk browseClassesWithNamesContaining: 'eMorph' caseSensitive: true "
	"Launch a class-list list browser on all classes whose names containg aString as a substring."

	| suffix aList |
	suffix _ caseSensitive
				ifTrue: [' (case-sensitive)']
				ifFalse: [' (use shift for case-sensitive)'].
	aList _ OrderedCollection new.
	Smalltalk allClassesDo: [ :class |
		(class name includesSubstring: aString caseSensitive: caseSensitive)
			ifTrue: [aList add: class name]].
	aList size > 0
		ifTrue: [HierarchyBrowserWindow forClassesNamed: aList asSet asSortedArray title: 'Classes whose names contain ' , aString , suffix]! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 3/2/2010 09:20'!
browseEqSmallConstant

    "Dan Ingalls' search for arithmetic use of ==
Answer whether the receiver contains the pattern <expression> == <constant>,
where constant is -1, 0, 1, or 2...
	Smalltalk browseEqSmallConstant
"

	Smalltalk browseAllSelect: [:m | m scanForEqSmallConstant]! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 2/14/2008 19:00'!
browseInstVarDefs: aClass
	"Copied from browseInstVarRefs.  Should be consolidated some day. 7/29/96 di
	7/30/96 sw: did the consolidation"

	aClass chooseInstVarThenDo:	
		[:aVar | self browseAllStoresInto: aVar from: aClass]! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 2/14/2008 23:41'!
browseInstVarRefs: aClass
	"1/16/96 sw: moved here from Browser so that it could be used from a variety of places.
	 7/30/96 sw: call chooseInstVarThenDo: to get the inst var choice"

	aClass chooseInstVarThenDo: 
		[:aVar | self browseAllAccessesTo: aVar from: aClass]! !

!SystemDictionary methodsFor: 'browsing'!
browseMessageList: messageList name: label 
	"Create and schedule a MessageSet browser on messageList."
	^ self browseMessageList: messageList name: label autoSelect: nil! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/25/2011 22:25'!
browseMessageList: messageList name: labelString autoSelect: autoSelectString
	| title aSize |
	"Create and schedule a MessageSet browser on the message list."

	messageList size = 0 ifTrue: 
		[^ (PopUpMenu labels: ' OK ')
				startUpWithCaption: 'There are no
' , labelString].

	title _ (aSize _ messageList size) > 1
		ifFalse:	[ labelString ]
		ifTrue:	[ '[', aSize printString, '] ', labelString].

	MessageSetWindow
		openMessageList: messageList
		label: title
		autoSelect: autoSelectString! !

!SystemDictionary methodsFor: 'browsing' stamp: 'RAA 5/28/2001 13:56'!
browseMethodsWithSourceString: aString
	"Smalltalk browseMethodsWithSourceString: 'SourceString' "
	"Launch a browser on all methods whose source code contains aString as a substring."

	| caseSensitive suffix |
	suffix _ (caseSensitive _ Sensor shiftPressed)
		ifTrue: [' (case-sensitive)']
		ifFalse: [' (use shift for case-sensitive)'].
	^ self 
		browseMessageList: (self allMethodsWithSourceString: aString matchCase: caseSensitive)
		name: 'Methods containing ' , aString printString , suffix
		autoSelect: aString! !

!SystemDictionary methodsFor: 'browsing' stamp: 'sw 11/21/2001 15:09'!
browseMethodsWithString: aString
	"Launch a browser on all methods that contain string literals with aString as a substring. The search is case-insensitive, unless the shift key is pressed, in which case the search is case-sensitive."

	'string for testing'.
	^ self browseMethodsWithString: aString matchCase: Sensor shiftPressed

	"Smalltalk browseMethodsWithString: 'Testing' matchCase: false"
	"Smalltalk browseMethodsWithString: 'Testing' matchCase: true"! !

!SystemDictionary methodsFor: 'browsing' stamp: 'sw 10/29/2001 09:00'!
browseMethodsWithString: aString matchCase: caseSensitive
	"Launch a browser on all methods that contain string literals with aString as a substring. Make the search case-sensitive or insensitive as dictated by the caseSensitive boolean parameter"

	self browseAllSelect:
			[:method |
				method  hasLiteralSuchThat: [:lit |
					lit class == String and:
					[lit includesSubstring: aString caseSensitive: caseSensitive]]]
		name:  'Methods with string ', aString printString, (caseSensitive ifTrue: [' (case-sensitive)'] ifFalse: [' (case-insensitive)'])
		autoSelect: aString.
! !

!SystemDictionary methodsFor: 'browsing' stamp: 'dtl 8/1/2010 10:12'!
browseMyChanges
	"Browse only the changes (in the changes file) by the current author.
	Smalltalk browseMyChanges
	"
	self browseAllSelect: [ :method |
       method fileIndex > 1 "only look at changes file"
               and: [ method timeStamp beginsWith: Utilities authorInitials ]]! !

!SystemDictionary methodsFor: 'browsing' stamp: 'ar 8/16/2001 13:29'!
browseObsoleteReferences   "Smalltalk browseObsoleteReferences"
	| references |
	references _ OrderedCollection new.
	(LookupKey allSubInstances select:
		[:x | ((x value isKindOf: Behavior) and: ['AnOb*' match: x value name]) or:
		['AnOb*' match: x value class name]]) 
		do: [:x | references addAll: (Smalltalk allCallsOn: x)].
	Smalltalk 
		browseMessageList: references 
		name: 'References to Obsolete Classes'! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/30/2011 08:23'!
browseViewReferencesFromNonViews
	"
	Smalltalk browseViewReferencesFromNonViews
	"
	| aList aLiteral aCollection |
	aCollection _ OrderedCollection new.

	"Tweak to look just for pluggables or also for menus (or maybe for all morphs)"
"	PopUpMenu withAllSubclasses , MenuMorph withAllSubclasses , PluggableMorph withAllSubclasses do: [ :view |"
	PluggableMorph withAllSubclassesDo: [ :view |
"	MenuMorph withAllSubclassesDo: [ :view |"
		
		aLiteral _ view name.
		
		"tweak to linclude refs to SysWindow subhierarchy or not"
		(view includesBehavior: SystemWindow) 
		& false
		ifFalse: [
		
			Smalltalk allBehaviorsDo: [ :class |
				((class includesBehavior: Morph) or: [ class includesBehavior: Morph class ]) ifFalse: [
					aList _ class
						whichSelectorsReferTo: aLiteral
						special: false
						byte: nil.
					aList do: [ :sel |
						"For special selectors, look for the literal in the source code.
						Otherwise, for example, searching for senders of #== will include senders of #ifNil:"
						((class sourceCodeAt: sel) findString: aLiteral) > 0  ifTrue: [
							aCollection add:
								(MethodReference new
									setStandardClass: class
									methodSymbol: sel) ]]]]]].
	Smalltalk
		browseMessageList: aCollection asSet asArray sort
		name: 'References to Views from non-Views'
		autoSelect: ''.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'sw 12/5/2000 14:18'!
showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock
	"Show a sorted menu of the given selectors, preceded by firstItem, and all
	abbreviated to 40 characters.  Evaluate choiceBlock if a message is chosen."

	^ self showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock withCaption: nil! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 3/13/2012 12:51'!
showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock withCaption: aCaption
	"Show a sorted menu of the given selectors, preceded by firstItem, and all abbreviated to 40 characters.  Use aCaption as the menu title, if it is not nil.  Evaluate choiceBlock if a message is chosen."
	| index menuLabels sortedList aMenu |
	sortedList _ selectorCollection asArray sort.
	menuLabels _ String streamContents: [ :strm |
		strm nextPutAll: (firstItem contractTo: 40).
		sortedList do: [ :sel |
			strm
				 newLine;
				 nextPutAll: (sel contractTo: 40) ]].
	aMenu _ PopUpMenu
		labels: menuLabels
		lines: #(1 ).
	index _ aCaption
		ifNil: [ aMenu startUp ]
		ifNotNil: [ aMenu startUpWithCaption: aCaption ].
	index = 1 ifTrue: [ choiceBlock value: firstItem ].
	index > 1 ifTrue: [ choiceBlock value: (sortedList at: index - 1) ].! !


!SystemDictionary methodsFor: 'private' stamp: 'jmv 4/7/2009 14:23'!
allSymbolsIn: anArray do: aBlock
	"Recursively dig into anArray and evaluate aBlock on any Symbols found"
	"allSymbols and variableBindings"
	
	anArray do: [ :elem |
		((elem isMemberOf: Symbol) or: [ elem isVariableBinding ])
			ifTrue: [ aBlock value: elem ]
			ifFalse: [
				(elem isMemberOf: Array) ifTrue: [ 
					self allSymbolsIn: elem do: aBlock ]]]! !


!SystemDictionary methodsFor: 'Compiler swapping' stamp: 'jmv 12/29/2009 11:41'!
actualCompiledMethodWithNodeClass
	"Sending this message replaces direct references to some class"
	^CompiledMethodWithNode! !

!SystemDictionary methodsFor: 'Compiler swapping' stamp: 'jmv 12/29/2009 11:42'!
actualDecompilerClass
	"Sending this message replaces direct references to some class"
	^Decompiler! !

!SystemDictionary methodsFor: 'Compiler swapping' stamp: 'jmv 12/29/2009 11:43'!
actualEncoderClass
	"Sending this message replaces direct references to some class"
	^Encoder! !

!SystemDictionary methodsFor: 'Compiler swapping' stamp: 'jmv 12/29/2009 11:46'!
actualScannerClass
	"Sending this message replaces direct references to some class"
	^Scanner! !


!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:43'!
browseMethodsWithClosuresThatAccessOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatAccessOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesUAR :anyClosureUsesSelf |
				anyClosureHasCopied ].
			])
		name: 'Closures that read or write to outer temps'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:43'!
browseMethodsWithClosuresThatOnlyReadOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatOnlyReadOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount
					:hasIndirectTemps :anyClosureHasCopiedValues :anyClosureDoesUAR :anyClosureUsesSelf |
				anyClosureHasCopiedValues & hasIndirectTemps not].
			])
		name: 'Closures that read but not write to outer temps'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:43'!
browseMethodsWithClosuresThatWriteOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatWriteOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesUAR :anyClosureUsesSelf |
				hasIndirectTemps ].
			])
		name: ' Closures that write to outer temps'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 11:04'!
browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise
	"
	Smalltalk browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesUAR :anyClosureUsesSelf |
				hasIndirectTemps and: [  anyClosureDoesUAR not and: [ anyClosureUsesSelf not ] ] ].
			])
		name: ' Closures that write to outer temps, but clean otherwise'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 3/2/2010 09:22'!
browseMethodsWithEmptyClosures
	"
	Smalltalk browseMethodsWithEmptyClosures
	"
	| emptyClosurePattern |
	emptyClosurePattern _ ByteArray readHexFrom: '8F000002737D'.
	self
		browseAllSelect: [ :m | | s |
			s _ InstructionStream on: m.
			s scanFor: [ :bytecode |
				bytecode = emptyClosurePattern first and: [
					(1 to: emptyClosurePattern size) allSatisfy: [ :i |
						(s method at: s pc + i - 1) = (emptyClosurePattern at: i) ]]]]
		name:  'Methods with empty closures'
		autoSelect: '[]'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:30'!
browseMethodsWithMoreThanOneClosure
	"
	Smalltalk browseMethodsWithMoreThanOneClosure
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesUAR :anyClosureUsesSelf |
				closuresCount > 1 ].
			])
		name: 'Methods with more than one Closure'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:31'!
browseMethodsWithOnlyCleanClosures
	"
	Smalltalk browseMethodsWithOnlyCleanClosures
	"
	self
		browseMessageList: (
			self allSelect: [ :m | 
				self eliotsClosureMeasurementsOn: m over: [ :closuresCount
						:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesUAR :anyClosureUsesSelf |
					closuresCount > 0 and: [
						(anyClosureHasCopied or: [ anyClosureDoesUAR or: [ anyClosureUsesSelf ]]) not ].
				]
			])
		name: 'Methods with only Clean Closures'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/7/2010 21:52'!
closuresInfoStringForClass: aClass selector: aSelector
	"
	Smalltalk closuresInfoStringFor: PlayingWithClosures class >> #exp01Argument
	"
	| answer all someDo noneDoes method |
	method _ aClass compiledMethodAt: aSelector ifAbsent: [ ^'' ].
	self eliotsClosureMeasurementsOn: method over: [ 
				:closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesUAR :anyClosureUsesSelf |
			
		closuresCount > 0
			ifFalse: [ answer _ 'No real (non-optimized) Closures' ]
			ifTrue: [
				closuresCount = 1
					ifTrue: [
						answer _ '1 Closure: '.
						all _ ''.
						someDo _ 'does'. 
						noneDoes _ 'does not' ]
					ifFalse: [
						answer _ closuresCount printString, ' Closures: '.
						all _ 'all '.
						someDo _ 'some do'. 
						noneDoes _ 'none does' ].
				(anyClosureHasCopied or: [ anyClosureDoesUAR or: [ anyClosureUsesSelf ]])
					ifFalse: [ answer _ answer, all, 'clean' ]
					ifTrue: [
						answer _ answer, (anyClosureHasCopied
							ifTrue: [
								hasIndirectTemps
									ifTrue: [ someDo, ' write (and maybe ', someDo, ' read)' ]
									ifFalse: [ someDo, ' read (but ', noneDoes, ' write)' ] ]
							ifFalse: [ noneDoes, ' access' ]), ' outer temps; '.
						answer _ answer, (anyClosureDoesUAR
							ifTrue: [ someDo ]
							ifFalse: [ noneDoes ]), ' ^return; '.
						answer _ answer, (anyClosureUsesSelf
							ifTrue: [ someDo ]
							ifFalse: [ noneDoes ]), ' use self'
					].
			]
		].
		^answer! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 11:19'!
eliotsClosureMeasurements
	"
	Smalltalk eliotsClosureMeasurements
	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	"
      | numMethods numMethodsWithClosure numMethodsWithIndirectTemps
         numClosures numClosuresWithCopiedValues numCopiedValuesForClosure
         numRemoteTemps numScopesWithRemoteTemps
         upArrowReturnsInClosure closureUsesSelfs upArrowReturnAndUsesSelfs numClean |

        numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
        numClosures := numClosuresWithCopiedValues := numCopiedValuesForClosure :=
        numRemoteTemps := numScopesWithRemoteTemps :=
        upArrowReturnsInClosure := closureUsesSelfs := upArrowReturnAndUsesSelfs := numClean := 0.
        self allSelect:
                 [:m| | s hasClosure hasIndirectTemps blkPc blkSz doesUAR usesSelf hasCopied sel|
			sel _ false.
                 hasClosure := hasIndirectTemps := false.
                 s := InstructionStream on: m.
                 s scanFor:
                          [:b|
                          b = 143 "closure creation" ifTrue:
                                   [hasClosure := true.
                                   numClosures := numClosures + 1.
                                   s followingByte >= 16 ifTrue:
                                            [numClosuresWithCopiedValues := numClosuresWithCopiedValues + 1.
                                             numCopiedValuesForClosure := numCopiedValuesForClosure + (s followingByte >> 4)]].
                          (b = 138 "indirect temp vector creation"
                           and: [s followingByte <= 127]) ifTrue:
                                   [hasIndirectTemps := true.
                                    numScopesWithRemoteTemps := numScopesWithRemoteTemps + 1.
                                    numRemoteTemps := numRemoteTemps + s followingByte].
                          false].
                 numMethods := numMethods + 1.
                 hasClosure ifTrue:
                          [numMethodsWithClosure := numMethodsWithClosure + 1.
                           s pc: m initialPC; scanFor: [:b| b = 143].

"jmv-This looks like the correct place to do this"
                           hasCopied := s followingByte >= 16.

                           blkSz := s interpretNextInstructionFor: BlockStartLocator new.
                           blkPc := s pc.
                           doesUAR := usesSelf := false.

"jmv-Doing this here looks like a bug. See the other comment"
                           hasCopied := s followingByte >= 16.

"jmv-Another bug. This only considers the first closure (and any nested closure in it), but not later ones"

                           s scanFor:
                                   [:b|
                                   s pc >= (blkPc + blkSz)
                                            ifTrue: [true]
                                            ifFalse:
                                                     [doesUAR := doesUAR or: [s willReturn and: [s willBlockReturn not]].
                                                      usesSelf := usesSelf or: [b = 112 "pushSelf"
                                                                                                  or: [b < 16 "pushInstVar"
                                                                                                  or: [(b = 128 and: [s followingByte <= 63]) "pushInstVar"
                                                                                                  or: [(b between: 96 and: 96 + 7) "storePopInstVar"
                                                                                                  or: [(b = 130 and: [s followingByte <= 63]) "storePopInstVar"
                                                                                                  or: [(b = 129 and: [s followingByte <= 63]) "storeInstVar"
                                                                                                  or: [b = 132 and: [s followingByte = 160]]]]]]]].
                                                     false]].
                           doesUAR ifTrue:
                                   [upArrowReturnsInClosure := upArrowReturnsInClosure + 1].
                           usesSelf ifTrue:
                                   [closureUsesSelfs := closureUsesSelfs + 1].
                           (doesUAR and: [usesSelf]) ifTrue:
                                   [upArrowReturnAndUsesSelfs := upArrowReturnAndUsesSelfs + 1].
                           (doesUAR or: [usesSelf or: [hasCopied]]) ifFalse:
                                   [numClean := numClean + 1]].
                 hasIndirectTemps ifTrue: [numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
                 sel].
^        { {'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. {'MethodsWithIndirectTemps'. numMethodsWithIndirectTemps}.
         {'Closures'. numClosures}. {'CopiedValuesForClosures'. numCopiedValuesForClosure}. {'ClosuresWithCopiedValues'. numClosuresWithCopiedValues}.
         {'RemoteTemps'. numRemoteTemps}. {'ScopesWithRemoteTemps'. numScopesWithRemoteTemps}.
         {'MethodsWithUpArrowReturnsInClosures'. upArrowReturnsInClosure}. {'MethodsWithReferencesToSelfInClosures'. closureUsesSelfs}. {'Both'. upArrowReturnAndUsesSelfs}.
         {'MethodsWithOnlyCleanClosures'. numClean} }! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:44'!
eliotsClosureMeasurements2
	"
	Smalltalk eliotsClosureMeasurements2
	"
	| numMethods numMethodsWithClosure numMethodsWithIndirectTemps anyClosureDoesUARCount anyClosureUsesSelfCount bothCount onlyCleanBlocksCount anyClosureHasCopiedCount |

	numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
	anyClosureDoesUARCount := anyClosureUsesSelfCount := bothCount := onlyCleanBlocksCount := 0.
	anyClosureHasCopiedCount _ 0.
	self allSelect: [ :m | 
		self eliotsClosureMeasurementsOn: m over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesUAR :anyClosureUsesSelf |
			numMethods := numMethods + 1.
			closuresCount > 0 ifTrue: [ numMethodsWithClosure := numMethodsWithClosure + 1 ].
			hasIndirectTemps ifTrue: [ numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
			anyClosureDoesUAR ifTrue: [ anyClosureDoesUARCount := anyClosureDoesUARCount + 1].
			anyClosureUsesSelf ifTrue: [ anyClosureUsesSelfCount := anyClosureUsesSelfCount + 1].
			(anyClosureDoesUAR and: [anyClosureUsesSelf]) ifTrue: [ bothCount := bothCount + 1].
			closuresCount > 0 ifTrue: [
				(anyClosureDoesUAR or: [anyClosureUsesSelf or: [anyClosureHasCopied]]) ifFalse: [
					onlyCleanBlocksCount := onlyCleanBlocksCount + 1]].
			anyClosureHasCopied ifTrue: [ anyClosureHasCopiedCount _ anyClosureHasCopiedCount + 1 ].
			false.
		]
	].
	^{
		{'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. 
		{'WithClosuresAccessingOuterTemps'. anyClosureHasCopiedCount}.
		{'WithClosuresWritingOuterTemps'. numMethodsWithIndirectTemps}.
		{'WithUpArrowReturnsInClosures'. anyClosureDoesUARCount}. 
		{'WithReferencesToSelfInClosures'. anyClosureUsesSelfCount}. 
		{'BothAbove'. bothCount}.
		{'WithOnlyCleanClosures'. onlyCleanBlocksCount}.
	}! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/7/2010 20:52'!
eliotsClosureMeasurements2On: aMethod
	"
	A Couple of Clean Closures
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01Argument
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01LocalTemp
	
	Closures reading and writing to outer temps
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTemp
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempWithAssignment

	Closure doing an up-arrow return
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01UpArrowReturn

	Closures sending messages to self & super
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01SelfSend
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01SuperSend

	A couple of non-closures, i.e. blocks that are optimized by the compiler and a closure is never created
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempOptimized
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempOptimizedWithAssignment
	
	A remote temp whose declaration can not be moved inside the block
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempCantBeMovedInside
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempAssignedTwice
	A remote temp whose declaration can be moved inside the block
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempCanBeMovedInside
	A not-so remote temp. The declaration was moved inside the block, making it a clean block
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01LocalTemp
	"
	| numMethods numMethodsWithClosure numMethodsWithIndirectTemps anyClosureDoesUARCount anyClosureUsesSelfCount bothCount onlyCleanBlocksCount anyClosureHasCopiedCount |

	numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
	anyClosureDoesUARCount := anyClosureUsesSelfCount := bothCount := onlyCleanBlocksCount := 0.
	anyClosureHasCopiedCount _ 0.
	self eliotsClosureMeasurementsOn: aMethod over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesUAR :anyClosureUsesSelf |
		numMethods := numMethods + 1.
		closuresCount > 0 ifTrue: [ numMethodsWithClosure := numMethodsWithClosure + 1 ].
		hasIndirectTemps ifTrue: [ numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
		anyClosureDoesUAR ifTrue: [ anyClosureDoesUARCount := anyClosureDoesUARCount + 1].
		anyClosureUsesSelf ifTrue: [ anyClosureUsesSelfCount := anyClosureUsesSelfCount + 1].
		(anyClosureDoesUAR and: [anyClosureUsesSelf]) ifTrue: [ bothCount := bothCount + 1].
		closuresCount > 0 ifTrue: [
			(anyClosureDoesUAR or: [anyClosureUsesSelf or: [anyClosureHasCopied]]) ifFalse: [
				onlyCleanBlocksCount := onlyCleanBlocksCount + 1]].
		anyClosureHasCopied ifTrue: [ anyClosureHasCopiedCount _ anyClosureHasCopiedCount + 1 ].
	].
	^{
		{'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. 
		{'WithClosuresAccessingOuterTemps'. anyClosureHasCopiedCount}.
		{'WithClosuresWritingOuterTemps'. numMethodsWithIndirectTemps}.
		{'WithUpArrowReturnsInClosures'. anyClosureDoesUARCount}. 
		{'WithReferencesToSelfInClosures'. anyClosureUsesSelfCount}. 
		{'BothAbove'. bothCount}.
		{'WithOnlyCleanClosures'. onlyCleanBlocksCount}.
	}! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 8/5/2011 18:52'!
eliotsClosureMeasurementsOn: m over: aFiveArgBlock
	"
	See senders.
	Or try something like:
		Smalltalk
			eliotsClosureMeasurementsOn: FileList >> #defaultContents
			over: [ :closuresCount :hasCopiedValuesForClosure :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesUAR :anyClosureUsesSelf |
				(Array with: closuresCount with: hasCopiedValuesForClosure with: hasIndirectTemps with: anyClosureHasCopied with: anyClosureDoesUAR with: anyClosureUsesSelf)]

	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	
	Note: This could perhaps be refactored to use the newer #embeddedBlockClosures and testing methods on the closures themselves.
	"
	| s nextScanStart thisClosureHasCopied closuresCount hasIndirectTemps blkPc blkSz anyClosureHasCopied anyClosureDoesUAR anyClosureUsesSelf analyzedClosures |
	closuresCount := 0.
	hasIndirectTemps := false.
	anyClosureHasCopied :=  anyClosureDoesUAR := anyClosureUsesSelf := false.
	s := InstructionStream on: m.
	s scanFor: [ :b |
		b = 16r8F "16r8F = 143 closure creation" ifTrue: [
			closuresCount := closuresCount + 1].
		(b = 16r8A "16r8A = 138indirect temp vector creation" and: [ s followingByte <= 127]) ifTrue: [
				hasIndirectTemps := true].
		false].
	nextScanStart := m initialPC.
	analyzedClosures := 0.
	[ analyzedClosures < closuresCount ] whileTrue: [
		s pc: nextScanStart; scanFor: [ :b | b = 16r8F ].	"16r8F = 143 Search for first closure"
		analyzedClosures := analyzedClosures + 1.
		thisClosureHasCopied := s followingByte >= 16r10.
		anyClosureHasCopied := anyClosureHasCopied | thisClosureHasCopied.
		blkSz := s interpretNextInstructionFor: BlockStartLocator new.		"Findout size of first closure"
		blkPc := s pc.
		s scanFor: [ :b |
			s pc >= (blkPc + blkSz)
				ifTrue: [
					nextScanStart := s pc.
					true]
				ifFalse: [
					b = 16r8F ifTrue: [			
						thisClosureHasCopied := s followingByte >= 16r10.
						anyClosureHasCopied := anyClosureHasCopied | thisClosureHasCopied.
						analyzedClosures := analyzedClosures + 1 ].
					anyClosureDoesUAR := anyClosureDoesUAR or: [s willReturn and: [s willBlockReturn not]].
					anyClosureUsesSelf := anyClosureUsesSelf or: [b = 16r70 "pushSelf"
										or: [b < 16r10 "pushInstVar"
										or: [(b = 16r80 and: [s followingByte <= 16r3F]) "pushInstVar"
										or: [(b between: 16r60 and: 16r60 + 7) "storePopInstVar"
										or: [(b = 16r82 and: [s followingByte <= 63]) "storePopInstVar"
										or: [(b = 16r81 and: [s followingByte <= 63]) "storeInstVar"
										or: [b = 16r84 and: [s followingByte = 160]]]]]]]].
					false]]].
	^aFiveArgBlock valueWithArguments: (Array
			with: closuresCount
			with: hasIndirectTemps
			with: anyClosureHasCopied
			with: anyClosureDoesUAR
			with: anyClosureUsesSelf)! !


!SystemDictionary methodsFor: 'removing' stamp: 'jmv 7/3/2010 16:38'!
removeKey: key ifAbsent: aBlock
	"Remove key (and its associated value) from the receiver. If key is not in
	the receiver, answer the result of evaluating aBlock. Otherwise, answer
	the value externally named by key."
	self flushClassNameCache.
	^super removeKey: key ifAbsent: aBlock! !


!SystemDictionary methodsFor: 'system attributes' stamp: 'eem 7/22/2010 17:44'!
flagInterpretedMethods: aBoolean
	"The Cog VM can be instructed to set the flag bit of CompiledMethods that
	 it executes but will only interpret.  This can be used e.g. to profile startup.
	 See CompiledMethod>>#flag & CompiledMethod>>#clearFlag.  
	 This flag persists across snapshots, stored in the image header."

	self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 2) + (aBoolean ifTrue: [2] ifFalse: [0])! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'eem 7/6/2010 20:43'!
isRunningCog
	"Returns true if we're running on a Cog VM (JIT or StackInterpreter)
	 (vmParameterAt: 42 is the number of stack pages)"

	^[(self vmParameterAt: 42) > 0] on: Error do:[:ex| ex return: false]! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'eem 7/6/2010 20:41'!
isRunningCogit
	"Returns true if we're running on the Cog JIT
	 (vmParameterAt: 46 is the size of the machine code zone)"

	^[(self vmParameterAt: 46) > 0] on: Error do:[:ex| ex return: false]! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'eem 7/22/2010 17:23'!
processHasThreadIdInstVar: aBoolean
	"The threaded VM needs to know if the 4th inst var of Process
	 is threadId which it uses to control process-to-thread binding.
	 This flag persists across snapshots, stored in the image header."
	aBoolean ifTrue: [self assert: (Process instVarNames at: 4) ='threadId'].
	self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 1) + (aBoolean ifTrue: [1] ifFalse: [0])! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'eem 7/22/2010 17:51'!
processPreemptionYields
	"Answer whether the VM causes a process to yield on process preemption,
	 i.e. to put a preempted process at the back of its run queue.  If the parameter
	 is unavailable (non-Cog VMs) or bit 2 (4) is 0 then preemption yields."

	^(([self vmParameterAt: 48]
			on: Error
			do: [:ex| ^true]) allMask: 4) not! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'eem 7/22/2010 17:26'!
processPreemptionYields: aBoolean
	"The Cog VM can be instructed not to yield on process preemption,
	 i.e. not to put a preempted process at the back of its run queue.  By
	 default preempting a process causes it to yield (Blue Book semantics)
	 which can have unfortunate effects.
	 This flag persists across snapshots, stored in the image header."

	self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 4) + (aBoolean ifTrue: [0] ifFalse: [4])! !


!SystemDictionary methodsFor: 'query' stamp: 'jmv 6/14/2013 19:35'!
hierarchySorted: classes do: aBlock

	| s |
	s _ classes asSet.
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			(s includes: cls) ifTrue: [
				aBlock value: cls ].
			(s includes: cls class) ifTrue: [
				aBlock value: cls class ].
			]
		startingLevel: 1! !

!SystemDictionary methodsFor: 'query' stamp: 'jmv 5/6/2012 19:26'!
methodsWithUnboundGlobals
	"Get all methods that use undeclared global objects that are not listed in Undeclared. For a clean image the result should be empty."
	"
	self assert: Smalltalk methodsWithUnboundGlobals isEmpty
	"
	^ self allSelect: [ :m |
		m literals anySatisfy: [ :l |
			l isVariableBinding and: [
				l key isSymbol and: [
					"avoid class-side methodClass literals"
					(m methodClass bindingOf: l key)
						ifNil: [
							(Undeclared
								associationAt: l key
								ifAbsent: [ ]) ~~ l ]
						ifNotNil: [ :b |
							b ~~ l ]]]]]! !

!SystemDictionary methodsFor: 'query' stamp: 'eem 10/22/2010 09:40'!
unusedBlocks
	"Answer all methods that contain a block that is not used (not
	 sent a message, returned, passed as an argument, or assigned)."
	"Smalltalk unusedBlocks"
	"Smalltalk
		browseMessageList: Smalltalk unusedBlocks
		name: 'unused blocks'"
	^self allSelect:
		[:m| | is |
		is := InstructionStream on: m.
		is scanFor: [:b| b = 143 and: [(m at: is thirdByte * 256 + is fourthByte + is pc + 4) = 135]]]! !


!SystemDictionary methodsFor: 'testing' stamp: 'jmv 6/27/2011 17:56'!
hasBindingThatBeginsWith: aString
	"Answer true if the receiver has a key that begins with aString, false otherwise"
	"
	[ 5000 timesRepeat: [Smalltalk hasBindingThatBeginsWith: 'Obj' ]] timeToRun 
		14
	Smalltalk hasBindingThatBeginsWith: 'Obj'
	[ 5000 timesRepeat: [Smalltalk hasBindingThatBeginsWith: 'zxzxObj' ]] timeToRun 
		47
	Smalltalk hasBindingThatBeginsWith: 'zxzxObj'
	"
	
	"Use the cached class and non-class names for better performance."
	| searchBlock |
	searchBlock _ [ :element |
		(element beginsWith: aString)
			ifTrue: [ 0 ]
			ifFalse: [
				aString < element
					ifTrue: [ -1 ]
					ifFalse: [ 1 ] ] ].

	self classNames 
		findBinary: searchBlock
		do: [ :found | ^true ]
		ifNone: [ :a :b | ].
	
	self nonClassNames 
		findBinary: searchBlock
		do: [ :found | ^true ]
		ifNone: [ :a :b | ].

	^false! !

!SystemDictionary methodsFor: 'testing' stamp: 'jmv 6/27/2011 18:09'!
namesBeginningWith: aString do: aBlock
	"
	Smalltalk namesBeginningWith: 'O' do: [ :each | each print ]
	Smalltalk namesBeginningWith: 'ObjectExplorer' do: [ :each | each print ]
	Smalltalk namesBeginningWith: 'ObjectExplorerWrapper' do: [ :each | each print ]
	"

	self namesBeginningWith: aString do: aBlock in:  self classNames.
	self namesBeginningWith: aString do: aBlock in:  self nonClassNames! !

!SystemDictionary methodsFor: 'testing' stamp: 'jmv 6/30/2011 08:33'!
namesBeginningWith: aString do: aBlock in: aCollection
	"aCollection is sorted"
	| count |

	"Find the first element  starting with aString"
	count _ aCollection size.
	aCollection
		findBinaryIndex: [ :element |
			element < aString
				ifFalse: [ -1 ]
				ifTrue: [ 1 ] ]
		do: [ :found | "Will never find any" ]
		ifNone: [ :a :b | | i n |
			i _ b.
			[ i <= count and: [ 
				n _ aCollection at: i.
				aString isEmpty or: [
					n beginsWith: aString ]]] whileTrue: [
				aBlock value: n.
				i _ i + 1 ]]! !


!SystemDictionary methodsFor: 'classes and traits' stamp: 'ul 10/8/2010 21:22'!
classNames
	"Answer a sorted collection of all class names. Use the return value of #fillCaches to avoid concurrency issues."
	
	^cachedClassNames ifNil: [ self fillCaches at: 1 ]! !

!SystemDictionary methodsFor: 'classes and traits' stamp: 'ul 10/8/2010 21:22'!
nonClassNames
	"Answer a sorted collection of all non-class names. Use the return value of #fillCaches to avoid concurrency issues."
	
	^cachedNonClassNames ifNil: [ self fillCaches at: 2 ]! !


!SystemDictionary methodsFor: 'image format' stamp: 'jmv 12/20/2012 21:40'!
imageFormatVersion
	"Answer an integer identifying the type of image in memory. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). This invokes an optional primitive
	that may not be available on all virtual machines."

	"
	Smalltalk imageFormatVersion
	"

	<primitive: 'primitiveImageFormatVersion'>

	"Cog provides a VM parameter"
	^[Smalltalk vmParameterAt: 41]
		on: Error
		do: [self notify: 'This virtual machine does not support the optional ',
				'primitive #primitiveImageFormatVersion'.
			nil]
! !

!SystemDictionary methodsFor: 'image format' stamp: 'jmv 8/31/2011 08:18'!
imageFormatVersionFromFile
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required)"

	"
	Smalltalk imageFormatVersionFromFile
	"

	| format |
	format := self imageFormatVersionFromFileAsIs.
	^format <= 16r00FFFFFF
		ifTrue: [  "same endianness as VM"
			format ]
		ifFalse: [ "convert endianness"
			((format bitAnd: 16rFF000000) >> 24)
			+ ((format bitAnd: 16r00FF0000) >> 8)
			+ ((format bitAnd: 16r0000FF00) << 8)
			+ ((format bitAnd: 16r000000FF) << 16)]! !

!SystemDictionary methodsFor: 'image format' stamp: 'jmv 8/31/2011 08:17'!
imageFormatVersionFromFileAsIs
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). If the image file has a different
	endianness than the VM, the format version will appear byte-swapped."
	"
	Smalltalk imageFormatVersionFromFileAsIs
	"
	| answer file |
	file _ FileStream readOnlyFileNamed: Smalltalk imageName.
	[
	answer _ (file
		 binary;
		 next: 4)
		unsignedLongAt: 1
		bigEndian: Smalltalk isBigEndian ] ensure: [ file close ].
	^ answer! !


!SystemDictionary methodsFor: 'AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
interruptChecksPerMSec
"
Smalltalk interruptChecksPerMSec
"
	"Primitive. Answer the number of interrupt checks per milliseconds that we execute
	on this machine. This can be used to adjust the sub-msecs profiler to check (roughly) 
	n times per millisecond."
	<primitive: 'primitiveInterruptChecksPerMSec'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: 'AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
longRunningPrimitive
	"Primitive. Answer an Array of the current long-running primitive method identified by the
	 heartbeat, and the number of heartbeats it ran for, or nil if none.  To use this, first install a
	 semaphore to wait upon via SystemDictionary>>longRunningPrimitiveSemaphore:"
	<primitive: 'primitiveLongRunningPrimitive'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: 'AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
longRunningPrimitiveSemaphore: aSemaphoreOrNil
	"Primitive. Install the semaphore to be used for collecting long-running primitives, 
	 or nil if no semaphore should be used.  The semaphore will be signalled once the
	 heartbeat detects a long-running primitive until the result is collected via
	 primitiveLongRunningPrimitive (see SystemDictionary>>longRunningPrimitive)"
	<primitive: 'primitiveLongRunningPrimitiveSemaphore'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: 'AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
profilePrimitive
	"Primitive. Answer the primitive sample by the profiler, or nil if the profiler isn't active.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfilePrimitive'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: 'AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
profileSample
	"Primitive. Answer the last sample taken by the profiler, or nil if the profiler isn't active.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSample'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: 'AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
profileSemaphore: aSemaphore
	"Primitive. Install the semaphore to be used for profiling, 
	or nil if no semaphore should be used.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSemaphore'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: 'AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
profileStart: counter
	"Primitive. Begin profiling execution every by using the interrupt check-counter instead of a time-based process (which is limited to timing resolution and triggers off the same signal that many of the processes being profiled trigger off leading to consistently wrong results).
	The argument is the number of interrupt checks (method activations) to let go by before taking a sample. The sample is being stored in the profileSample iVar which can be retrieved by executing primitiveProfileSample. When a sample is taken, it signals the semaphore specified in primitiveProfileSemaphore.
	If the argument is less or equal to zero, it disables profiling."
	<primitive: 'primitiveProfileStart'>
	^self primitiveFailed! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SystemDictionary class' category: #'System-Support'!
SystemDictionary class
	instanceVariableNames: ''!

!SystemDictionary class methodsFor: 'initialization' stamp: 'jmv 2/17/2010 23:07'!
initialize
	"SystemDictionary initialize"

	| oldList |
	oldList := StartUpList.
	StartUpList := OrderedCollection new.
	"These get processed from the top down..."
	#(
		#Delay 
		#DisplayScreen 
		#Cursor 
		#InputSensor 
		#ProcessorScheduler 	"Starts low space watcher and bkground."
		#FileDirectory 			"Enables file stack dump and opens sources."
		#ShortIntegerArray 
		#ShortRunArray)
			do: [ :clsName | 	
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToStartUpList: cls]].

	oldList ifNotNil: [
		oldList do: [ :className | 
			Smalltalk 
				at: className
				ifPresent: [:theClass | Smalltalk addToStartUpList: theClass]]].

	#(
		#PasteUpMorph) 
			do:  [ :clsName | 
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToStartUpList: cls]].
			
	oldList := ShutDownList.
	ShutDownList := OrderedCollection new.
	"These get processed from the bottom up..."
	#(
		#Delay 
		#DisplayScreen 
		#InputSensor 
		#Form 
		#PasteUpMorph 
		#StrikeFont 
		#Color 
		#FileDirectory 
		#SoundPlayer ) 
			do: [ :clsName | 
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToShutDownList: cls]].

	oldList ifNotNil: [
		oldList reverseDo: [ :className | 
			Smalltalk 
				at: className
				ifPresent: [:theClass | Smalltalk addToShutDownList: theClass]]]! !


!SystemDictionary class methodsFor: 'class initialization' stamp: 'jmv 3/19/2012 08:41'!
startUp
	"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Smalltalk.
	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk
	"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk! !


!SystemDictionary class methodsFor: 'copyright' stamp: 'jmv 1/15/2014 09:53'!
copyright
	"The Smalltalk copyright.
	Parts are copyright of many contributors to Squeak and Cuis projects."

	^
'Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982.
Copyright (c) Apple Computer, Inc. 1985-1996.
Copyright (c) Contributors to Squeak and Cuis projects. 1997-2014.'! !


!classDefinition: #ExternalSemaphoreTable category: #'System-Support'!
Object subclass: #ExternalSemaphoreTable
	instanceVariableNames: ''
	classVariableNames: 'ProtectTable'
	poolDictionaries: ''
	category: 'System-Support'!
!ExternalSemaphoreTable commentStamp: '<historical>' prior: 0!
By John M McIntosh johnmci@smalltalkconsulting.com
This class was written to mange the external semaphore table. When I was writing a Socket test server I discovered various race conditions on the access to the externalSemaphore table. This new class uses class side methods to restrict access using a mutex semaphore. It seemed cleaner to deligate the reponsibility here versus adding more code and another class variable to SystemDictionary 

Note that in Smalltalk recreateSpecialObjectsArray we still directly play with the table.!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ExternalSemaphoreTable class' category: #'System-Support'!
ExternalSemaphoreTable class
	instanceVariableNames: ''!

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'JMM 6/6/2000 20:36'!
clearExternalObjects
	"Clear the array of objects that have been registered for use in non-Smalltalk code."

	ProtectTable critical: [Smalltalk specialObjectsArray at: 39 put: Array new].
! !

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'JMM 6/6/2000 21:01'!
externalObjects
	^ProtectTable critical: [Smalltalk specialObjectsArray at: 39].! !

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'JMM 6/6/2000 20:44'!
registerExternalObject: anObject
	^ ProtectTable critical: [self safelyRegisterExternalObject: anObject]
! !

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'JMM 6/6/2000 20:57'!
safelyRegisterExternalObject: anObject
	"Register the given object in the external objects array and return its index. If it is already there, just return its index."

	| objects firstEmptyIndex obj sz newObjects |
	objects _ Smalltalk specialObjectsArray at: 39.

	"find the first empty slot"
	firstEmptyIndex _ 0.
	1 to: objects size do: [:i |
		obj _ objects at: i.
		obj == anObject ifTrue: [^ i].  "object already there, just return its index"
		(obj == nil and: [firstEmptyIndex = 0]) ifTrue: [firstEmptyIndex _ i]].

	"if no empty slots, expand the array"
	firstEmptyIndex = 0 ifTrue: [
		sz _ objects size.
		newObjects _ objects species new: sz + 20.  "grow linearly"
		newObjects replaceFrom: 1 to: sz with: objects startingAt: 1.
		firstEmptyIndex _ sz + 1.
		Smalltalk specialObjectsArray at: 39 put: newObjects.
		objects _ newObjects].

	objects at: firstEmptyIndex put: anObject.
	^ firstEmptyIndex
! !

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'JMM 6/6/2000 20:59'!
safelyUnregisterExternalObject: anObject
	"Unregister the given object in the external objects array. Do nothing if it isn't registered.
	JMM change to return if we clear the element, since it should only appear once in the array"

	| objects |
	anObject ifNil: [^ self].
	objects _ Smalltalk specialObjectsArray at: 39.
	1 to: objects size do: [:i |
		(objects at: i) == anObject ifTrue: 
		[objects at: i put: nil.
		^self]].
! !

!ExternalSemaphoreTable class methodsFor: 'accessing' stamp: 'JMM 6/6/2000 20:45'!
unregisterExternalObject: anObject
	ProtectTable critical: [self safelyUnregisterExternalObject: anObject]
! !


!ExternalSemaphoreTable class methodsFor: 'initialization' stamp: 'JMM 6/6/2000 20:32'!
initialize
	ProtectTable _ Semaphore forMutualExclusion! !


!classDefinition: #ObjectFinalizer category: #'System-Support'!
Object subclass: #ObjectFinalizer
	instanceVariableNames: 'receiver selector arguments'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!ObjectFinalizer methodsFor: 'finalization' stamp: 'ar 5/19/2003 20:13'!
finalize
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO garantuee that the resource associated with the receiver hasn't been free'd before so take care that you don't run into trouble - this all may happen with interrupt priority."
	[receiver perform: selector withArguments: arguments] 
		on: Error do:[:ex| ex return].
! !


!ObjectFinalizer methodsFor: 'initialization' stamp: 'ar 5/19/2003 20:12'!
receiver: aReceiver selector: aSelector argument: anObject
	receiver := aReceiver.
	selector := aSelector.
	arguments := Array with: anObject! !


!classDefinition: #Preference category: #'System-Support'!
Object subclass: #Preference
	instanceVariableNames: 'name value defaultValue helpString categoryList changeInformee changeSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!Preference commentStamp: 'sw 1/14/2003 22:33' prior: 0!
Represents a true/false flag that is under user control and which can be interrogated by a call to Preferences

	name 				a symbol, the formal name of the preference.
	value				a boolean, the current value
	defaultValue		the default value of the preference
	helpString 			string or text, constituting the help message
	localToProject		boolean, whether each project holds its own version
	categoryList			list of categories under which to offer this
	changeInformee 	whom, if anyone, to inform if the value changes:
	changeSelector 		what selector to send to the changeInformee when the value changes!


!Preference methodsFor: 'initialization' stamp: 'jmv 9/2/2013 09:04'!
name: aName defaultValue: aValue helpString: aString categoryList: aList changeInformee: informee changeSelector:  aChangeSelector
	"Initialize the preference from the given values.  There is an extra tolerence here for the symbols #true, #false, and #nil, which are interpreted, when appropriate, as meaning true, false, and nil"

	name _ aName asSymbol.
	defaultValue _ aValue.
	defaultValue = #true ifTrue: [ defaultValue _ true ].
	defaultValue = #false ifTrue: [ defaultValue _ false ].
	value _ defaultValue.
	helpString _ aString.
	categoryList _ (aList ifNil: [OrderedCollection with: #unclassified]) collect:
		[:elem | elem asSymbol].

	changeInformee _ (informee == nil or: [informee == #nil])
						ifTrue: [nil]
						ifFalse:	[(informee isSymbol)
							ifTrue: [
								Smalltalk at: informee]
							ifFalse: [
								informee]].
	changeSelector  _ aChangeSelector! !


!Preference methodsFor: 'menu' stamp: 'sw 4/10/2001 15:02'!
name
	"Answer this preference's name"

	^ name! !


!Preference methodsFor: 'value' stamp: 'sw 4/10/2001 15:01'!
defaultValue
	"Answer this preference's defaultValue"

	^ defaultValue! !

!Preference methodsFor: 'value' stamp: 'sw 4/10/2001 15:35'!
preferenceValue
	"Answer the current value of the preference"

	^ value! !

!Preference methodsFor: 'value' stamp: 'sw 4/12/2001 23:28'!
preferenceValue: aValue
	"set the value as indicated, and invoke the change selector if appropriate"

	| oldValue |
	oldValue _ value.
	value _ aValue.
	oldValue ~~ value ifTrue:
		[self notifyInformeeOfChange]! !

!Preference methodsFor: 'value' stamp: 'sw 4/12/2001 00:04'!
restoreDefaultValue
	"restore the default value to the preference"

	value _ defaultValue! !

!Preference methodsFor: 'value' stamp: 'sw 4/12/2001 00:04'!
togglePreferenceValue
	"Toggle whether the value of the preference"

	value _ value not.
	self notifyInformeeOfChange! !


!Preference methodsFor: 'change notification' stamp: 'sw 4/12/2001 01:39'!
changeInformee: informee changeSelector: aSelector
	"Set the changeInformee and changeSelector as specified"

	changeInformee _ informee.
	changeSelector _ aSelector! !

!Preference methodsFor: 'change notification' stamp: 'sw 4/12/2001 00:03'!
notifyInformeeOfChange
	"If there is a changeInformee, notify her that I have changed value"

	changeInformee ifNotNil: [changeInformee perform: changeSelector]! !


!Preference methodsFor: 'debugging' stamp: 'sw 4/13/2001 00:05'!
printOn: aStream
	"Print a string decribing the receiver to the given stream"

	super printOn: aStream.
	aStream nextPutAll: name storeString, ' ', value storeString! !


!classDefinition: #Preferences category: #'System-Support'!
Object subclass: #Preferences
	instanceVariableNames: ''
	classVariableNames: 'DesktopColor DictionaryOfPreferences Parameters'
	poolDictionaries: ''
	category: 'System-Support'!
!Preferences commentStamp: '<historical>' prior: 0!
A general mechanism to store preference choices.  The default setup treats any symbol as a potential boolean flag; flags unknown to the preference dictionary are always returned as false.  

	To open the control panel:		Preferences openFactoredPanel
	To read how to use the panel (and how to make a preference be per-project):
		 Preferences giveHelpWithPreferences

All messages are on the class side.

To query a a preference:
	Preferences logDebuggerStackToFile
or some people prefer the more verbose
	Preferences valueOfFlag: #logDebuggerStackToFile

You can make up a new preference any time.  Do not define a new message in Preferences class. Accessor methods are compiled automatically when you add a preference as illustrated below:

To add a preference (e.g. in the Postscript of a fileout):
	Preferences addPreference: #samplePreference categories: #(general browsing)
		default: true balloonHelp: 'This is an example of a preference added by a do-it'
		projectLocal: false changeInformee: nil changeSelector: nil.

To change a preference programatically:
	Preferences disable: #logDebuggerStackToFile.
Or to turn it on,
	Preferences enable: #logDebuggerStackToFile.
!


!Preferences methodsFor: 'look in class' stamp: 'di 12/4/1999 15:11'!
seeClassSide
	"All the code for Preferences is on the class side"! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Preferences class' category: #'System-Support'!
Preferences class
	instanceVariableNames: ''!

!Preferences class methodsFor: 'add preferences' stamp: 'jmv 5/21/2006 19:47'!
addPreference: prefSymbol categories: categoryList default: defaultValue balloonHelp: helpString 
	"Add an item repreesenting the given preference symbol to the system"

	self addPreference: prefSymbol  categories: categoryList  default:  defaultValue balloonHelp: helpString changeInformee: nil changeSelector: nil! !

!Preferences class methodsFor: 'add preferences' stamp: 'jmv 5/21/2006 19:47'!
addPreference: prefSymbol categories: categoryList default: aValue balloonHelp: helpString changeInformee: informeeSymbol  changeSelector: aChangeSelector
	"Add or replace a preference as indicated.  Reuses the preexisting Preference object for this symbol, if there is one, so that UI artifacts that interact with it will remain valid."

	| aPreference |
	aPreference _ DictionaryOfPreferences at: prefSymbol ifAbsent: [Preference new].
	aPreference name: prefSymbol defaultValue: aValue helpString: helpString categoryList: categoryList changeInformee: informeeSymbol changeSelector: aChangeSelector.
	DictionaryOfPreferences at: prefSymbol put: aPreference.
	self compileAccessMethodForPreference: aPreference! !

!Preferences class methodsFor: 'add preferences' stamp: 'sw 2/5/2001 22:20'!
addPreference: prefSymbol category: categorySymbol default: defaultValue balloonHelp: helpString 
	"Add the given preference, putting it in the given category, with the given default value, and with the given balloon help"

	self addPreference: prefSymbol categories: {categorySymbol} default: defaultValue balloonHelp: helpString ! !


!Preferences class methodsFor: 'fonts' stamp: 'jmv 3/16/2010 18:54'!
properDisplayAlphaForFonts
	^ self
		valueOfFlag: #properDisplayAlphaForFonts
		ifAbsent: [false]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 9/25/2011 22:56'!
restoreDefaultFonts
	"Since this is called from menus, we can take the opportunity to prompt for missing font styles."
	"Preferences restoreDefaultFonts"

	Preferences setDefaultFonts: #(
		(setSystemFontTo: 'DejaVu' 9)
		(setListFontTo: 'DejaVu' 9)
		(setMenuFontTo: 'DejaVu' 10)
		(setWindowTitleFontTo: 'DejaVu' 12)
		(setCodeFontTo: 'DejaVu' 9)
		(setButtonFontTo: 'DejaVu' 9))! !

!Preferences class methodsFor: 'fonts' stamp: 'sw 12/8/1999 22:06'!
setButtonFontTo: aFont
	Parameters at: #standardButtonFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 12/1/2004 13:26'!
setCodeFontTo: aFont
	"Not currently sent, but once protocols are sorted out so that we can discriminate on whether a text object being launched is for code or not, might deserve to be reincorporated"

	Parameters at: #standardCodeFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 7/27/2009 17:38'!
setDefaultFonts: defaultFontsSpec
		
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ AbstractFont familyName: triplet second pointSize: triplet third.
		triplet size > 3 ifTrue: [
			font _ font emphasized: triplet fourth ].
		self
			perform: triplet first
			with: font]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 7/24/2009 19:22'!
setListFontTo: aFont
	"Set the list font as indicated"

	Parameters at: #standardListFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 1/24/2011 14:59'!
setMenuFontTo: aFont

	Parameters at: #standardMenuFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/15/2014 09:23'!
setSystemFontTo: aFont
	"Establish the default text font and style"

	aFont ifNil: [^ self].
	AbstractFont default: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 2/12/2008 20:05'!
setWindowTitleFontTo: aFont
	"Set the window-title font to be as indicated"

	Parameters at: #windowTitleFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 7/27/2009 19:21'!
standardButtonFont
	"Answer an attractive font to use for buttons"
	"Answer the font to be used for textual flap tab labels"
	^ Parameters at: #standardButtonFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/15/2014 09:23'!
standardCodeFont
	"Answer the font to be used in code"

	 ^ Parameters at: #standardCodeFont ifAbsent:
		[Parameters at: #standardCodeFont put: AbstractFont default]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/15/2014 09:23'!
standardListFont
	"Answer the font to be used in lists"

	 ^ Parameters at: #standardListFont ifAbsent:
		[Parameters at: #standardListFont put: AbstractFont default]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/15/2014 09:23'!
standardMenuFont
	"Answer the font to be used in menus"

	 ^ Parameters at: #standardMenuFont ifAbsent:
		[Parameters at: #standardMenuFont put: AbstractFont default]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 8/21/2009 08:54'!
subPixelRenderColorFonts
	^ self
		valueOfFlag: #subPixelRenderColorFonts
		ifAbsent: [true]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 8/21/2009 08:55'!
subPixelRenderFonts
	^ self
		valueOfFlag: #subPixelRenderFonts
		ifAbsent: [true]! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 7/27/2009 19:21'!
windowTitleFont
	"Answer the standard font to use for window titles"
	^  Parameters at: #windowTitleFont! !


!Preferences class methodsFor: 'get/set' stamp: 'sw 4/12/2001 23:29'!
disable: aSymbol
	"Shorthand access to enabling a preference of the given name.  If there is none in the image, conjure one up"

	| aPreference |
	aPreference _ self preferenceAt: aSymbol ifAbsent:
		[self addPreference: aSymbol category: 'unclassified' default: false balloonHelp: 'this preference was added idiosyncratically and has no help message.'.
		self preferenceAt: aSymbol].
	aPreference preferenceValue: false! !

!Preferences class methodsFor: 'get/set' stamp: 'HB 7/23/2014 16:08'!
disableFocusFollowsMouse

	"disable the window selection following the position of the mouse and base it on explicit mouse click"

	
	Preferences disable: #focusFollowsMouse.! !

!Preferences class methodsFor: 'get/set' stamp: 'sw 11/11/1998 11:40'!
doesNotUnderstand: aMessage
	"Look up the message selector as a flag."
	aMessage arguments size > 0 ifTrue: [^ super doesNotUnderstand: aMessage].
	^ self valueOfFlag: aMessage selector
! !

!Preferences class methodsFor: 'get/set' stamp: 'sw 4/12/2001 23:29'!
enable: aSymbol
	"Shorthand access to enabling a preference of the given name.  If there is none in the image, conjure one up"

	| aPreference |
	aPreference _ self preferenceAt: aSymbol ifAbsent:
		[self addPreference: aSymbol category: 'unclassified' default: true balloonHelp: 'this preference was added idiosyncratically and has no help message.'.
		self preferenceAt: aSymbol].
	aPreference preferenceValue: true! !

!Preferences class methodsFor: 'get/set' stamp: 'HB 7/23/2014 16:08'!
enableFocusFollowsMouse

	"base the window selection on the present location of the mouse"
	
	Preferences enable: #focusFollowsMouse.! !

!Preferences class methodsFor: 'get/set' stamp: 'jmv 7/28/2010 11:30'!
setPreference: prefSymbol toValue: aValue 
	"Set the given preference to the given value, and answer that value"
	(self
		preferenceAt: prefSymbol
		ifAbsent: [ 
			^ self
				addPreference: prefSymbol
				category: #misc
				default: aValue
				balloonHelp: '' ])
		preferenceValue: aValue! !

!Preferences class methodsFor: 'get/set' stamp: 'sw 4/12/2001 00:04'!
togglePreference: prefSymbol
	"Toggle the given preference"

	(self preferenceAt: prefSymbol ifAbsent: [self error: 'unknown preference: ', prefSymbol]) togglePreferenceValue! !

!Preferences class methodsFor: 'get/set' stamp: 'sw 4/13/2001 01:07'!
valueOfFlag: aFlagName
	"Answer the value of the given flag"

	^ (self preferenceAt: aFlagName ifAbsent: [^ false]) preferenceValue! !

!Preferences class methodsFor: 'get/set' stamp: 'jmv 7/28/2010 11:40'!
valueOfFlag: aFlagName ifAbsent: aBlock
	"Answer the value of the given flag
	Not limited to booleans..."

	^ (self preferenceAt: aFlagName ifAbsent: [^ aBlock value]) preferenceValue! !


!Preferences class methodsFor: 'halos' stamp: 'cbr 5/16/2011 14:31'!
haloSpecifications
	"Answer a list of HaloSpecs that describe which halos are to be used, what they should look 
like, and where they should be situated"

	^ Parameters at: #HaloSpecs ifAbsent: [
			self installHaloTheme: #iconicHaloSpecifications.
			^ Parameters at: #HaloSpecs]

	"Preferences haloSpecifications"
	"Preferences resetHaloSpecifications"
! !

!Preferences class methodsFor: 'halos' stamp: 'jmv 8/18/2012 18:00'!
iconicHaloSpecifications
	"Answer an array that characterizes the locations, colors, icons, and selectors of the halo handles that may be used in the iconic halo scheme"

	"
	Preferences resetHaloSpecifications
	"

^ #(
	"selector						horiz				vert					color info						icon key 						balloon help
	 ---------						------				-----------			-------------------------------		---------------"
	(addCollapseHandle:		left				topCenter		(tan)							haloCollapseIcon 			'Collapse')
	(addDebugHandle:			right				topCenter		(orange)						haloDebugIcon 				'Debug')
	(addDismissHandle:			left				top				(red)							haloDismissIcon 				'Remove')
	(addRotateHandle:			left				bottom			(blue)							haloRotateIcon 				'Rotate')
	(addMenuHandle:			leftCenter		top				(blue lighter)					haloMenuIcon 				'Menu')
	(addGrabHandle:				center			top				(black)							haloGrabIcon 				'Pick up')
	(addDragHandle:				rightCenter		top				(brown)						haloDragIcon 				'Move')
	(addDupHandle:				right				top				(green)						haloDuplicateIcon 			'Duplicate')	
	(addHelpHandle:				center			bottom			(lightBlue)					haloHelpIcon 				'Help')
	(addGrowHandle:			right				bottom			(yellow)						haloScaleIcon 				'Change size')
	(addFontSizeHandle:		leftCenter		bottom			(lightGreen)					haloFontSizeIcon 			'Change font')
	(addFontEmphHandle:		rightCenter		bottom			(lightBrown darker)			haloFontEmphasisIcon 		'Emphasis & alignment')
	(addRecolorHandle:			right				bottomCenter	(magenta darker)			haloColorIcon 				'Change color')
)! !

!Preferences class methodsFor: 'halos' stamp: 'jmv 8/18/2012 18:04'!
installHaloSpecsFromArray: anArray

	| aColor |
	^ Parameters at: #HaloSpecs put: (anArray collect: [ :each |
				aColor _ Color.
				each fourth do: [ :sel | aColor _ aColor perform: sel].
				HaloSpec new 
					horizontalPlacement: each second
					verticalPlacement: each third 
					color: aColor
					iconSymbol: each fifth
					addHandleSelector: each first
					hoverHelp: each sixth])! !

!Preferences class methodsFor: 'halos' stamp: 'sw 1/27/2000 23:15'!
installHaloTheme: themeSymbol
	self installHaloSpecsFromArray: (self perform: themeSymbol).
	self setParameter: #HaloTheme to: themeSymbol
	! !

!Preferences class methodsFor: 'halos' stamp: 'jmv 3/2/2010 10:03'!
resetHaloSpecifications
	"Preferences resetHaloSpecifications"

	^ Parameters removeKey: #HaloSpecs ifAbsent: nil! !


!Preferences class methodsFor: 'hard-coded prefs' stamp: 'sw 9/6/2000 05:26'!
desktopMenuTitle
	"Answer the title to be used for the 'meta menu'.  For now, you can hard-code this, later someone should make this be a parameter the user can easily change.  sw 9/6/2000"

	^ 'World'    "This is what it has always been"

	"^ 'Desktop'
	^ 'Squeak'
	^ 'Mike''s Control Panel'"! !


!Preferences class methodsFor: 'initialization' stamp: 'NS 1/28/2004 14:43'!
compileAccessMethodForPreference: aPreference
	"Compile an accessor method for the given preference"

	self class compileSilently: (aPreference name, '
	^ self valueOfFlag: #', aPreference name, ' ifAbsent: [', aPreference defaultValue storeString, ']') classified: 'standard queries'! !

!Preferences class methodsFor: 'initialization' stamp: 'jmv 8/17/2012 20:43'!
initialize
	"
	Preferences initialize
	"
	Preferences cuisDefaults! !

!Preferences class methodsFor: 'initialization' stamp: 'sw 4/4/2001 00:09'!
initializeDictionaryOfPreferences
	"Initialize the DictionaryOfPreferences to be an empty IdentityDictionary"

	"Preferences initializeDictionaryOfPreferences"

	DictionaryOfPreferences _ IdentityDictionary new.! !

!Preferences class methodsFor: 'initialization' stamp: 'jmv 1/17/2011 09:55'!
removeParameter: aSymbol
	"Remove all memory of the given parameter symbol in my various structures."

	self parameterAt: aSymbol ifAbsent: [^ self].

	Parameters removeKey: aSymbol ifAbsent: nil.
	self class removeSelector: aSymbol

"Preferences removeParameter: #tileToggleInBrowsers"! !

!Preferences class methodsFor: 'initialization' stamp: 'jmv 1/17/2011 09:54'!
removePreference: aSymbol
	"Remove all memory of the given preference symbol in my various structures."

	self preferenceAt: aSymbol ifAbsent: [^ self].

	DictionaryOfPreferences removeKey: aSymbol ifAbsent: nil.
	self class removeSelector: aSymbol

"Preferences removePreference: #tileToggleInBrowsers"! !

!Preferences class methodsFor: 'initialization' stamp: 'sw 4/21/2002 05:13'!
setPreferencesFrom: listOfPairs
	"Given a list of <preferenceName, value> pairs, set preference values.  This method is tolerent of the value being supplied either a Boolean or else one of the symbols #true and #false.  Also, a new-value of #noOpinion will result in that 'preference's value not being changed."

	listOfPairs do:
		[:aPair |
			(aPair second == #noOpinion) ifFalse:
				[Preferences setPreference: aPair first toValue: ((aPair second == #true) or: [aPair second == true])]]

"
Preferences setPreferencesFrom: #(( mouseOverForKeyboardFocus false))
Preferences setPreferencesFrom: {{  #mouseOverForKeyboardFocus. true}}
"! !


!Preferences class methodsFor: 'menu parameters' stamp: 'sw 11/3/1998 11:16'!
menuBorderWidth
	^ Parameters at: #menuBorderWidth! !

!Preferences class methodsFor: 'menu parameters' stamp: 'jmv 1/2/2012 15:37'!
restoreDefaultMenuParameters
	"Restore the four color choices of the original implementors of  
	MorphicMenus"
	" 
	Preferences restoreDefaultMenuParameters
	"
	Parameters at: #menuBorderWidth put: 2! !


!Preferences class methodsFor: 'misc' stamp: 'jmv 8/19/2010 19:13'!
cleanUp
	| dict |
	dict _ DictionaryOfPreferences.
	(dict keys select: [ :each |
		(((dict at: each) instVarNamed: 'changeInformee') isKindOf: Behavior) and: [
			((dict at: each) instVarNamed: 'changeInformee') isObsolete ]]) do: [ :eachKey |
				dict removeKey: eachKey ].
	(dict keys select: [ :each |
		(Smalltalk isThereAReferenceTo: each) not ]) do: [ :eachKey |
		dict removeKey: eachKey ].! !

!Preferences class methodsFor: 'misc' stamp: 'jmv 1/17/2011 09:44'!
inspectUnused
	"
	To be used for:
		a) removing unneeded accessors in Preferences class
		b) cleaning the dictionaries themselves for a release
	(Warning: check senders before removing stuff!!)
	Preferences inspectUnused.
	Preferences inspectUnusedParameters
	"
	(DictionaryOfPreferences keys select: [ :a |
		((Smalltalk allCallsOn: a) reject: [ :b |
			b classSymbol = #Preferences ]) isEmpty ]) inspect.! !

!Preferences class methodsFor: 'misc' stamp: 'jmv 1/17/2011 09:44'!
inspectUnusedParameters
	"
	To be used for:
		a) removing unneeded accessors in Preferences class
		b) cleaning the dictionaries themselves for a release
	(Warning: check senders before removing stuff!!)
	Preferences inspectUnused.
	Preferences inspectUnusedParameters
	"
	(Parameters keys select: [ :a |
		((Smalltalk allCallsOn: a) reject: [ :b |
			b classSymbol = #Preferences ]) isEmpty ]) inspect.! !

!Preferences class methodsFor: 'misc' stamp: 'jmv 2/22/2011 22:47'!
staggerPolicyString
	"Answer the string to be shown in a menu to represent the 
	stagger-policy status"
	^ ((self valueOfFlag: #reverseWindowStagger)
		ifTrue: ['<yes>']
		ifFalse: ['<no>']), 'stagger windows'! !

!Preferences class methodsFor: 'misc' stamp: 'sw 6/11/1999 20:49'!
toggleWindowPolicy
	self togglePreference: #reverseWindowStagger! !

!Preferences class methodsFor: 'misc' stamp: 'sw 8/29/2000 16:12'!
wantsChangeSetLogging
	"Answer whether method changes in the receiver should be logged to current change set.  This circumlocution avoids such logging for programmatically-compiled methods in Preferences, removing an annoyance"

	^ Utilities authorInitialsPerSe  ~= 'programmatic'! !


!Preferences class methodsFor: 'parameters' stamp: 'jmv 6/22/2013 19:50'!
annotationInfo 
	"Answer a list of pairs characterizing all the available kinds of annotations; in each pair, the first element is a symbol representing the info type, and the second element is a string providing the corresponding balloon help"

	^ #(
		(timeStamp				'The time stamp of the last submission of the method.')
		(firstComment			'The first comment in the method, if any.')
		(masterComment		'The comment at the beginning of the supermost implementor of the method if any.')
		(documentation			'Comment at beginning of the method or, if it has none, comment at the beginning of a superclass''s implementation of the method')
		(messageCategory		'Which method category the method lies in')
		(sendersCount			'A report of how many senders there of the message.')
		(implementorsCount	'A report of how many implementors there are of the message.')
		(allChangeSets			'A list of all change sets bearing the method.')
		(priorVersionsCount	'A report of how many previous versions there are of the method' )
		(priorTimeStamp			'The time stamp of the penultimate submission of the method, if any')
		(closuresInfo				'Details about BlockClosures in the method')
		(packages				'Details about CodePackages including the method')
	)! !

!Preferences class methodsFor: 'parameters' stamp: 'sw 2/17/1999 00:40'!
defaultAnnotationRequests
	^ Parameters at: #MethodAnnotations ifAbsent:
		[self setDefaultAnnotationInfo]
	"Preferences annotationInfo"! !

!Preferences class methodsFor: 'parameters' stamp: 'sma 6/1/2000 12:08'!
defaultAuthorName
	"Answer the author name to be planted, by default, in a changeset-preamble template.  You can hard-code this to hold your name, thus saving you time when writing the preambles of subsequent changesets"

	^ Utilities authorName! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 11/23/2010 12:00'!
initializeParameters
	"Preferences initializeParameters"
	Parameters _ IdentityDictionary new.
	self restoreDefaultMenuParameters.
	Parameters at: #maxBalloonHelpLineLength put: 28! !

!Preferences class methodsFor: 'parameters' stamp: 'sw 1/24/2001 21:44'!
inspectParameters
	"Open up an inspector on the Parameters of Preferences.  This is crude!!"

	Parameters inspectWithLabel: 'Parameters'! !

!Preferences class methodsFor: 'parameters' stamp: 'sw 11/5/1998 16:49'!
maxBalloonHelpLineLength
	^ Parameters at: #maxBalloonHelpLineLength! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 3/2/2010 10:50'!
parameterAt: aKey ifAbsent: aBlock
	"Answer the parameter saved at the given key; if there is no such key in the Parameters dictionary, evaluate aBlock"

	^ Parameters at: aKey ifAbsent: aBlock! !

!Preferences class methodsFor: 'parameters' stamp: 'sw 9/28/2001 08:40'!
parameterAt: aKey ifAbsentPut: defaultValueBlock
	"Return the Parameter setting at the given key.  If there is no entry for this key in the Parameters dictionary, create one with the value of defaultValueBlock as its value"

	^ Parameters at: aKey ifAbsentPut: defaultValueBlock! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 12/26/2012 13:44'!
setDefaultAnnotationInfo
	"
	Preferences setDefaultAnnotationInfo
	"
	^ Parameters at: #MethodAnnotations put: #(timeStamp messageCategory implementorsCount sendersCount closuresInfo allBaseSystemChangeSets packages)! !

!Preferences class methodsFor: 'parameters' stamp: 'stp 01/13/2000 13:29'!
setParameter: paramName to: paramValue
	"Set the given field in the parameters dictionary."

	Parameters at: paramName put: paramValue! !


!Preferences class methodsFor: 'personalization' stamp: 'NS 1/28/2004 14:43'!
compileHardCodedPref: prefName enable: aBoolean
	"Compile a method that returns a simple true or false (depending on the value of aBoolean) when Preferences is sent prefName as a message"

	self class compileSilently: (prefName asString, '
	"compiled programatically -- return hard-coded preference value"
	^ ', aBoolean storeString) classified: 'hard-coded prefs'.
	
"Preferences compileHardCodedPref: #testing enable: false"! !

!Preferences class methodsFor: 'personalization' stamp: 'jmv 6/7/2013 23:24'!
disableProgrammerFacilities
	"Warning: do not call this lightly!!  It disables all access to menus, debuggers, halos.  There is no guaranteed return from this, which is to say, you cannot necessarily reenable these things once they are disabled -- you can only use whatever the UI of the current project affords, and you cannot even snapshot -- you can only quit. 

     You can completely reverse the work of this method by calling the dual Preferences method enableProgrammerFacilities, provided you have left yourself leeway to bring about a call to that method.

	To set up a system that will come up in such a state, you have to request the snapshot in the same breath as you disable the programmer facilities.  To do this, put the following line into the 'do' menu and then evaluate it from that 'do' menu:

         Preferences disableProgrammerFacilities.

You will be prompted for a new image name under which to save the resulting image."

	Smalltalk beep.
	(self confirm: 'CAUTION!!!!
This is a drastic step!!
Do you really want to do this?')
		ifFalse: [
			Smalltalk beep.  ^ self inform: 'whew!!'].

	self disable: #cmdDotEnabled.       "No user-interrupt-into-debugger"

	"self enable: #noviceMode."           "No control-menu"
	self disable: #warnIfNoSourcesFile.
	self disable: #warnIfNoChangesFile.
	Smalltalk saveAs! !

!Preferences class methodsFor: 'personalization' stamp: 'jmv 12/16/2010 14:01'!
enableProgrammerFacilities
	"Meant as a one-touch recovery from a #disableProgrammerFacilities call."
	"Preferences enableProgrammerFacilities"

	self enable: #cmdDotEnabled.
"	self disable: #noviceMode."
	self enable: #warnIfNoSourcesFile.
	self enable: #warnIfNoChangesFile.! !


!Preferences class methodsFor: 'preference-object access' stamp: 'sw 4/13/2001 00:06'!
allPreferenceObjects
	"Answer a list of all the Preference objects registered in the system"

	^ DictionaryOfPreferences values! !

!Preferences class methodsFor: 'preference-object access' stamp: 'jmv 3/2/2010 10:03'!
preferenceAt: aSymbol
	"Answer the Preference object at the given symbol, or nil if not there"

	^ DictionaryOfPreferences at: aSymbol ifAbsent: nil! !

!Preferences class methodsFor: 'preference-object access' stamp: 'jmv 3/2/2010 10:50'!
preferenceAt: aSymbol ifAbsent: aBlock
	"Answer the Preference object at the given symbol, or the value of aBlock if not present"

	^ DictionaryOfPreferences at: aSymbol ifAbsent: aBlock! !


!Preferences class methodsFor: 'preferences panel' stamp: 'jmv 1/25/2011 09:42'!
openPreferencesInspector
	"Open a window on the current set of preferences choices, allowing the user to view and change their settings"
	
	DictionaryOfPreferences inspectWithLabel: 'Preferences'! !


!Preferences class methodsFor: 'scrollbar parameters' stamp: 'jmv 2/25/2011 16:35'!
scrollbarThickness
	"Includes border"
	^Preferences standardListFont pointSize + 4! !


!Preferences class methodsFor: 'standard queries' stamp: 'jmv 5/27/2009 23:26'!
aaFontsColormapDepth
	"Adjust balance between colored AA text quality (especially if subpixel AA is used) and space / performance.
	5 is optimal quality. Each colorMap takes 128kB of RAM, and takes several seconds to build.
	4 is a reasonable balance. Each colorMap takes 16kB of RAM and builds fast on a fast machine.
	3 is good for slow hardware or memory restrictions. Each colorMap takes 2 kb of RAM."
	^self
		valueOfFlag: #aaFontsColormapDepth
		ifAbsent: [4]! !

!Preferences class methodsFor: 'standard queries'!
allowBlockArgumentAssignment
	^ self
		valueOfFlag: #allowBlockArgumentAssignment
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
allowUnderscoreAssignments
	^ self
		valueOfFlag: #allowUnderscoreAssignments
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
allowUnderscoreSelectors
	^ self
		valueOfFlag: #allowUnderscoreSelectors
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 3/14/2012 08:09'!
alternativeBrowseIt
	^ self
		valueOfFlag: #alternativeBrowseIt
		ifAbsent: [ false ]! !

!Preferences class methodsFor: 'standard queries'!
automaticPlatformSettings
	^ self
		valueOfFlag: #automaticPlatformSettings
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
balloonHelpEnabled
	^ self
		valueOfFlag: #balloonHelpEnabled
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
biggerCursors
	^ self
		valueOfFlag: #biggerCursors
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
browseWithPrettyPrint
	^ self
		valueOfFlag: #browseWithPrettyPrint
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
canRecordWhilePlaying
	^ self
		valueOfFlag: #canRecordWhilePlaying
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
caseSensitiveFinds
	^ self
		valueOfFlag: #caseSensitiveFinds
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
changeSetVersionNumbers
	^ self
		valueOfFlag: #changeSetVersionNumbers
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
cheapWindowReframe
	^ self
		valueOfFlag: #cheapWindowReframe
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
checkForSlips
	^ self
		valueOfFlag: #checkForSlips
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
classicNewMorphMenu
	^ self
		valueOfFlag: #classicNewMorphMenu
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
cmdDotEnabled
	^ self
		valueOfFlag: #cmdDotEnabled
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 5/30/2011 08:33'!
commandClickOpensHalo
	"
	Preferences enable: #commandClickOpensHalo
	Preferences disable: #commandClickOpensHalo
	"
	^ self
		valueOfFlag: #commandClickOpensHalo
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
debugHaloHandle
	^ self
		valueOfFlag: #debugHaloHandle
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries' stamp: 'mir 3/5/2004 19:22'!
debugLogTimestamp
	^ self
		valueOfFlag: #debugLogTimestamp
		ifAbsent: [false]! !

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 4/8/2013 09:45'!
debugShowDamage
	"Useful for debugging, but also for simulating vnc over a slow network"
	^ (self
		valueOfFlag: #debugShowDamage
		ifAbsent: [ false ])! !

!Preferences class methodsFor: 'standard queries'!
decorateBrowserButtons
	^ self
		valueOfFlag: #decorateBrowserButtons
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
diffsInChangeList
	^ self
		valueOfFlag: #diffsInChangeList
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
diffsWithPrettyPrint
	^ self
		valueOfFlag: #diffsWithPrettyPrint
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
dismissAllOnOptionClose
	^ self
		valueOfFlag: #dismissAllOnOptionClose
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
extraDebuggerButtons
	^ self
		valueOfFlag: #extraDebuggerButtons
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
focusFollowsMouse
	^ self
		valueOfFlag: #focusFollowsMouse
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 4/11/2009 23:16'!
focusIndicatorWidth
	^ self
		valueOfFlag: #focusIndicatorWidth
		ifAbsent: [1]! !

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 12/9/2009 00:13'!
fullScreenLeavesDeskMargins
	^ self
		valueOfFlag: #fullScreenLeavesDeskMargins
		ifAbsent: [true]! !

!Preferences class methodsFor: 'standard queries'!
haloEnclosesFullBounds
	^ self
		valueOfFlag: #haloEnclosesFullBounds
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
logDebuggerStackToFile
	^ self
		valueOfFlag: #logDebuggerStackToFile
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
menuIcons
	^ self
		valueOfFlag: #menuIcons
		ifAbsent: [ #basicIcons ].! !

!Preferences class methodsFor: 'standard queries'!
menuKeyboardControl
	^ self
		valueOfFlag: #menuKeyboardControl
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
menuWithIcons
	^ self
		valueOfFlag: #menuWithIcons
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
optionalButtons
	^ self
		valueOfFlag: #optionalButtons
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 8/13/2010 22:04'!
prettyPrintRectangularBlocks
	^ self
		valueOfFlag: #prettyPrintRectangularBlocks
		ifAbsent: [false]! !

!Preferences class methodsFor: 'standard queries'!
reverseWindowStagger
	^ self
		valueOfFlag: #reverseWindowStagger
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
selectionsMayShrink
	^ self
		valueOfFlag: #selectionsMayShrink
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
selectiveHalos
	^ self
		valueOfFlag: #selectiveHalos
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries' stamp: 'al 7/31/2007 16:17'!
serverMode
	^ self
		valueOfFlag: #serverMode
		ifAbsent: [false]! !

!Preferences class methodsFor: 'standard queries'!
showLinesInHierarchyViews
	^ self
		valueOfFlag: #showLinesInHierarchyViews
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
smartUpdating
	^ self
		valueOfFlag: #smartUpdating
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
soundQuickStart
	^ self
		valueOfFlag: #soundQuickStart
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
soundStopWhenDone
	^ self
		valueOfFlag: #soundStopWhenDone
		ifAbsent: [ false ].! !

!Preferences class methodsFor: 'standard queries'!
soundsEnabled
	^ self
		valueOfFlag: #soundsEnabled
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
systemWindowEmbedOK
	^ self
		valueOfFlag: #systemWindowEmbedOK
		ifAbsent: [false]! !

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 9/30/2011 17:08'!
tapAndHoldEmulatesButton2
	"
	Preferences enable: #tapAndHoldEmulatesButton2
	Preferences disable: #tapAndHoldEmulatesButton2
	"
	^ self
		valueOfFlag: #tapAndHoldEmulatesButton2
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
thoroughSenders
	^ self
		valueOfFlag: #thoroughSenders
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
twentyFourHourFileStamps
	^ self
		valueOfFlag: #twentyFourHourFileStamps
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
warnIfNoChangesFile
	^ self
		valueOfFlag: #warnIfNoChangesFile
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
warnIfNoSourcesFile
	^ self
		valueOfFlag: #warnIfNoSourcesFile
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'standard queries'!
wordStyleCursorMovement
	^ self
		valueOfFlag: #wordStyleCursorMovement
		ifAbsent: [ true ].! !


!Preferences class methodsFor: 'menu icons' stamp: 'cbr 5/16/2011 16:53'!
useAllIcons

	self setPreference: #menuIcons toValue: #allIcons.
! !

!Preferences class methodsFor: 'menu icons' stamp: 'cbr 5/16/2011 16:53'!
useBasicIcons

	self setPreference: #menuIcons toValue: #basicIcons.
! !

!Preferences class methodsFor: 'menu icons' stamp: 'cbr 5/16/2011 16:53'!
useNoIcons

	self setPreference: #menuIcons toValue: #noIcons.
! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 12/29/2013 13:13'!
cuisDefaults
	"
	Preferences cuisDefaults
	"
	self setPreferencesFrom:

	#(
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds false)
		(checkForSlips true)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(simpleMenus false)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(allowUnderscoreAssignments true)
		(allowUnderscoreSelectors true)
		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
		(cheapWindowReframe false)
	)! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 12/29/2013 13:14'!
slowMachine
	"
	Preferences slowMachine
	"
	self setPreferencesFrom: #(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(simpleMenus false)
		(smartUpdating false)
		(subPixelRenderFonts false)
		(thoroughSenders false)
		(cheapWindowReframe true)
	).
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 12/29/2013 13:15'!
smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak in recent years. Caution: this theme removes the standard Squeak flaps, turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more.

	Preferences smalltalk80
	"

	self setPreferencesFrom:

	#(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(simpleMenus false)
		(smartUpdating false)
		(thoroughSenders false)
		(cheapWindowReframe true)
	)! !


!Preferences class methodsFor: 'shout' stamp: 'jmv 6/25/2013 17:29'!
italicsInShout
	^ self
		valueOfFlag: #italicsInShout
		ifAbsent: [true]! !

!Preferences class methodsFor: 'shout' stamp: 'jmv 4/11/2010 21:53'!
shoutInWorkspaces
	^ self
		valueOfFlag: #shoutInWorkspaces
		ifAbsent: [true]! !

!Preferences class methodsFor: 'shout' stamp: 'jmv 8/21/2009 08:54'!
syntaxHighlightingAsYouType
	^ self
		valueOfFlag: #syntaxHighlightingAsYouType
		ifAbsent: [true]! !

!Preferences class methodsFor: 'shout' stamp: 'jmv 8/21/2009 08:55'!
syntaxHighlightingAsYouTypeAnsiAssignment
	^ self
		valueOfFlag: #syntaxHighlightingAsYouTypeAnsiAssignment
		ifAbsent: [false]! !

!Preferences class methodsFor: 'shout' stamp: 'jmv 4/14/2010 10:17'!
syntaxHighlightingAsYouTypeLeftArrowAssignment
	^ self
		valueOfFlag: #syntaxHighlightingAsYouTypeLeftArrowAssignment
		ifAbsent: [ false ]! !


!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2011 22:56'!
bigFonts
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences bigFonts
	"
	
	Preferences setDefaultFonts: #(
		(setSystemFontTo: 'DejaVu' 11)
		(setListFontTo: 'DejaVu' 11)
		(setMenuFontTo: 'DejaVu' 12)
		(setWindowTitleFontTo: 'DejaVu' 14)
		(setCodeFontTo: 'DejaVu' 11)
		(setButtonFontTo: 'DejaVu' 11)).
	Preferences enable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2011 22:56'!
hugeFonts
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences hugeFonts
	"
	
	Preferences setDefaultFonts: #(
		(setSystemFontTo: 'DejaVu' 17)
		(setListFontTo: 'DejaVu' 17)
		(setMenuFontTo: 'DejaVu' 17)
		(setWindowTitleFontTo: 'DejaVu' 22)
		(setCodeFontTo: 'DejaVu' 17)
		(setButtonFontTo: 'DejaVu' 17)).
	Preferences enable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2011 22:56'!
smallFonts
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences smallFonts
	"
	
	Preferences setDefaultFonts: #(
		(setSystemFontTo: 'DejaVu' 8)
		(setListFontTo: 'DejaVu' 8)
		(setMenuFontTo: 'DejaVu' 8)
		(setWindowTitleFontTo: 'DejaVu' 11)
		(setCodeFontTo: 'DejaVu' 8)
		(setButtonFontTo: 'DejaVu' 8)).
	Preferences disable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2011 22:55'!
standardFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences standardFonts
	"
	
	Preferences setDefaultFonts: #(
		(setSystemFontTo: 'DejaVu' 9)
		(setListFontTo: 'DejaVu' 9)
		(setMenuFontTo: 'DejaVu' 10)
		(setWindowTitleFontTo: 'DejaVu' 12)
		(setCodeFontTo: 'DejaVu' 9)
		(setButtonFontTo: 'DejaVu' 9)).
	Preferences disable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2011 22:56'!
tinyFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences tinyFonts
	"
	
	Preferences setDefaultFonts: #(
		(setSystemFontTo: 'DejaVu' 7)
		(setListFontTo: 'DejaVu' 5)
		(setMenuFontTo: 'DejaVu' 5)
		(setWindowTitleFontTo: 'DejaVu' 7)
		(setCodeFontTo: 'DejaVu' 5)
		(setButtonFontTo: 'DejaVu' 5)).
	Preferences disable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2011 22:56'!
veryBigFonts
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences veryBigFonts
	"
	
	Preferences setDefaultFonts: #(
		(setSystemFontTo: 'DejaVu' 14)
		(setListFontTo: 'DejaVu' 14)
		(setMenuFontTo: 'DejaVu' 14)
		(setWindowTitleFontTo: 'DejaVu' 17)
		(setCodeFontTo: 'DejaVu' 14)
		(setButtonFontTo: 'DejaVu' 14)).
	Preferences enable: #biggerCursors! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/25/2011 22:56'!
verySmallFonts
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences verySmallFonts
	"
	
	Preferences setDefaultFonts: #(
		(setSystemFontTo: 'DejaVu' 8)
		(setListFontTo: 'DejaVu' 7)
		(setMenuFontTo: 'DejaVu' 7)
		(setWindowTitleFontTo: 'DejaVu' 9)
		(setCodeFontTo: 'DejaVu' 7)
		(setButtonFontTo: 'DejaVu' 7)).
	Preferences disable: #biggerCursors! !


!classDefinition: #ProjectX category: #'System-Support'!
Object subclass: #ProjectX
	instanceVariableNames: ''
	classVariableNames: 'UIProcess'
	poolDictionaries: ''
	category: 'System-Support'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ProjectX class' category: #'System-Support'!
ProjectX class
	instanceVariableNames: ''!

!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 5/21/2006 16:06'!
currentInterruptNameX: aString
"
CurrentProjectRefactoring currentInterruptName:
"
	^self interruptNameX: aString! !

!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 7/9/2014 11:25'!
interruptNameX: labelString
	"Create a Notifier on the active scheduling process with the given label."
	| preemptedProcess  label |
	preemptedProcess _ Processor preemptedProcess.

	"Only debug preempted process if its priority is >= UIProcess' priority"
	preemptedProcess priority < UIProcess priority ifTrue: [
		preemptedProcess _ UIProcess ].

	label _ labelString,
					' - Process: ', preemptedProcess name,
					' - Priority: ', preemptedProcess priority printString.
	preemptedProcess isTerminated
		ifTrue: [
			ProjectX newProcessIfUIX: preemptedProcess.
			self notify: 'Can not debug a terminated process: ', label ]
		ifFalse: [
			preemptedProcess suspend.
			Debugger
				openInterrupt: label 
				onProcess: preemptedProcess ]! !

!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 5/9/2013 23:38'!
newProcessIfUIX: suspendedProcess

	suspendedProcess animatedUI ifNotNil: [ :aWorld |
		self spawnNewMorphicProcessFor: aWorld ]! !

!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 5/5/2014 09:50'!
spawnNewMorphicProcessFor: aWorld

	UIProcess ifNotNil: [ UIProcess animatedUI: nil ].
	UIProcess _ [
		aWorld clearWaitDelay.
		aWorld clearCanvas.
		[ aWorld doOneCycle. Processor yield. true] whileTrue: [].
	] newProcess priority: Processor userSchedulingPriority.
	UIProcess
		name: 'Morphic UI';
		animatedUI: aWorld;
		resume! !

!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 10/23/2012 23:18'!
ui
	^UIProcess animatedUI! !


!classDefinition: #RealEstateAgent category: #'System-Support'!
Object subclass: #RealEstateAgent
	instanceVariableNames: ''
	classVariableNames: 'ReverseStaggerOffset StaggerOffset StaggerOrigin StandardSize StandardWindowOrigins'
	poolDictionaries: ''
	category: 'System-Support'!
!RealEstateAgent commentStamp: '<historical>' prior: 0!
Responsible for real-estate management on the screen, which is to say, controlling where new windows appear, with what sizes, etc.  5/20/96 sw!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'RealEstateAgent class' category: #'System-Support'!
RealEstateAgent class
	instanceVariableNames: ''!

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 8/13/2013 08:54'!
initialFrameFor: aView initialExtent: initialExtent world: aWorld 
	"Find a plausible initial screen area for the supplied view, which should be a StandardSystemView, taking into account the 'reverseWindowStagger' Preference, the size needed, and other windows currently on the screen."

	| allOrigins screenRight screenBottom putativeOrigin putativeFrame allowedArea staggerOrigin otherFrames |
	Preferences reverseWindowStagger 
		ifTrue: 
			[^self 
				strictlyStaggeredInitialFrameFor: aView
				initialExtent: initialExtent
				world: aWorld].
	allowedArea := self maximumUsableAreaInWorld: aWorld.
	screenRight := allowedArea right.
	screenBottom := allowedArea bottom.
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [ :w | w isCollapsed not]) 
						collect: [:w | w morphBoundsInWorld].
	allOrigins := otherFrames collect: [:f | f origin].
	(self standardPositionsInWorld: aWorld) do: 
			[:aPosition | 
			"First see if one of the standard positions is free"

			(allOrigins includes: aPosition) 
				ifFalse: 
					[^(aPosition extent: initialExtent) 
						translatedAndSquishedToBeWithin: allowedArea]].
	staggerOrigin := (self standardPositionsInWorld: aWorld) first.	"Fallback: try offsetting from top left"
	putativeOrigin := staggerOrigin.
	
	[putativeOrigin := putativeOrigin + StaggerOffset.
	putativeFrame := putativeOrigin extent: initialExtent.
	putativeFrame bottom < screenBottom 
		and: [putativeFrame right < screenRight]] 
			whileTrue: 
				[(allOrigins includes: putativeOrigin) 
					ifFalse: 
						[^(putativeOrigin extent: initialExtent) 
							translatedAndSquishedToBeWithin: allowedArea]].
	^(self scrollBarSetback @ self screenTopSetback extent: initialExtent) 
		translatedAndSquishedToBeWithin: allowedArea! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'RAA 5/25/2000 08:13'!
initialFrameFor: aView world: aWorld
	"Find a plausible initial screen area for the supplied view.  See called method."

	^ self initialFrameFor: aView initialExtent: aView initialExtent world: aWorld! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 8/16/2010 14:40'!
initialize
	"Initialize the class variables in the receiver.  5/22/96 sw"
	"RealEstateAgent initialize"

	StaggerOffset _ 6 @ 20.
	ReverseStaggerOffset _ -6 @ 20.
	StaggerOrigin _ 200 @ 30.
	StandardSize _ 600@480.! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 9/24/2012 18:43'!
maximumUsableArea

	| allowedArea |
	allowedArea _ Display boundingBox.
	^allowedArea
! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 5/20/2011 11:12'!
maximumUsableAreaInWorld: aWorldOrNil

	| allowedArea |
	allowedArea _ Display boundingBox.
	aWorldOrNil ifNotNil: [allowedArea _ allowedArea intersect: aWorldOrNil viewBox].
	^allowedArea
! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 2/12/2008 19:16'!
screenTopSetback
	^ 0! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 2/12/2008 19:17'!
scrollBarSetback
	^ 16-3  "width = 16; inset from border by 3"! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'RAA 5/25/2000 08:43'!
standardPositionsInWorld: aWorldOrNil
	"Return a list of standard window positions -- this may have one, two, or four of them, depending on the size and shape of the display screen.  "

	| anArea aList  midX midY |

	anArea _ self maximumUsableAreaInWorld: aWorldOrNil.

	midX _ self scrollBarSetback +   ((anArea width - self scrollBarSetback)  // 2).
	midY _ self screenTopSetback + ((anArea height - self screenTopSetback) // 2).
	aList _ OrderedCollection with: (self scrollBarSetback @ self screenTopSetback).
	self windowColumnsDesired > 1
		ifTrue:
			[aList add: (midX @ self screenTopSetback)].
	self windowRowsDesired > 1
		ifTrue:
			[aList add: (self scrollBarSetback @ (midY+self screenTopSetback)).
			self windowColumnsDesired > 1 ifTrue:
				[aList add: (midX @ (midY+self screenTopSetback))]].
	^ aList! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 4/7/2011 10:54'!
standardWindowExtent
	"Answer the standard default extent for new windows.  "

	| effectiveExtent width strips height grid allowedArea maxLevel |
	effectiveExtent _ self maximumUsableArea extent
					- (self scrollBarSetback @ self screenTopSetback).
	Preferences reverseWindowStagger ifTrue:
		["NOTE: following copied from strictlyStaggeredInitialFrameFor:"
		allowedArea _ self maximumUsableArea insetBy: (
			self scrollBarSetback @ self screenTopSetback extent: 0@0
		).
		"Number to be staggered at each corner (less on small screens)"
		maxLevel _ allowedArea area > 700000 ifTrue: [3] ifFalse: [2].
		"Amount by which to stagger (less on small screens)"
		grid _ allowedArea area > 700000 ifTrue: [40] ifFalse: [20].
		^ (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))) min: 40@32 * Preferences standardCodeFont height ].
	width _ (strips _ self windowColumnsDesired) > 1
		ifTrue:
			[effectiveExtent x // strips]
		ifFalse:
			[(3 * effectiveExtent x) // 4].
	height _ (strips _ self windowRowsDesired) > 1
		ifTrue:
			[effectiveExtent y // strips]
		ifFalse:
			[(3 * effectiveExtent y) //4].
	^ width @ height

"RealEstateAgent standardWindowExtent"! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 8/17/2012 18:55'!
strictlyStaggeredInitialFrameFor: aStandardSystemView initialExtent: initialExtent world: aWorld 
	"This method implements a staggered window placement policy that I (di) like.
	Basically it provides for up to 4 windows, staggered from each of the 4 corners.
	The windows are staggered so that there will always be a corner visible."

	| allowedArea grid initialFrame otherFrames cornerSel corner delta putativeCorner free maxLevel |
	allowedArea := (self maximumUsableAreaInWorld: aWorld) 
				insetBy: (self scrollBarSetback @ self screenTopSetback extent: 0 @ 0).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel := allowedArea area > 300000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid := allowedArea area > 500000 ifTrue: [40] ifFalse: [20].
	initialFrame := 0 @ 0 extent: initialExtent.
	"min: (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))))
							min: 600@400"
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [:w | w isCollapsed not]) 
						collect: [:w | w morphBoundsInWorld].
	0 to: maxLevel
		do: 
			[:level | 
			1 to: 4
				do: [:ci | 
					cornerSel := #(#topLeft #topRight #bottomRight #bottomLeft) at: ci.
					corner := allowedArea perform: cornerSel.
					"The extra grid//2 in delta helps to keep title tabs distinct"
					delta := ((maxLevel - level) * grid + (grid // 2)) @ (level * grid).
					1 to: ci - 1 do: [ :i | delta _ delta y negated @ delta x ].	"slow way"
					putativeCorner := corner + delta.
					free := true.
					otherFrames 
						do: [:w | free := free & ((w perform: cornerSel) ~= putativeCorner)].
					free 
						ifTrue: 
							[^(initialFrame aligned: (initialFrame perform: cornerSel)
								with: putativeCorner) translatedAndSquishedToBeWithin: allowedArea]]].
	"If all else fails..."
	^(self scrollBarSetback @ self screenTopSetback 
		extent: initialFrame extent) translatedAndSquishedToBeWithin: allowedArea! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'RAA 11/21/1999 22:54'!
windowColumnsDesired
	"Answer how many separate vertical columns of windows are wanted.  5/22/96 sw"
	^ Preferences reverseWindowStagger
		ifTrue:
			[1]
		ifFalse:
			[(self maximumUsableArea width > 640)
				ifTrue:
					[2]
				ifFalse:
					[1]]! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'RAA 11/21/1999 22:54'!
windowRowsDesired
	"Answer how many separate horizontal rows of windows are wanted.  5/22/96 sw"
	^ Preferences reverseWindowStagger
		ifTrue:
			[1]
		ifFalse:
			[(self maximumUsableArea height > 480)
				ifTrue:
					[2]
				ifFalse:
					[1]]! !


!classDefinition: #SharedPool category: #'System-Support'!
Object subclass: #SharedPool
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!SharedPool commentStamp: '<historical>' prior: 0!
A shared pool represents a set of bindings which are accessible to all classes which import the pool in its 'pool dictionaries'. SharedPool is NOT a dictionary but rather a name space. Bindings are represented by 'class variables' - as long as we have no better way to represent them at least.!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SharedPool class' category: #'System-Support'!
SharedPool class
	instanceVariableNames: ''!

!SharedPool class methodsFor: 'name lookup' stamp: 'jmv 5/24/2014 11:03'!
bindingOf: varName
	"Answer the binding of some variable resolved in the scope of the receiver"
	| aSymbol binding |
	aSymbol _ varName asSymbol.

	"First look in classVar dictionary."
	binding _ self classPool bindingOf: aSymbol.
	binding ifNotNil: [ ^binding ].

	"Next look in shared pools."
	self sharedPools do: [ :pool | 
		| poolBinding |
		poolBinding _ pool bindingOf: aSymbol.
		poolBinding ifNotNil: [ ^poolBinding ].
	].

	"subclassing is not preserved"
	^nil! !

!SharedPool class methodsFor: 'name lookup' stamp: 'ar 5/18/2003 20:33'!
bindingsDo: aBlock
	^self classPool bindingsDo: aBlock! !

!SharedPool class methodsFor: 'name lookup' stamp: 'ar 5/18/2003 18:14'!
classBindingOf: varName
	"For initialization messages grant the regular scope"
	^super bindingOf: varName! !

!SharedPool class methodsFor: 'name lookup' stamp: 'tween 9/13/2004 10:10'!
hasBindingThatBeginsWith: aString
	"Answer true if the receiver has a binding that begins with aString, false otherwise"

	"First look in classVar dictionary."
	(self classPool hasBindingThatBeginsWith: aString) ifTrue:[^true].
	"Next look in shared pools."
	self sharedPools do:[:pool | 
		(pool hasBindingThatBeginsWith: aString) ifTrue: [^true]].
	^false! !

!SharedPool class methodsFor: 'name lookup' stamp: 'tpr 5/29/2003 18:12'!
includesKey: aName
	"does this pool include aName"
	^(self bindingOf: aName) notNil! !


!SharedPool class methodsFor: 'enumerating' stamp: 'tpr 12/14/2004 12:34'!
keysDo: aBlock
"A hopefully temporary fix for an issue arising from miss-spelled variable names in code being compiled. The correction code (see Class>possibleVariablesFor:continuedFrom: assumes that sharedPools are Dictionaries. The proper fix would involve making sure all pools are actually subclasses of SharedPool, which they are not currently."
	self bindingsDo:[:b|
		aBlock value: b key]! !


!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'versionMajor versionMinor date highestUpdate updates'
	classVariableNames: 'Current'
	poolDictionaries: ''
	category: 'System-Support'!

!SystemVersion methodsFor: 'accessing'!
date
	^date! !

!SystemVersion methodsFor: 'accessing'!
date: newDate
	date _ newDate! !

!SystemVersion methodsFor: 'accessing' stamp: 'mir 5/1/2001 18:19'!
datedVersion
	"Answer the version of this release."

	^ self version asString , ' of ' , self date printString! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 12:40'!
highestUpdate
	highestUpdate ifNil: [
		highestUpdate _ updates
			ifEmpty: [ 0 ]
			ifNotEmpty: [
				(updates detectMax: [ :updateName | updateName initialIntegerOrNil ifNil: [0]])
					initialIntegerOrNil ifNil: [0]]].
	^ highestUpdate! !

!SystemVersion methodsFor: 'accessing'!
highestUpdate: anInteger
	highestUpdate _ anInteger! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 12:39'!
registerUpdate: changeSetOrPackageName

	changeSetOrPackageName initialIntegerOrNil ifNotNil: [ :number |
		highestUpdate _ self highestUpdate max: number ].
	updates add: changeSetOrPackageName! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 4/5/2012 09:28'!
unregisterUpdate: update
	updates remove: update ifAbsent: nil! !

!SystemVersion methodsFor: 'accessing'!
updates
	^updates! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 7/7/2013 17:55'!
version
	^String streamContents: [ :strm |
		strm nextPutAll: 'Cuis '.
		versionMajor printOn: strm.
		strm nextPut: $. .
		versionMinor printOn: strm ]! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 5/27/2014 22:27'!
versionMajor
	^versionMajor! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 5/27/2014 22:27'!
versionMinor
	^versionMinor! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 7/7/2013 17:53'!
versionRevision
	"Answer an array of version.revision.
	Try to do something not completely unlike 'Semantic Versioning'.
	For example, for Cuis 4.1, updated to #1744, answer
	{ 41 . 1744 }
	SystemVersion current versionRevision
	"
	^{versionMajor * 10 + versionMinor . self highestUpdate}! !


!SystemVersion methodsFor: 'printing' stamp: 'mir 5/1/2001 18:20'!
printOn: stream
	stream
		nextPutAll: self datedVersion;
		nextPutAll: ' update ' , self highestUpdate printString! !


!SystemVersion methodsFor: 'testing' stamp: 'jmv 1/28/2013 12:16'!
isCuis
	"Sure we are!!!!
	Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!!"
	^true! !

!SystemVersion methodsFor: 'testing' stamp: 'jmv 1/28/2013 12:16'!
isPharo
	"Nope. We are Cuis.
	Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!!"
	^false! !

!SystemVersion methodsFor: 'testing' stamp: 'jmv 1/28/2013 12:16'!
isSqueak
	"Nope. We are Cuis.
	Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!!"
	^false! !


!SystemVersion methodsFor: 'initialization' stamp: 'jmv 7/7/2013 17:57'!
versionMajor: aNumber versionMinor: anotherNumber
	versionMajor _ aNumber.
	versionMinor _ anotherNumber.
	date _ Date today.
	updates _ OrderedCollection new.
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SystemVersion class' category: #'System-Support'!
SystemVersion class
	instanceVariableNames: ''!

!SystemVersion class methodsFor: 'accessing'!
current
	Current ifNil: [Current _ SystemVersion new].
	^Current! !

!SystemVersion class methodsFor: 'accessing' stamp: 'mir 8/10/2001 11:52'!
parseVersionString: versionString
	"Answer the version of this release as version, date, update."
	"SystemVersion parseVersionString: 'Squeak3.1alpha of 28 February 2001 [latest update: #3966]' "

	| stream version date update |

	[stream _ ReadStream on: versionString.
	version _ stream upToAll: ' of '.
	date _ Date readFrom: stream.
	stream upToAll: ' #'.
	update _ Number readFrom: stream]
		on: Error
		do: [^nil].
	^{version. date. update.}! !

!SystemVersion class methodsFor: 'accessing' stamp: 'mir 8/10/2001 11:53'!
pluginVersion: availableVersionString newerThan: currentVersionString
	| currentVersion availableVersion |
	(currentVersionString isEmptyOrNil
		or: [availableVersionString isEmptyOrNil])
		ifTrue: [^true].
	currentVersion _ self parseVersionString: currentVersionString.
	availableVersion _ self parseVersionString: availableVersionString.
	(currentVersion isNil
		or: [availableVersion isNil])
		ifTrue: [^false].
	^(currentVersion at: 2) < (availableVersion at: 2)! !


!SystemVersion class methodsFor: 'instance creation' stamp: 'jmv 7/7/2013 18:01'!
newVersionMajor: aNumber versionMinor: anotherNumber
	| newVersion |
	newVersion _ self versionMajor: aNumber versionMinor: anotherNumber.
	newVersion
		highestUpdate: self current highestUpdate.
	Current _ newVersion
! !

!SystemVersion class methodsFor: 'instance creation' stamp: 'jmv 7/7/2013 18:00'!
versionMajor: aNumber versionMinor: anotherNumber
	"
	SystemVersion versionMajor: 4 versionMinor: 1
	"
	^self basicNew versionMajor: aNumber versionMinor: anotherNumber! !


!SystemVersion class methodsFor: 'updating' stamp: 'jmv 2/15/2008 00:50'!
currentPluginVersion
	^Smalltalk vmVersion! !


!classDefinition: #Transcript category: #'System-Support'!
Object subclass: #Transcript
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!Transcript commentStamp: '<historical>' prior: 0!
A new implementation of Transcript.
- Thread safe.
- Very fast.
- Independent of Morphic or any other UI framework.
- Immediate feedback.
- Can log to file.
- Not an editor. Only used for output.
- All protocol is on the Class side!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay innerRectangle lastDisplayTime'!

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 6/7/2014 13:00'!
bounds: aRectangle

	innerRectangle _ aRectangle insetBy: self borderWidth + self padding! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 7/29/2013 23:36'!
clear
	| stream |
	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex _ 1.
		lastIndex _ 1.
		entries at: 1 put: 'Transcript'.	
		unfinishedEntry reset.
		
		logToFile ifTrue: [
			stream _ FileStream forceNewFileNamed: self filename.
			[
				stream nextPutAll: 'Transcript log started: '.
				DateAndTime now printOn: stream.
				stream
					newLine;
					nextPutAll: '------------------------------------------------------------------------';
					newLine
			] ensure: [ stream close ]]].
	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 5/11/2011 08:53'!
clearAll

	self clearInternal.
	logToFile ifTrue: [
		self clearFile ]! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 7/29/2013 23:36'!
clearFile
	| stream |
	accessSemaphore critical: [
		stream _ FileStream forceNewFileNamed: self filename.
		[
			stream nextPutAll: 'Transcript log started: '.
			DateAndTime now printOn: stream.
			stream
				newLine;
				nextPutAll: '------------------------------------------------------------------------';
				newLine
		] ensure: [ stream close ]]! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 5/11/2011 08:51'!
clearInternal

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex _ 1.
		lastIndex _ 1.
		entries at: 1 put: 'Transcript'.	
		unfinishedEntry reset ].
	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 2/18/2011 14:16'!
log: aString
	self addEntry: aString.
	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 2/18/2011 16:46'!
logToFile: aBoolean
	"
	self logToFile
	"
	logToFile _ aBoolean! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 5/11/2011 08:41'!
logsToFile
	^logToFile! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 2/18/2011 16:46'!
showOnDisplay: aBoolean
	"
	self logToFile
	"
	showOnDisplay _ aBoolean! !


!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 2/18/2011 11:21'!
endEntry
	"For compatibility with old TranscriptStream. nop here"! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 3/13/2012 12:10'!
newLine
	"WriteStream protocol.
	In the older TranscriptStream, it added a CR character.
	Now, finish the current incomplete entry."

	self finishEntry! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 2/18/2011 14:17'!
nextPut: aCharacter
	"WriteStream protocol.
	Append aCharacter to the unfinishedEntry.
	cr characters sent with this message do NOT finish the current unfinishedEntry."

	unfinishedEntry nextPut: aCharacter.
	self displayUnfinishedEntry! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 2/18/2011 14:17'!
nextPutAll: aString
	"WriteStream protocol.
	Append aString to the unfinishedEntry.
	cr characters sent with this message do NOT finish the current unfinishedEntry."

	unfinishedEntry nextPutAll: aString.
	self displayUnfinishedEntry! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 2/18/2011 11:20'!
print: anObject
	"Stream protocol"
	anObject printOn: self! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 2/18/2011 11:22'!
show: anObject
	"Old TranscriptStream protocol."
	self nextPutAll: anObject asString! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 2/18/2011 11:22'!
space
	"WriteStream protocol.
	Append a space character to the receiver."

	self nextPut: Character space! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 2/18/2011 11:22'!
tab
	"WriteStream protocol.
	Append a tab character to the receiver."

	self nextPut: Character tab! !


!Transcript class methodsFor: 'private' stamp: 'jmv 3/13/2012 12:04'!
addEntry: aString
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."
	| msg now |
	logToFile ifTrue: [
		now _ DateAndTime now.
		msg _ String streamContents: [ :strm |
			now printWithMsOn: strm.
			strm
				nextPutAll: ' process:';
				nextPutAll: Processor activeProcess priority printString;
				nextPut: $ ;
				nextPutAll: Processor activeProcess hash printString;
				nextPut: $ ;
				nextPutAll: aString;
				newLine ]].

	self addEntry: aString logToFile: msg! !

!Transcript class methodsFor: 'private' stamp: 'jmv 7/29/2013 23:35'!
addEntry: aString logToFile: otherString
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."
	| stream |
	accessSemaphore critical: [
		
		"Internal circular collection"
		lastIndex _ lastIndex \\ self maxEntries + 1.
		firstIndex = lastIndex ifTrue: [
			firstIndex _ firstIndex \\ self maxEntries + 1 ].
		entries at: lastIndex put: aString.
		
		"external file"
		otherString ifNotNil: [
			[
				stream _ FileStream fileNamed: self filename.
				stream
					setToEnd;
					nextPutAll: otherString;
					flush]
			ensure: [ stream close ]
		]
	]! !

!Transcript class methodsFor: 'private' stamp: 'jmv 3/13/2012 12:56'!
contents
	^String streamContents: [ :strm |
		self entriesDo: [ :e |
			strm nextPutAll: e; newLine ]]! !

!Transcript class methodsFor: 'private' stamp: 'jmv 2/18/2011 14:16'!
finishEntry
	| newEntry |
	newEntry _ unfinishedEntry contents.
	unfinishedEntry reset.
	self addEntry: newEntry.
	self display! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 3/14/2011 10:19'!
display
	showOnDisplay ifTrue: [
		self displayOn: Display.
		lastDisplayTime _ DateAndTime now ].
	self changed: #redraw	"So any morph in front of us is repaired when Morphic cycles"! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/15/2014 09:25'!
displayOn: aForm
	"
	Transcript displayOn: Display
	"
	| font count i string x y fh f bw r canvas |
	bw _ self borderWidth  .
	r _ innerRectangle outsetBy: bw + self padding.
	aForm border: r width: bw. 
	aForm fill: r fillColor: Color white.
	font _ AbstractFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.
	y _ innerRectangle top.
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ (lastIndex - count max: f) \\ self maxEntries + 1.
	canvas _ aForm getCanvas.
	canvas newClipRect: innerRectangle.
	[
		string _ entries at: i.	
		canvas drawString: string at: x@y font: font color: Color veryDarkGray.
		y _ y + fh.
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: Color veryDarkGray! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 2/18/2011 14:23'!
displayUnfinishedEntry
	showOnDisplay ifTrue: [
		(lastDisplayTime isNil or: [ (DateAndTime now - lastDisplayTime) totalSeconds > 1 ])
			ifTrue: [ ^self display ].
		self displayUnfinishedEntryOn: Display ]! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/15/2014 09:25'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas |
	font _ AbstractFont default.
	
	fh _ font height.
	count _ innerRectangle height // fh-1.
	x _ innerRectangle left.

	string _ unfinishedEntry contents.
	y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font height + innerRectangle top.
	canvas _ aForm getCanvas.
	canvas
		newClipRect: innerRectangle;
		drawString: string at: x@y font: font color: Color veryDarkGray! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 5/11/2011 08:44'!
padding
	^ 3! !


!Transcript class methodsFor: 'class initialization' stamp: 'jmv 6/14/2012 22:02'!
initialize
	"
	self initialize
	"
	showOnDisplay _ true.
	innerRectangle _ 20@20 extent: 300@500.
	logToFile _ false.
	entries _ Array new: self maxEntries.
	unfinishedEntry _ String new writeStream.
	accessSemaphore _ Semaphore forMutualExclusion.
	self clear! !


!Transcript class methodsFor: 'constants' stamp: 'jmv 2/18/2011 16:49'!
borderWidth
	^1! !

!Transcript class methodsFor: 'constants' stamp: 'jmv 2/18/2011 12:33'!
filename
	^'transcript.txt'! !

!Transcript class methodsFor: 'constants' stamp: 'jmv 2/18/2011 12:59'!
maxEntries
	^1000! !


!Transcript class methodsFor: 'iteration' stamp: 'jmv 4/14/2011 10:59'!
entriesDo: aBlock
	| f i |
	f _ firstIndex-1.
	firstIndex > lastIndex ifTrue: [ f _ f - self maxEntries ].
	i _ f \\ self maxEntries + 1.
	[
		aBlock value: (entries at: i).
		i = lastIndex
	] whileFalse: [ i _ i \\ self maxEntries + 1 ].

	aBlock value: unfinishedEntry contents.	! !


!classDefinition: #Utilities category: #'System-Support'!
Object subclass: #Utilities
	instanceVariableNames: ''
	classVariableNames: 'AuthorInitials AuthorName LastStats'
	poolDictionaries: ''
	category: 'System-Support'!
!Utilities commentStamp: '<historical>' prior: 0!
A repository for general and miscellaneous utilities; much of what is here are in effect global methods that don't naturally attach to anything else.  1/96 sw!


!Utilities methodsFor: 'look in class' stamp: 'sw 10/13/1998 13:14'!
seeClassSide
	"All the code for Utilitites is on the class side"! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Utilities class' category: #'System-Support'!
Utilities class
	instanceVariableNames: ''!

!Utilities class methodsFor: 'common requests' stamp: 'jmv 6/8/2013 14:10'!
saveDisplay
	"Save Display to a file"

	"SampledSound playSoundNamed: 'camera'."
	Display writeBMPfileNamed: 'CuisDisplay.bmp'! !

!Utilities class methodsFor: 'common requests' stamp: 'jmv 6/8/2013 14:11'!
saveScreenshot
	"Make a screenshot of the world and save it to a file"

	"SampledSound playSoundNamed: 'camera'."
	(self runningWorld imageForm: 32)  writeBMPfileNamed: 'CuisWorld.bmp'! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 5/12/2012 23:25'!
authorInitials
	"Answer the initials to be used to identify the current code author.  "

	[AuthorInitials isEmpty] whileTrue: [self setAuthor].
	^ AuthorInitials! !

!Utilities class methodsFor: 'identification' stamp: 'sw 7/6/1998 11:49'!
authorInitialsPerSe
	"Answer the currently-prevailing author initials, such as they, empty or not"

	^ AuthorInitials! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 5/12/2012 23:25'!
authorName
	[AuthorName isEmpty] whileTrue: [self setAuthor].
	^ AuthorName! !

!Utilities class methodsFor: 'identification' stamp: 'tk 4/10/98 06:25'!
changeStamp 
	"Answer a string to be pasted into source code to mark who changed it and when."
	^ self authorInitials , ' ' , Date today mmddyyyy, ' ',
		((String streamContents: [:s | Time now print24: true on: s]) copyFrom: 1 to: 5)! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 5/12/2012 23:27'!
clearAuthor
	AuthorInitials _ ''.
	AuthorName _ ''! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 4/17/2013 11:57'!
dateTimeSuffix
	"Answer a string which indicates the date and time, intended for use in building fileout filenames, etc."

	"Utilities dateTimeSuffix"
	^ Preferences twentyFourHourFileStamps
		ifFalse:
			[self monthDayTimeStringFrom: Time localSecondClock]
		ifTrue:
			[self monthDayTime24StringFrom: Time localSecondClock]! !

!Utilities class methodsFor: 'identification' stamp: 'di 6/13/97 13:52'!
fixStamp: changeStamp 
	| parts |
	parts _ changeStamp findTokens: ' '.
	(parts size > 0 and: [parts last first isLetter]) ifTrue:
		["Put initials first in all time stamps..."
		^ String streamContents:
				[:s | s nextPutAll: parts last.
				parts allButLast do: [:p | s space; nextPutAll: p]]].
	^ changeStamp! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 5/27/2014 13:01'!
methodsWithInitials: targetInitials
	"Based on a do-it contributed to the Squeak mailing list by Göran Hultgen:
- Browse methods whose initials (in the time-stamp, as logged to disk) match the given initials.
- Print out the complete time-stamp table to the Transcript.
- Answer a list of (initials -> count) associations.

CAUTION: It may take several minutes for this to complete."

	"Time millisecondsToRun: [Utilities methodsWithInitials: 'bf'] 2031"

	| initials timeStamp  allSubmitters |
	initials _ ''.
	timeStamp _ ''.
	allSubmitters _ Bag new.
	Smalltalk
		browseAllSelect:
			[:cm |
				timeStamp _ cm timeStamp.
				initials _ timeStamp isEmpty
					ifTrue:
						['']
					ifFalse:
						[timeStamp substrings first].
				initials _ initials isEmpty
					ifTrue:
						['<no time stamp>']
					ifFalse:
						[initials first isDigit
							ifTrue:
								['<date>']
							ifFalse:
								[initials]].
				allSubmitters add: initials.
				(initials = targetInitials)]
		name: ('Methods with initials ', targetInitials)
		autoSelect: nil.

	allSubmitters sortedCounts do: [:elem | Transcript newLine; show: elem asString].
	^ allSubmitters
! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 1/5/2014 20:40'!
monthDayTime24StringFrom: aSecondCount
	| aDate aTime |
	"From the date/time represented by aSecondCount, produce a string which indicates the date and time in the compact form
             ddMMMhhmm		where dd is a two-digit day-of-month, MMM is the alpha month abbreviation and hhmm is the time on a 24-hr clock.

          Utilities monthDayTime24StringFrom: Time primSecondsClock
	"

	aDate _ Date fromSeconds: aSecondCount.
	aTime _ Time fromSeconds: aSecondCount \\ Time secondsInDay.
	^String streamContents: [ :strm |
		aDate dayOfMonth printOn: strm integerDigits: 2 fractionDigits: 0.
		strm
			nextPutAll: (aDate monthName copyFrom: 1 to: 3);
			nextPutAll: aTime hhmm24 ]! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 5/18/2014 15:50'!
monthDayTimeStringFrom: aSecondCount
	| aDate aTime |
	"From the date/time represented by aSecondCount, produce a string which indicates the date and time in the form:
		ddMMMhhmmPP	  where:
							dd is a two-digit day-of-month,
							MMM is the alpha month abbreviation,
							hhmm is the time,
							PP is either am or pm

          Utilities monthDayTimeStringFrom: Time localSecondClock
	"

	aDate _ Date fromSeconds: aSecondCount.
	aTime _ Time fromSeconds: aSecondCount \\ Time secondsInDay.
	^ String streamContents: [ :strm |
		aDate dayOfMonth printOn: strm integerDigits: 2 fractionDigits: 0.
		strm nextPutAll: (aDate monthName copyFrom: 1 to: 3).
		aTime hour \\ 12 printOn: strm integerDigits: 2 fractionDigits: 0.
		aTime minute printOn: strm integerDigits: 2 fractionDigits: 0.
		strm nextPutAll: (aTime hour > 12 ifTrue: ['pm'] ifFalse: ['am'])
		]! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 9/24/2012 19:48'!
setAuthor
	"Put up a dialog allowing the user to specify the author's initials.
	Utilities setAuthor
	"
	| authorName |
	AuthorInitials _ (FillInTheBlankMorph
		request: 'Please type your initials: '
		initialAnswer: AuthorInitials) withBlanksTrimmed.
	authorName _ (Smalltalk knownInitialsAndNames
		detect: [ :pair |
			pair first = AuthorInitials ]
		ifNone: [
			AuthorName _ (FillInTheBlankMorph
				request: 'Please type your name:'
				initialAnswer: 'Your Name') withBlanksTrimmed.
			^ self ]) second withBlanksTrimmed.
	(self confirm: 'Are you ' , authorName , '?')
		ifTrue: [ AuthorName _ authorName ]
		ifFalse: [
			self inform: 'Please enter different initials, then'.
			self setAuthor ]! !


!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 3/2/2010 10:13'!
awaitMouseUpIn: box repeating: doBlock ifSucceed: succBlock
	"The mouse has gone down in box; track the mouse, inverting the box while it's within, and if, on mouse up, the cursor was still within the box, execute succBlock.  While waiting for the mouse to come up, repeatedly execute doBlock. 5/11/96 sw
	6/10/96 sw: call new method that adds extra feature"

	^ self awaitMouseUpIn: box whileMouseDownDo: doBlock whileMouseDownInsideDo: nil ifSucceed: succBlock! !

!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 1/14/2013 21:14'!
awaitMouseUpIn: box whileMouseDownDo: doBlock1 whileMouseDownInsideDo: doBlock2 ifSucceed: succBlock
	"The mouse has gone down in box; track the mouse, inverting the box while it's within, and if, on mouse up, the cursor was still within the box, execute succBlock.  While waiting for the mouse to come up, repeatedly execute doBlock1, and also, if the cursor is within the box, execute doBlock2.  6/10/96 sw
3/31/00 JMM added logic to stop multiple redraws"

	| p inside lightForm darkForm isLight |

	p _ Sensor mousePoint.
	inside _ box insetBy: 1.
	isLight _ true.
	lightForm _ Form fromDisplay: inside.
	darkForm _ lightForm copy reverse.
	[Sensor isAnyButtonPressed] whileTrue:
		[doBlock1 value.
		(box containsPoint: (p _ Sensor mousePoint))
			ifTrue: [doBlock2 value.
					isLight ifTrue: 
						[isLight _ false.
						darkForm displayAt: inside origin]]
			ifFalse: [isLight ifFalse:
						[isLight _ true.
						lightForm displayAt: inside origin]]].
	(box containsPoint: p)
		ifTrue: [lightForm displayAt: inside origin.
				^ succBlock value]
! !

!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 3/13/2012 13:00'!
decommissionTheAllCategory
	"Utilities decommissionTheAllCategory"
	"Moves all methods that are in a category named 'all' into the default 'as yet unclassified' category"
	| org aCategory methodCount classCount any |
	methodCount _ 0.
	classCount _ 0.
	Smalltalk allBehaviorsDo: [ :aClass |
		org _ aClass organization.
		any _ false.
		aClass selectorsDo: [ :aSelector |
			aCategory _ org categoryOfElement: aSelector.
			aCategory = #all ifTrue: [
				org
					classify: aSelector
					under: ClassOrganizer default
					suppressIfDefault: false.
				methodCount _ methodCount + 1.
				any _ true ]].
		any ifTrue: [ classCount _ classCount + 1 ].
		org removeEmptyCategories ].
	Transcript
		 newLine;
		 show: methodCount printString , ' methods in ' , classCount printString , ' classes moved
from "all" to "as yet unclassified"'.! !

!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 3/13/2012 13:00'!
garbageCollectAndReport
	"Do a garbage collection, and report results to the user."
	| cc reportString |
	reportString _ String streamContents: [ :aStream |
		aStream nextPutAll: Smalltalk bytesLeftString.
		Smalltalk
			at: #Command
			ifPresent: [ :cmdClass |
				(cc _ cmdClass instanceCount) > 0 ifTrue: [
					aStream
						 newLine;
						 nextPutAll: '(note: there are ' , cc printString , ' undo record(s) present in your
system; purging them may free up more space.)' ]]].
	self inform: reportString! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 4/25/2011 21:21'!
browseVersionsForClass: aClass selector: aSelector
	VersionsBrowserWindow
		browseVersionsOf: (aClass compiledMethodAt: aSelector)
		class: aClass
		meta: aClass isMeta
		category: (aClass organization categoryOfElement: aSelector)
		selector: aSelector! !

!Utilities class methodsFor: 'summer97 additions' stamp: 'sw 8/16/97 13:13'!
chooseFileWithSuffix: aSuffix
	"Utilities chooseFileWithSuffix: '.gif'"
	| aList aName |
	aList _ FileDirectory default fileNamesMatching: '*', aSuffix.
	aList size > 0
		ifTrue:
			[aName _ (SelectionMenu selections: aList) startUpWithCaption: 'Choose a file'.
			^ aName]
		ifFalse:
			[self inform: 'Sorry, there are no files
whose names end with "', aSuffix, '".'.
			^ nil]! !

!Utilities class methodsFor: 'summer97 additions' stamp: 'sw 10/5/1998 17:58'!
chooseFileWithSuffixFromList: aSuffixList withCaption: aCaption
	"Pop up a list of all files in the default directory which have a suffix in the list.  Return #none if there are none; return nil if the user backs out of the menu without making a choice."
	"Utilities chooseFileWithSuffixFromList: #('.gif' '.jpg')"
	| aList aName |
	aList _ OrderedCollection new.
	aSuffixList do:
		[:aSuffix | aList addAll: (FileDirectory default fileNamesMatching: '*', aSuffix)].
	^ aList size > 0
		ifTrue:
			[aName _ (SelectionMenu selections: aList) startUpWithCaption: aCaption.
			aName]
		ifFalse:
			[#none]! !

!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 8/28/2013 10:28'!
classFromPattern: pattern withCaption: aCaption
	"If there is a class whose name exactly given by pattern, return it.
	If there is only one class in the system whose name matches pattern, return it.
	Otherwise, put up a menu offering the names of all classes that match pattern, and return the class chosen, else nil if nothing chosen.
	This method ignores tab, space, & cr characters in the pattern"

	| toMatch potentialClassNames classNames exactMatch index |
	(toMatch _  pattern copyWithoutAll:
			{Character space.  Character cr.  Character tab})
		isEmpty ifTrue: [^ nil].
	Symbol hasInterned: toMatch ifTrue:
		[:patternSymbol | Smalltalk at: patternSymbol ifPresent:
			[:maybeClass | (maybeClass isKindOf: Class) ifTrue: [^ maybeClass]]].

	toMatch _ (toMatch copyWithout: $.) asLowercase.
	potentialClassNames _ Smalltalk classNames asOrderedCollection.
	classNames _ pattern last = $. 
		ifTrue: [potentialClassNames select:
					[:nm |  nm asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: 
					[:n | n includesSubstring: toMatch caseSensitive: false]].
	classNames isEmpty ifTrue: [^ nil].
	exactMatch _ classNames detect: [:each | each asLowercase = toMatch] ifNone: nil.

	index _ classNames size = 1
		ifTrue:	[1]
		ifFalse:	[exactMatch
			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpWithCaption: aCaption]
			ifNotNil: [classNames addFirst: exactMatch.
				(PopUpMenu labelArray: classNames lines: #(1)) startUpWithCaption: aCaption]].
	index = 0 ifTrue: [^ nil].
	^ Smalltalk at: (classNames at: index) asSymbol

"
	Utilities classFromPattern: 'CharRecog'
	Utilities classFromPattern: 'rRecog'
	Utilities classFromPattern: 'znak'
	Utilities classFromPattern: 'orph'
"
! !

!Utilities class methodsFor: 'summer97 additions' stamp: 'sw 3/20/2001 14:04'!
hierarchyOfClassesSurrounding: aClass
	"Answer a list of classes in the hierarchy both above and below the given class "
	| list aClassNonMeta isMeta theClassOrMeta |

	aClass ifNil: [^ OrderedCollection new].
	aClass ifNil: [^ self].
	aClassNonMeta _ aClass theNonMetaClass.
	isMeta _ aClassNonMeta ~~ aClass.
	list _ OrderedCollection new.
	aClass allSuperclasses reverseDo:
		[:cl | list addLast: cl].
	aClassNonMeta allSubclassesWithLevelDo:
		[:cl :level |
		theClassOrMeta _ isMeta ifTrue: [cl class] ifFalse: [cl].
		list addLast: theClassOrMeta]
	 	startingLevel: 0.
	^ list

"Utilities hierarchyOfClassesSurrounding: StringHolder "! !

!Utilities class methodsFor: 'summer97 additions' stamp: 'sw 3/20/2001 14:06'!
hierarchyOfImplementorsOf: aSelector forClass: aClass
	"Answer a list of classes in the hierarchy both above and below the given class which implement the given selector."

	^ (self hierarchyOfClassesSurrounding: aClass) select:
		[:cl | cl includesSelector: aSelector]

"Utilities hierarchyOfImplementorsOf: #contents forClass: StringHolder"! !

!Utilities class methodsFor: 'summer97 additions' stamp: 'sw 4/30/1998 12:20'!
inviolateInstanceVariableNames
	"Answer a list of instance variable names not to be used.  (Place holder for real list)"
	^ #('thisContext' 'self')! !

!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 12/7/2009 17:42'!
isLegalInstVarName: aString
	"Answer whether aString is a legal instance variable name."

	^ ((Smalltalk actualScannerClass isLiteralSymbol: aString) and: [(aString includes: $:) not]) and:
		[(self inviolateInstanceVariableNames includes:  aString) not]! !

!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 4/14/2010 13:42'!
wellFormedInstanceVariableNameFrom: aString
	"Answer a legal instance variable name, derived from aString"

	| cleansedString |
	cleansedString _ aString select: [:ch | ch isGenerallyValidInIdentifiers].
	(cleansedString isEmpty or: [cleansedString first isGenerallyValidStartOfInIdentifiers not])
		ifTrue: [cleansedString _ 'a', cleansedString]
		ifFalse:	[cleansedString _ cleansedString withFirstCharacterDownshifted].

	[self isLegalInstVarName: cleansedString] whileFalse:
		[cleansedString _ cleansedString, 'x'].
	^ cleansedString

"Utilities wellFormedInstanceVariableNameFrom:  '234 xx\ Uml /ler42342380-4'"! !


!Utilities class methodsFor: 'support windows' stamp: 'gsa 9/21/2013 12:27'!
codeManagementInCuisContents
	^ self class firstCommentAt: #codeManagementInCuisContents

"
(from http://jvuletich.org/Cuis/CodeManagementInCuis4.html )

Cuis 4 includes new tools and new suggested procedures for managing Smalltalk code. Code that is not part of the Cuis Core image itself, like applications, frameworks and libraries, should be stored in Packages. New code that is meant as patches, fixes or additions; that could eventually become part of Cuis itself, is not part of any Package, and is therefore automatically stored in Change Sets.


Packages
------------

Let's start with Packages. The Package implementation in Cuis 4 is based on PackageInfo, the standard way to specify packages in Squeak and its derivatives, and used, for example, by Monticello. It uses Package names, to specify prefixes for Class and Method categories. Classes and Methods whose categories match a Package's prefixes belong in that Package. More details about how PackageInfo decides what code belongs in a package are available at http://wiki.squeak.org/squeak/3329 .

To install packages (.pck.st files) in Cuis, use the FileList, navigate to the appropriate directory (on disk, or in a GitHub repository, etc), select the package file and click on [Install Package].

Cuis includes a tool to manage installed Packages. It is at World / Open / Installed Packages. To create a new package (instead of installing an existing one from a file), click on [Create Package] This creates a new package, and associates with it all the existing code in the image that matches the package name.

The operations available on installed or newly created packages are:

[Save] Saves a package on the file system. Overwrites any existing version. It is good to save package from time to time, to reduce the risk of losing code.

[Delete] Removes the Package instance from the image. Does not remove any code. This means, effectively, to merge back the code into Cuis.

[Browse unsaved Changes] This opens a ChangeSorter on the ChangeSet that captures all the changes done to the Package since it was last saved. Therefore it shows the work done on the package that would be lost if the package is not saved.

[Browse Package Code] This opens a Class Browser that only shows the code that belongs in the package. This is useful for working on a package, or studying it.

The tool shows, for each Package, the name, whether it is dirty (has unsaved changes) and the file it was installed from / saved to.

Handling Packages like this, Cuis behaves as a sort of document editor (like, for example a regular text editor) whose documents are Package files (.pck.st). Cuis doesn't handle Package versions, ancestries, etc. If versioning of Packages is desired, the best is to use a versioning file repository, such as Git or Mercurial. The recommendation is to use a GitHub repository with a name beginning with 'Cuis-Smalltalk', so it will be easy for anybody to find it. Cuis Package files (.pck.st) are uncompressed, use Lf (ASCII 10) as newLine, and are encoded in ISO 8859-15. This means that are Git friendly, and Git/GitHub can diff and merge them, and browse them with syntax highlighting.

This is not unlike using Git or GitHub with a more conventional development environment such as Eclipse or a text editor. Like Cuis 4, these tools don't do version handling themselves, they just load and save files; and let Git do its magic.


Changes to the Cuis base image
--------------------------------------

The way ChangeSets are created and managed in Cuis 4 is very different from previous versions of Cuis (and Squeak & derivatives). This was done to make ChangeSets a good way to manage changes to the base Cuis Core image, while keeping code in Packages out of the way, so they don't get mixed together.

What is not in a Package belongs (at least temporarily) in the Cuis Core image. Such code is automatically captured in a ChangeSet. The ChangeSet for Core changes is created automatically and named like '1243-CuisCore-JuanVuletich-2012Apr03-22h50m'. The number at the beginning is the next number for the Cuis update stream, and is provided only as a suggestion. The 'CuisCore' part is to reveal that the code belongs in the base image and not in some package. Then we have author name and date / time of creation. These ChangeSets are created automatically. There is no longer a way to manually create them, or make them 'current' or 'active'. It is best not to rename them. These ChangeSets will not capture any code that belongs in a Package.

Opening a Change Sorter will show the CuisCore change set. This is useful, for example, to check that no code that was intended for a Package ends here by mistake (because of the wrong class or method category). But it is also useful when doing changes to the base system. Now, we can do changes both to the base system and to a number of packages, all in the same session, without having to be careful about selecting the proper change set before saving a method: The code is automatically added to the proper Package or ChangeSet, simply following the class or method category. Gone are the days of messed up change sets and lost code!!

When the changes to the base system are complete, it is a good time to review the CuisCore change set and, maybe remove from it changes that we don't want to keep (for example, experiments, halts, etc). Then, just do right click / File out and remove. This saves the ChangeSet on disk. It also removes it from the ChangeSorter (but it doesn't remove any code). This is good, because the next changes done will end in a new CuisCore change set, and there's no risk of having undesired changes in the old one. As changes to the base image progress, and several CuisCore change sets are saved to disk, these numbered files are created in sequence. They will be ready to be loaded back in proper order in a fresh Cuis image, or to be sent to Cuis maintainers for integration in the update stream and in next releases of Cuis.


Loading ChangeSet files into Cuis
---------------------------------------

There are two ways to load ChangeSet files (.cs): [FileIn] and [Install].

[FileIn] loads the code without creating a new ChangeSet object. This means that changes that belong in the base image (and not in a package) will be added to the current ChangeSet for Cuis core changes, as if they were done by the user. This is appropriate when we are combining code from more than one source into a single ChangeSet. Any change that belongs in an installed package will be added to it, and the package will appear as dirty.

[Install] loads the code into a separate ChangeSet object (viewable in the ChangeSorter tool). This is appropriate for loading Cuis updates, or other code that we are not authoring, as it doesn't add new items (class or method definitions) to the current ChangeSet for our changes to Cuis. Usually any ChangeSets should be installed before doing changes to the image. The reason is that an installed ChangeSet could overwrite changes done by you, or packages you have installed. If this is the case, the affected packages would appear as dirty, and your change set would include any installed changes (that don't belong in a package). Be careful when saving packages or change sets if this was the case!!
"

"
Utilities codeManagementInCuisContents edit
"! !

!Utilities class methodsFor: 'support windows' stamp: 'gsa 9/21/2013 13:30'!
cuisAndGitHubContents
	^ self class firstCommentAt: #cuisAndGitHubContents

"
(from http://jvuletich.org/Cuis/CuisAndGitHub.html )

Cuis 4 includes new tools and new suggested procedures for managing Smalltalk code. Central to this is the management of Packages and Package Files (.pck.st). But Cuis doesn't do version control. Instead, we suggest using external VCS tools.  In particular, we're using GitHub, and the first project we're hosting there is StyledTextEditor.

The guiding principle is do not duplicate concepts and behavior. As we're using an external tool (Git) for version control, then use it as it meant to be used. Most people use Git for version control and a file based IDE such as Eclipse for development. Such IDEs don't do version control themselves. It is done by Git. Do the same: do not include package version control in Cuis. This is a departure from the Monticello /Git integration (smallsource and MonticelloFileTree) by Otto Behrens, Dale Henrichs, etc.

We use GitHub to host, version, diff and merge external packages (.pck.st files), i.e. code that is maintained independently and outside Cuis.

Package files need to be simple text files. Cuis encoding for latin alphabet (ISO 8859-15) is handled without problems by GitHub. Cuis uses the LF (ascii code 10) newline convention, as preferred in GitHub. This allows Git/GitHub to diff versions, and merge branches.

Each GitHub repository has one set of users and permissions. Each GitHub repository has one state (Git commits repositories, not individual files). Branch and merges are done on the whole repository and not on individual files. Therefore, we need a separate GitHub repository for each project, i.e., for each package or set of closely related packages that are always loaded and maintained together as a whole.
Development process for External Packages

This is the suggested procedure for developing external packages. Usually do this every day.

    Start with a standard (i.e. fresh) Cuis image. Never save the image.

    Set up Git repositories for external packages (if not already done)

    Install packages from Git repositories.

    Develop. Modify and/or create packages.

    Save own packages (to Git repositories).

    Git add / commit / push as appropriate.

    Fileout changes that are not part of any package. These are automatically captured in numbered changesets, separated from changes to packages.

    Exit the image. Usually without saving.

"

"
Utilities cuisAndGitHubContents edit
"! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 1/2/2013 15:26'!
openCodeManagementInCuis
	"
	Utilities openCodeManagementInCuis
	"

	self codeManagementInCuisContents editLabel: 'Code Management in Cuis'! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 1/2/2013 15:26'!
openCuisAndGitHub
	"
	Utilities openCuisAndGitHub
	"

	self cuisAndGitHubContents editLabel: 'Using Git and GitHub to host and manage Cuis code'! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 1/2/2013 15:11'!
openUsefulExpressions
	"Open up a throwaway workspace with useful expressions in it.  1/22/96 sw"
	"
	Utilities openUsefulExpressions
	"

	| w |
	w _ Workspace new.
	w
		contents: self usefulExpressionsContents;
		openLabel: 'Useful Expressions'! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 12/31/2013 15:53'!
usefulExpressionsContents
	^ self class firstCommentAt: #usefulExpressionsContents

	"'Querying the system'.
Smalltalk aboutThisSystem.
SystemVersion current.
SystemDictionary copyright.
SystemOrganization categories.
SystemOrganization categoryOfElement: #Morph.
Editor organization categoryOfElement: #clearSelection.
Smalltalk allUnSentMessages.
Smalltalk browseAllUnSentMessages.


'find selectors
Remember you can also do World / Open / Method Names'.
Symbol selectorsContaining: 'rsCon'.
'Cuis uses iso885915 internally and UTF8 externally'.
Symbol selectorsContaining: 'iso8859'. 
Symbol selectorsContaining: 'utf8'. 


'Example of loading a package with requirements on other packages'
Feature require: 'SignalProcessing'.


'To create a *.pck.st file for each category with tests and  then to remove these categories'.
(SystemOrganization categoriesMatching: '*Test*')
	do: [ :cat | (CodePackage named: cat
				createIfAbsent: true
				registerIfNew: false) save.
			SystemOrganization removeSystemCategory: cat.
	].


'List of CodePackages which are not saved and save them in one go'.
'Save dirty packages:' print.
CodePackage installedPackages do: [ :each |
	each hasUnsavedChanges ifTrue: [
		each name print.
		each save ]].


'Globals'.
Cursor wait showWhile: [Sensor waitButton].
Smalltalk  inspectGlobals.


'Source code management'.
ChangeList browseRecentLog.


'Space and object count statistics'.
Smalltalk bytesLeft asStringWithCommas.
Symbol instanceCount.


'Anaylize memory usage by class'.
(String streamContents: [ :strm | SpaceTally new printSpaceAnalysis: 1 on: strm ]) edit.


'Remove a lot of stuff and reduce the image'.
Smalltalk reduceCuis.


'Performance measurement'.
Time millisecondsToRun: [Smalltalk allCallsOn: #asOop].
MessageTally spyOn: [Smalltalk allCallsOn: #asOop].


'Opening a Text editor'.
'Something' editLabel: 'Title'.


'Opening a workspace'.
Workspace new
		contents: '';
		openLabel: 'Workspace'.

'Morphic stuff'
LayoutMorph example1.				'Resize it'
LayoutMorph example11.			'Resize it. Play with separators.'
LayoutMorph example2.				'Resize it'
LayoutMorph example3.				'Resize it'
LayoutMorph example13.			'Resize it. Play with separators.'
LayoutMorph example6.				'Resize it'
LayoutMorph launcherExample.		'Have a Transcript open to use this'.

'Code authors - Potentially slow!!'.
Smalltalk knownInitialsAndNames do: [ :each | each print ].
Smalltalk allContributors do: [ :each | each print ].
Smalltalk contributionsOf: 'KenD'"! !


!Utilities class methodsFor: 'user interface' stamp: 'jmv 4/18/2011 14:41'!
informUser: aString during: aBlock 
	"Display a message above (or below if insufficient room) the cursor during execution of the given block."

	"Utilities informUser: 'Just a sec!!' during: [(Delay forSeconds: 1) wait]"

	(MVCMenuMorph from: (SelectionMenu labels: '') title: aString) 
				displayAt: Sensor mousePoint
				during: aBlock! !

!Utilities class methodsFor: 'user interface' stamp: 'jmv 4/18/2011 14:41'!
informUserDuring: aBlock 
	"Display a message above (or below if insufficient room) the cursor during execution of the given block."

	"Utilities informUserDuring:[:bar|
		#(one two three) do:[:info|
			bar value: info.
			(Delay forSeconds: 1) wait]]"

	(MVCMenuMorph from: (SelectionMenu labels: '') title: '						') 
				informUserAt: Sensor mousePoint
				during: aBlock! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 10/21/2012 20:28'!
reportCPUandRAM
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	Utilities reportCPUandRAM
	"	

	| stream tally |
	
	"VM statistics (Memory use and GC, mainly)"
	stream _ FileStream forceNewFileNamed: 'MemoryStats.txt'.
	[ stream nextPutAll: Utilities vmStatisticsReportString ] 
		ensure: [ stream close ].
	
	"Process list"
	stream _ FileStream forceNewFileNamed: 'ProcessList.txt'.
	[
		ProcessBrowser new processNameList 
			do: [ :each | 
				stream nextPutAll: each; newLine ]
	] ensure: [ stream close ].

"Fork all these, so they run in sequence, as the system is back running"
[
	
	"Process taking most CPU"
	stream _ FileStream forceNewFileNamed: 'ThePig.txt'.
	ProcessBrowser dumpPigStackOn: stream andClose: true.
	
	"Tally of all processes"
	stream _ FileStream forceNewFileNamed: 'FullTally.txt'.
	[
		tally _ MessageTally new.
		tally reportOtherProcesses: true.	"actually irrelevant"
		tally spyAllEvery: 1 on: [ (Delay forMilliseconds: 1000) wait ].
		tally report: stream ] ensure: [ stream close ].

	"Memory Analysis"
	stream _ FileStream forceNewFileNamed: 'MemoryAnalysis.txt'.
	[ SpaceTally new printSpaceAnalysis: 1 on: stream ]
		ensure: [ stream close ]

] forkNamed: 'CPU usage analysis'! !

!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 7/2/2010 11:10'!
textMarkerForShortReport

	^  'Since last view	'! !

!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 5/18/2014 16:32'!
vmStatisticsReportString
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	| params oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount fullGCs2 fullGCTime2 incrGCs2 incrGCTime2 tenureCount2 str |
	params := Smalltalk getVMParameters.
	oldSpaceEnd			:= params at: 1.
	youngSpaceEnd		:= params at: 2.
	memoryEnd			:= params at: 3.
	fullGCs				:= params at: 7.
	fullGCTime			:= params at: 8.
	incrGCs				:= params at: 9.
	incrGCTime			:= params at: 10.
	tenureCount			:= params at: 11.

	str := WriteStream on: (String new: 1000).

	str	nextPutAll: 'memory			';
		nextPutAll: memoryEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd printStringWithCommas; nextPutAll: ' bytes ('.
	oldSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) printStringWithCommas; nextPutAll: ' bytes ('.
	youngSpaceEnd - oldSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd printStringWithCommas; nextPutAll: ' bytes ('.
	youngSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) printStringWithCommas; nextPutAll: ' bytes ('.
	memoryEnd - youngSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.

	str	nextPutAll: 'GCs				';
		nextPutAll: (fullGCs + incrGCs) printStringWithCommas.
	str newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime printStringWithCommas; nextPutAll: 'ms'.
	fullGCs = 0 ifFalse: [
		str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime printStringWithCommas; nextPutAll: 'ms';
		nextPutAll: ', avg '; print: ((incrGCTime / incrGCs) rounded); nextPutAll: 'ms'; newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse: [
		str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.

LastStats ifNil: [LastStats := Array new: 6]
ifNotNil: [
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).

	str	nextPutAll: self textMarkerForShortReport ;
		nextPutAll: (fullGCs2 + incrGCs2) printStringWithCommas.
	str newLine.
	str	nextPutAll: '	full			';
		print: fullGCs2; nextPutAll: ' totalling '; nextPutAll: fullGCTime2 printStringWithCommas; nextPutAll: 'ms'.
	fullGCs2 = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: ((fullGCTime2 / fullGCs2) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs2; nextPutAll: ' totalling '; nextPutAll: incrGCTime2 printStringWithCommas; nextPutAll: 'ms';
		nextPutAll: ', avg '.
	incrGCs2 > 0 ifTrue: [
		 str print: ((incrGCTime2 / incrGCs2) rounded); nextPutAll: 'ms'
	].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount2 printStringWithCommas.
	tenureCount2 = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs2 / tenureCount2) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
].
	LastStats at: 2 put: fullGCs.
	LastStats at: 3 put: fullGCTime.
	LastStats at: 4 put: incrGCs.
	LastStats at: 5 put: incrGCTime.
	LastStats at: 6 put: tenureCount.

	^ str contents
! !

!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 3/13/2012 22:57'!
vmStatisticsShortString
	"Convenience item for access to recent statistics only"
	"
	(TextModel new contents: Utilities vmStatisticsShortString) openLabel: 'VM Recent Statistics'
	"

	^ (ReadStream on: self vmStatisticsReportString) upToAll: 'Since'; crLfNextLine; upToEnd
! !


!Utilities class methodsFor: 'tailoring system' stamp: 'jmv 3/2/2010 17:21'!
stripMethods: tripletList messageCode: messageString
	"Used to 'cap' methods that need to be protected for proprietary reasons, etc.; call this with a list of triplets of symbols of the form  (<class name>  <#instance or #class> <selector name>), and with a string to be produced as part of the error msg if any of the methods affected is reached"

	| aClass sel keywords codeString |
	tripletList do:
		[:triplet |  
			(aClass _ (Smalltalk at: triplet first ifAbsent: nil)) ifNotNil: [
				triplet second == #class ifTrue: [
					aClass _ aClass class].
				sel _ triplet third.
				keywords _ sel keywords.
				(keywords size = 1 and: [keywords first asSymbol isKeyword not])
					ifTrue: [
						codeString _ keywords first asString]
					ifFalse: [
						codeString _ ''.
						keywords withIndexDo:
							[:kwd :index |
								codeString _ codeString, ' ', (keywords at: index), ' ',
									'arg', index printString]].
				codeString _ codeString, '
	self codeStrippedOut: ', (messageString surroundedBySingleQuotes).

				aClass compile: codeString classified: 'stripped']]! !


!Utilities class methodsFor: 'closure support' stamp: 'jmv 3/13/2012 12:59'!
compileUsingClosures	"Utilities compileUsingClosures"
	"Recompile the system and do some minimal clean-ups"
	| classes compilationErrors |
	Preferences setPreference: #allowBlockArgumentAssignment toValue: false.
	compilationErrors := Set new.
	classes := Smalltalk allClasses reject: [:c| c name == #GeniePlugin].

	'Recompiling The System' displayProgressAt: Sensor mousePoint
		from: 0 to: classes size during:[:bar |
			classes withIndexDo:[:c :i|
				bar value: i.
				{ c. c class } do:[:b|
					"Transcript cr; print: b; endEntry."
					b selectors "asArray sort" do: [ :s | 
						"Transcript cr; show: b asString, '>>', s."
						[b recompile: s from: b] on: Error do:[:ex|
							Transcript
								newLine; nextPutAll: 'COMPILATION ERROR: ';
								print: b; nextPutAll: '>>'; nextPutAll: s.
							compilationErrors add: (MethodReference class: b selector: s)]]]]].

	compilationErrors notEmpty ifTrue:[
		Smalltalk
			browseMessageList: compilationErrors asArray sort
			name: 'Compilation Errors' ]! !

!Utilities class methodsFor: 'closure support' stamp: 'jmv 3/24/2010 21:57'!
initializeClosures	"Utilities initializeClosures"
	"Eliminate the prototype BlockContext from the specialObjectsArray.  The VM doesn't use it. This paves the way for removing BlockCOntext altogether and merging ContextPart and MethodContext into e.g. Context."
	(Smalltalk specialObjectsArray at: 38) class name == #BlockContext 
		ifTrue:[Smalltalk specialObjectsArray at: 38 put: nil].
	"Remove unused class vars from CompiledMethod since we can't redefine its class definition directly. Add the new BlockClosure to the specialObjectsArray"
	(#(	BlockNodeCache MethodProperties SpecialConstants) 
			intersection: CompiledMethod classPool keys asSet) 
				do:[:classVarName| CompiledMethod removeClassVarName: classVarName].
	Smalltalk recreateSpecialObjectsArray.
	"Recompile methods in ContextPart, superclasses and subclasses that access inst vars"
	ContextPart withAllSuperclasses, ContextPart allSubclasses asArray do:[:class|
		class instSize > 0 ifTrue:[
			class allInstVarNames do:[:ivn|
				(class whichSelectorsAccess: ivn) do:[:sel| class recompile: sel]]]]! !

!Utilities class methodsFor: 'closure support' stamp: 'jmv 10/23/2012 18:03'!
postRecompileCleanup	"Utilities postRecompileCleanup"
	"Cleanup after loading closure bootstrap"
	"Before doing this, please start a new UI process (for example, by hitting alt-period and closing the debugger)."
	| unboundMethods contexts |
	self runningWorld ifNotNil: [ :w | w removeAllKnownFailing ].
	ProcessorScheduler startUp.
	WeakArray restartFinalizationProcess.
	MethodChangeRecord allInstancesDo:[:x| x noteNewMethod: nil].
	Smalltalk cleanOutUndeclared.
	Delay startTimerEventLoop.
	EventSensor install.
	WorldState allInstancesDo:[:ws| ws convertAlarms; convertStepList].
	Workspace allInstancesDo:[:ws| ws initializeBindings].
	Smalltalk garbageCollect.
	Smalltalk
		at: #DebuggerMethodMap
		ifPresent: [ :dmm | dmm voidMapCache ].
	Smalltalk garbageCollect.
	unboundMethods _ CompiledMethod unboundMethods.
	unboundMethods notEmpty ifTrue: [
		unboundMethods inspectWithLabel: 'Unbound Methods'].
	Smalltalk at: #BlockContext ifPresent: [ :bc |
		contexts _ bc allInstances.
		contexts ifNotEmpty: [
			contexts inspect. 
			self inform: 'There are left-over BlockContexts'.
			^self ]].
	unboundMethods isEmpty ifTrue:[
		self inform:'Congratulations - The bootstrap is now complete.'.
	]! !

SystemDictionary initialize!
ExternalSemaphoreTable initialize!
Preferences initialize!
RealEstateAgent initialize!
Transcript initialize!