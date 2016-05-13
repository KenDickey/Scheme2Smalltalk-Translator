!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedRectMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp popUpOwner activeSubMenu titleMorph oldKeyboardFocus'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!MenuMorph commentStamp: '<historical>' prior: 0!
Instance variables:
	defaultTarget 		<Object>				The default target for creating menu items
	selectedItem		<MenuItemMorph> 	The currently selected item in the receiver
	stayUp 			<Boolean>				True if the receiver should stay up after clicks
	popUpOwner 		<MenuItemMorph>		The menu item that automatically invoked the receiver, if any.
	activeSubMenu 	<MenuMorph>			The currently active submenu.!


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 9/8/2012 20:26'!
addBlankIconsIfNecessary: anIcon
	"If any of my items have an icon, ensure that all do by using anIcon for those that don't"

	| withIcons withoutIcons |
	withIcons _ Set new.
	withoutIcons _ Set new.
	self items do: [ :item |
		item hasIcon | item hasMarker
			ifTrue: [ withIcons add: item ]
			ifFalse: [ withoutIcons add: item ].
		item hasSubMenu ifTrue: [ item subMenu addBlankIconsIfNecessary: anIcon ]].
	(withIcons isEmpty or: [ withoutIcons isEmpty ]) ifTrue: [ ^self ].
	withoutIcons do: [ :item | item icon: anIcon ].! !

!MenuMorph methodsFor: 'accessing' stamp: 'ar 9/18/2000 13:19'!
defaultTarget
	^defaultTarget! !

!MenuMorph methodsFor: 'accessing' stamp: 'jmv 9/2/2013 10:37'!
items

	^ submorphs select: [:m | m is: #MenuItemMorph]
! !

!MenuMorph methodsFor: 'accessing' stamp: 'jmv 9/2/2013 10:37'!
lastItem
	^ submorphs reverse
		detect: [ :m | m is: #MenuItemMorph ]
		ifNone: [ submorphs last ]! !

!MenuMorph methodsFor: 'accessing' stamp: 'jmv 4/28/2012 11:34'!
popUpOwner
	^popUpOwner! !

!MenuMorph methodsFor: 'accessing' stamp: 'jmv 3/1/2011 12:01'!
stayUp

	^ self stayUp: true
! !

!MenuMorph methodsFor: 'accessing' stamp: 'nk 3/31/2002 15:13'!
stayUp: aBoolean

	stayUp _ aBoolean.
	aBoolean ifTrue: [ self removeStayUpBox ].! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/19/2011 16:28'!
add: aString action: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	self add: aString
		target: defaultTarget
		selector: aSymbol
		argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 1/5/2013 22:46'!
add: aString action: aSymbol enabled: aBoolean
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	(self add: aString
		target: defaultTarget
		selector: aSymbol
		argumentList: #())
			isEnabled: aBoolean! !

!MenuMorph methodsFor: 'construction' stamp: 'sw 5/1/1998 00:48'!
add: aString selector: aSymbol argument: arg

	self add: aString
		target: defaultTarget
		selector: aSymbol
		argumentList: (Array with: arg)
! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 11/4/97 07:46'!
add: aString subMenu: aMenuMorph
	"Append the given submenu with the given label."

	| item |
	item _ MenuItemMorph new.
	item contents: aString;
		subMenu: aMenuMorph.
	self addMorphBack: item.
! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/19/2011 16:29'!
add: aString target: aTarget action: aSymbol
	self add: aString
		target: aTarget
		selector: aSymbol
		argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/19/2011 16:29'!
add: aString target: anObject selector: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	self add: aString
		target: anObject
		selector: aSymbol
		argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 11/4/97 07:46'!
add: aString target: target selector: aSymbol argument: arg
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given argument."

	self add: aString
		target: target
		selector: aSymbol
		argumentList: (Array with: arg)
! !

!MenuMorph methodsFor: 'construction' stamp: 'sw 11/6/2000 13:44'!
add: aString target: target selector: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  Answer the appended menu item."

	| item |
	item _ MenuItemMorph new
		contents: aString;
		target: target;
		selector: aSymbol;
		arguments: argList asArray.
	self addMorphBack: item.
	^ item
! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 9/2/2013 10:46'!
addLine
	"Append a divider line to this menu. Suppress duplicate lines."

	submorphs isEmpty ifTrue: [^ self].
	(self lastSubmorph class == MenuLineMorph)
		ifFalse: [self addMorphBack: MenuLineMorph new].
! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/19/2011 16:33'!
addList: aList
	"Add the given items to this menu, where each item is a pair (<string> <actionSelector>)..  If an element of the list is simply the symobl $-, add a line to the receiver.  The optional third element of each entry, if present, provides balloon help."

	| target |
	aList do: [:tuple |
		tuple == #-
			ifTrue: [ self addLine ]
			ifFalse: [
				target _ tuple size > 3
					ifTrue: [ defaultTarget perform: tuple fourth ]
					ifFalse: [ defaultTarget ].
				self add: tuple first target: target action: tuple second.
				tuple size > 2 ifTrue: [
					self balloonTextForLastItem: tuple third]]]! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/19/2011 11:02'!
addServices: services for: served extraLines: linesArray

	services withIndexDo: [:service :i |
		service addServiceFor: served toMenu: self.
		submorphs last setBalloonText: service description.
		(linesArray includes: i) | service useLineAfter 
			ifTrue: [self addLine]].
! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 6/6/2014 11:58'!
addStayUpIcons
	| closeBox pinBox w |
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	closeBox _ PluggableButtonMorph model: self action: #delete.
	closeBox icon: Theme current closeIcon.
	pinBox _ PluggableButtonMorph model: self action: #stayUp.
	pinBox icon: Theme current pushPinIcon.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: 19);
			color: Color transparent;
			addMorph: closeBox fixedWidth: 20;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: pinBox fixedWidth: 20).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 6/8/2014 20:32'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w |
	
	titleMorph _ RectangleLikeMorph new.
	titleMorph color: Theme current menuTitleBar.
	pp _ 8@2.
	aString asString linesDo: [ :line |
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 16) @ (pp y).
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/19/2011 16:29'!
addUpdating: aWordingSelector action: aSymbol

	self addUpdating: aWordingSelector target: defaultTarget selector: aSymbol argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/19/2011 16:29'!
addUpdating: aWordingSelector target: aTarget action: aSymbol

	self addUpdating: aWordingSelector target: aTarget selector: aSymbol argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'sw 11/6/2000 13:43'!
addUpdating: wordingSelector target: target selector: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  In this variant, the wording of the menu item is obtained by sending the wordingSelector to the target,  Answer the item added."

	| item |
	item _ UpdatingMenuItemMorph new
		target: target;
		selector: aSymbol;
		wordingProvider: target wordingSelector: wordingSelector;
		arguments: argList asArray.
	self addMorphBack: item.
	^ item
! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/19/2011 16:29'!
addWithLabel: aLabel enablement: anEnablementSelector action: aSymbol

	self addWithLabel: aLabel enablementSelector: anEnablementSelector target: defaultTarget selector: aSymbol argumentList: #()! !

!MenuMorph methodsFor: 'construction' stamp: 'sw 8/28/2000 18:01'!
addWithLabel: aLabel enablementSelector: enablementSelector target: target selector: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  In this variant, the wording of the menu item is constant, and the optional enablementSelector determines whether or not the item should be enabled."

	| item |
	item _ UpdatingMenuItemMorph new
		target: target;
		selector: aSymbol;
		contents: aLabel;
		wordingProvider: target wordingSelector: nil;
		enablementSelector: enablementSelector;
		arguments: argList asArray.
	self addMorphBack: item.
! !

!MenuMorph methodsFor: 'construction' stamp: 'sw 11/5/1998 21:13'!
balloonTextForLastItem: balloonText
	submorphs last setBalloonText: balloonText! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 11/4/97 07:46'!
defaultTarget: anObject
	"Set the default target for adding menu items."

	defaultTarget _ anObject.
! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 3/13/2012 22:16'!
labels: labelList lines: linesArray selections: selectionsArray
	"This method allows the receiver to accept old-style SelectionMenu creation messages. It should be used only for backward compatibility during the MVC-to-Morphic transition. New code should be written using the other menu construction protocol such as addList:."
	"Labels can be either a sting with embedded crs, or a collection of strings."

	| labelArray |
	labelArray _ (labelList isMemberOf: String)
		ifTrue: [ labelList lines ]
		ifFalse: [ labelList ].
	1 to: labelArray size do: [ :i |
		self add: (labelArray at: i) action: (selectionsArray at: i).
		(linesArray includes: i) ifTrue: [ self addLine ]]! !

!MenuMorph methodsFor: 'construction' stamp: 'sw 7/1/1999 22:21'!
title: aString
	"Add a title line at the top of this menu."

	self addTitle: aString! !


!MenuMorph methodsFor: 'control' stamp: 'ar 9/17/2000 20:38'!
activeSubmenu: aSubmenu
	activeSubMenu ifNotNil:[activeSubMenu delete].
	activeSubMenu _ aSubmenu.! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 8/12/2013 09:12'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	stayUp ifFalse: [ self delete ].
	popUpOwner ifNotNil: [
		popUpOwner isSelected: false.
		popUpOwner deleteIfPopUp: evt ].
	evt ifNotNil: [ evt hand ifNotNil: [ :h | h releaseMouseFocus: self ]].

	oldKeyboardFocus ifNotNil: [.
		oldKeyboardFocus world ifNotNil: [ :w |
			w activeHand newKeyboardFocus: oldKeyboardFocus ] ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/6/2014 11:31'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	self adjustSubmorphsLayout.
	sourceItem world addMorphFront: self.
	self world startSteppingSubmorphsOf: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld
			amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPointInWorld first value: false;
		value: rightOrLeftPointInWorld last - (extent x @ 0) value: false;
		value: rightOrLeftPointInWorld first value: true! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/6/2014 12:18'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self adjustSubmorphsLayout.
	self runningWorld addMorphFront: self position: aPoint - (2 @ 8).
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'ar 12/27/2001 22:46'!
popUpAt: aPoint forHand: hand in: aWorld
	"Present this menu at the given point under control of the given hand.  Allow keyboard input into the menu."

	^ self popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: Preferences menuKeyboardControl! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/6/2014 12:18'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	self adjustSubmorphsLayout.
	aWorld addMorphFront: self position: aPoint - (2 @ 8).
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 9/22/2012 15:36'!
popUpForHand: hand in: aWorld
	| p |
	"Present this menu under control of the given hand."

	p _ hand morphPosition truncated.
	^self popUpAt: p forHand: hand in: aWorld
! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 10/23/2012 17:58'!
popUpInWorld
	"Present this menu in the current World"

	^ self popUpInWorld: self runningWorld! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 9/22/2012 15:36'!
popUpInWorld: aWorld
	"Present this menu under control of the given hand."
	^self
		popUpAt: aWorld activeHand morphPosition
		forHand: aWorld activeHand
		in: aWorld
! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 1/29/2014 23:43'!
selectItem: aMenuItem
	selectedItem ifNotNil: [ selectedItem deselect ].
	selectedItem _ aMenuItem.
	selectedItem ifNotNil: [ selectedItem select ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 12/8/2008 15:41'!
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph.  A single-item MenuMorph is in effect a button rather than a menu, and as such should not be reluctant to be dropped into another object."

	^ (aMorph isWorldMorph or: [submorphs size = 1]) or:
		[Preferences systemWindowEmbedOK]! !


!MenuMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/25/2012 17:42'!
justDroppedInto: newOwnerMorph event: evt
	| halo |
	super justDroppedInto: newOwnerMorph event: evt.
	halo _ evt hand halo.
	(halo notNil and:[halo target hasOwner: self]) ifTrue: [
		"Grabbed single menu item"
		self addHalo: evt ].
	stayUp ifFalse: [ evt hand newMouseFocus: self ]! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:45'!
keyStroke: aKeyboardEvent 
	| matchString char asc selectable help |
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self world activeHand.
	char := aKeyboardEvent keyCharacter.
	asc := char asciiValue.
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: [
					selectedItem hasSubMenu 
						ifTrue: [
							aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
							^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: [
							"self delete."
							^selectedItem invokeWithEvent: aKeyboardEvent]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: aKeyboardEvent].
			^self].
	asc = 27 
		ifTrue: [
			"escape key"
			self
				valueOfProperty: #matchString
				ifPresentDo: [ :str | 
					str isEmpty 
						ifFalse: [
							"If filtered, first ESC removes filter"
							self setProperty: #matchString toValue: String new.
							self selectItem: nil.
							^self displayFiltered: aKeyboardEvent]].
			"If a stand-alone menu, just delete it"
			popUpOwner ifNil: [^self delete].
			"If a sub-menu, then deselect, and return focus to outer menu"
			self selectItem: nil.
			aKeyboardEvent hand newMouseFocus: popUpOwner owner.
			^aKeyboardEvent hand newKeyboardFocus: popUpOwner owner].
	(asc = 28 or: [asc = 29]) 
		ifTrue: [
			"left or right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: [
					aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]].
	asc = 30 ifTrue: [^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	asc = 31 ifTrue: [^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	asc = 11 ifTrue: [^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	asc = 12 ifTrue: [^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsent: [String new].
	matchString := char = Character backspace 
				ifTrue: [
					matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent.
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self world activeHand! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 1/19/2013 16:15'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self fullContainsPoint:localEventPosition ]) 
		ifFalse: [ ^self deleteIfPopUp: aMouseButtonEvent ]. "click outside"

	"Grab the menu and drag it to some other place
	This is reimplemented here because we handle the event, and if the following lines are commented, a menu can't be grabbed with the hand. This is not nice and shouldn't be needed"
	self isSticky ifTrue: [ ^self ].
	aMouseButtonEvent hand grabMorph: self! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 1/14/2013 23:13'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsPoint: localEventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		aMouseButtonEvent hand ifNotNil: [ :h | h releaseMouseFocus: self ].
		^ self deleteIfPopUp: aMouseButtonEvent ].
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ]! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 1/27/2013 00:54'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		"mejor en un lugar que no dependa de esto... es una porqueria"
		aMouseButtonEvent hand newMouseFocus: self ]! !


!MenuMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:52'!
handlesKeyboard
	"Answer whether the receiver handle keyboard events"

	^self visible! !

!MenuMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:56'!
handlesMouseDown: aMouseButtonEvent
	^true! !


!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 17:50'!
handleFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."
	| eventPositionInOurCoordinates |
	eventPositionInOurCoordinates _ self internalizeFromWorld: aMorphicEvent eventPosition.

	self dispatchEvent: aMorphicEvent localPosition: eventPositionInOurCoordinates.

	"Need to handle keyboard input if we have the focus."
	aMorphicEvent isKeyboard ifTrue: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(aMorphicEvent isMouseOver or: [aMorphicEvent isMouse not]) ifTrue: [ ^self ].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [^self]].
	"Note: The following does not traverse upwards but it's the best I can do for now"
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue: [^self]].! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:41'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ Theme current roundWindowCorners
		ifTrue: [0]
		ifFalse: [Preferences menuBorderWidth]! !

!MenuMorph methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:40'!
defaultColor
	^ Theme current menu! !

!MenuMorph methodsFor: 'initialization' stamp: 'ar 10/10/2000 01:57'!
delete
	activeSubMenu ifNotNil:[activeSubMenu delete].
	^super delete! !

!MenuMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:15'!
initialize
	super initialize.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !


!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 6/8/2014 20:32'!
displayFiltered: evt
	| matchStr allItems isMatch matches feedbackMorph |
	matchStr _ self valueOfProperty: #matchString.
	allItems _ self submorphs select: [ :m |
		m is: #MenuItemMorph ].
	matches _ allItems select: [ :m |
		isMatch _ matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch.
		isMatch ].
	feedbackMorph _ self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph _ StringMorph new color: Color veryDarkGray.
		self addMorphBack: feedbackMorph lock position: (0@ -20).
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	matches notEmpty ifTrue: [
		self selectItem: matches first ]! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 3/14/2011 09:19'!
keyboardFocusChange: aBoolean
	"Notify change due to green border for keyboard focus"

	self redrawNeeded! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 1/29/2014 23:45'!
moveSelectionDown: direction event: evt
	"Move the current selection up or down by one, presumably under keyboard control.
	direction = +/-1"

	| index m |
	index _ (submorphs indexOf: selectedItem ifAbsent: [1-direction]) + direction.
	submorphs do: "Ensure finite"
		[:unused | m _ submorphs atWrap: index.
		((m is: #MenuItemMorph) and: [m isEnabled]) ifTrue: [
			^ self selectItem: m ].
		"Keep looking for an enabled item"
		index _ index + direction sign].
	^ self selectItem: nil! !


!MenuMorph methodsFor: 'menu' stamp: 'jmv 2/22/2011 22:42'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addLine.
	aCustomMenu add: 'add title...' action: #addTitle.
	aCustomMenu add: 'sight target' action: #sightTarget:.
	defaultTarget ifNotNil: [
		aCustomMenu add: 'add item...' action: #addItem].
	aCustomMenu add: 'add line' action: #addLine! !

!MenuMorph methodsFor: 'menu' stamp: 'jmv 9/24/2012 19:43'!
addItem

	| string sel |
	string _ FillInTheBlankMorph request: 'Label for new item?'.
	string isEmpty ifTrue: [^ self].
	sel _ FillInTheBlankMorph request: 'Selector?'.
	sel isEmpty ifFalse: [sel _ sel asSymbol].
	self add: string action: sel.
! !

!MenuMorph methodsFor: 'menu' stamp: 'jmv 9/24/2012 19:43'!
addTitle

	| string |
	string _ FillInTheBlankMorph request: 'Title for this menu?'.
	string isEmpty ifTrue: [^ self].
	self addTitle: string.
! !

!MenuMorph methodsFor: 'menu' stamp: 'jmv 3/1/2011 16:27'!
removeStayUpBox
	| box |
	submorphs isEmpty ifTrue: [^self].
	(submorphs first is: #LayoutMorph) ifFalse: [^self].
	box _ submorphs first submorphs first.
	(box isKindOf: PluggableButtonMorph) 
		ifTrue: [ box hide ]! !

!MenuMorph methodsFor: 'menu' stamp: 'jmv 7/17/2014 12:07'!
sightTarget: event 
	| bullseye menu newTarget |
	owner
		ifNil: [^ self ].
	bullseye _ Point fromUserWithCursor: Cursor target.
	self world activeHand morphPosition: bullseye.
	menu _ CustomMenu new.
	"ToDo"
	self flag: #jmvVer2.
	"
	(owner morphsAt: bullseye) do: [ :m |
		menu add: m printString action: m ].
	menu title: self printString, ' targets... '.
	newTarget _ menu startUp.
	newTarget
		ifNil: [^ self].
	self target: newTarget
	"! !

!MenuMorph methodsFor: 'menu' stamp: 'jmv 1/12/2010 11:09'!
target: anObject
	"Set the default target object to be used for add item commands, and re-target all existing items to the new target or the the invoking hand."

	defaultTarget == anObject 
		ifTrue: [ ^self ].
		
	defaultTarget _ anObject.
	"re-target all existing items"
	self items do: [ :item | 
		(item target is: #HandMorph)
			ifFalse: [ item target: defaultTarget ]]! !


!MenuMorph methodsFor: 'modal control' stamp: 'sw 2/3/2002 14:26'!
invokeModal
	"Invoke this menu and don't return until the user has chosen a value.
	See example below on how to use modal menu morphs."
	^ self invokeModal: Preferences menuKeyboardControl

	"Example:
	| menu sub entry |
	menu _ MenuMorph new.
	1 to: 3 do: [:i |
		entry _ 'Line', i printString.
		sub _ MenuMorph new.
		menu add: entry subMenu: sub.
		#('Item A' 'Item B' 'Item C')  do:[:subEntry|
			sub add: subEntry target: menu 
				selector: #modalSelection: argument: {entry. subEntry}]].
	menu invokeModal.
"

! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 9/21/2013 20:06'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w oldFocus actHand |
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneCycle.
	self	
		popUpAt: actHand morphPosition
		forHand: actHand 
		allowKeyboard: allowKeyboardControl.
	self isModalInvokationDone: false.
	[ self isInWorld & self isModalInvokationDone not ] whileTrue: [ w doOneCycle ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ self modalSelection! !

!MenuMorph methodsFor: 'modal control' stamp: 'ar 1/5/2002 21:33'!
isModalInvokationDone
	^self valueOfProperty: #isModalInvokationDone ifAbsent:[false]! !

!MenuMorph methodsFor: 'modal control' stamp: 'ar 1/5/2002 21:34'!
isModalInvokationDone: aBool
	self setProperty: #isModalInvokationDone toValue: aBool
! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 3/2/2010 09:57'!
modalSelection
	^self valueOfProperty: #modalSelection ifAbsent: nil! !

!MenuMorph methodsFor: 'modal control' stamp: 'ar 1/5/2002 21:34'!
modalSelection: anObject
	self setProperty: #modalSelection toValue: anObject.
	self isModalInvokationDone: true! !


!MenuMorph methodsFor: 'private' stamp: 'jmv 6/6/2014 11:21'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p h |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: (
			(each respondsTo: #minItemWidth)
				ifTrue: [each minItemWidth]
				ifFalse: [each morphWidth])].

	w _ w + 1.
	p _ 5 @ 5.
	submorphs do: [ :m |
		h _ m morphHeight.
		m morphPosition: p extent: w@h.
		p _ p + (0@(h + 1)) ].

	self morphExtent: w+4 @ p y + 5! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 6/6/2014 12:12'!
fitInWorld
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| delta |
	"If it doesn't fit, show it to the left, not to the right of the hand."
	self morphBoundsInWorld right > owner world morphBoundsInWorld right
		ifTrue: [
			self morphPosition: ((self morphPosition x + 10 - extent x) @ self morphPosition y) ].

	"Make sure that the menu fits in the world."
	delta _ self morphBoundsInWorld amountToTranslateWithin:
		(owner world morphBoundsInWorld withHeight:
			((owner world morphExtentInWorld y) max: (self morphPosition y) + 1)).
	delta = (0 @ 0) ifFalse: [ self morphPosition: self morphPosition + delta ]! !

!MenuMorph methodsFor: 'private' stamp: 'ar 9/18/2000 12:12'!
selectedItem
	^selectedItem! !


!MenuMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:54'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: (0@0 extent: extent) color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas fillRectangle: (0@0 extent: extent) color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !



!MenuMorph methodsFor: 'misc' stamp: 'jmv 8/20/2012 17:49'!
activate: evt
	"Receiver should be activated; e.g., so that control passes correctly."
	evt hand newMouseFocus: self.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MenuMorph class' category: #'Morphic-Menus'!
MenuMorph class
	instanceVariableNames: ''!

!MenuMorph class methodsFor: 'example' stamp: 'jmv 5/17/2009 23:42'!
example
	"MenuMorph example openInWorld"

	| menu |
	menu _ MenuMorph new.
	menu addStayUpIcons.
	menu add: 'apples' action: #apples.
	menu add: 'oranges' action: #oranges.
	menu addLine.
	menu addLine.  "extra lines ignored"
	menu add: 'peaches' action: #peaches.
	menu addLine.
	menu add: 'pears' action: #pears.
	menu addLine.
	^ menu
! !

!MenuMorph class methodsFor: 'example' stamp: 'jmv 5/17/2009 23:42'!
example2
	"MenuMorph example2 openInWorld"

	| menu |
	menu _ MenuMorph entitled: 'sdfasdfa sfda sdf as df as fd'.
	menu addStayUpIcons.
	menu add: 'apples' action: #apples.
	menu add: 'oranges' action: #oranges.
	menu addLine.
	menu addLine.  "extra lines ignored"
	menu add: 'peaches' action: #peaches.
	menu addLine.
	menu add: 'pears' action: #pears.
	menu addLine.
	^ menu
! !


!MenuMorph class methodsFor: 'instance creation' stamp: 'jm 5/14/1998 17:21'!
entitled: aString
	"Answer a new instance of me with the given title."

	^ self new addTitle: aString
! !


!classDefinition: #MVCMenuMorph category: #'Morphic-Menus'!
MenuMorph subclass: #MVCMenuMorph
	instanceVariableNames: 'done mvcSelection'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!MVCMenuMorph commentStamp: '<historical>' prior: 0!
I simulate the MVC menu classes PopUpMenu, SelectionMenu, and CustomMenu when running in a Morphic world. I am also used to implement Utilities>informUser:during:.!


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 12/3/2012 00:29'!
displayAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	self runningWorld ifNotNil: [ :w |
		w addMorph: self centeredNear: aPoint.
		self world ifNotNil: [ w displayWorld ].  "show myself"
		].
	aBlock value.
	self delete! !

!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 4/17/2013 12:04'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ self world ifNil: [ self runningWorld ].
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString morphWidth + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w ifNotNil: [
			w displayWorld	].	 "show myself"
	]. 
	self delete.
	w ifNotNil: [
		w displayWorld ]! !

!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 9/21/2013 20:05'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	|actHand w oldFocus |
	self flag: #bob.		"is <aPoint> global or local?"
	self flag: #arNote.	"<aPoint> is local to aWorld"
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneCycle.
	self
		popUpAt: aPoint
		forHand: actHand
		allowKeyboard: aBoolean.
	done _ false.
	[ self isInWorld & done not ] whileTrue: [ w doOneCycle ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ mvcSelection ! !


!MVCMenuMorph methodsFor: 'private' stamp: 'ar 9/18/2000 12:10'!
cancelValue: selectionOrNil
	"Set the value to be returned if the user cancels without making a selection."

	mvcSelection _ selectionOrNil.
! !

!MVCMenuMorph methodsFor: 'private' stamp: 'ar 9/18/2000 12:16'!
selectMVCItem: item
	"Called by the MenuItemMorph that the user selects.
	Record the selection and set the done flag to end this interaction."

	mvcSelection _ item.
	done _ true.
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MVCMenuMorph class' category: #'Morphic-Menus'!
MVCMenuMorph class
	instanceVariableNames: ''!

!MVCMenuMorph class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 22:15'!
from: aPopupMenu title: titleStringOrNil
	"Answer a MenuMorph constructed from the given PopUpMenu. Used to simulate MVC-style menus in a Morphic-only world."

	| menu items lines selections labelString j emphasis |
	menu _ self new.
	titleStringOrNil ifNotNil: [
		titleStringOrNil isEmpty ifFalse: [menu addTitle: titleStringOrNil]].
	labelString _ aPopupMenu labelString.
	items _ labelString asString lines.
	(labelString is: #Text) ifTrue: [
		"Pass along text emphasis if present"
		j _ 1.
		items _ items collect: [ :item |
			j _ labelString asString findString: item startingAt: j.
			emphasis _ TextEmphasis new emphasisCode: (labelString emphasisAt: j).
			item asText addAttribute: emphasis]].
	lines _ aPopupMenu lineArray.
	lines ifNil: [lines _ #()].
	menu cancelValue: 0.
	menu defaultTarget: menu.
	selections _ (1 to: items size) asArray.
	1 to: items size do: [ :i |
		menu add: (items at: i) selector: #selectMVCItem: argument: (selections at: i).
		(lines includes: i) ifTrue: [menu addLine]].
	^ menu
! !


!classDefinition: #MenuLineMorph category: #'Morphic-Menus'!
RectangleLikeMorph subclass: #MenuLineMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!MenuLineMorph commentStamp: '<historical>' prior: 0!
Just a line for separating items in menus.!


!MenuLineMorph methodsFor: 'drawing' stamp: 'jmv 7/18/2014 15:35'!
drawOn: aCanvas 
	| baseColor |
	baseColor _ owner color.
	aCanvas
		fillRectangle: (0@0 corner: extent x @ (extent y / 2))
		color: baseColor twiceDarker.
			
	aCanvas
		fillRectangle: (0 @ (extent y / 2) corner: extent)
		color: baseColor twiceLighter! !


!MenuLineMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:13'!
initialize
	super initialize.
	extent _ 50 @ 2! !


!MenuLineMorph methodsFor: 'layout' stamp: 'jmv 5/14/2009 15:44'!
minItemWidth
	^ 0
! !


!classDefinition: #MenuItemMorph category: #'Morphic-Menus'!
StringMorph subclass: #MenuItemMorph
	instanceVariableNames: 'isEnabled subMenu isSelected target selector arguments icon'
	classVariableNames: 'SubMenuMarker'
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!MenuItemMorph commentStamp: 'jmv 5/6/2009 22:52' prior: 0!
I represent an item in a menu.

Instance variables:
	isEnabled 	<Boolean>			True if the menu item can be executed.
	subMenu 	<MenuMorph | nil>	The submenu to activate automatically when the user mouses over the item.
	isSelected 	<Boolean>			True if the item is currently selected.
	target 		<Object>			The target of the associated action.
	selector 	<Symbol>			The associated action.
	arguments 	<Array>			The arguments for the associated action.
	icon		<Form | nil>		An optional icon form to be displayed to my left.

If I have a dynamic marker, created by strings like <yes> or <no> in my contents, it will be installed as a submorph.!


!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 11/4/97 07:46'!
arguments

	^ arguments
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 11/4/97 07:46'!
arguments: aCollection

	arguments _ aCollection.
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'ar 1/16/2001 16:58'!
contentString
	^self valueOfProperty: #contentString! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 3/2/2010 16:11'!
contentString: aString 
	aString 
		ifNil: [self removeProperty: #contentString]
		ifNotNil: [self setProperty: #contentString toValue: aString]! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'ar 9/17/2000 18:32'!
contents: aString
	^self contents: aString withMarkers: true! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'ar 1/16/2001 16:57'!
contents: aString withMarkers: aBool
	^self contents: aString withMarkers: aBool inverse: false! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 6/6/2014 15:27'!
contents: aString withMarkers: aBool inverse: inverse 
	"Set the menu item entry. If aBool is true, parse aString for embedded markers."

	| markerIndex marker |
	self contentString: nil.	"get rid of old"
	aBool ifFalse: [^super contents: aString].
	self removeAllMorphs.	"get rid of old markers if updating"
	self hasIcon ifTrue: [ self icon: nil ].
	(aString notEmpty and: [aString first = $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	self contentString: aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph new image: marker.
	self addMorphFront: marker position: 0@2! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'nk 3/10/2004 15:19'!
hasIcon
	"Answer whether the receiver has an icon."
	^ icon notNil! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'nk 3/10/2004 15:25'!
hasMarker
	"Answer whether the receiver has a marker morph."
	^ submorphs isEmpty not! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'ar 9/18/2000 11:40'!
hasSubMenu
	"Return true if the receiver has a submenu"
	^subMenu notNil! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'dgd 3/22/2003 14:45'!
icon
	"answer the receiver's icon"
	^ icon! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'dgd 3/22/2003 14:45'!
icon: aForm 
	"change the the receiver's icon"
	icon := aForm! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 11/4/97 07:46'!
isEnabled

	^ isEnabled
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 3/13/2009 10:04'!
isEnabled: aBoolean

	isEnabled = aBoolean ifTrue: [^ self].
	isEnabled _ aBoolean.
	self color: (aBoolean ifTrue: [Color black] ifFalse: [Color gray]).
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 11/4/97 07:46'!
selector

	^ selector
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 11/4/97 07:46'!
selector: aSymbol

	selector _ aSymbol.
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 11/4/97 07:46'!
subMenu

	^ subMenu
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 3/14/2011 09:19'!
subMenu: aMenuMorph
	subMenu _ aMenuMorph.
	self redrawNeeded! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 11/4/97 07:46'!
target

	^ target! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 11/4/97 07:46'!
target: anObject

	target _ anObject.
! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2014 22:41'!
drawOn: aCanvas 
	| stringColor leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: (0@0 extent: extent) color: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	self hasIcon
		ifTrue: [| iconForm | 
			iconForm _ isEnabled ifTrue: [ self icon ] ifFalse: [ self icon asGrayScale ].
			aCanvas image: iconForm at: leftEdge+1 @ (extent y - iconForm height // 2).
			leftEdge _ leftEdge + iconForm width + self iconSeparation].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: SubMenuMarker
			at: extent x - 8 @ (extent y - SubMenuMarker height // 2) ]! !


!MenuItemMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:55'!
handlesMouseDown: aMouseButtonEvent

	^ true! !

!MenuItemMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:55'!
handlesMouseOver: anEvent
	^true! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 9/20/2012 00:14'!
activateOwnerMenu: evt
	"Activate our owner menu; e.g., pass control to it"
	owner ifNil: [ ^false ]. "not applicable"
	(owner morphContainsPoint: (owner internalizeFromWorld: evt eventPosition))
		ifFalse: [ ^false ].
	owner activate: evt.
	^true! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:46'!
activateSubmenu: evt
	"Activate our submenu; e.g., pass control to it"
	subMenu ifNil: [ ^false ]. "not applicable"
	(subMenu morphContainsPoint: (subMenu internalizeFromWorld: evt eventPosition)) ifFalse:[^false].
	subMenu activate: evt.
	self removeAlarm: #deselectTimeOut.
	^true! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:46'!
deselectTimeOut
	"Deselect timout. Now really deselect"
	owner selectedItem == self ifTrue: [
		owner selectItem: nil ]! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 5/26/2013 16:14'!
invokeWithEvent: evt
	"Perform the action associated with the given menu item."

	| selArgCount w |
	self isEnabled ifFalse: [^ self].
	owner ifNotNil: [
		self flag: #workAround. "The tile system invokes menus straightforwardly so the menu might not be in the world."
		(w _ self world) ifNotNil:[
			owner deleteIfPopUp: evt.
			"Repair damage before invoking the action for better feedback"
			w displayWorldSafely]].
	selector ifNil: [ ^self ].
	(selArgCount _ selector numArgs) = 0
		ifTrue: [
			target perform: selector]
		ifFalse: [
			selArgCount = arguments size
				ifTrue: [target perform: selector withArguments: arguments]
				ifFalse: [target perform: selector withArguments: (arguments copyWith: evt)]]! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:44'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	aMouseButtonEvent shiftPressed ifTrue: [
		^ super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].  "enable label editing" 
	aMouseButtonEvent hand newMouseFocus: owner. "Redirect to menu for valid transitions"
	owner selectItem: self! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/30/2014 00:53'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	owner hasMouseFocus ifFalse: [ ^self ].
	"This will happen if the menu has toggles in it. (for instance, the 'show...' button)
	Update the look, refresh the world and wait a bit,
	to give the user some visual feedback"
	self contentString ifNotNil: [
		self contents: self contentString withMarkers: true inverse: true.
		self refreshWorld.
		(Delay forMilliseconds: 200) wait].
	self deselect.
	self invokeWithEvent: aMouseButtonEvent! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:46'!
mouseEnter: evt
	"The mouse entered the receiver"
	owner popUpOwner ifNotNil: [ :parentItem |
		parentItem removeAlarm: #deselectTimeOut ].
	owner ifNil: [ ^self ].
	owner selectItem: self! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/29/2014 23:47'!
mouseLeave: evt 
	"The mouse left the receiver. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	owner ifNil: [^self].
	"If we have a submenu, make sure we've got some time to enter it before actually leaving the menu item"
	subMenu 
		ifNil: [ owner selectItem: nil ]
		ifNotNil: [
			self addAlarm: #deselectTimeOut after: 500 ]! !


!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:22'!
processMouseEnter: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	"Do #mouseEnter: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	self wantsBalloon ifTrue: [
		aMouseEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEvent) ifTrue:[
		aMouseEvent wasHandled: true.
		self mouseEnter: aMouseEvent ]! !

!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 19:22'!
processMouseLeave: aMouseEvent localPosition: localEventPosition
	"System level event handling."

	"Do #mouseLeave: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	aMouseEvent hand removePendingBalloonFor: self.
	(self handlesMouseOver: aMouseEvent) ifTrue: [
		aMouseEvent wasHandled: true.
		self mouseLeave: aMouseEvent ]! !


!MenuItemMorph methodsFor: 'grabbing' stamp: 'jmv 6/8/2014 19:27'!
aboutToBeGrabbedBy: aHand
	"Don't allow the receiver to act outside a Menu"
	| menu |
	self isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: self.
	menu adjustSubmorphsLayout.
	menu stayUp.
	^ menu! !

!MenuItemMorph methodsFor: 'grabbing' stamp: 'jmv 6/8/2014 19:27'!
duplicateMorph: evt
	"Don't allow the receiver to act outside a Menu"
	| dup menu |
	dup _ self duplicate isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: dup.
	menu adjustSubmorphsLayout.
	menu stayUp.
	^ menu! !


!MenuItemMorph methodsFor: 'initialization' stamp: 'ar 10/10/2000 02:05'!
deleteIfPopUp: evt
	"Recurse up for nested pop ups"
	owner ifNotNil:[owner deleteIfPopUp: evt].! !

!MenuItemMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:14'!
initialize
	"initialize the state of the receiver"
	super initialize.
	""
	extent _ 10@10.
	contents _ ''.
	isEnabled _ true.
	subMenu _ nil.
	isSelected _ false.
	target _ nil.
	selector _ nil.
	arguments _ nil.
	font _ Preferences standardMenuFont! !


!MenuItemMorph methodsFor: 'layout' stamp: 'jmv 5/26/2011 09:06'!
iconSeparation
	^5! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jmv 5/26/2011 09:08'!
measureContents
	| e |
	e _ super measureContents.
	^e y > 12
		ifTrue: [e+(2@2)]
		ifFalse: [e+(2@1)]! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jmv 9/8/2012 20:30'!
minItemWidth
	| fontToUse iconWidth subMenuWidth markerWidth |
	fontToUse _ self fontToUse.
	subMenuWidth _ self hasSubMenu
				ifFalse: [0]
				ifTrue: [10].
	iconWidth _ self hasIcon
				ifTrue: [self icon width + self iconSeparation]
				ifFalse: [0].
	markerWidth _ self hasMarker
		ifTrue: [ submorphs first morphWidth + 8 ]
		ifFalse: [ 0 ].
	^ (fontToUse widthOfString: contents)
		+ subMenuWidth + iconWidth + markerWidth.! !


!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 1/29/2014 23:46'!
deselect

	self isSelected: false.
	subMenu ifNotNil: [
		owner ifNotNil:[ owner activeSubmenu: nil ].
		self removeAlarm: #deselectTimeOut ]! !

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 3/14/2011 09:18'!
isSelected: aBoolean

	isSelected _ aBoolean.
	self redrawNeeded! !

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 1/29/2014 23:44'!
select
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: (Array with: self morphBoundsInWorld topRight + (10@0)
									with: self morphBoundsInWorld topLeft)
			from: self.
		subMenu selectItem: nil ]! !


!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/9/2012 23:55'!
offImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.9) 
			borderWidth: 1 borderColor: Color black.
	^form! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 9/9/2012 23:55'!
onImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 16.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: (Color gray: 0.8) 
			borderWidth: 1 borderColor: Color black;
		fillRectangle: (form boundingBox insetBy: 2) color: Color black.
	^form! !


!MenuItemMorph methodsFor: 'testing' stamp: 'jmv 9/2/2013 10:36'!
is: aSymbol
	^ aSymbol == #MenuItemMorph or: [ super is: aSymbol ]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MenuItemMorph class' category: #'Morphic-Menus'!
MenuItemMorph class
	instanceVariableNames: ''!

!MenuItemMorph class methodsFor: 'class initialization' stamp: 'jm 11/16/97 09:17'!
initialize
	"MenuItemMorph initialize"

	| f |
	f _ Form
		extent: 5@9
		fromArray: #(2147483648 3221225472 3758096384 4026531840 4160749568 4026531840 3758096384 3221225472 2147483648)
		offset: 0@0.
	SubMenuMarker _ ColorForm mappingWhiteToTransparentFrom: f.
! !


!classDefinition: #UpdatingMenuItemMorph category: #'Morphic-Menus'!
MenuItemMorph subclass: #UpdatingMenuItemMorph
	instanceVariableNames: 'wordingProvider wordingSelector enablementSelector wordingArgument'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!UpdatingMenuItemMorph commentStamp: '<historical>' prior: 0!
A menu item whose textual label and whose enablement are updatable.  The wordingProvider provides the current wording, upon being being sent the wordingSelector.

The item can also dynamically update whether or not it should be enabled; to do this, give it an enablementSelector, which is also sent to the wordingProvider..!


!UpdatingMenuItemMorph methodsFor: 'enablement' stamp: 'ajh 1/21/2003 13:17'!
enablement 

	enablementSelector isBlock
		ifTrue: [^ enablementSelector value]
		ifFalse: [enablementSelector numArgs = 0
				ifTrue: [^ wordingProvider perform: enablementSelector]
				ifFalse: [^ wordingProvider perform: enablementSelector
										withArguments: arguments]]! !

!UpdatingMenuItemMorph methodsFor: 'enablement' stamp: 'eem 7/21/2008 12:07'!
enablementSelector: aSelector 
	enablementSelector := aSelector isBlock 
				ifTrue: [aSelector copyForSaving]
				ifFalse: [aSelector] ! !


!UpdatingMenuItemMorph methodsFor: 'stepping' stamp: 'jmv 2/3/2014 22:53'!
stepAt: millisecondClockValue

	super stepAt: millisecondClockValue.
	self updateContents! !

!UpdatingMenuItemMorph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:03'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message"

	^true! !


!UpdatingMenuItemMorph methodsFor: 'testing' stamp: 'jmv 9/2/2013 10:42'!
is: aSymbol
	^ aSymbol == #UpdatingMenuItemMorph or: [ super is: aSymbol ]! !

!UpdatingMenuItemMorph methodsFor: 'testing' stamp: 'sw 6/11/1999 18:31'!
stepTime
	^ 1200! !


!UpdatingMenuItemMorph methodsFor: 'wording' stamp: 'sw 11/6/2000 09:55'!
wordingArgument: anArgument
	"Set the receiver's wordingArgument as indicated"

	wordingArgument _ anArgument! !

!UpdatingMenuItemMorph methodsFor: 'wording' stamp: 'sw 6/11/1999 15:12'!
wordingProvider: aProvider wordingSelector: aSelector
	wordingProvider _ aProvider.
	wordingSelector _ aSelector! !


!UpdatingMenuItemMorph methodsFor: 'world' stamp: 'jmv 8/28/2011 21:51'!
updateContents
	"Update the receiver's contents"

	| newString enablement nArgs |
	((wordingProvider isNil) or: [wordingSelector isNil]) ifFalse: [
		nArgs _ wordingSelector numArgs.
		newString _ nArgs = 0
			ifTrue: [
				wordingProvider perform: wordingSelector]
			ifFalse: [
				(nArgs = 1 and: [wordingArgument notNil])
					ifTrue: [
						wordingProvider perform: wordingSelector with: wordingArgument]
					ifFalse: [
						nArgs = arguments size ifTrue: [
							wordingProvider perform: wordingSelector withArguments: arguments]]].
		newString = (self contentString ifNil: [ contents ])
			ifFalse: [
				self contents: newString.
				Theme current decorateMenu: owner ]].
	enablementSelector ifNotNil: [
		(enablement _ self enablement) == isEnabled 
			ifFalse:	[self isEnabled: enablement]]! !


!classDefinition: #TheWorldMenu category: #'Morphic-Menus'!
Object subclass: #TheWorldMenu
	instanceVariableNames: 'myWorld myHand'
	classVariableNames: 'ExtraOpenCommands'
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!TheWorldMenu commentStamp: 'jmv 1/4/2013 13:27' prior: 0!
Instances of TheWorldMenu serve to present the primary Squeak menu obtained by clicking on open desktop.!


!TheWorldMenu methodsFor: 'action' stamp: 'jmv 11/4/2011 10:23'!
doMenuItem: aCollection with: event
	| realTarget selector nArgs |
	selector _ aCollection second.
	nArgs _ selector numArgs.
	realTarget _ aCollection first.
	realTarget == #myWorld ifTrue: [realTarget _ myWorld].
	realTarget == #myHand ifTrue: [realTarget _ myHand].
	realTarget == #theWorldMenu ifTrue: [realTarget _ self].
	^nArgs = 0 
		ifTrue:[realTarget perform: selector]
		ifFalse:[realTarget perform: selector with: event].
! !

!TheWorldMenu methodsFor: 'action' stamp: 'jmv 4/15/2011 16:12'!
openMessageNames
	"Bring a MessageNames tool to the front"

	MessageNamesWindow open: MessageNames new label: 'Message Names'! !

!TheWorldMenu methodsFor: 'action' stamp: 'RAA 5/26/2000 11:31'!
staggerPolicyString

	^ Preferences staggerPolicyString! !

!TheWorldMenu methodsFor: 'action' stamp: 'RAA 5/26/2000 11:32'!
toggleWindowPolicy

	Preferences toggleWindowPolicy! !


!TheWorldMenu methodsFor: 'commands' stamp: 'ar 10/5/2000 18:54'!
changeBackgroundColor
	"Let the user select a new background color for the world"

	myWorld changeColorTarget: myWorld selector: #color: originalColor: myWorld color hand: myWorld activeHand.
! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 8/13/2013 09:21'!
cleanUpWorld
	(SelectionMenu confirm: 'This will remove all windows except those
containing unsubmitted text edits, and will
also remove all non-window morphs (other
than flaps) found on the desktop.  Are you
sure you want to do this?') ifFalse: [ ^ self ].
	myWorld allNonFlapRelatedSubmorphs do: [ :m |
		m delete ].
	(SystemWindow
		windowsIn: myWorld
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/25/2012 22:52'!
fullScreenOff

	Display fullScreenMode: false.
	DisplayScreen checkForNewScreenSize.
	myWorld restoreMorphicDisplay! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/25/2012 22:52'!
fullScreenOn

	Display fullScreenMode: true.
	DisplayScreen checkForNewScreenSize.
	myWorld restoreMorphicDisplay! !

!TheWorldMenu methodsFor: 'commands' stamp: 'nk 2/15/2004 09:37'!
garbageCollect
	"Do a garbage collection, and report results to the user."

	Utilities garbageCollectAndReport! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 4/3/2011 22:55'!
newMorphOfClass: morphClass event: evt
	"Attach a new morph of the given class to the invoking hand."

	| m |
	m _ morphClass initializedInstance.
	evt hand attachMorph: m.
	myWorld startSteppingSubmorphsOf: m.
! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 3/21/2011 16:44'!
openBrowser
	BrowserWindow openBrowser! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 4/15/2011 16:58'!
openChangeSorter1

	ChangeSorterWindow open: ChangeSorter new label: nil! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 4/25/2011 21:43'!
openFileList
	FileListWindow openFileList! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 3/28/2012 09:14'!
openPackageList
	CodePackageListWindow openPackageList! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 4/17/2011 22:20'!
openTextEditor

	SystemWindow editText: TextModel new label: 'Text Editor' wrap: true! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 5/20/2011 17:17'!
openTranscript

	TranscriptWindow openTranscript! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 6/1/2011 22:44'!
openWorkspace

	Workspace new
		contents: '';
		openLabel: 'Workspace'! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 6/22/2013 21:21'!
quitSession
	Smalltalk 
		snapshot: (self confirm: 'Save the current image before quitting?' orCancel: [^ self])
		andQuit: true! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 2/15/2008 00:47'!
saveAndQuit

	Smalltalk snapshot: true andQuit: true! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 12/7/2013 18:17'!
saveWorldInFile
	"Save the world's submorphs, model, and stepList in a file.  "

	| fileName fileStream |
	fileName _ FillInTheBlankMorph request: 'File name for this morph?'.
	fileName isEmpty ifTrue: [^ self].  "abort"

	"Save only model, stepList, submorphs in this world"
	myWorld submorphsDo: [:m |
		m allMorphsDo: [:subM | subM prepareToBeSaved]].	"Amen"

	fileStream _ FileStream newFileNamed: fileName, '.morph'.
	fileStream fileOutObject: myWorld.
! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 2/22/2011 22:53'!
setDisplayDepth
	"Let the user choose a new depth for the display. "

	| result oldDepth allDepths allLabels menu hasBoth |
	oldDepth := Display nativeDepth.
	allDepths := #(1 -1 2 -2 4 -4 8 -8 16 -16 32 -32) 
				select: [:d | Display supportsDisplayDepth: d].
	hasBoth := (allDepths anySatisfy: [:d | d > 0]) 
				and: [allDepths anySatisfy: [:d | d < 0]].
	allLabels := allDepths collect: 
					[:d | 
					String streamContents: 
							[:s | 
							s nextPutAll: (d = oldDepth ifTrue: ['<on>'] ifFalse: ['<off>']).
							s print: d abs.
							hasBoth 
								ifTrue: 
									[s 
										nextPutAll: (d > 0 ifTrue: ['  (big endian)'] ifFalse: ['  (little endian)'])]]].
	menu := SelectionMenu labels: allLabels selections: allDepths.
	result := menu startUpWithCaption: 'Choose a display depth'.
	result ifNotNil: [Display newDepth: result].
	oldDepth := oldDepth abs! !

!TheWorldMenu methodsFor: 'commands' stamp: 'aoy 2/15/2003 21:19'!
splitNewMorphList: list depth: d 
	| middle c prev next out |
	d <= 0 ifTrue: [^Array with: list].
	middle := list size // 2 + 1.
	c := (list at: middle) name first.
	prev := middle - 1.
	[prev > 0 and: [(list at: prev) name first = c]] 
		whileTrue: [prev := prev - 1].
	next := middle + 1.
	[next <= list size and: [(list at: next) name first = c]] 
		whileTrue: [next := next + 1].
	"Choose the better cluster"
	middle := middle - prev < (next - middle) 
				ifTrue: [prev + 1]
				ifFalse: [next]. 
	middle = 1 ifTrue: [middle := next].
	middle >= list size ifTrue: [middle := prev + 1].
	(middle = 1 or: [middle >= list size]) ifTrue: [^Array with: list].
	out := WriteStream on: Array new.
	out nextPutAll: (self splitNewMorphList: (list copyFrom: 1 to: middle - 1)
				depth: d - 1).
	out 
		nextPutAll: (self splitNewMorphList: (list copyFrom: middle to: list size)
				depth: d - 1).
	^out contents! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/25/2012 22:52'!
startMessageTally
	"Tally on all the processes in the system, and not only the UI"
	
	(self confirm: 'MessageTally all the processes in
the system, until the mouse pointer
goes to the top of the screen') ifTrue: [
		MessageTally spyAllOn: [
			[Sensor peekMousePt y > 0] whileTrue: [myWorld doOneCycle]]]! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/25/2012 22:52'!
startThenBrowseMessageTally
	"Tally only the UI process"
	
	(self confirm: 'MessageTally the UI process until the
mouse pointer goes to the top of the screen')
		ifTrue: [TimeProfileBrowser
				onBlock: [[Sensor peekMousePt y > 10]
						whileTrue: [myWorld doOneCycle]]]! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 11/25/2010 14:52'!
vmStatistics
	"Open a string view on a report of vm statistics"

	(TextModel new contents: Utilities vmStatisticsReportString)
		openLabel: 'VM Statistics'! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 8/23/2010 10:20'!
alphabeticalMorphMenu
	| list splitLists menu firstChar lastChar subMenu |
	list := Morph withAllSubclasses select: [:m | m includeInNewMorphMenu].
	list := list asArray sort: [:c1 :c2 | c1 name < c2 name].
	splitLists := self splitNewMorphList: list depth: 3.
	menu := MenuMorph new defaultTarget: self.
	1 to: splitLists size
		do: 
			[:i | 
			firstChar := i = 1 
				ifTrue: [$A]
				ifFalse: 
					[((splitLists at: i - 1) last name first asInteger + 1) 
								asCharacter].
			lastChar := i = splitLists size 
						ifTrue: [$Z]
						ifFalse: [(splitLists at: i) last name first].
			subMenu := MenuMorph new.
			(splitLists at: i) do: 
					[:cl | 
					subMenu 
						add: cl name
						target: self
						selector: #newMorphOfClass:event:
						argument: cl].
			menu add: firstChar asString , ' - ' , lastChar asString subMenu: subMenu].
	^menu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 6/3/2014 06:07'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	self colorForDebugging: menu.
	menu addStayUpIcons.
	self fillIn: menu
		from: {
				{ 'Open...'. { self. #openWindow}}.
				{ 'New morph...'. { self. #newMorph}.
					'Offers a variety of ways to create new objects'}.
				{ 'Preferences...'. { self. #preferencesDo}.
					'put up a menu offering many controls over appearance and system preferences.'}.
				{ 'Windows...'. { self. #windowsDo}}.
				{ 'Help...'. { self. #helpDo}.
					'puts up a menu of useful items for updating the system, determining what version you are running, and much else'}.
				nil.
				{ 'Changes...'. { self. #changesDo}}.
				{ 'Debug...'. { self. #debugDo}.
					'a menu of debugging items'}.
				{ 'Restore Display (r)'. { myWorld. #restoreMorphicDisplay}.
					'repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'}.
				nil.
				{ 'Save options...'.  { self. #saveOptionsDo}.
					'a menu with options to save the Cuis image'}.
				{ 'Quit'. { self. #quitSession}.
					'quit out of Cuis.'}}.
	^menu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/27/2014 12:30'!
changesMenu
        "Build the changes menu for the world."

	| menu |
	menu _ self menu: 'Changes...'.
	self fillIn: menu from: {
		{ 'Change Sorter' . {self. #openChangeSorter1}.  'Open a 3-paned changed-set viewing tool'}.
		nil.

		{ 'Install New Updates' . { ChangeSet. #installNewUpdates }.  'Install in the current image the new updates availables in the sibling directory named ../Cuis-Smalltalk-Dev/CoreUpdates'}.
		nil.

		{ 'Browse my Changes' . { Smalltalk . #browseMyChanges }.
				'Browse all of my changes since the last time #condenseSources was run.'}.
		{ 'Recently logged Changes...' . { ChangeList . #browseRecentLog}.'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'}.

		nil.
		{ 'Save World as morph file' . {self. #saveWorldInFile}. 'Save a file that, when reloaded, reconstitutes the current World.'}.
	}.
	^ menu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'RAA 6/16/2000 09:50'!
colorForDebugging: aMenu

        "aMenu color: self myMenuColor"

        "aMenu color: Color lightRed"

! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/20/2011 10:10'!
debugMenu

        | menu |

        menu _ self menu: 'Debug...'.
        ^self fillIn: menu from: { 
                { 'Inspect World' . { #myWorld . #inspect } }.
                { 'Explore World' . { #myWorld . #explore } }.
                { 'MessageTally all Processes' . { self . #startMessageTally } }.
                { 'MessageTally UI and Browse' . { self . #startThenBrowseMessageTally } }.
                nil.
                { 'Start drawing all again' . { #myWorld . #removeAllKnownFailing } }.
                { 'Start stepping again' . { #myWorld . #resumeAfterStepError } }.
                { 'Close all Debuggers' . { DebuggerWindow . #closeAllDebuggers } }.
        }
! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 9/2/2013 09:05'!
fillIn: aMenu from: dataForMenu
	"A menu constructor utility by RAA.  dataForMenu is a list of items which mean:
			nil								Indicates to add a line

			first element is symbol		Add updating item with the symbol as the wording selector
			second element is a list		second element has the receiver and selector

			first element is a string		Add menu item with the string as its wording
			second element is a list		second element has the receiver and selector

			a third element exists			Use it as the balloon text
			a fourth element exists		Use it as the enablement selector (updating case only)"

	| item |

	dataForMenu do: [ :itemData |
		itemData ifNil: [aMenu addLine] ifNotNil: [
			item _ itemData first isSymbol
				ifTrue: [
					aMenu 
						addUpdating: itemData first 
						target: self 
						selector: #doMenuItem:with: 
						argumentList: {itemData second}]
				 ifFalse: [
					aMenu 
						add: itemData first
						target: self 
						selector: #doMenuItem:with: 
						argumentList: {itemData second}].
			itemData size >= 3 ifTrue: [
				aMenu balloonTextForLastItem: itemData third.
			itemData size >= 4 ifTrue: [
				item enablementSelector: itemData fourth ]]]].

	^ aMenu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 1/2/2013 15:05'!
helpMenu
	"Build the help menu for the world."
	| menu |
	menu _ self menu: 'Help...'.
	self
		fillIn: menu
		from: {
			{'About this System...'. {Smalltalk. #aboutThisSystem}. 'current version information.'}.
			{'Code management in Cuis'. {Utilities. #openCodeManagementInCuis}. 'current version information.'}.
			{'Using GitHub to host Cuis packages'. {Utilities. #openCuisAndGitHub}. 'current version information.'}.
			nil.
			{'Editor keyboard shortcuts'. {SmalltalkEditor. #openHelp}. 'summary of keyboard shortcuts in editors for Smalltalk code.'}
		}.
	self addGestureHelpItemsTo: menu.
	self
		fillIn: menu
		from: {
			{'Useful Expressions'. {Utilities. #openUsefulExpressions}. 'a window full of useful expressions.'}.
			nil.
			{'VM Statistics'. {self. #vmStatistics}. 'obtain some intriguing data about the vm.'}.
			{'Space Left'. {self. #garbageCollect}. 'perform a full garbage-collection and report how many bytes of space remain in the image.'}
		}.
	^ menu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/20/2011 10:21'!
newMorph
	"The user requested 'new morph' from the world menu.  Put up a menu that allows many ways of obtaining new morphs.  If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used."
	| menu subMenu catDict shortCat class |
	menu _ self menu: 'Add a new morph'.
	menu

		add: 'From Clipboard'
		target: myHand
		action: #pasteMorph;
		
		add: 'From Alphabetical List'
		subMenu: self alphabeticalMorphMenu.
	menu addLine.
	Preferences classicNewMorphMenu ifTrue: [
		menu addLine.
		catDict _ Dictionary new.
		SystemOrganization categories do: [ :cat |
			((cat beginsWith: 'Morphic-') and: [ (#('Morphic-Menus' 'Morphic-Support' ) includes: cat) not ]) ifTrue: [
				shortCat _ (cat
					copyFrom: 'Morphic-' size + 1
					to: cat size).
				(SystemOrganization listAtCategoryNamed: cat) do: [ :cName |
					class _ Smalltalk at: cName.
					((class inheritsFrom: Morph) and: [ class includeInNewMorphMenu ]) ifTrue: [
						(catDict includesKey: shortCat)
							ifTrue: [ (catDict at: shortCat) addLast: class ]
							ifFalse: [
								catDict
									at: shortCat
									put: (OrderedCollection with: class) ]]]]].
		catDict keys sort do: [ :categ |
			subMenu _ MenuMorph new.
			((catDict at: categ) asArray sort: [ :c1 :c2 |
				c1 name < c2 name ]) do: [ :cl |
				subMenu
					add: cl name
					target: self
					selector: #newMorphOfClass:event:
					argument: cl ].
			menu
				add: categ
				subMenu: subMenu ]].
	self doPopUp: menu.! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/13/2012 21:51'!
openMenu
	"Build the open window menu for the world."

	| menu |
	menu _ self menu: 'Open...'.

	ExtraOpenCommands ifNotNil: [
		self fillIn: menu from: ExtraOpenCommands ].

	self fillIn: menu from: {
		{'Text Editor' . {self . #openTextEditor}. 'A window for composing text' }.
		{'Workspace' . {self . #openWorkspace}. 'A window for evaluating Smalltalk expressions' }.
		{'Browser' . { self . #openBrowser}. 'A Smalltalk code browser, for studying and modifying the system'}.
		{'Message Names' . { self . #openMessageNames} . 'A tool for finding and editing methods that contain any given keyword in their names.'}.
		{'Transcript' . {self . #openTranscript}. 'A window showing contents of the System Transcript' }.
		nil.
		{'Installed Packages' . { self . #openPackageList}. 'A tool for managing Packages (optional units of code) installed in the system'}.
		{'Change Sorter' . {self . #openChangeSorter1} . 'A tool allowing you to view the methods in a Change Set, especially changes to the Base System' }.
		nil.
		{ 'Process Browser' . { ProcessBrowserWindow . #openProcessBrowser } }.
		{ 'Emergency Evaluator'. { Transcripter. #emergencyEvaluator } }.
		nil.
		{'File List' . {self . #openFileList} . 'An explorer of the File System' }.
		{'SUnit Test Runner' . {TestRunnerWindow . #openTestRunner} . 'A tool allowing you to compare and manipulate two change sets concurrently' }.
	}.
	^menu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'HB 7/23/2014 16:11'!
preferencesMenu
	"Build the preferences menu for the world."

	^self fillIn: (self menu: 'Preferences...') from: {
		{'Themes...' . {Theme . #changeTheme} . 'switch to another theme.'}.
		{'Font Sizes...' . {Theme . #changeFontSizes} . 'use larger or smaller text and widgets'}.
		{'Icons...' . {Theme . #changeIcons} . 'show more or less icons.'}.
		nil.
		{'Show taskbar' . {Taskbar . #show} . 'show the taskbar'}.
		{'Hide taskbar' . {Taskbar . #hide} . 'hide the taskbar'}.
		nil.
		{'Full screen on' . { self . #fullScreenOn} . 'puts you in full-screen mode, if not already there.'}.
		{'Full screen off' . { self . #fullScreenOff} . 'if in full-screen mode, takes you out of it.'}.
		nil.
		{'Set display depth...' . {self. #setDisplayDepth} . 'choose how many bits per pixel.'}.
		{'Set desktop color...' . {self. #changeBackgroundColor} . 'choose a uniform color to use as desktop background.'}.
		nil.
		{'Set Code Author...'. {Utilities. #setAuthor}. 'supply initials to be used to identify the author of code and other content.'}.
		{'All preferences...'. {Preferences. #openPreferencesInspector}. 'view and change various options.'}.
		nil.
		{'Base active window on last mouse click'. {Preferences. #disableFocusFollowsMouse}. 'At all times, make the active window the one where the mouse was clicked.'}.
		{'Base active window on mouse position'. {Preferences. #enableFocusFollowsMouse}. 'At all times, make the active window the one on which the mouse is located.'}.
	}! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 6/3/2014 06:06'!
saveMenu

        | menu |

        menu _ self menu: 'Save options...'.
        ^self fillIn: menu from: { 
				{ 'Save'. { Smalltalk . #saveSession}.
					'save the current version of the image on disk'}.
				{ 'Save as...'. { Smalltalk . #saveAs}.
					'save the current version of the image on disk under a new name.'}.
				{ 'Save as New Version'. { Smalltalk . #saveAsNewVersion}.
					'give the current image a new version-stamped name and save it under that name on disk.'}.
"				{ 'Save and Quit'. { self. #saveAndQuit}.
					'save the current image on disk, and quit out of Cuis.'}."
        }! !


!TheWorldMenu methodsFor: 'mechanics' stamp: 'jmv 3/9/2011 13:39'!
menu: titleString
	"Create a menu with the given title, ready for filling"

	| menu |
	(menu _ MenuMorph entitled: titleString) 
		defaultTarget: self; 
		addStayUpIcons.
	self colorForDebugging: menu.
	^ menu
! !

!TheWorldMenu methodsFor: 'mechanics' stamp: 'jmv 5/20/2006 16:38'!
world: aWorld hand: aHand

	myWorld _ aWorld.
	myHand _ aHand.! !


!TheWorldMenu methodsFor: 'popups' stamp: 'RAA 5/26/2000 10:26'!
changesDo
	"Build the changes menu for the world."

	self doPopUp: self changesMenu! !

!TheWorldMenu methodsFor: 'popups' stamp: 'RAA 5/26/2000 10:26'!
debugDo

	self doPopUp: self debugMenu! !

!TheWorldMenu methodsFor: 'popups' stamp: 'RAA 6/12/2000 09:13'!
doPopUp: aMenu

	aMenu popUpForHand: myHand in: myWorld.
! !

!TheWorldMenu methodsFor: 'popups' stamp: 'RAA 5/26/2000 10:26'!
helpDo
	"Build and show the help menu for the world."

	self doPopUp: self helpMenu! !

!TheWorldMenu methodsFor: 'popups' stamp: 'RAA 5/26/2000 10:26'!
openWindow

	self doPopUp: self openMenu! !

!TheWorldMenu methodsFor: 'popups' stamp: 'jmv 8/28/2012 08:35'!
preferencesDo
	"Build and show the preferences menu for the world."

	self doPopUp: self preferencesMenu! !

!TheWorldMenu methodsFor: 'popups' stamp: 'jmv 6/3/2014 06:06'!
saveOptionsDo

	self doPopUp: self saveMenu! !


!TheWorldMenu methodsFor: 'windows & flaps menu' stamp: 'RAA 5/26/2000 10:27'!
windowsDo
	"Build the windows menu for the world."

	self doPopUp: self windowsMenu! !

!TheWorldMenu methodsFor: 'windows & flaps menu' stamp: 'jmv 5/20/2011 10:19'!
windowsMenu
        "Build the windows menu for the world."

	^ self fillIn: (self menu: 'Windows') from: {  
		{ 'Find Window' . { #myWorld . #findWindow: }. 'Presents a list of all windows; if you choose one from the list, it becomes the active window.'}.
		{ 'Find changed Browsers...' . { #myWorld . #findDirtyBrowsers: }. 'Presents a list of browsers that have unsubmitted changes; if you choose one from the list, it becomes the active window.'}.
		{ 'Find changed Windows...' . { #myWorld . #findDirtyWindows: }. 'Presents a list of all windows that have unsubmitted changes; if you choose one from the list, it becomes the active window.'}.
		nil.
		{ 'Find a Transcript' . { #myWorld . #findATranscript: }. 'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'}.
		{ 'Find a FileList' . { #myWorld . #findAFileList: }. 'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'}.
		{ 'Find a Change Sorter' . { #myWorld . #findAChangeSorter: }. 'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'}.
		{ 'Find Message Names' . { #myWorld . #findAMessageNamesWindow: }. 'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'}.
		nil.
		{ #staggerPolicyString . { self . #toggleWindowPolicy }. 'stagger: new windows positioned so you can see a portion of each one.
			tile: new windows positioned so that they do not overlap others, if possible.'}.
		nil.
		{ 'Collapse all Windows' . { #myWorld . #collapseAll }. 'Reduce all open windows to collapsed forms that only show titles.'}.
		{ 'Expand all Windows' . { #myWorld . #expandAll }. 'Expand all collapsed windows back to their expanded forms.'}.
		{ 'Close top Window' . { SystemWindow . #closeTopWindow }. 'Close the topmost window if possible.'}.
		{ 'Send top Window to back' . { SystemWindow . #sendTopWindowToBack  }. 'Make the topmost window become the backmost one, and activate the window just beneath it.'}.
		{ 'Move Windows onscreen' . { #myWorld . #bringWindowsFullOnscreen }. 'Make all windows fully visible on the screen'}.
		nil.
		{ 'Delete unchanged Windows' . { #myWorld . #closeUnchangedWindows }. 'Deletes all windows that do not have unsaved text edits.'}.
		{ 'Delete non Windows' . { #myWorld . #deleteNonWindows }. 'Deletes all non-window morphs lying on the world.'}.
		{ 'Delete Both of the Above' . { self . #cleanUpWorld }. 'deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'}.

	}! !


!TheWorldMenu methodsFor: 'menu' stamp: 'nk 2/15/2004 09:38'!
addGestureHelpItemsTo: aMenuMorph 
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'TheWorldMenu class' category: #'Morphic-Menus'!
TheWorldMenu class
	instanceVariableNames: ''!

!TheWorldMenu class methodsFor: 'services' stamp: 'jmv 11/4/2011 10:28'!
addExtraOpenCommands: anArray
	| labels |
	ExtraOpenCommands _ ExtraOpenCommands
		ifNil: [ anArray ]
		ifNotNil: [
			labels _ (anArray collect: [ :a | a first ]) asSet.
			anArray, { nil} , (ExtraOpenCommands reject: [ :any | any notNil and: [ labels includes: any first ]])]! !

MenuItemMorph initialize!