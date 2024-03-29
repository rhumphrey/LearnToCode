heading := method(title, do(
    (title size) repeat("-" print)
    "" println
    title println
    (title size) repeat("-" print)
    "" println
    )
)
// Working through and experimenting with the Io tutorial contents - https://iolanguage.org/tutorial.html

// Math
heading("Math")
(1 + 1) println
(1 + 1 == 2) println
2 sqrt println
-1.34 abs println
1 type println
Number slotNames sort println

// Variables
heading("Variables")
a := 1
a println
b := 2 * 3
(a + b) println

// Conditionals
heading("Conditionals")
a := 2
(a == 1) ifTrue("a is one" println) ifFalse("a is not one" println)
a = 1
(a == 1) ifTrue("a is one" println) ifFalse("a is not one" println)
a = 2
if(a == 1, writeln("a is one"), writeln("a is not one"))
a = 1
if(a == 1, writeln("a is one"), writeln("a is not one"))

// Lists
heading("Lists")
list := List clone append(30, 10, 5, 20)
list println
list size println
list := list sort println
list first println
list last println
list at(2) println
list remove(30) println
list atPut(1, 123) println
list(30, 10, 5, 20) select(>10) println
list(30, 10, 5, 20) detect(>10) println
list(30, 10, 5, 20) detect(>30) println
list(30, 10, 5, 20) map(*2) println
list(30, 10, 5, 20) map(v, v*2) println

// Loops
heading("Loops")
for(i, 1, 10, write(i, " "))
"" println
list foreach(i, v, writeln(i, ": ", v))
list("abc", "def", "ghi") foreach(println)

// Dictionaries
heading("Dictionaries")
dict := Map clone    
dict atPut("hello", "a greeting")   
dict atPut("goodbye", "a parting")   
dict hasKey("hello") println
dict hasValue("a greeting") println
dict at("hello") println
dict keys println
dict foreach(key, value, (key .. ": " .. value) println)
dict asObject println
dict asList println
dict size println

// Strings
heading("Strings")
a := "Henlo " println
b := "Frendo!" println
c := a .. b 
c println
c at(0) println
c at(0) asCharacter println
sentence := "this is a test" println
words := sentence split(" ", "\t")  println
words join(" ") println
sentence findSeq("is") println
sentence findSeq("test") println
sentence exSlice(10) println
sentence exSlice(2, 10) println

// Objects
heading("Objects")
Contact := Object clone
Contact println
Contact type println
Contact proto type println
Contact name ::= nil
Contact address ::= nil
Contact city ::= nil
Contact println
holmes := Contact clone setName("Holmes") setAddress("221B Baker St") setCity("London")
holmes println
holmes slotNames println
Contact fullAddress := method(list(name, address, city) join("\n"))
holmes fullAddress println
holmes getSlot("fullAddress") println
// Defining Objects with do()
Contact := Object clone do(
	name ::= nil
	address ::= nil
	city ::= nil
	fullAddress := method(list(name, address, city) join("\n"))
)
Contact println

// Inheritance
heading("Inheritance")
BusinessContact := Contact clone do(
	companyName ::= ""
	fullAddress := method(
		list(companyName, "Care of: " .. name, address, city) join("\n")
	)
)
BusinessContact println
steve := BusinessContact clone do(
	setName("Steve") 
	setCompanyName("Apple Inc.") 
	setAddress("1 Infinite Loop")
	setCity("Cupertino")
)
steve println
steve fullAddress println

// Lazy Evaluation
heading("Lazy Evaluation")
assert := method(
	call sender doMessage(call message argAt(0)) ifFalse(
		Exception raise("failed assertion: " .. call message asString)
	)
)
// Change line 143 to assert(1 == 3) to see the resulting message
"Note: change line 143 to assert(1 == 3) to see the resulting message" println
assert(1 == 1)

// Introspection
heading("Introspection")
Address := Object clone do(
	fields ::= list("name", "street", "city", "state", "zipCode")

	init := method(
		fields foreach(key, 
			if (self hasSlot(key) not,
				self newSlot(key, nil)
			) 
		)
	)

	emptyFields := method(
		fields select(k, self getSlot(k) == nil)
	)

	isValid := method(errors size == 0)

	assertValid := method(
		if (emptyFields size, 
			Exception raise(
			   self type .. " missing: " .. emptyFields join(", ")
			)
		)
	)
)

anAddress := Address clone setName("Alan") setStreet("6502 Mem Ln")
// Uncomment line 176 to see the resulting message
"Note: Uncomment line 176 to see the resulting message" println
// anAddress assertValid

// Namespace
heading("Namespace")
Lobby slotNames println
Lobby Protos slotNames println
Lobby protos Core slotNames println
Lobby protos Core Date slotNames println

// Code as data
heading("Code as data")
mSlot := method(a(b) + c)
getSlot("mSlot") message println
getSlot("mSlot") message next println
getSlot("mSlot") message name println
getSlot("mSlot") message setName("Doggo")
getSlot("mSlot") message name println
getSlot("mSlot") message arguments println
Message slotNames println

// Exceptions
heading("Exceptions")
e := try(
    anAddress assertValid
)

e catch(Exception,
    writeln("Caught: ", e error, "\nstack:\n", e coroutine backTraceString)
)