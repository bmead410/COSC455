def doubler(x: Int) : Int = x *2

val myList = List( 1, 2, 3, 4, 5)

//takes another function as a parameter
myList.map(doubler)

//map takes some function and applies it to each
//element in a list

//pass an anonymous func. into map. map applies
//it to each element of the list
myList.map(x => x + 1)

println(myList)
//list doesn't change. list is immutable

//create list the old fashioned way
List('a', 'b', 'c')
List(1, 2, 3, 4)

//Lists are immutable
val myList2 = List(10, 20, 30)

//create list new lazy way
val turtles = "I like turtles"
//can also say .toList

// Lets try to change turtles

//"Hello" :: turtles
//List that has items of type any. mixed data types
//polymorphic
123 :: turtles

//cannot manipulate turtles because immutable

//use scala.collection.mutable

123 :: turtles
List()
List()  == Nil //making it nothing

List[String]()
//can create lists using cons ( or  :: ) operator
//cons oper. takes cons element :: a list
123 :: Nil

"ABC" :: Nil
"abc" :: "def" :: Nil

//three important functions
//head, tail, isEmpty

val wisc = "Wisconsin".toList
wisc head //first element
//wisc.tail.head   second element head of tail
//wisc.tail //should return everything but the first element


def removeThird(myList : List[Any]) : List[Any] = {
  if(myList.isEmpty) Nil
  else
    (myList.head) :: (myList.tail.head) :: (myList.tail.tail.tail)
}


removeThird(wisc)

wisc take 4 //takes first four
wisc drop 4 //drops first four

//moreThan42 only returns ints greater than 42

def moreThan42 (myList : List [Int] : List [Int]) = myList.filter ( >42) {

}