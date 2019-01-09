Xor:*->*->*
Unit:*
Prod:*->*->*
Fix:(*->*)-*
Int:*
Bool:*
Num:*

List = \x-> Fix(\list-> Xor(Prod(x,list),Unit))
Stack = \x-> Fix(\stack -> Xor(Prod(stack,x),Unit))
Maybe = \x-> Xor(x,Unit)
List = \x-> Xor(Prod(x,List(x)),Unit)
Either = \x -> \y -> Xor(x,y)
Tuple2 = \x -> \y -> Prod(x,y)

struct X {
	name:String
	age:Int
	address:String
}
XN
XNames = prod(String,String)
X = Prod(String,Prod(Int,String))