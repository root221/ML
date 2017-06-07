fun only_capitals(lst:string list) =
    List.filter (fn str => Char.isUpper(String.sub(str,0))) lst 

fun longest_string1(lst:string list) =
    foldl (fn (acc,str) => if str > acc then str else acc) "" lst 

fun longest_string2(lst:string list) =
    foldl (fn (acc,str) => if acc<=str then str else acc) "" lst 
 
fun longest_capitalized(lst:string list) = 
    (longest_string1 o only_capitals) lst
    
fun rev_string(str:string) = 
    (implode o rev o explode)  str
   
exception NoAnswer
datatype pattern = Wildcard
    | Variable of string
    | UnitP
    | ConstP of int
    | TupleP of pattern list
    | ConstructorP of string * pattern

datatype valu = Const of int
    | Unit
    | Tuple of valu list
    | Constructor of string * valu

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            Wildcard          => f1 ()
            | Variable x        => f2 x
            | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
            | ConstructorP(_,p) => r p
            | _                 => 0
    end
    
fun first_answer(f) = 
    let
        fun name(lst) = 
            case lst of
                [] => raise NoAnswer
                |  x::xs' => 
                    case f(x) of
                        SOME v => v
                        | NONE => name xs'
    in
        name
    end
