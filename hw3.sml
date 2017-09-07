fun only_capitals(lst:string list) =
    List.filter (fn str => Char.isUpper(String.sub(str,0))) lst 

fun longest_string1(lst:string list) =
    foldl (fn (str,acc) => if String.size(str) > String.size(acc) then str else acc) "" lst 

fun longest_string2(lst:string list) =
    foldl (fn (str,acc) => if String.size(str) >= String.size(acc) then str else acc) "" lst 
 
(* type (int * int -> bool) -> string list -> string *)
fun longest_string_helper f lst = 
    foldl (fn (str,acc) => if f(String.size(str),String.size(acc)) then str else acc) "" lst


val longest_string3 = longest_string_helper (fn(x,y) => x > y) 
val longest_string4 = longest_string_helper (fn(x,y) => x >= y) 

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
    
fun first_answer f lst = 
    case lst of
        [] => raise NoAnswer
        |  x::xs' => case f(x) of
                        SOME v => v
                        | NONE => first_answer f xs'
   
fun all_answers f lst = 
    let 
        fun aux (x,acc) = 
            case (f(x),acc) of 
                (_,NONE) => NONE
                | (SOME u,SOME v) => SOME (u @ v)
                | (NONE,_) => NONE
    in 
        foldl aux (SOME[]) lst
    end
                      
fun count_wildcards(pat:pattern) = 
    g (fn()=>1) (fn str => 0) pat
    
fun count_wild_and_variable_lengths(pat:pattern) = 
    g (fn()=>1) (fn str => String.size str) pat 
    
fun count_some_var(str:string,pat:pattern) = 
    g (fn()=>0) (fn s => if str = s then 1 else 0) pat
    
fun check_pat (pat:pattern) = 
    let 
        fun get_var_list(pat:pattern) = 
            case pat of
                Variable x => [x]
                | TupleP ps => List.foldl (fn(p,acc) => get_var_list(p) @ acc) [] ps
                | ConstructorP(_,p) => get_var_list(p)
                | _ => []
        fun check_lst(lst) = 
            case lst of 
                [] => true
                | x::xs' => List.all (fn y => y<>x) xs' andalso check_lst(xs')
    in
        check_lst(get_var_list(pat))
    end
    
fun match(v:valu,pat:pattern) = 
    case (v,pat) of 
        (_,Wildcard) => SOME([])
        | (_,Variable s) => SOME([(s,v)])
        | (Unit,UnitP) => SOME([])
        | (Const i,ConstP j) => if i=j then SOME([]) else NONE
        | (Tuple vs,TupleP ps) => all_answers (fn (x:valu,y:pattern) => match(x,y)) (ListPair.zip(vs,ps)) 
        | (Constructor(s1,v),ConstructorP(s2,p)) => if s1=s2 then match(v,p) else NONE  
        | _  => NONE 

fun first_match v pat  = 
    SOME(first_answer (fn (v,p) => match(v,p)) (List.map (fn p => (v,p)) pat)) 
    handle NoAnswer => NONE  

