(* You may not use the functions null, hd, tl, isSome, or valOf*)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
fun all_except_option(s:string,xs: string list) =
    case xs of
      [] => NONE
    | x::xs' => if same_string(x,s) then SOME(xs')
                else 
                    case all_except_option(s,xs') of  
                      NONE => NONE
                    | SOME lst => SOME (x::lst)

(* returns a string list *)               
fun get_substitutions1(substitutions:string list list ,s:string) = 
    case substitutions of
      [] => []
    | x::xs' => 
        case all_except_option(s,x) of
          NONE => get_substitutions1(xs',s)
        | SOME lst => lst @ get_substitutions1(xs',s) 

    
fun get_substitutions2(substitutions:string list list ,s:string) = 
    let
        fun aux(xs,s,acc) = 
            case xs of
              [] => acc
            | x::xs' => 
                case all_except_option(s,x) of
                  NONE => aux(xs',s,acc)
                | SOME lst => aux(xs',s,lst @ acc)
    in
        aux(substitutions,s,[])
    end

fun similar_names(substitutions:string list list,full_name) = 
    let val {first=x,middle=y,last=z} = full_name in
            let
                fun get_full_name_lst(xs:string list) = 
                    case xs of
                      [] => []
                    | x::xs' => {first=x,middle=y,last=z}::get_full_name_lst(xs')   
            in
                full_name::get_full_name_lst(get_substitutions2(substitutions,x))
            end
    end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
    
fun card_color(c:card) =
    case c of 
      (Clubs,_) => Black 
    | (Diamonds,_) => Red
    | (Hearts,_) => Red
    | (Spades,_) => Black   
   
fun card_value(c:card) = 
    case c of 
      (_,Num i) => i
    | (_,Ace) => 11
    | _ => 10
   
fun remove_card(cards:card list,c,e) =
    case cards of
      [] => raise e 
    | card::cards' => if card = c then cards'
                    else card::remove_card(cards',c,e)  
   
(* todo test remove card *)
   
fun all_same_color(cards: card list) =
    case cards of
      card::next_card::cards'=> card_color(card) = card_color(next_card) andalso all_same_color(next_card::cards')  
    | card::[] => true 
    | [] => true
   
fun sum_cards(cards: card list) = 
    let 
        fun aux(xs,acc) = 
            case xs of
              [] => acc
            | x::xs' => aux(xs',card_value(x) + acc)
    in
        aux(cards,0)
    end
    
fun score(cards: card list, goal:int) = 
    let
        val sum = sum_cards(cards)
        val preliminary_score = if (sum > goal) then 3 * (sum - goal) else goal - sum 
    in
        if all_same_color(cards) then preliminary_score div 2 
        else preliminary_score
    end
(* locally defined recursive helper function that takes several arguments that together represent the current state of the game *)
fun officiate(cards: card list, moves: move list,goal: int) =   
    let
        fun state(card_lst,moves,held_cards) = 
            case moves of
              [] => held_cards
            | (Discard c)::moves' => state(cards,moves',remove_card(held_cards,c,IllegalMove))
            | Draw :: moves' =>
                case card_lst of  
                  [] => held_cards
                | card::cards' => if sum_cards(card::held_cards) > goal 
                                  then card::held_cards 
                                  else card::state(cards',moves',held_cards)
    in
        score(state(cards,moves,[]),goal)
    end 
