fun number_in_month(xs: (int*int*int) list ,month: int) =
    if null xs
    then 0
    else 
        if #2(hd xs) = month 
        then 1 + number_in_month(tl xs,month) 
        else  number_in_month(tl xs,month)
    
    
