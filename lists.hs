
sumList ls = 
    if ls == [] then 0
    else (head ls) + sumList(tail ls)
    
flipList ls = 
    if ls == [] then []
    else last ls : flipList(init ls)
    
maxList ls = 
    if length ls == 1 then head ls
    else
        if head ls > maxList(tail ls) then head ls
        else maxList(tail ls)
        
multLists ls ns = 
    if ls == [] || ns == [] then []
    else ((head ls) * (head ns)) : multLists (tail ls) (tail ns)
    
oeList n = [ [x,y] | let ns = take n [1..], x <- ns, odd x, y <- ns, even y ]        