
zipLists xs ys = 
    if xs == [] || ys == [] then []
    else (head xs, head ys) : zipLists tail xs tail ys
    
    