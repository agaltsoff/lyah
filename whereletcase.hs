
showFats :: [(String, Double, Double)] -> [String]
showFats xs = [ n | (n, w, h) <- xs, let bmi = w/h^2, bmi > 25.0] 

caseAB a b =
    case a == b of
        True -> "a equals b"
        _ -> "a not equals b"