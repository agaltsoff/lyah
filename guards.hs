
bmiTell :: Int -> String
bmiTell bmi
    | bmi < 19 = "Emo"
    | bmi < 25 = "OK"
    | bmi < 30 = "Fat"
    | otherwise = "Fat-Fat"