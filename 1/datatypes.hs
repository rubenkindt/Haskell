data Name = Nnname String
data Pair = Ppair Int Int
data Gender = Female | Male | Other
data Age = Aage Int
data Person = Name Age Gender

stringToGender :: String -> Gender
stringToGender x
  | x=="Male" = Male
  | x=="Female"= Female
  | otherwise = Other

genderToString :: Gender -> String
genderToString Male=  "Male"
genderToString Female=  "Femail"
genderToString Other=  "Other"
