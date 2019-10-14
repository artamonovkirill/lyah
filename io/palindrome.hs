main = interact $ unlines . map convert . lines

convert :: String -> String
convert x = if isPalindrome x then "palindrome" else "not a palindrome"

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x