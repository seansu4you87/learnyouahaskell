main = interact respondPalindromes

respondPalindromes = unlines . map mapPalindrome . lines
  where mapPalindrome x = if isPalindrome x then "palindrome" else "not a palindrome"
        isPalindrome x = x == reverse x
