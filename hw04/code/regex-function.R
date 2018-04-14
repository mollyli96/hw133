#title: hw04-regex-functions
#desceuption:writing function,documenting functions with Roxygen comments, using conditionals,using loops
#author: Molly Li
#date: 4/10/2018

library(stringr)

#2.1) split_chars


split_chars <- function(x){
  unlist(str_split(x, pattern = ""))
}


split_chars('Go Bears!')
split_chars('Expecto Patronum')


#2.2) Number of Vowels

num_vowels <- function(x){
  vowels <- c(a=0,e=0,i=0,o=0,u=0)
  for(i in 1:5)
  vowels[i] <- length(grep(names(vowels[i]), x,ignore.case=T))
  return(vowels)
}

vec <- c('G', 'o', ' ', 'B', 'e', 'a', 'r', 's', '!') 
num_vowels(vec)

#2.3) Counting Vowel

count_vowels <- function(x){
  x <- tolower(x)
  return(num_vowels(split_chars(x)))
}

count_vowels("The quick brown fox jumps over the lazy dog")
count_vowels("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG")

#2.4) Reversing Characters

reverse_chars <- function(x){
  paste(rev(split_chars(x)), collapse = "")
}

reverse_chars("gattaca")
reverse_chars("Lumox Maxima")


#2.5) Reversing Sentences by Words

reverse_words <- function(x){
  breaks <- unlist(sapply(x,strsplit,""))
  len <- length(breaks)
  new <- vector(length=len)
  for(i in 1:len){
    new[i]=breaks[len-i+1]
  }
  paste(new,collapse = "")
}

reverse_words("sentence! this reverse")
reverse_words("string")

