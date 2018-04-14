#title: hw04-archive-functions
#desceuption:writing function,documenting functions with Roxygen comments, using conditionals,using loops
#author: Molly Li
#date: 4/4/2018

library(XML)
library(stringr)


tbl_html <- readHTMLTable('http://cran.r-project.org/src/contrib/Archive/stringr')
tbl_html



#remove NA
tbl_html$`NULL`
df <- tbl_html$`NULL`
dat <- na.omit(df)[-1,]


#1.1) read archive
read_archive <- function(x){
  a <- str_replace('http://cran.r-project.org/src/contrib/Archive/stringr', "stringr", x)
  b <- readHTMLTable(a)
  c <- b$`NULL`
  d <- na.omit(c)[-1,]
  return(d)
}

raw_data <- read_archive('stringr')
raw_data


#1.2 Data Cleaning

version_names <- function (x) {
  name1 <- str_split(read_archive(x)$Name, pattern = "_")
  name2 <- lapply(name1, function(x) x[1])
  name3 <- as.character(unlist(name2))
  return(name3)
}

version_names('stringr')

version_numbers <- function(x) {
  name1 <- str_split(read_archive(x)$Name, pattern = "_")
  number1 <- lapply(name1, function(x) x[2])
  number2 <- str_sub(number1, start = 1, end = -8)
  return(number2)
}
version_numbers('stringr')

version_dates <- function(x) {
  date1 <- str_sub(read_archive(x)$"Last modified", start = 1, end = 10)
  date2 <- as.Date(date1)
  return(date2)
}
version_dates('stringr')

version_sizes <- function(x) {
  size1 <- which(str_detect(read_archive(x)$Size, "M"))
  size2 <- as.numeric(str_replace(read_archive(x)$Size, 'M|K', ''))
  size2[size1] <- size2[size1]*1024
  return(size2)
}
version_sizes('stringr')

clean_archive <- function(x) {
  name <- version_names(x)
  version <- version_numbers(x)
  date <- version_dates(x)
  size <- version_sizes(x)
  clean1 <- data.frame(name, version, date, size)
  return(clean1)
}

clean_data <- clean_archive("stringr")
clean_data

# 1.3) Timeline plot
# write a function plot_archive() to visualize the timeline with the version sizes of a package.
library(ggplot2)
plot_archive <- function(x) {
  ggplot(data = x, mapping = aes(date, size, group = 1)) +
    geom_point(aes(date, size, col = "steelblue2")) +
    geom_step(aes(date, size, col = "steelblue2")) +
    ggtitle(paste0(x$name[1],': timeline of version sizes')) +
    xlab('date') +
    ylab('Size(kilotypes)')
}
plot_archive(clean_data)


