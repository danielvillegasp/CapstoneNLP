# 1. 
file.size("../data/LOCALE/en_US/en_US.blogs.txt") / 1e6

# 2.
length(readLines("../data/LOCALE/en_US/en_US.twitter.txt"))

# 3.
files <- list(readlines("../data/LOCALE/en_US/en_US.blogs.txt"),
          readlines("../data/LOCALE/en_US/en_US.news.txt"),
          readlines("../data/LOCALE/en_US/en_US.twitter.txt"))
files<-setNames(files, c("blogs", "news", "tweets"))
sapply(files, function(l){
  max(sapply(l, nchar))
})

# 4.
round(sum(grepl("love", files[[3]])) / sum(grepl("hate", files[[3]])))

# 5.
files[[3]][grepl("biostats", files[[3]])]

# 6.
sum(grepl("^A computer once beat me at chess, but it was no match for me at kickboxing$",
    files[[3]]))