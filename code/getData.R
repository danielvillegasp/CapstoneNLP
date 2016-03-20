library(tm)
read.HCC <- function(base.path, LOCALE="en_US", category, n.lines=-1L){
  require(plyr)
  files = list()
  for(loc in LOCALE){
    for(cat in category){
      path = paste(base.path, loc, 
                   paste(loc, cat, 'txt', sep='.'), 
                   sep='/')
      files[[path]] <- data.frame(origin=cat, 
                                  language=loc, 
                                  document=readLines(path,n = n.lines))
    }
  }
  ldply(files, data.frame)
}

add_pstart_pstop_tags <- function(x){
  paste('PSTART',x,'PSTOP', sep=" ")
}

HCCcorpus <- read.HCC(base.path = '../data/LOCALE',
                      category=c('twitter', 'news', 'blogs'))
n<-VCorpus(DataframeSource(data.frame(origin=c('t','n','b'), document=c('cfr', 'rfc', 'fr'))))
