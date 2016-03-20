read.HCC <- function(base.path, LOCALE="en_US", category, n.lines=-1L){
  paths <- lapply(LOCALE,function(loc){
    filename = paste(loc, category, "txt", sep=".")
    if(grepl("\\/$", base.path)){
      paste(base.path, paste(loc, filename, sep="/"), sep="")
    }else{
      paste(base.path, paste(loc, filename, sep="/"), sep="/")
    }
  })
  paths <- setNames(paths, LOCALE)
  files.HCC <- lapply(paths, function(pl){
    tmp<-lapply(pl, readLines, n=n.lines)
    setNames(tmp, category)
  })
  setNames(files.HCC, LOCALE)
}


tokenizer <- function(HCC.list, 
                      split="([[:punct:]]*[[:space:]]+[[:punct:]]*)|([[:punct:]]+)",
                      normalize=T){
  lapply(HCC.list, function(cat.list){
    lapply(cat.list, function(lines){
      if(normalize){
        tolower(sapply(lines, strsplit, split=split,USE.NAMES = F))
      }else{
        sapply(lines, strsplit, split=split,USE.NAMES = F)
      }
    })
  })
}

tag.profane <- function(tk.HCC.list, 
                        profanities){
  lapply(tk.HCC.list, function(loc){
    lapply(loc, function(cat){
      lapply(cat, function(s){
        p.list <- profanities[profanities%in%s]
        p.list
      })
    })
  })
}

eng<-read.HCC("../data/LOCALE/", c("en_US"), c("twitter"), 100)
tk.eng<-tokenizer(eng, split="([[:punct:]]*[[:space:]]+[[:punct:]]*)|([[:punct:]]+)")
prof <- readLines("../data/profanities.txt")
tag.profane(tk.eng, prof)
