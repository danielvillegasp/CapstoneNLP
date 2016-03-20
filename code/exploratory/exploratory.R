library(tm)
library(NLP)
library(ggplot2)
library(tidyr)
library(dplyr)
library(igraph)
library(networkD3)
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

add_pstart_pstop_tags <- function(x){
  paste('PSTART',x,'PSTOP', sep=" ")
}

HCCcorpus <- read.HCC(base.path = '../../data/LOCALE', category=c('twitter', 'news', 'blogs'), n.lines = 100)
v.corpus <- unlist(HCCcorpus)

corpus <- data.frame(names=attributes(v.corpus),
                     text=v.corpus) %>%
  separate(names, into = c('locale', 'category'), sep='\\.') %>%
  mutate(category = sapply(category, gsub, pattern='[0-9]+', replacement=''),
         text=as.character(text))

vcorp <- VCorpus(x=VectorSource(corpus$text))
for(i in names(vcorp)){
  vcorp[[i]]$meta$origin <- corpus$category[as.numeric(i)]
}

vcorp <- tm_map(vcorp, stripWhitespace)
vcorp <- tm_map(vcorp, content_transformer(tolower))
vcorp <- tm_map(vcorp, removeWords, stopwords('english'))
vcorp <- tm_map(vcorp, stemDocument, language='english')
vcorp <- tm_map(vcorp, content_transformer(add_pstart_pstop_tags))
tk_corp <- tm_map(vcorp, content_transformer(MC_tokenizer))
tk_corp <- tm_map(tk_corp, content_transformer(function(x){x[x!=""]}))
bigram_corp <- tm_map(tk_corp, content_transformer(ngrams), 2)
trigram_corp <- tm_map(tk_corp, content_transformer(ngrams), 3)
qgram_corp <- tm_map(tk_corp, content_transformer(ngrams), 4)

unigrams <- data.frame(unigram=unlist(sapply(tk_corp, function(x){
  x$content
}))) %>%
  dplyr::filter(unigram!='PSTOP',unigram!='PSTART') %>%
  dplyr::group_by(unigram) %>%
  dplyr::summarise(cnt=n()) %>%
  ungroup() %>%
  mutate(w0=unigram, p = cnt / sum(cnt)) %>%
  select(w0, cnt, p) %>%
  dplyr::arrange(desc(cnt))

bigrams <- data.frame(bigram=unlist(sapply(bigram_corp, function(x){
  x$content
})))  %>%
  mutate(bigram=sapply(as.character(bigram), function(x) substr(x, 4, nchar(x)-2))) %>%
  separate(bigram, into=c('w0', 'w1'), sep = "\", \"") %>%
  dplyr::group_by(w0, w1) %>%
  dplyr::summarise(cnt=n()) %>%
  ungroup() %>%
  mutate(p = cnt / sum(cnt))

trigrams <- data.frame(trigram=unlist(sapply(trigram_corp, function(x){
  x$content
}))) %>%
  mutate(trigram=sapply(as.character(trigram), function(x) substr(x, 4, nchar(x)-2))) %>%
  separate(trigram, into=c('w0', 'w1', 'w2'), sep = "\", \"")  %>%
  dplyr::group_by(w0, w1, w2) %>%
  dplyr::summarise(cnt=n()) %>%
  ungroup() %>%
  mutate(p = cnt / sum(cnt))

ggplot(unigrams, aes(x=p)) + geom_histogram(binwidth = 0.001)
ggplot(bigrams, aes(x=p)) + geom_histogram()
ggplot(trigrams, aes(x=p)) + geom_histogram()

ggplot(unigrams[1:20,], aes(x=factor(w0, levels=w0[order(p,decreasing = T)]), y=p)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(bigrams[1:20,] %>% 
         mutate(bg=paste(w0, w1,sep="-")), aes(x=factor(bg,  levels=bg[order(p,decreasing = T)]), y=p)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(trigrams[1:20,] %>% 
         mutate(tg=paste(w0, w1, w2, sep="-")), aes(x=factor(tg,  levels=tg[order(p,decreasing = T)]), y=p)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

comparison <- rbind(
  data.frame(type=rep('unigram', nrow(unigrams)), p=unigrams$p),
  data.frame(type=rep('bigram', nrow(bigrams)), p=bigrams$p),
  data.frame(type=rep('trigram', nrow(trigrams)), p=trigrams$p)
)

ggplot(comparison, aes(x=p)) + geom_density() + facet_wrap(~type)

# graph
bigrams2 <- bigrams %>%
  arrange(desc(p))
bigrams2 <- bigrams2[sample(1:nrow(bigrams2), size = 300),]
words <- unique(c(unique(bigrams2$w0), unique(bigrams2$w1)))
nodes <- data.frame(id=1:length(words), node=words, group=rep(1, length(words)))
el <- data.frame(w0=sapply(bigrams2$w0, function(x) nodes$id[nodes$node==x]),
                 w1=sapply(bigrams2$w1, function(x) nodes$id[nodes$node==x]),
                 p=bigrams2$cnt)
forceNetwork(Links=el, Nodes=nodes, Source = 'w0',
             Target='w1', NodeID = 'node', 
             Group='group', Value='p')
#simpleNetwork(data.frame(bigrams2$w0, bigrams2$w1))
#plot(g1, edge.arrow.size=.4,vertex.label=NA)






