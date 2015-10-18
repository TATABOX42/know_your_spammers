# ***********************************************
# ***********************************************
# Author: Benjamin Tovar
# Date: October, 2015
# Post: http://btovar.com/2015/10/spam-comment-analysis-in-r/ ‎
# ***********************************************
# ***********************************************

# library(reshape)
library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)
library(rjson)
library(gridExtra)
library(pheatmap)

# ****************************
# load data 
# ***************************

# read dump of spam messages
d <- read.delim("data.csv",header=TRUE,sep=",")
# match ip date
# extracted from http://software77.net/geo-ip/multi-lookup/
ipd <- read.delim("country_and_ip.csv",header=TRUE,sep=",")
# merge new dataset
d <- merge(d,ipd,by="comment_author_IP")
# keep useful columns (features)
d <- d %>% select(comment_ID,
                  comment_author,
                  comment_author_email,
                  comment_author_IP,
                  ip_country_code,
                  ip_country_name,
                  comment_date,
                  comment_content)
# order d based on comment_ID
d <- d[order(d$comment_ID),]

# *************************************
# compute Delta in hours
# this is, difference in hours
# from one message to the previous one
# *************************************

get_hour_delta <- function(xdate) {
  l <- length(xdate)
  z <- numeric(l-1)
  for (i in 1:(l-1)) {
    z[i] <- as.numeric(difftime(xdate[i+1],xdate[i],units="hour"))
  }
  return(z)
}

d$diff_hours_per_message <- c(0,get_hour_delta(d$comment_date))

# *************************************
# extract email domain
# *************************************

d$comment_author_email_domain <- gsub("\\..*","",gsub("^.*\\@","",d$comment_author_email))

# *******************************
# explore how many unique entries 
# per feature 
# *******************************

d1 <-  apply(d,2,function(feature) 
        sum(ifelse(table(feature)> 1,TRUE,FALSE)) ) 
d1 <- data.frame(feature=names(d1),unique_entries=as.numeric(d1))  

# > d1
#                        feature unique_entries
# 1                   comment_ID              0
# 2               comment_author             14
# 3         comment_author_email              0
# 4            comment_author_IP             23
# 5              ip_country_code              8
# 6              ip_country_name              8
# 7                 comment_date              0
# 8              comment_content              0
# 9       diff_hours_per_message              0
# 10 comment_author_email_domain             17

# plot result
pdf("unique_entries_per_feature.pdf",width=8,height=6)
  d1 %>% 
    filter(unique_entries>0) %>%
      ggplot(aes(x=feature,y=unique_entries)) +
        geom_bar(stat="identity",aes(fill=feature),alpha=0.6) +
          coord_flip() + 
            theme(legend.position="none") +
              labs(title="Feature with more than one repeated entry",
                x="Feature",y="Unique entries per feature")
dev.off()

# ***********************
# study spammer features
# ***********************

# set min threshold of repetitions
min_threshold <- 2

# **********************
# comment_author analysis
# **********************

p1 <-  
  d %>%
    group_by(comment_author) %>%
      summarise(n_comment_author = n()) %>% 
        filter(n_comment_author > min_threshold) %>%
          ggplot(aes(x=comment_author,y=n_comment_author)) +
            geom_bar(stat="identity",aes(fill=comment_author),alpha=0.5) +
              coord_flip() + 
                theme(legend.position="none") +
                  labs(title="Frequency of messages by comment_author",
                    y="Number of messages",x="comment_author")

p2 <-  
  d %>%
    group_by(comment_author) %>%
      summarise(mean_hours_per_message=mean(diff_hours_per_message),
                n_messages = n(),
                n_comment_author = n()) %>%
        filter(n_comment_author > min_threshold) %>%
          ggplot(aes(x=factor(comment_author),y=mean_hours_per_message)) +
            geom_bar(stat="identity",aes(fill=comment_author),alpha=0.5) +
              theme(legend.position="none") +
                labs(title="Average difference (hours)\nbetween messages per comment_author",
                  x="comment_author",y="AVG(hours)") +
                  coord_flip()

pdf("message_stats_by_comment_author.pdf",width=7,height=9)
  grid.arrange(p1,p2,ncol=1)
dev.off()

# **********************
# comment_author_IP analysis
# **********************

p1 <-  
  d %>%
    group_by(comment_author_IP) %>%
      summarise(n_comment_author_IP = n()) %>% 
        filter(n_comment_author_IP > min_threshold) %>%
          ggplot(aes(x=comment_author_IP,y=n_comment_author_IP)) +
            geom_bar(stat="identity",aes(fill=comment_author_IP),alpha=0.5) +
              coord_flip() + 
                theme(legend.position="none") +
                  labs(title="Frequency of messages by comment_author_IP",
                    y="Number of messages",x="comment_author_IP")

p2 <-  
  d %>%
    group_by(comment_author_IP) %>%
      summarise(mean_hours_per_message=mean(diff_hours_per_message),
                n_messages = n(),
                n_comment_author_IP = n()) %>%
        filter(n_comment_author_IP > min_threshold) %>%
          ggplot(aes(x=factor(comment_author_IP),y=mean_hours_per_message)) +
            geom_bar(stat="identity",aes(fill=comment_author_IP),alpha=0.5) +
              theme(legend.position="none") +
                labs(title="Average difference (hours)\nbetween messages per comment_author_IP",
                  x="comment_author_IP",y="AVG(hours)") +
                  coord_flip()

pdf("message_stats_by_comment_author_IP.pdf",width=7,height=9)
  grid.arrange(p1,p2,ncol=1)
dev.off()



# **********************
# ip_country_name analysis
# **********************

p1 <-  
  d %>%
    group_by(ip_country_name) %>%
      summarise(n_ip_country_name = n()) %>% 
        filter(n_ip_country_name > min_threshold) %>%
          ggplot(aes(x=ip_country_name,y=n_ip_country_name)) +
            geom_bar(stat="identity",aes(fill=ip_country_name),alpha=0.5) +
              coord_flip() + 
                theme(legend.position="none") +
                  labs(title="Frequency of messages by ip_country_name",
                    y="Number of messages",x="ip_country_name")

p2 <-  
  d %>%
    group_by(ip_country_name) %>%
      summarise(mean_hours_per_message=mean(diff_hours_per_message),
                n_messages = n(),
                n_ip_country_name = n()) %>%
        filter(n_ip_country_name > min_threshold) %>%
          ggplot(aes(x=factor(ip_country_name),y=mean_hours_per_message)) +
            geom_bar(stat="identity",aes(fill=ip_country_name),alpha=0.5) +
              theme(legend.position="none") +
                labs(title="Average difference (hours)\nbetween messages per ip_country_name",
                  x="ip_country_name",y="AVG(hours)") +
                  coord_flip()

pdf("message_stats_by_ip_country_name.pdf",width=7,height=9)
  grid.arrange(p1,p2,ncol=1)
dev.off()


# **********************
# comment_author_email_domain analysis
# **********************

p1 <-  
  d %>%
    group_by(comment_author_email_domain) %>%
      summarise(n_comment_author_email_domain = n()) %>% 
        filter(n_comment_author_email_domain > min_threshold) %>%
          ggplot(aes(x=comment_author_email_domain,y=n_comment_author_email_domain)) +
            geom_bar(stat="identity",aes(fill=comment_author_email_domain),alpha=0.5) +
              coord_flip() + 
                theme(legend.position="none") +
                  labs(title="Frequency of messages by comment_author_email_domain",
                    y="Number of messages",x="comment_author_email_domain")

p2 <-  
  d %>%
    group_by(comment_author_email_domain) %>%
      summarise(mean_hours_per_message=mean(diff_hours_per_message),
                n_messages = n(),
                n_comment_author_email_domain = n()) %>%
        filter(n_comment_author_email_domain > min_threshold) %>%
          ggplot(aes(x=factor(comment_author_email_domain),y=mean_hours_per_message)) +
            geom_bar(stat="identity",aes(fill=comment_author_email_domain),alpha=0.5) +
              theme(legend.position="none") +
                labs(title="Average difference (hours)\nbetween messages per comment_author_email_domain",
                  x="comment_author_email_domain",y="AVG(hours)") +
                  coord_flip()

pdf("message_stats_by_comment_author_email_domain.pdf",width=7,height=9)
  grid.arrange(p1,p2,ncol=1)
dev.off()

# **********************
# correlations between
# country and email domain
# **********************

# compute a contingency matrix
M <- as.matrix(table(d$ip_country_name,d$comment_author_email_domain))
# compute a distance matrix (country vs country given email domain)
D1 <- as.matrix(dist(M))
# compute a distance matrix (email domain vs email domain given country)
D2 <- as.matrix(dist(t(M)))

# display heatmaps
pheatmap(M,main="Contingency matrix of country vs email domain",
          display_numbers=TRUE,number_format="%d",filename="heatmap_M.pdf")
pheatmap(D1,main="Distance matrix country vs country given email domain",
          display_numbers=TRUE,number_format="%d",filename="heatmap_D1.pdf")
pheatmap(D2,main="Distance matrix email domain vs email domain given country",
          display_numbers=TRUE,number_format="%d",filename="heatmap_D2.pdf")


# ******************************
# ******************************
# text analytics 
# ******************************
# ******************************

# change class of comments
d$comment_content <- as.character(d$comment_content)

# ****************************
# Pre-process data
# ***************************

# get unique class name list
unique_labels <- unique(d$ip_country_name)
# merge documents that match 
# certain class into a list object
dataset_s <- sapply(unique_labels,function(label) 
              list( d[d$ip_country_name %in% label,"comment_content"] ) )
names(dataset_s) <- unique_labels
# double check that we are including each document into each list index
as.data.frame(lapply(dataset_s,length))
#   United.States Italy Ukraine Germany Canada France Taiwan Sweden
# 1            59     7       2      28      2     44      1      1
#   Russian.Federation China Estonia Virgin.Islands
# 1                  2     2       1              1

# keep only countries > min_threshold
dataset_s <- dataset_s[ifelse(as.data.frame(lapply(dataset_s,length))>min_threshold, TRUE, FALSE)]

as.data.frame(lapply(dataset_s,length))
# > as.data.frame(lapply(dataset_s,length))
#   United.States Italy Germany France
# 1            59     7      28     44

# update unique labels
unique_labels <- names(dataset_s)

# ****************************
# Compute document corpus 
# to make "text mining"
# ***************************

# convert each list content into a corpus
dataset_corpus <- lapply(dataset_s, function(x) Corpus(VectorSource( toString(x) ))) 
# merge all documents into one single corpus
dataset_corpus_all <- dataset_corpus[[1]]
for (i in 2:length(unique_labels)) {
  dataset_corpus_all <- c(dataset_corpus_all,dataset_corpus[[i]])
}

# ****************************
# text pre-processing
# ***************************

# remove punctuation
dataset_corpus_all <- tm_map(dataset_corpus_all, removePunctuation)
# remove numbers
dataset_corpus_all <- tm_map(dataset_corpus_all, removeNumbers)
# remove stopwords
dataset_corpus_all <- tm_map(dataset_corpus_all, function(x) removeWords(x,stopwords("english")))

# set a list of words to remove 
# which I found out appears many times 
# in all documents, but does not say "much"
# about document class.
# feel free to comment next lines
words_to_remove <- c("said","from","what","told","over","more","other","have","last",
                    "with","this","that","such","when","been","says","will","also",
                     "where","why","would","today")
dataset_corpus_all <- tm_map(dataset_corpus_all, removeWords, words_to_remove)


# ****************************
# compute term matrices M[i,j]
# where:
# [i,j] <- frequency of word i in document class j
# ***************************

# compute term matrix
document_tm <- TermDocumentMatrix(dataset_corpus_all)
# convert to matrix class
document_tm_mat <- as.matrix(document_tm)
colnames(document_tm_mat) <- unique_labels
# compute dimensions of matrix
dim(document_tm_mat)

document_tm_clean <- removeSparseTerms(document_tm, 0.8)
# convert to matrix class
document_tm_clean_mat <- as.matrix(document_tm_clean)
colnames(document_tm_clean_mat) <- unique_labels
# compute dimensions of matrix
dim(document_tm_clean_mat)


# ****************************
# For extra quality control
# remove words in term matrix
# with length < 4
# ***************************

# compute row index
index <- as.logical(sapply(rownames(document_tm_clean_mat), function(x) (nchar(x)>3) ))
# extract new term matrix
document_tm_clean_mat_s <- document_tm_clean_mat[index,]
# compute dimensions of matrix
dim(document_tm_clean_mat_s)

# ****************************
# compute comparison 
# word clouds
# ***************************

# FROM: ?comparison.cloud
# 
#      Let p_{i,j} be the rate at which word i occurs in document j, and
#      p_j be the average across documents(sum_ip_{i,j}/ndocs). The size
#      of each word is mapped to its maximum deviation (
#      max_i(p_{i,j}-p_j) ), and its angular position is determined by
#      the document where that maximum occurs.


# first top 2000 discriminant words
pdf("comparison_cloud_top_2000_words.pdf",height=8,width=8)
  comparison.cloud(document_tm_clean_mat_s,
                  max.words=2000,
                  random.order=FALSE,c(4,0.4),
                  title.size=1.4)
dev.off()

# ****************************
# compute commonality 
# word clouds
# ***************************

# FROM: ?commonality.cloud
#   Plot a cloud of words shared across documents

pdf("commonality_cloud.pdf",height=3,width=3)
  commonality.cloud(document_tm_clean_mat_s,
                  max.words=50,
                  random.order=FALSE)
dev.off()



