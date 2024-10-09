# A differenct kind of dendrogram 
#the purpose of this is to cluster the available stands 
#using correlation-based clustering. 
#this will tell us which stands grow together and grow lean togehter, 
#as the global climate changes 
library(tidyverse)
library(dplR)
library(tidyverse)

rwis <- readRDS("~/Desktop/DENDRO PROJECT/itrdbTrends/dataVault/rwis.rds")
#here I need a vector of names to keep all of the data organised 
#Now I save the chronologies, for now I will just be using the mean detrending method 
chronologies<- lapply(seq_along(rwis$AgeDepSpline), function(i) {
  print(paste("Building Chronology #:", i))
  #I remove stands with insufficient data.
  if (length(rwis$AgeDepSpline[[i]]) == 1) {
    print("Recent data is insufficient")
    return(NA)
  }
  rwi <- rwis$AgeDepSpline[[i]]
  #here I don't use bieweight but I do use prewiten. This is an arbitrary choice
  chron(rwi, biweight = F, prewhiten = TRUE)
})

saveRDS(chronologies, file = "dataVault/chronologies.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)


stand_id<-rownames(meta)

str(chronologies)
insuf_data<-lapply(seq_along(chronologies), function(i){
  is.atomic(chronologies[[i]])
  })
insuf_data<-unlist(insuf_data)


#first we haveto put our chronologies in a matrix, which requires lining them up and filling in the missing years with NAs, 
oldest_growth<-c()
youngest_growth<-c()
for(i in seq_along(chronologies)){
  if(length(chronologies[[i]]) == 1){
    years<-c(NA)  #collecting the years with insufficient data
    } else {
  df<-as.data.frame(chronologies[i])
  years<-as.numeric(rownames(df))#extract years
    }
  oldest_growth<-c(oldest_growth,min(years))#get oldest year
  youngest_growth<-c(youngest_growth,max(years))#get youngest year
}


min(oldest_growth, na.rm=T) #this is the year of our oldest tree ring
max(youngest_growth, na.rm=T) #this is the year of our youngest tree ring 

#now we pad them out so we can make a rectangle with the vectors 
stand_chronologies<-list()
for(i in seq_along(chronologies)){
  df<-as.data.frame(chronologies[i])
  if(length(df)==1) { #filling the stands with insufficient data with NA vectors
    chron<-as.data.frame(c(as.numeric(rep(NA, max(youngest_growth, na.rm=T) - min(oldest_growth, na.rm=T) +1)))) #collecting the years with insufficient data
  } else {
          years<-as.numeric(rownames(df))#extract years
          nold<- -(min(oldest_growth, na.rm=T) - min(years, na.rm=T))  #for the oldest years  
          if (nold != 0){
                    nold<- matrix(rep(as.numeric(NA),nold*3), ncol = 3) %>% # this is how many NA's we're stackign ontop 
                          as.data.frame()
                    rownames(nold)<-c((min(oldest_growth, na.rm=T):c(min(years, na.rm=T)-1)))
                    names(nold)<-names(df)
                    df<-rbind(nold,df)
                    }
          #for the youngest years 
          nyoung<- (max(youngest_growth, na.rm=T) - max(years, na.rm=T)) 
          if (nyoung != 0){
                    nyoung<-matrix(rep(as.numeric(NA),nyoung*3), ncol = 3) %>%# this is how many NA's we're stacking below 
                          as.data.frame()
                    rownames(nyoung)<-c(c(max(years, na.rm=T)+1):(max(youngest_growth, na.rm=T)))
                    names(nyoung)<-names(df)
                    df<-rbind(df,nyoung)
          }
          chron<-as.data.frame(df$res)
  }
  print(nrow(chron))
  stand_chronologies[[i]]<-chron

}

df<-data.frame(stand_chronologies)#now we can line them all up in a dataframe
names(df)<-stand_id[seq_along(chronologies)]#and give them thier names from the meta record. 
rownames(df)<-c(min(oldest_growth, na.rm=T):max(youngest_growth, na.rm=T))#give them the names of the years 
subsetdf<- df[,!insuf_data]
str(subsetdf)
corre<-t(cor(subsetdf, use = "pairwise.complete.obs"))#computing cor matrix
cor_matrix <- na.omit(as.dist(1 - corre))#similer obs should have less distance
complete_clustering <- hclust(cor_matrix, method="complete")

plot(complete_clustering , main="Hierarchical Clustering", xlab="Trees",ylab = "Complete Linkage Length", sub= "Correlation as Dissimilarity Index",
     cex =.9, labels = FALSE)


hclustering2<-cutree(complete_clustering, 2)
hclustering3<-cutree(complete_clustering, 3)
hclustering4<-cutree(complete_clustering, 4)
hclustering5<-cutree(complete_clustering, 5)
hclustering6<-cutree(complete_clustering, 6)
hclustering7<-cutree(complete_clustering, 7)
hclustering8<-cutree(complete_clustering, 8)

#now lets map these different groups
str(hclustering2)
hclusters<-tibble(hclustering2,hclustering3,hclustering4,hclustering5,hclustering6,hclustering7,hclustering8)
hclusters$id<-names(hclustering2)
meta$id<-rownames(meta)
clustering<-left_join(hclusters, meta, by="id")

saveRDS(clustering, file = "dataVault/clustering_stands.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)



