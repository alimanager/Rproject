library(tm)
#Chargement des données de la saga Harry potter: 
hp=VCorpus(DirSource("/home/abdelali/Documents/ETU/Logiciel R/tm_Harrypotter/tm/",encoding = "UTF-8"))
inspect(hp)

#netoyage des données : 
hp=tm_map(hp,stripWhitespace)
hp=tm_map(hp, removePunctuation)
hp=tm_map(hp, content_transformer(tolower))
hp=tm_map(hp, removeWords, stopwords("english"))  
hp=tm_map(hp, removeNumbers)
hp=tm_map(hp, stripWhitespace)

#stemming : j'ai negligé le stemming,a un effet negatifs sur les données.
library(SnowballC)
#hp=tm_map(hp,stemDocument)

#TermDocumentMatrix : 
TerDocHp=DocumentTermMatrix(hp)
# DocTerHp=TermDocumentMatrix(hp)
dtm=inspect(TerDocHp)
dtm[1:5,1:5]
#sparsité
TerDocHp=removeSparseTerms(TerDocHp, 0.6)

#frequente terme 
findFreqTerms(TerDocHp[2,],100)
# le nombre d'apprence de chaque terme : 
freq.saga=sapply(as.data.frame(dtm),function(x){length(which(x > 0))})
# apparue dans tous les docs :
freq.saga[which(freq.saga>5)] 


## une solution pour détecter les personnages et co-ocuurences de leur passage dans le saga:
## Charger la liste des vrais personnages de harry potter : 
act=VCorpus(DirSource("/home/abdelali/Documents/ETU/Logiciel R/actors/actor",encoding = "UTF-8"))
#actors=read.csv("/home/abdelali/Documents/ETU/Logiciel R/castActors_idbm.csv",sep=",")
#actorsC=VCorpus(DataframeSource(actors$Harry.Potter.and.the.Sorcerer.s.Stone))

act=tm_map(act, removePunctuation)
act=tm_map(act, content_transformer(tolower))
act=tm_map(act, removeWords, stopwords("english")) 
name.personnage.saga.HP=tm_map(act,stripWhitespace)

#les livres et les vrais occurence des personnages 
name.personnage.dtm=DocumentTermMatrix(act)
name.personnage=inspect(name.personnage.dtm)
name.personnage[,1:10]


# frequant personnage 
freq.actor <- sapply(as.data.frame(name.personnage),function(x){length(which(x > 0))})
freq.actor2=freq.actor[which(freq.actor>2)]

#Association entre les vrais personnages et leur co-occurence dans les romans
cher=findAssocs(TerDocHp,names(freq.actor2),corlimit = 0.9)
data.frame(cher$harry)

# les personnages moins fréquant : 
# namePerso=colnames(name.personnage[1,which(freq.actor>2)]) #les personnages du premiere saga au moins frequent 2 fois 
namePerso=names(which(name.personnage[1,]>0)) #les personnages du premiere saga 
# namePerso_tous=colnames(name.personnage)

# pour tout le saga 
charachterHP_saga= which(TerDocHp$dimnames$Terms %in% namePerso)
CharMatTD_saga=TerDocHp[,charachterHP]
CharMat_saga=inspect(TerDocHp[,charachterHP])

## Harry Potter and the Chamber of Secrets 
#les personnages du premiere saga 
#------------------------------------------
namePerso=names(which(name.personnage[1,]>0))

#filtrage des terms selon les vrais personnage du roman:
charachterHP = which(TerDocHp[1,]$dimnames$Terms %in% namePerso)
CharMatTD=TerDocHp[1,charachterHP]
CharMat=inspect(TerDocHp[1,charachterHP])


#Graphe : intensité des relation entre acteurs 
library(igraph)
# Matrice Terme_Terme
# CharMat[CharMat>=1]<-1
# termmatrix=CharMat_in %*% t(CharMat_in)
termmatrix=crossprod(as.matrix(CharMat))
diag(termmatrix)<-0
termmatrix[1:5,1:5]
termmatrix["harry", "dobby"]
termmatrix["harry", "ron"]
g<-graph.adjacency(termmatrix,weighted = T, mode = "undirected")
set.seed(3952)
g<-simplify(g)
g
V(g)$label <-V(g)$name
V(g)$degree <- degree(g)
plot(g,layout=layout.kamada.kawai)

#personaliser le graphe : 
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
plot(g, layout=layout.kamada.kawai)

# métriques de l'analyses des résaux sociaux: 
betweenness(g,directed = F)
closeness(g, mode = "out")
degree(g, mode = "out")



## les amis de "HARRY" sur tout le saga  : 
##----------------------------------------
# les terme associé avec harry: 
Harry=findAssocs(TerDocHp,"harry",0.5)
#filtré seulement les vrais personnages 
relation_avec_harry=names(Harry$harry)[  names(Harry$harry) %in% colnames(name.personnage)]
tdm_harry=TerDocHp[,colnames(TerDocHp)%in% relation_avec_harry]
inspect(tdm_harry)

#Matrice de Term-term Matrix: 
TTM=crossprod(as.matrix(tdm_harry))
diag(TTM)<-0
TTM[1:10,1:10]
g_harry<-graph.adjacency(TTM,weighted = T, mode = "undirected")
g_harry<-simplify(g_harry)

set.seed(10000)
V(g_harry)$label <-V(g_harry)$name
V(g_harry)$degree <- degree(g_harry)
plot(g_harry,layout=layout.kamada.kawai)

# métriques de l'analyses des résaux sociaux: 
sort(betweenness(g_harry,directed = F),decreasing = T)
sort(closeness(g_harry, mode = "out"), decreasing = T)
sort(degree(g_harry, mode = "out"),decreasing = T)




##les relations entre les professeurs de harry potter: 
#---------------------------------
professor=findAssocs(TerDocHp,"professor",0.85)
relation_avec_professor=names(professor$professor)[  names(professor$professor) %in% colnames(name.personnage)]
tdm_professor=TerDocHp[,colnames(TerDocHp)%in% relation_avec_professor]
inspect(tdm_professor)
#Matrice de Term-term Matrix: 
TTM_pro=crossprod(as.matrix(tdm_professor))
diag(TTM_pro)<-0
TTM_pro
set.seed(10000)
g_professor<-graph.adjacency(TTM_pro,weighted = T, mode = "undirected")
g_professor<-simplify(g_professor)
V(g_professor)$label <-V(g_professor)$name
V(g_professor)$degree <- degree(g_professor)
plot(g_professor,layout=layout.kamada.kawai)

sort(betweenness(g_professor,directed = F),decreasing = T)
sort(closeness(g_professor, mode = "out"),decreasing = F)
degree(g_professor, mode = "out")

#modularité de la communiauté des professeur 
wc <- walktrap.community(g_professor)
wc
modularity(wc)
membership(wc)
plot(g_professor, vertex.color=membership(wc))

 
# #personaliser le graphe : 
# V(g_professor)$label.cex <- 2.2 * V(g_professor)$degree / max(V(g_professor)$degree)+ .2
# V(g_professor)$label.color <- rgb(0, 0, .2, .8)
# V(g_professor)$frame.color <- NA
# egam <- (log(E(g_professor)$weight)+.4) / max(log(E(g_professor)$weight)+.4)
# E(g_professor)$color <- rgb(.5, .5, 0, egam)
# E(g_professor)$width <- egam
# plot(g_professor, layout=layout.kamada.kawai)