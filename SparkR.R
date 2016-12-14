#Spark & R Connection : 
Sys.setenv(SPARK_HOME="/etc/spark/spark-1.6.0")
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"),"R","lib"), .libPaths()))

library(SparkR)
library(magrittr)

Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.10:1.3.0" "sparkr-shell"')

sc <- sparkR.init()
sqlContext <- sparkRSQL.init(sc)

#import CSV File : 
flights <- read.df(sqlContext, "/home/abdelali/Documents/ETU/Logiciel R/csv-txt.csv", "com.databricks.spark.csv", header="true", inferSchema ="true")
printSchema(flights)
showDF(flights)
head(flights)

titanic<-read.df(sqlContext,"/home/abdelali/Documents/ETU/Logiciel R/train.csv","com.databricks.spark.csv", header="true", inferSchema ="true")
head(titanic)

#créer l'environnement sparkContext & sqlContext: 
sc <- sparkR.init(sparkPackages="com.databricks:spark-csv_2.11:1.0.3")
sc <- sparkR.init()
sqlContext <- sparkRSQL.init(sc)

#DF FROM LOCAL R DataFrame
iris_DF=createDataFrame(sqlContext, iris)
head(iris_DF)

# import JSON
people <- read.df(sqlContext, "/etc/spark/spark-1.6.0/examples/src/main/resources/people.json", "json")
head(people)
saveDF(titanic,"titanic.json","json")

#to Parquet Format 
saveAsParquetFile(titanic,"titanic2.parquet")
titanic_pq=parquetFile(sqlContext, "titanic2.parquet")                                                                                                                                                                      
head(titanic_pq)

#Hive Tables : 
sc<-sparkRHive.init()
hiveContext <- sparkRHive.init(sc)
sql(hiveContext, "CREATE TABLE IF NOT EXISTS src (key INT, value STRING)")
sql(hiveContext, "LOAD DATA LOCAL INPATH 'examples/file.txt' INTO TABLE src")
results <- sql(hiveContext, "FROM src SELECT key, value")

# Sql 
registerTempTable(titanic,"titanic")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
count(sql(sqlContext,"SELECT Ticket FROM titanic"))


#Operations sur les DF :
head(filter(iris_DF,iris_DF$Species=="setosa"))
head(filter(iris_DF,iris_DF$Petal_Length > 1.5))

head(select(iris_DF,iris_DF$Sepal_Length))

x=head(summarize(groupBy(iris_DF,  iris_DF$Species), count = n(iris_DF$Species)))

head(orderBy(iris_DF,desc(x) ))


#summary
class(titanic)
typeof(titanic)
#convert Spark dataFrame to R local dataFrame : head(), count(), take()
str(titanic)  
str(head(titanic))  #head converts SparkR DataFrame to local R data frame 
#creatDataFrame

head(describe(titanic)) 
head(describe(titanic,"Ticket"))

#dimension du data frame 
head(dim(titanic))

# types des variables: 
head(dtypes(titanic))
printSchema(titanic)

#netoyage des données :
isNaN(titanic$Ticket)


# Machine Learning : 
DF <- createDataFrame(sqlContext,iris )
head(flights)

printSchema(DF)
model <- glm(Sepal_Length ~ Sepal_Width + Species, data = DF, family = "gaussian")
summary(model)
