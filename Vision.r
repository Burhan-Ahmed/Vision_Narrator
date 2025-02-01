library(dplyr)
library(tidyr)

ShowMean <- function(data){
  
  clean <- data %>% 
    drop_na(sulfate,nitrate)
  
  Mean <- clean %>%
    summarise(
      s <- round(mean(sulfate),2),
      n <- round(mean(nitrate),2)
    )
  print(Mean)
  print(sprintf("Total Mean : %.2f",
                sum(Mean$s, Mean$n)/2),
        quote=FALSE)

}

ReadFile <- function(data){
  count <- 0
  record <- sort(list.files(path=data, pattern='\\.csv$'))
  
  for(i in seq_along(record)){

    print(sprintf('File Name : %s',record[i]),quote=FALSE)
    
    catalog <- read.csv(file.path(data,record[i]))
    print(sprintf('Records : %d',nrow(catalog)),quote=FALSE)
    
    clean <- catalog %>% 
      drop_na(sulfate,nitrate)
    
    corel=cor(clean$sulfate,clean$nitrate)
    
    if(!is.na(corel) && corel>0){
      print(sprintf('Valid Correlation : %f',corel),quote=FALSE)
    }
    else if(is.na(corel)){
      print('Invalid File Content',quote=FALSE)
    }
    else{
      print(sprintf('Invalid Correlation : %f',corel),quote=FALSE)
    }
    
    count <- count + nrow(catalog)
    i=i+1
  }
  
  print(sprintf("Total Records : %d",count),quote=FALSE)
  print(sprintf("Total Number of Files in Directory : %d",
                length(record)),quote=FALSE)
}

makeCacheMatrix <- function(){
  data<-matrix(1:16,ncol=4,nrow=4)
  print(data)
}

cacheSolve <- function(){}

main <- function(){
  md<-read.csv('./specdata/002.csv')
  dirPath <- ('./specdata')

  #ReadFile(dirPath)
  #ShowMean(md)
  
  makeCacheMatrix()
  
}

main()