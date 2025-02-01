library(ggplot2)

mortality <- function(data) {
  data <- (as.numeric(data[,11]))
  clean <- na.omit(data) 
  
  print(hist(clean,
             main='Mortality Rate in 30 Days',
             ylab = 'Mortality',
             col = "black", 
             border = "red",
             breaks=100,
             ylim = c(0,100),
             xlab = 'Days'),
        quote=FALSE)
  grid(col = "lightgray", lty = "dotted")
  
}

info <-function(data){
 
  clean <- data %>%
    filter(data[, 11] != "Not Available")
  
  death <- as.numeric(clean[,11])
  county <- as.factor(clean[,7])
  
  maxdeath <- max(death)
  minideath <- min(death)
  death_status <- ifelse(death == maxdeath, "Max Death State", 
                              ifelse(death == minideath, "Min Death State",'Others'))
  
  plot <- ggplot(clean,aes(x=death,y=county,color = death_status))+
    geom_point(size=2,stroke=1.5)+
    scale_color_manual(values = c("Max Death State" = "red", 
                                  "Min Death State" = "green",
                                  "Others" = "lightblue")) +
  labs(title='Death Rate due to Heart Attack in US per State',
       x='Death Rate',
       y='State',
       color = "Death Status")
  print(plot)
}

best <-function(state){
  data = read.csv('./usa/outcome-of-care-measures.csv')
  data = data[data[,7]==state,]
  
  clean = data %>%
    filter(data[,23]!='Not Available')
  
  hospitals = as.character(clean[,2])
  deaths = as.numeric(clean[,23])
  status= ifelse(deaths == min(deaths),'Minimum Death Rate', 'Others')
  
  print(sprintf('%s has the minimum death rate of %.1f',
         (hospitals[which(deaths==min(deaths))]),min(deaths)),
        quote=FALSE)
  
  plot = ggplot(clean,aes(y=deaths,x=hospitals,fill=status))+
    geom_col()+
    scale_fill_manual(values = c('Minimum Death Rate'='red',
                                  'Others'='lightgreen'))+
    labs(title=sprintf('%s Death Rate due to Pneumonia',state),
         x='Death Rate',
         y='Hospitals',
         fill='Status')
  
  print(plot)
}

rankhospital <-function(state,rows=15){
  data = read.csv('./usa/outcome-of-care-measures.csv')
  data = data[data[,7]==state,]
  clean = data %>%
    filter(data[,23]!='Not Available')
  clean=(clean[1:rows,])
  clean <- clean[order(as.numeric(clean[,17])),]
  
  
  hospitals = as.character(clean[,2])
  deaths = as.numeric(clean[,17])
  
  plot = ggplot(clean,aes(x=deaths,y=reorder(hospitals,deaths)))+
    geom_bar(stat = "identity",fill='yellow',color='black')+
    geom_text(aes(label=sprintf('Deaths Rate %.1f',deaths)),size=5,vjust=0.5,hjust=1.2)+
  labs(title=sprintf('Hospital Ranking in %s State',state),
         x='Hospitals',
         y='Deaths Rate')
  
  print(plot)
}

main <-function(){
  print('US Hospital Data Analysis',quote=FALSE)
  data <- read.csv('./usa/outcome-of-care-measures.csv')
  #mortality(data)
  #info(data)
  #best('NY')
  #rankhospital('NY')

}
main()