#require packages (make sure to install them before first use)
require(lubridate)
require(shiny)
require(mosaic)
require(L1pack)

#Load the data set
Raleigh <- read.csv("Raleigh.csv", header=FALSE)
Fairbanks <- read.csv("Fairbanks.csv", header=FALSE)
McGuireAFB <-read.csv("McGuireAFB.csv", header=FALSE)
NewOrleans <- read.csv("NewOrleans.csv", header=FALSE)

Fairbanks$V1 <- ymd(Fairbanks$V1)
McGuireAFB$V1 <- ymd(McGuireAFB$V1)
NewOrleans$V1 <- ymd(NewOrleans$V1)
Raleigh$V1<- ymd(Raleigh$V1)

#set up the years
nyears=nrow(table(year(Raleigh$V1)))
years <- 1955:2010
years2 <- 1955:2009

#remove the leap days
NewOrleans <- subset(NewOrleans, (month(NewOrleans$V1)!=2 | day(NewOrleans$V1)!=29))
McGuireAFB <- subset(McGuireAFB, (month(McGuireAFB$V1)!=2 | day(McGuireAFB$V1)!=29))
Fairbanks <- subset(Fairbanks, (month(Fairbanks$V1)!=2 | day(Fairbanks$V1)!=29))
Raleigh <- subset(Raleigh, (month(Raleigh$V1)!=2 | day(Raleigh$V1)!=29))

#remove partial years.
NewOrleans <- subset(NewOrleans, (year(NewOrleans$V1)!=2010))
McGuireAFB <- subset(McGuireAFB, (year(McGuireAFB$V1)!=2010))
Fairbanks <- subset(Fairbanks, (year(Fairbanks$V1)!= 2010))

nobsR<- nrow(Raleigh)
nobsF<- nrow(Fairbanks)
nobsN<- nrow(NewOrleans)
nobsM<- nrow(McGuireAFB)

#format data into a matrix for Raleigh
temp.mat.Raleigh=matrix(0, nrow=365, ncol=nyears)
for(i in 1:nyears){
  temp.mat.Raleigh[1:365, i]=Raleigh$V2[1:365+(i-1)*365]
}
colnames(temp.mat.Raleigh)=years

#format data into a matrix for New Orleans
temp.mat.NewOrleans<- matrix(0, nrow=365, ncol=(nyears-1))
for(i in 1:(nyears-1)){
  temp.mat.NewOrleans[1:365, i]=NewOrleans$V2[1:365+(i-1)*365]
}
colnames(temp.mat.NewOrleans)=years2

#format data into a matrix for McGuireAFB
temp.mat.McGuireAFB=matrix(0, nrow=365, ncol=(nyears-1))
for(i in 1:(nyears-1)){
  temp.mat.McGuireAFB[1:365, i]=McGuireAFB$V2[1:365+(i-1)*365]
}
colnames(temp.mat.McGuireAFB)=years2

#format data into a matrix for Fairbanks
temp.mat.Fairbanks=matrix(0, nrow=365, ncol=(nyears-1))
for(i in 1:(nyears-1)){
  temp.mat.Fairbanks[1:365, i]=Fairbanks$V2[1:365+(i-1)*365]
}
colnames(temp.mat.Fairbanks)=years2

#######################################
#Interactive Plots Begin Here
#######################################

#All Data
# Define UI for the application
ui <- fluidPage(#theme = shinytheme("superhero"),
  # Add a sidebar layout to the application
  sidebarLayout(
    sidebarPanel(
      h3("Inputs:"),
      radioButtons("data", 
                   label = h4("Data Set"),
                   choices = list("Raleigh" = 1, 
                                  "New Orleans" = 2, 
                                  "McGuire Air Force Base" = 3,
                                  "Fairbanks" = 4),
                   
                   selected = 1),
      selectInput(  
        "whichplot", 
        "Choose Plot:",
        c("Years" = 1,
          "Months" = 2,
          "Days" = 3) ) 
    )
    ,
    
    mainPanel(plotOutput("plot"))
  ))



# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$data == 1){
    if(input$whichplot==1){ boxplot(Raleigh$V2~year(Raleigh$V1), horizontal=FALSE, xlab = "Year", ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(Raleigh$V1))))}
    else if(input$whichplot==2) {boxplot(Raleigh$V2~month(Raleigh$V1),notch=FALSE, horizontal=FALSE, xlab = "Month", ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(Raleigh$V1))))}
    else if(input$whichplot==3) {boxplot(Raleigh$V2~yday(Raleigh$V1), horizontal=FALSE, xlab = "Day",axes = FALSE , ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(Raleigh$V1))))
      axis(side = 2, at = c(0,20,40,60,80,100))
      axis(side = 1, at = c(0,50,100,150,200,250, 300, 350))}
    } else if (input$data == 2){
      if(input$whichplot==1){ boxplot(NewOrleans$V2~year(NewOrleans$V1), horizontal=FALSE, xlab = "Year", ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(NewOrleans$V1))))}
      else if(input$whichplot==2) {boxplot(NewOrleans$V2~month(NewOrleans$V1),notch=FALSE, horizontal=FALSE, xlab = "Month", ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(NewOrleans$V1))))}
      else if(input$whichplot==3) {boxplot(NewOrleans$V2~yday(NewOrleans$V1), horizontal=FALSE, xlab = "Day",axes = FALSE , ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(NewOrleans$V1))))
        axis(side = 2, at = c(0,20,40,60,80,100))
        axis(side = 1, at = c(0,50,100,150,200,250, 300, 350))}
      
    } else if (input$data == 3){
      if(input$whichplot==1){ boxplot(McGuireAFB$V2~year(McGuireAFB$V1), horizontal=FALSE, xlab = "Year", ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(McGuireAFB$V1))))}
      else if(input$whichplot==2) {boxplot(McGuireAFB$V2~month(McGuireAFB$V1),notch=FALSE, horizontal=FALSE, xlab = "Month", ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(McGuireAFB$V1))))}
      else if(input$whichplot==3) {boxplot(McGuireAFB$V2~yday(McGuireAFB$V1), horizontal=FALSE, xlab = "Day",axes = FALSE , ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(McGuireAFB$V1))))
        axis(side = 2, at = c(0,20,40,60,80,100))
        axis(side = 1, at = c(0,50,100,150,200,250, 300, 350))}
    } else{
      if(input$whichplot==1){ boxplot(Fairbanks$V2~year(Fairbanks$V1), horizontal=FALSE, xlab = "Year", ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(Fairbanks$V1))))}
      else if(input$whichplot==2) {boxplot(Fairbanks$V2~month(Fairbanks$V1),notch=FALSE, horizontal=FALSE, xlab = "Month", ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(Fairbanks$V1))))}
      else if(input$whichplot==3) {boxplot(Fairbanks$V2~yday(Fairbanks$V1), horizontal=FALSE, xlab = "Day",axes = FALSE , ylab = "Avg. Temp (F)", col = c(rep("dodgerblue1", length(Fairbanks$V1))))
        axis(side = 2, at = c(-60,-40,-20,0,20,40,60,80,100))
        axis(side = 1, at = c(0,50,100,150,200,250, 300, 350))}
    }
    
    
    
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


