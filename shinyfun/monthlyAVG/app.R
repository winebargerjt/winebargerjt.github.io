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
NewOrleans <- subset(NewOrleans, (year(NewOrleans$V1)!=2010))
McGuireAFB <- subset(McGuireAFB, (year(McGuireAFB$V1)!=2010))
Fairbanks <- subset(Fairbanks, (year(Fairbanks$V1)!= 2010))
Raleigh <- subset(Raleigh, (month(Raleigh$V1)!=2 | day(Raleigh$V1)!=29))
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

month.lengths=c(31,28,31, 30,31,30,31,31,30,31,30,31)
month.ends=c(1,cumsum(month.lengths))
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
      numericInput(inputId="n", 
                   "Enter a Month (1-12)" ,
                   value=1, 
                   min=1, 
                   max=12), 
      plotOutput(outputId="line") 
    )
    ,
    
    mainPanel(plotOutput("plot"))
  ))



# Define the server logic
server <- function(input, output) {
  
  output$plot <- renderPlot({
    if (input$data == 1){
    plot(years,colMeans(temp.mat.Raleigh[month.ends[input$n]:month.ends[input$n+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year")
    abline(lm(colMeans(temp.mat.Raleigh[month.ends[input$n]:month.ends[input$n+1],])~years))
    }else if(input$data == 2){
      plot(years2,colMeans(temp.mat.NewOrleans[month.ends[input$n]:month.ends[input$n+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year")
      abline(lm(colMeans(temp.mat.NewOrleans[month.ends[input$n]:month.ends[input$n+1],])~years2))
    }else if(input$data == 3){
      plot(years2,colMeans(temp.mat.McGuireAFB[month.ends[input$n]:month.ends[input$n+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year")
      abline(lm(colMeans(temp.mat.McGuireAFB[month.ends[input$n]:month.ends[input$n+1],])~years2))
    }else{
      plot(years2,colMeans(temp.mat.Fairbanks[month.ends[input$n]:month.ends[input$n+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year")
      abline(lm(colMeans(temp.mat.Fairbanks[month.ends[input$n]:month.ends[input$n+1],])~years2))
    }
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


