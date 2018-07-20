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
                   
                   selected = 3),
      checkboxGroupInput(
        "whichplot", 
        "Choose what to include:",
        c("Line of Best Fit"="lines")) 
      
    )
    ,
    
    mainPanel(plotOutput("plot"))
  ))



# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$data == 1){
    if("text"%in%input$whichplot && "lines"%in%input$whichplot ) {
      plot(years, colMeans(temp.mat.Raleigh), type='l', ylab="Avg Annual Temp", xlab = "Year")
      text(years, colMeans(temp.mat.Raleigh), round(colMeans(temp.mat.Raleigh), 2), cex=0.8)
      abline(lm(colMeans(temp.mat.Raleigh)~years), col="blue")
      #abline(lad(colMeans(temp.mat)~years), lty="dashed")
    }else if("text"%in%input$whichplot) {
      plot(years, colMeans(temp.mat.Raleigh), type='l', ylab="Avg. Annual Temp", xlab = "Year")
      text(years, colMeans(temp.mat.Raleigh), round(colMeans(temp.mat.Raleigh), 2), cex=0.8)
    }else if("lines"%in%input$whichplot ) {
      plot(years, colMeans(temp.mat.Raleigh), type='l', ylab="Avg. Annual Temp", xlab = "Year")
      abline(lm(colMeans(temp.mat.Raleigh)~years), col="red")
      cf <- round(lm(colMeans(temp.mat.Raleigh)~years)$coefficients,2)
      eq <- paste0("Avg. Temp = ", cf[2], " Year",
                   ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
      mtext(eq, 3, line=-1, at = c(1965, 58))
      
      
    } else{
      plot(years, colMeans(temp.mat.Raleigh), type='l', ylab="Avg. Annual Temp", xlab = "Year")
    }
    }else if (input$data == 2){
      if("text"%in%input$whichplot && "lines"%in%input$whichplot ) {
        plot(years2, colMeans(temp.mat.NewOrleans), type='l', ylab="Avg Annual Temp", xlab = "Year")
        text(years2, colMeans(temp.mat.NewOrleans), round(colMeans(temp.mat.NewOrleans), 2), cex=0.8)
        abline(lm(colMeans(temp.mat.NewOrleans)~years2), col="blue")
        #abline(lad(colMeans(temp.mat)~years), lty="dashed")
      }else if("text"%in%input$whichplot) {
        plot(years2, colMeans(temp.mat.NewOrleans), type='l', ylab="Avg. Annual Temp", xlab = "Year")
        text(years2, colMeans(temp.mat.NewOrleans), round(colMeans(temp.mat.NewOrleans), 2), cex=0.8)
      }else if("lines"%in%input$whichplot ) {
        plot(years2, colMeans(temp.mat.NewOrleans), type='l', ylab="Avg. Annual Temp", xlab = "Year")
        abline(lm(colMeans(temp.mat.NewOrleans)~years2), col="red")
        cf <- round(lm(colMeans(temp.mat.NewOrleans)~years2)$coefficients,2)
        eq <- paste0("Avg. Temp = ", cf[2], " Year",
                     ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
        mtext(eq, 3, line=-1, at = c(1965, 58))
        
        
      } else{
        plot(years2, colMeans(temp.mat.NewOrleans), type='l', ylab="Avg. Annual Temp", xlab = "Year")
      }
      }else if (input$data == 3){
        if("text"%in%input$whichplot && "lines"%in%input$whichplot ) {
          plot(years2, colMeans(temp.mat.McGuireAFB), type='l', ylab="Avg Annual Temp", xlab = "Year")
          text(years2, colMeans(temp.mat.McGuireAFB), round(colMeans(temp.mat.McGuireAFB), 2), cex=0.8)
          abline(lm(colMeans(temp.mat.McGuireAFB)~years2), col="blue")
          #abline(lad(colMeans(temp.mat)~years), lty="dashed")
        }else if("text"%in%input$whichplot) {
          plot(years2, colMeans(temp.mat.McGuireAFB), type='l', ylab="Avg. Annual Temp", xlab = "Year")
          text(years2, colMeans(temp.mat.McGuireAFB), round(colMeans(temp.mat.McGuireAFB), 2), cex=0.8)
        }else if("lines"%in%input$whichplot ) {
          plot(years2, colMeans(temp.mat.McGuireAFB), type='l', ylab="Avg. Annual Temp", xlab = "Year")
          abline(lm(colMeans(temp.mat.McGuireAFB)~years2), col="red")
          cf <- round(lm(colMeans(temp.mat.McGuireAFB)~years2)$coefficients,2)
          eq <- paste0("Avg. Temp = ", cf[2], " Year",
                       ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
          mtext(eq, 3, line=-1, at = c(1965, 58))
          
          
        } else{
          plot(years2, colMeans(temp.mat.McGuireAFB), type='l', ylab="Avg. Annual Temp", xlab = "Year")
        }
      } else {
        if("text"%in%input$whichplot && "lines"%in%input$whichplot ) {
          plot(years2, colMeans(temp.mat.Fairbanks), type='l', ylab="Avg Annual Temp", xlab = "Year")
          text(years2, colMeans(temp.mat.Fairbanks), round(colMeans(temp.mat.Fairbanks), 2), cex=0.8)
          abline(lm(colMeans(temp.mat.Fairbanks)~years2), col="blue")
          #abline(lad(colMeans(temp.mat)~years), lty="dashed")
        }else if("text"%in%input$whichplot) {
          plot(years2, colMeans(temp.mat.Fairbanks), type='l', ylab="Avg. Annual Temp", xlab = "Year")
          text(years2, colMeans(temp.mat.Fairbanks), round(colMeans(temp.mat.Fairbanks), 2), cex=0.8)
        }else if("lines"%in%input$whichplot ) {
          plot(years2, colMeans(temp.mat.Fairbanks), type='l', ylab="Avg. Annual Temp", xlab = "Year")
          abline(lm(colMeans(temp.mat.Fairbanks)~years2), col="red")
          cf <- round(lm(colMeans(temp.mat.Fairbanks)~years2)$coefficients,2)
          eq <- paste0("Avg. Temp = ", cf[2], " Year",
                       ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
          mtext(eq, 3, line=-1, at = c(1965, 58))
          
          
        } else{
          plot(years2, colMeans(temp.mat.Fairbanks), type='l', ylab="Avg. Annual Temp", xlab = "Year")
        }
      }
      
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


