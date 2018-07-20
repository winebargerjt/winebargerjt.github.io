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
      h3("Inputs: (Note that the year doesn't matter)"),
      radioButtons("data", 
                   label = "Data Set",
                   choices = list("Raleigh" = 1, 
                                  "New Orleans" = 2, 
                                  "McGuire Air Force Base" = 3,
                                  "Fairbanks" = 4),
                   
                   selected = 1),
      dateInput("date1", 
                "Date (Blue) YYYY-MM-DD:", 
                value = "2018-01-01",  
                min="2018-01-01", 
                max="2018-12-31"),
      radioButtons("radio", 
                   label = "Fixed Date Option",
                   choices = list("Daily" = 1, "Monthly" = 2), 
                   selected = 1),
      checkboxGroupInput(
        "whichplot", 
        "Choose what to include:",
        c("Line of Best Fit"="lines")),
      
      plotOutput(outputId="lines")  
    )
    ,
    
    mainPanel(plotOutput("plot"))
  ))



# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$data == 1){
    # Graphs the Raleigh Set
    if("lines"%in%input$whichplot && input$radio == 1) {
      plot(years,temp.mat.Raleigh[yday(input$date1),], type='l', ylim=c(0,100), col="blue", ylab = "Avg. Daily Temp (F)", xlab = "Year",
           axes = FALSE)
      axis(side = 2, at = c(0,20,40,60,80,100))
      axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
      
      abline(lm(temp.mat.Raleigh[yday(input$date1),] ~ years), col = "red")
      
      cf <- round(lm(temp.mat.Raleigh[yday(input$date1),] ~ years)$coefficients,2)
      eq <- paste0("Avg. Temp = ", cf[2], " Year",
                   ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
      mtext(eq, 3, line=-1, at = c(1965, 58))
      
      
    }
    else if("lines"%in%input$whichplot && input$radio == 2){
      plot(years,colMeans(temp.mat.Raleigh[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year", axes = FALSE, col="blue")
      abline(lm(colMeans(temp.mat.Raleigh[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],])~years), col="red")   
      axis(side = 2, at = c(0,20, 25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
      axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
      cf <- round(lm(colMeans(temp.mat.Raleigh[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],])~years)$coefficients,2)
      eq <- paste0("Avg. Temp = ", cf[2], " Year",
                   ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
      mtext(eq, 3, line=-1, at = c(1965, 58))
    }else if(input$radio == 2){
      plot(years,colMeans(temp.mat.Raleigh[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year", axes = FALSE, col="blue")
      axis(side = 2, at = c(0,20, 25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
      axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
    }else{
      plot(years,temp.mat.Raleigh[yday(input$date1),], type='l', ylim=c(0,100), col="blue", ylab = "Avg. Daily Temp (F)", xlab = "Year",
               axes = FALSE)
      axis(side = 2, at = c(0,20,40,60,80,100))
      axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
    }
    }else if (input$data == 2){
      # Graphs the New Orleans Set
      if("lines"%in%input$whichplot && input$radio == 1) {
        plot(years2,temp.mat.NewOrleans[yday(input$date1),], type='l', ylim=c(0,100), col="blue", ylab = "Avg. Daily Temp (F)", xlab = "Year",
             axes = FALSE)
        axis(side = 2, at = c(0,20,40,60,80,100))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
        
        abline(lm(temp.mat.NewOrleans[yday(input$date1),] ~ years2), col = "red")
        
        cf <- round(lm(temp.mat.NewOrleans[yday(input$date1),] ~ years2)$coefficients,2)
        eq <- paste0("Avg. Temp = ", cf[2], " Year",
                     ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
        mtext(eq, 3, line=-1, at = c(1965, 58))
        
        
      }
      else if("lines"%in%input$whichplot && input$radio == 2){
        plot(years2,colMeans(temp.mat.NewOrleans[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year", axes = FALSE, col="blue")
        abline(lm(colMeans(temp.mat.NewOrleans[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],])~years2), col="red")   
        axis(side = 2, at = c(0,20, 25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
        cf <- round(lm(colMeans(temp.mat.NewOrleans[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],])~years2)$coefficients,2)
        eq <- paste0("Avg. Temp = ", cf[2], " Year",
                     ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
        mtext(eq, 3, line=-1, at = c(1965, 58))
      }else if(input$radio == 2){
        plot(years2,colMeans(temp.mat.NewOrleans[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year", axes = FALSE, col="blue")
        axis(side = 2, at = c(0,20, 25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
      }else{
        plot(years2,temp.mat.NewOrleans[yday(input$date1),], type='l', ylim=c(0,100), col="blue", ylab = "Avg. Daily Temp (F)", xlab = "Year",
             axes = FALSE)
        axis(side = 2, at = c(0,20,40,60,80,100))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
      }
        
    }else if (input$data == 3){
      # Graphs the McGuire Set
      if("lines"%in%input$whichplot && input$radio == 1) {
        plot(years2,temp.mat.McGuireAFB[yday(input$date1),], type='l', ylim=c(0,100), col="blue", ylab = "Avg. Daily Temp (F)", xlab = "Year",
             axes = FALSE)
        axis(side = 2, at = c(0,20,40,60,80,100))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
        
        abline(lm(temp.mat.McGuireAFB[yday(input$date1),] ~ years2), col = "red")
        
        cf <- round(lm(temp.mat.McGuireAFB[yday(input$date1),] ~ years2)$coefficients,2)
        eq <- paste0("Avg. Temp = ", cf[2], " Year",
                     ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
        mtext(eq, 3, line=-1, at = c(1965, 58))
        
        
      }
      else if("lines"%in%input$whichplot && input$radio == 2){
        plot(years2,colMeans(temp.mat.McGuireAFB[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year", axes = FALSE, col="blue")
        abline(lm(colMeans(temp.mat.McGuireAFB[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],])~years2), col="red")   
        axis(side = 2, at = c(0,20, 25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
        cf <- round(lm(colMeans(temp.mat.McGuireAFB[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],])~years2)$coefficients,2)
        eq <- paste0("Avg. Temp = ", cf[2], " Year",
                     ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
        mtext(eq, 3, line=-1, at = c(1965, 58))
      }else if(input$radio == 2){
        plot(years2,colMeans(temp.mat.McGuireAFB[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year", axes = FALSE, col="blue")
        axis(side = 2, at = c(0,20, 25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
      }else{
        plot(years2,temp.mat.McGuireAFB[yday(input$date1),], type='l', ylim=c(0,100), col="blue", ylab = "Avg. Daily Temp (F)", xlab = "Year",
             axes = FALSE)
        axis(side = 2, at = c(0,20,40,60,80,100))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
      }  
    }else{
      # Graphs the fairbanks Set
      if("lines"%in%input$whichplot && input$radio == 1) {
        plot(years2,temp.mat.Fairbanks[yday(input$date1),], type='l', ylim=c(-60,100), col="blue", ylab = "Avg. Daily Temp (F)", xlab = "Year",
             axes = FALSE)
        axis(side = 2, at = c(-60,-40,-20,0,20,40,60,80,100))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
        
        abline(lm(temp.mat.Fairbanks[yday(input$date1),] ~ years2), col = "red")
        
        cf <- round(lm(temp.mat.Fairbanks[yday(input$date1),] ~ years2)$coefficients,2)
        eq <- paste0("Avg. Temp = ", cf[2], " Year",
                     ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
        mtext(eq, 3, line=-1, at = c(1965, 58))
        
        
      }
      else if("lines"%in%input$whichplot && input$radio == 2){
        plot(years2,colMeans(temp.mat.Fairbanks[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year", axes = FALSE, col="blue")
        abline(lm(colMeans(temp.mat.Fairbanks[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],])~years2), col="red")   
        axis(side = 2, at = c(-60,-40,-20,0,20, 25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
        cf <- round(lm(colMeans(temp.mat.Fairbanks[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],])~years2)$coefficients,2)
        eq <- paste0("Avg. Temp = ", cf[2], " Year",
                     ifelse(sign(cf[1])==1, " + ", " - "), abs(cf[1]))
        mtext(eq, 3, line=-1, at = c(1965, 58))
      }else if(input$radio == 2){
        plot(years2,colMeans(temp.mat.Fairbanks[month.ends[month(input$date1)]:month.ends[month(input$date1)+1],]), type='l', ylab = "Avg. Temp (F)", xlab="Year", axes = FALSE, col="blue")
        axis(side = 2, at = c(-60,-40,-20,0,20, 25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
      }else{
        plot(years2,temp.mat.Fairbanks[yday(input$date1),], type='l', ylim=c(-60,100), col="blue", ylab = "Avg. Daily Temp (F)", xlab = "Year",
             axes = FALSE)
        axis(side = 2, at = c(-60,-40,-20,0,20,40,60,80,100))
        axis(side = 1, at = c(1955, 1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010))
      } 
    }
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


