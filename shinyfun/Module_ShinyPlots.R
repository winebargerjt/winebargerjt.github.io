#require packages (make sure to install them before first use)
require(lubridate)
require(shiny)
require(mosaic)
require(L1pack)

#Load the data set
Raleigh <- read.csv("C:/Users/James/Desktop/MPE_Module/Raleigh.csv", header=FALSE)
Raleigh$V1=mdy(Raleigh$V1)
nobs=nrow(Raleigh)

#set up the years
nyears=nrow(table(year(Raleigh$V1)))
years=1955:2010

#remove the leap days
Raleigh=Raleigh[!(month(Raleigh$V1)==2 & day(Raleigh$V1)==29),]
nobs=nrow(Raleigh)

#format data into a matrix
temp.mat=matrix(0, nrow=365, ncol=nyears)
for(i in 1:nyears){
  temp.mat[1:365, i]=Raleigh$V2[1:365+(i-1)*365]
}
colnames(temp.mat)=years
  
#######################################
#Interactive Plots Begin Here
#######################################

#All Data
ui=fluidPage(checkboxGroupInput("whichplot", "Choose What to Include:",
                                c("Line" = "line") ), plotOutput(outputId="box"))
server=function(input,output){output$box<???renderPlot({
if("line"%in%input$whichplot ) {
  plot(1:nobs, Raleigh$V2)
    time=1:nobs
    abline(lm(c(temp.mat)~(time)), col="Red")
  } else{
    plot(1:nobs, Raleigh$V2)
  }
})}
shinyApp(ui=ui , server = server)

#Figure 2: compare two years
ui=fluidPage(numericInput(inputId="n", "Enter a Year (Blue)" ,value=1955, min=1955, max=2010), numericInput(inputId="k", "Enter a Year (Red)" ,value=2000, min=1955, max=2010),plotOutput(outputId="line") )
server=function(input,output){output$line<???renderPlot({plot(1:365,temp.mat[,which(years==input$n)], type='p', col="blue", xlab="Day of the Year", ylab="Avg. Temp (F)") 
  points(1:365, temp.mat[,which(years==input$k)], col="red")})}
shinyApp(ui=ui , server = server)

#Figure: compare two days
ui=fluidPage(dateInput("date1", "Date (Blue):", value = "2018-01-01",  min="2018-01-01", max="2018-12-31"), dateInput("date2", "Date (Red):", value = "2018-06-30", min="2018-01-01", max="2018-12-31"),plotOutput(outputId="line") )
server=function(input,output){output$line<???renderPlot({plot(1:nyears,temp.mat[yday(input$date1),], type='l', ylim=c(0,100), col="blue") 
  lines(1:nyears, temp.mat[yday(input$date2),], col="red")})}
shinyApp(ui=ui , server = server)

#Figure 4: boxplots
ui=fluidPage(selectInput("whichplot", "Choose Plot:",
                                c("Years" = 1,
                                  "Months" = 2,
                                  "Days" = 3) ), plotOutput(outputId="box"))
server=function(input,output){output$box<???renderPlot({
  if(input$whichplot==1){ bwplot(Raleigh$V2~year(Raleigh$V1), horizontal=FALSE, xlab=years)}
  else if(input$whichplot==2) {bwplot(Raleigh$V2~month(Raleigh$V1),notch=T, horizontal=FALSE)}
  else if(input$whichplot==3) {bwplot(Raleigh$V2~yday(Raleigh$V1), horizontal=FALSE)}
  })}
shinyApp(ui=ui , server = server)

#One year differences
plot(c((temp.mat[,2:nyears]-temp.mat[,1:(nyears-1)])))


#plots by monthly average
month.lengths=c(31,28,31, 30,31,30,31,31,30,31,30,31)
month.ends=c(1,cumsum(month.lengths))
ui=fluidPage(numericInput(inputId="n", "Enter a Month (1-12)" ,value=1, min=1, max=12), plotOutput(outputId="line") )
server=function(input,output){output$line<???renderPlot({
  plot(years,colMeans(temp.mat[month.ends[input$n]:month.ends[input$n+1],]), type='l')
  abline(lm(colMeans(temp.mat[month.ends[input$n]:month.ends[input$n+1],])~years))
})}
shinyApp(ui=ui , server = server)


#Average over entire year
ui=fluidPage(checkboxGroupInput("whichplot", "Choose what to inclide:",
                                c("text"="text", "lines"="lines")), plotOutput(outputId = "box"))             
             
server=function(input,output){output$box<???renderPlot({
  if("text"%in%input$whichplot && "lines"%in%input$whichplot ) {
    plot(years, colMeans(temp.mat), type='l', ylab="Average Annual Temperature")
    text(years, colMeans(temp.mat), round(colMeans(temp.mat), 2), cex=0.8)
    abline(lm(colMeans(temp.mat)~years))
    #abline(lad(colMeans(temp.mat)~years), lty="dashed")
  }else if("text"%in%input$whichplot) {
    plot(years, colMeans(temp.mat), type='l', ylab="Average Annual Temperature")
    text(years, colMeans(temp.mat), round(colMeans(temp.mat), 2), cex=0.8)
  }else if("lines"%in%input$whichplot ) {
    plot(years, colMeans(temp.mat), type='l', ylab="Average Annual Temperature")
    abline(lm(colMeans(temp.mat)~years))
    #abline(lad(colMeans(temp.mat)~years), lty="dashed")
  } else{
    plot(years, colMeans(temp.mat), type='l')
  }
})}
shinyApp(ui=ui , server = server)



