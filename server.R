##### Server Side Functions Ambulance Network Project #####

library(shiny)
library(devtools)
library(RgoogleMaps)
library(TeachingDemos)
library(lattice)

##### Loading Data - Completed #####

dataPop <- read.csv(file = "www/data/populationFinal.csv", header=FALSE)
colnames(dataPop) <- c("County","Population","Male","Female","0-14","15-64",
                       "65+","% Male","% Female","% 0-14","% 15-64","% 65+")

dataHosp <- read.csv(file = "www/data/hospital.csv", header = TRUE)
dataStations <- read.csv(file = "www/data/stations.csv", header = TRUE, colClasses = c("factor", "numeric", "numeric", "factor"))
dataHospAnalysis <- rbind(dataHosp[8,], dataHosp[27,], dataHosp[77,], dataHosp[86,],dataHosp[92,], dataHosp[93,], 
                          dataHosp[96,], dataHosp[104,], dataHosp[113,],dataHosp[116,], dataHosp[123,], dataHosp[124,], dataHosp[131,])
colnames(dataHospAnalysis) <- colnames(dataHosp)

irelandMap <- GetMap(center = c(53.5, -8), zoom = 7, destfile = "ireland.png", maptype = "terrain")

# Convert degrees to radians
deg2rad <- function(deg){
   return(deg*pi/180)
}

dist.gps2km <- function(long1, lat1, long2, lat2) {
   R <- 6371 # Earth mean radius [km]
   delta.long <- (long2 - long1)
   delta.lat <- (lat2 - lat1)
   a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
   c <- 2 * asin(min(1,sqrt(a)))
   d = R * c
   return(d) # Distance in km
}

distance.matrix <- matrix(0, nrow = nrow(dataStations), ncol = nrow(dataHospAnalysis))
colnames(distance.matrix) <- dataHospAnalysis$Hospital

for(i in 1:nrow(dataHospAnalysis)){
   for(j in 1:nrow(dataStations)){
      distance.matrix[j,i] <- round(dist.gps2km(deg2rad(dataHospAnalysis[i,3]),deg2rad(dataHospAnalysis[i,4]),deg2rad(dataStations[j,2]),deg2rad(dataStations[j,3])), digits = 1)
   }
}

index.min <- rep(0, nrow(distance.matrix))

for(i in 1:nrow(distance.matrix)){
   index.min[i] <- which(distance.matrix[i,] == min(distance.matrix[i,]))
}

classDistance <- matrix(0, ncol = 3, nrow = 3*(ncol(distance.matrix)))
classDistance[,1] <- rep(colnames(distance.matrix), times = 3)
classDistance[,2] <- c(rep("<20 km", times = 13), rep("20-40 km", times = 13),rep("40+ km", times = 13))
colnames(classDistance) <- c("Hospitals", "Distance", "Frequency")

classDistance <- as.data.frame(classDistance)
classDistance$Frequency <- as.numeric(classDistance$Frequency)
classDistance$Frequency <- 0

for(i in 1:ncol(distance.matrix)){
   for(j in 1:length(index.min)){
      if(index.min[j]==i){
         if(distance.matrix[j,i]<=20){
            classDistance[i,3] <- classDistance[i,3] + 1
         }else if(distance.matrix[j,i]>20 && distance.matrix[j,i]<=40){
            loc <- i + 13
            classDistance[loc,3] <- classDistance[loc,3] + 1
         }else{
            loc <- i + 26
            classDistance[loc,3] <- classDistance[loc,3] + 1
         }
      }
   }
}

##### Start Shiny Server Commands #####
shinyServer(function(input, output, session){
   
##### Data Tab Functions - Completed #####
   
   output$dataPop <- renderDataTable(dataPop, options = list(lengthMenu = list(c(10, 26, 50, -1), c('10', '26', '50', 'All')), pageLength = 26))
   output$dataHosp <- renderDataTable(dataHosp, options = list(lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')), pageLength = 25))
   output$dataStations <- renderDataTable(dataStations, options = list(lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')), pageLength = 25))
   
   datasetInput <- reactive({
      switch(input$dataset,
             "Population Data" = dataPop,
             "Hospital Data" = dataHosp,
             "Ambulance Stations Data" = dataStations
             )
   })
   
   output$downloadData <- downloadHandler(
      filename = function() { paste(input$dataset, '.csv', sep='') },
      content = function(file) {
         write.csv(datasetInput(), file)
      }
   )
   

##### Contact Tab Functions - Completed #####
   
   writeComments <- function(name, email, message){
      if(!file.exists("comments.txt")){
         file.create("comments.txt")
      }
      
      name <- c("Name: ", name)
      email <- c("Email: ", email)
      message <- c("Message:", message)
      
      fileConn <- file("comments.txt")
      writeLines(c(name, email, message), fileConn)
      close(fileConn)
   }
   
   output$CommentAccepted <- renderText({
      if(input$submit!=0){
         writeComments(input$name, input$email, input$message)
         return("Your comment has been submitted successfully!")
      }
   }) 

##### Analysis Tab Functions #####

   observe({
      
      if(input$barvar == "Counties"){
         options <- c("Gender", "Age Groups")
      }else if(input$barvar == "Gender"){
         options <- c("Counties")
      }else if(input$barvar == "Age Groups"){
         options <- c("Counties")
      }
      updateSelectInput(session, "stackedbarvar", choices = options)
   })

   output$popbar <- renderPlot({
      if(input$stackedbar == FALSE){
         if(input$barvar == "Counties"){
            barplot(height = dataPop$Population, names.arg = dataPop$County, las=3, col = 1:26, ylab = "Population")
            title("Barchart of Population per County")
         }else if(input$barvar == "Gender"){
            heights <- c(sum(dataPop$Male), sum(dataPop$Female))
            barplot(height = heights, names.arg = c("Male", "Female"), col = 1:2, xlab = "Gender", ylab = "Population")
            title("Barchart of Population by Gender")
         }else{
            heights <- c(sum(dataPop[,5]), sum(dataPop[,6]), sum(dataPop[,7]))
            barplot(height = heights, names.arg = c("0-14", "15-64", "65+"), col = 1:3, xlab = "Age Groups", ylab = "Population")
            title("Barchart of Population by Age Groups")
         }
      }else if(input$stackedbar == TRUE){
         if(input$barvar == "Counties"){
            if(input$stackedbarvar == "Gender"){
               barplot(height = t(dataPop[,3:4]), names.arg = dataPop$County, beside = FALSE, col = c(2,4), las = 3, ylab = "Population")
               legend("topright", legend = c("Male", "Female"), fill = c(2,4), bty = "n")
               title("Stacked barchart showing Gender population levels per County")
            }else if(input$stackedbarvar == "Age Groups"){
               barplot(height = t(dataPop[,5:7]), names.arg = dataPop$County, beside = FALSE, col = c(2:4), ylab = "Population", las = 3)
               legend("topright", legend = c("0-14", "15-64", "65+"), fill = c(2:4), bty = "n")
               title("Stacked barchart showing Age Group population levels per county")
            }
         }else if(input$barvar == "Gender"){
            if(input$stackedbarvar == "Counties"){
               barplot(height = t(dataPop[,3:4]), names.arg = dataPop$County, beside = FALSE, col = c(2,4), las = 3, ylab = "Population")
               legend("topright", legend = c("Male", "Female"), fill = c(2,4), bty = "n")
               title("Stacked barchart showing Gender population levels per County")
            }
         }else if(input$barvar == "Age Groups"){
            if(input$stackedbarvar == "Counties"){
               barplot(height = t(dataPop[,5:7]), names.arg = dataPop$County, beside = FALSE, col = c(2:4), ylab = "Population", las = 3)
               legend("topright", legend = c("0-14", "15-64", "65+"), fill = c(2:4), bty = "n")
               title("Stacked barchart showing Age Group population levels per county")
            }
         }
      }
   })

   output$hospmap <- renderPlot({
      PlotOnStaticMap(MyMap = irelandMap, lat = dataHosp$Latitude, lon = dataHosp$Longitude, destfile = "ireland.png", add = FALSE, col = "red", pch = 8)
   })

   output$stationsmap <- renderPlot({
      PlotOnStaticMap(MyMap = irelandMap, lat = dataStations$Latitude, lon = dataStations$Longitude, destfile = "ireland.png", col = "red", pch = 8, add = FALSE)
   })

   output$fullAnalysisMap <- renderPlot({
      PlotOnStaticMap(MyMap = irelandMap, lat = dataStations$Latitude, lon = dataStations$Longitude, destfile = "ireland.png", col = "blue", pch = 20, add = FALSE)
      PlotOnStaticMap(MyMap = irelandMap, lat = dataHospAnalysis$Latitude, lon = dataHospAnalysis$Longitude, destfile = "ireland.png", col = "red", pch = 8, add = TRUE)
   })

   output$distanceMatrix <- renderDataTable(distance.matrix, options = list(lengthMenu = list(),pageLength = 92))

   output$GroupedBar <- renderPlot({
      barchart(Frequency~Hospitals, groups = Distance, data = classDistance, scales=list(x=list(rot=90,cex=0.8)), auto.key = TRUE)
      #legend("topright", legend = unique(classDistance$Distance), col = c("green", "yellow", "red"), fill = TRUE)
   })

##### End of Script #####  
})