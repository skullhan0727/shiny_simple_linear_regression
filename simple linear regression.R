#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Simple Linear Regression"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        "Try to find values for the slope and intercept that minimize the residual error from the linear model.",
        sliderInput("Intercept",
                    "Intercept",
                    min = -20,
                    max = -15,
                    value =-18,
                    round=T
                    ),
        sliderInput("Slope",
                    "Slope",
                    min = 0,
                    max = 5,
                    value = 4,
                    round=T),
        checkboxInput("checkbox","Show summary(lm(y ~ x))", value = FALSE)
      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("scatterplot"),
         plotOutput("sse"),
         conditionalPanel(
           "Linear model summary",
           condition = "input.checkbox == true",
           verbatimTextOutput("summary")
           
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$scatterplot <- renderPlot({
     plot(cars$speed,cars$dist,axes=F,pch=16,xlab="x",ylab="y",main="Linear Model Y~X")
     axis(side=1,las=0)
     axis(side=2,las=0)
     abline(input$Intercept, input$Slope,lwd=2)
     segments(cars$speed, input$Intercept+cars$speed*input$Slope, cars$speed, cars$dist, lwd=2,col = "red")
     legend(5,110, legend=paste("y=",input$Intercept,'+', input$Slope,'*x'),col=c("black"),lty=1,lwd=2,
            box.lty=0)
     
   })
   
   output$sse <- renderPlot({
     par(mfrow=c(2,1))
     plot(seq(80000), rep(0,80000), 
          axes=FALSE, #Don't plot the axis 
          type="n",  #hide the points
          ylab="", xlab="",main="Sum of Squares of Residuals")
     axis(1)
     
     correct_sse=sum((cars$dist-(-18+cars$speed*4))^2)
     predict_sse=sum((cars$dist-(input$Intercept+cars$speed*input$Slope))^2)
     points(correct_sse,-0.5,pch=4,cex=3,lwd=2)
     if (input$Intercept==-18 &input$Slope==4){
       points(predict_sse,-0.5,pch=1,cex=3,col="green",lwd=2) 
     }else 
     {points(predict_sse,-0.5,pch=1,cex=3,col="red",lwd=2) }
     
     
     residuals=cars$dist-(input$Intercept+cars$speed*input$Slope)
     correct_residuals=cars$dist-(-18+cars$speed*4)
     hist(residuals,main="Distribution of Residuals",col ="grey",axes=F,xlab="",ylab="",probability = T,xlim=c(-80,150))
     axis(1)
     rug(residuals, col="black", lwd=2)
     if (input$Intercept==-18 &input$Slope==4){
       lines(density(residuals),col="green",lwd=2)
     }else {lines(density(correct_residuals),col="red",lwd=2)}
   })
   
   
   output$summary=renderPrint({
     summary(lm(cars$dist~cars$speed))})
}

# Run the application 
shinyApp(ui = ui, server = server)

