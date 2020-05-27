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
    titlePanel("Projectile Flight Simulation"),
    hr(),
    
    fluidRow(
        
        column(4,
                wellPanel(
                    h3("Projectile"),
                    numericInput("projectileDiameter", "Diameter (m)", 1, min = 0.001),
                    numericInput("projectileMass", "Mass (kg)", 1, min = 0.0001),
                    hr(),
                    h3("Launch Paramters"),
                    numericInput("initVelocity", "Launch Velocity (m/s)", 10, min = 1),
                    numericInput("launchHeight", "Launch Height (m)", 0, min = 0),
                    sliderInput("launchAngle", label = h5("Launch Angle"), min = 0, 
                               max = 90, value = 45, width = "100%"),
                    hr(),
                    numericInput("fluidDensity", "Fluid density (kg/m^3)", 1.225, min = 0), 
                    textOutput("fluidDensityInfo"),
               )      
        ),
        
        column(8,
               fluidRow(
                   plotOutput("trajectoryPlot")
               ),
               column(width = 7,
                  wellPanel(
                      fluidRow(
                          column(width = 6,
                             h4("Vacuum"),
                          ),
                          column(width = 6,
                             h4("Drag"),
                          ),
                          column(width = 6,
                                 h5("Distance"),
                                 textOutput("maxDistance"),
                                 h5("Max Height"),
                                 textOutput("maxHeight"),
                                 h5("Landing Velocity"),
                                 textOutput("landingSpeed"),
                          ),
                          column(width = 6,
                                 h5("Distance"),
                                 textOutput("maxDistanceDrag"),
                                 h5("Max Height"),
                                 textOutput("maxHeightDrag"),
                                 h5("Landing Velocity"),
                                 textOutput("landingSpeedDrag"),
                          )
                      )
                  )
               ),
               column(width = 5,
                   wellPanel(
                       h4("Chart Configurations"),
                       fluidRow(
                           column(width = 6,
                               checkboxInput("lockMaxX", "Lock X", value = FALSE),
                               conditionalPanel(
                                    condition = "input.lockMaxX == true",
                                    numericInput("maxX", "X axis", 11, min = 10))
                            ),
                           column(width = 6,
                               checkboxInput("lockMaxY", "Lock Y", value = FALSE),
                               conditionalPanel(
                                    condition = "input.lockMaxY == true",
                                    numericInput("maxY", "Y axis", 3, min = 10))
                            )
                       )
                   )
               )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observe({
        if(is.na(input$projectileMass) | 
           is.na(input$projectileDiameter) |
           is.na(input$initVelocity) | 
           is.na(input$launchHeight) |
           is.na(input$fluidDensity)
           ){
            return()
        }
        
        dragParameter = quadraticDragParameter(input$fluidDensity, input$projectileDiameter)
        trajectoryWithDrag = computeTrajectoryWithDrag(dragParameter, input$projectileMass, input$initVelocity, input$launchAngle, input$launchHeight, 0.01)
        trajectory = computeTrajectoryWithDrag(0, input$projectileMass, input$initVelocity, input$launchAngle, input$launchHeight, 0.03)
        
        xLockMax = round(getLims(trajectory$x, trajectoryWithDrag$x, FALSE)[2], 1)
        yLockMax = round(getLims(trajectory$y, trajectoryWithDrag$y, FALSE)[2], 1)
        
        updateNumericInput(session, "maxX", value = xLockMax)
        updateNumericInput(session, "maxY", value = yLockMax)
        
        output$maxDistance <- function(){return(paste(round(max(trajectory$x), 1), "m"))}
        output$maxHeight <- function(){return(paste(round(max(trajectory$y), 1), "m"))}
        
        output$maxDistanceDrag <- function(){return(paste(round(max(trajectoryWithDrag$x), 1), "m"))}
        output$maxHeightDrag <- function(){return(paste(round(max(trajectoryWithDrag$y), 1), "m"))}
        
        output$trajectoryPlot <- renderPlot({
            xlimits = getLims(trajectory$x, trajectoryWithDrag$x, input$lockMaxX, input$maxX)
            ylimits = getLims(trajectory$y, trajectoryWithDrag$y, input$lockMaxY, input$maxY)
            
            plot(c(-1, -1), xlim = xlimits, ylim = ylimits, xlab = "x", ylab = "y", main = "Flight trajectory of a projectile with and without drag")
            lines(trajectoryWithDrag$x, trajectoryWithDrag$y, col = 'blue', lwd = 4)
            lines(trajectory$x, trajectory$y, col = 'red', lwd = 4)
            legend("topright", legend=c("vacuum", "drag"),
                   col=c("red", "blue"), lty=1:1, cex=0.8, lwd = 4)
        })
    })

    quadraticDragParameter <- function(fluidDensity, diameter){
        return(pi / 16 * fluidDensity * diameter ^ 2)
    }
    
    dragForce <- function(velocity, dragParameter, mass){
        velocityUnitVetcor = velocity / abs(velocity)
        
        dragForce = -1 * dragParameter * velocity ^ 2  * velocityUnitVetcor
        
        return(dragForce)
    }
    
    gravitationalForce<- function(mass){
        gravitationalForce = -9.834 * mass
        
        return(gravitationalForce)
    }
    
    horizontalAcceleration <- function(horizontalVelocity, dragParameter, mass){
        hAcc = dragForce(horizontalVelocity, dragParameter, mass) / mass
        
        return(hAcc)
    }
    
    verticalAcceleration <- function(verticalVelocity, dragParamerter, mass){
        vAcc = (gravitationalForce(mass) + dragForce(verticalVelocity, dragParamerter, mass)) / mass
        
        return(vAcc)
    }
    
    velocity <- function(currentVelocity, acceleration, timeStep){
        velocity = currentVelocity + acceleration * timeStep
        
        return(velocity)
    }
    
    position <- function(currentPosition, velocity, timeStep){
        position = currentPosition + velocity * timeStep
        
        return(position)
    }
    
    radians <- function(angle){
        rad = angle / 180 * pi
        
        return(rad)
    }
    
    computeTrajectoryWithDrag <- function(dragParameter, mass, initVelocity, launchAngle, launchHeight, timeStep){
        trajectory = data.frame(x = 0, y = launchHeight)
        
        xVelocity = initVelocity * cos(radians(launchAngle))
        yVelocity = initVelocity * sin(radians(launchAngle))
        
        repeat{
            currentPosition = tail(trajectory, n = 1)
            currentPosition$x = position(currentPosition$x, xVelocity, timeStep)
            currentPosition$y = position(currentPosition$y, yVelocity, timeStep)
            trajectory = rbind(trajectory, c(currentPosition$x, currentPosition$y))
            
            if(currentPosition$y <= 0){
                
                landingSpeed = sqrt(xVelocity * xVelocity + yVelocity * yVelocity)
                
                if(dragParameter == 0){
                    output$landingSpeed <- function(){return(paste(round(landingSpeed, 1), "m/s"))}
                }else{
                    output$landingSpeedDrag <- function(){return(paste(round(landingSpeed, 1), "m/s"))}
                }

                break
            }
            
            xAcceleration = horizontalAcceleration(xVelocity, dragParameter, mass)
            yAcceleration = verticalAcceleration(yVelocity, dragParameter, mass)
            
            xVelocity = velocity(xVelocity, xAcceleration, timeStep)
            yVelocity = velocity(yVelocity, yAcceleration, timeStep)
        }
        
        return(trajectory)
    }
    
    getLims <- function(x1, x2, lockMax, lockMaxValue){
        x1Max = max(x1)
        x2Max = min(x2)
        
        if(lockMax){
            max = lockMaxValue
        }else{
            max = max(c(x1Max, x2Max)) 
        }

        return(c(0, max))
    }
    
    output$fluidDensityInfo <- function(){
        return("Default set to air density: 1.225 kg/m^3")
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
