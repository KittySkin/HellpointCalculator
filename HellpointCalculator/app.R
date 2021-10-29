library(shiny)
library(sp)
library(raster)

ui <- fluidPage(
    titlePanel("Hellpoint Calculator"),
    fluidRow(
        column(3,
               p(strong("Character Stats")),
               numericInput("characterStrengthInput", "Character Strength", value = "1"),
               numericInput("characterReflexInput", "Character Reflex", value = "1"),
               numericInput("characterCognitionInput", "Character Cognition", value = "1"),
               numericInput("characterForesightInput", "Character Foresight", value = "1")
        ),
        column(3,
               p(strong("Weapon Base Damage")),
               numericInput("physicalDamageInput", "Base Physical", value = "0"),
               numericInput("energyDamageInput", "Base Energy", value = "0"),
               numericInput("nihilDamageInput", "Base Nihil", value = "0"),
               numericInput("inductionDamageInput", "Base Induction", value = "0"),
               numericInput("entropyDamageInput", "Base Entropic", value = "0"),
               numericInput("radiationDamageInput", "Base Radiation", value = "0")
        ),
        column(3,
               p(strong("Weapon Scaling")),
               numericInput("bonusStrengthInput", "Bonus Strength", value = "0"),
               numericInput("bonusReflexInput", "Bonus Reflex", value = "0"),
               numericInput("bonusCognitionInput", "Bonus Cognition", value = "0"),
               numericInput("bonusForesightInput", "Bonus Foresight", value = "0")
        ),
        column(3,
               p(strong("Weapon Damage")),
               textOutput("finalPhysicalDamage"),
               textOutput("finalEnergyDamage"),
               textOutput("finalNihilDamage"),
               textOutput("finalInductionDamage"),
               textOutput("finalEntropicDamage"),
               textOutput("finalRadiationDamage"),
               actionButton("calculateButton", "Calculate")
        )
    ),
    fluidRow(
        column(3,
               p(strong("Conductor Information")),
               selectInput("conductorTypeInput", "Conductor Type", choices = c("None" = "none", "Reflex" = "reflex", "Strength" = "strength", "Martial" = "martial", "Light" = "light", "Induction" = "induction", "Radiation" = "radiation")),
               numericInput("conductorLevelInput", "Conductor Level", value = "1")
        ),
        column(9)
    ),
    fluidRow(
        column(12,
               p(strong("Made with love by KittySkin")),
               p(strong("With the help of Cradle Games")),
               p(strong("And the support and help of Patkin"))
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$calculateButton,{
        #initial values to start working
        
        #weapon base damage
        physicalDamage <<- input$physicalDamageInput
        energyDamage <<- input$energyDamageInput
        nihilDamage <<- input$nihilDamageInput
        inductionDamage <<- input$inductionDamageInput
        entropyDamage <<- input$entropyDamageInput
        radiationDamage <<- input$radiationDamageInput
        
        #weapon scaling stats
        bonusStrength <<- input$bonusStrengthInput
        bonusReflex <<- input$bonusReflexInput
        bonusCognition <<- input$bonusCognitionInput
        bonusForesight <<- input$bonusForesightInput
        
        #character stats
        characterStrength <<- input$characterStrengthInput
        characterReflex <<- input$characterReflexInput
        characterCognition <<- input$characterCognitionInput
        characterForesight <<- input$characterForesightInput
        
        #conductor values
        #can be reflex, strength, martial, light, induction, radiation or none
        conductorType <<- input$conductorTypeInput
        conductorLevel <<- input$conductorLevelInput
        #need more information on this, so will be set at 0.00 for the time being, supposedly be a value between 0.02 and 0.03
        conductorVariation <- 0.00
        
        #create the vectors containing the character stats and weapon stats
        characterStat <- c(characterStrength, characterReflex, characterCognition, characterForesight)
        weaponBonusStat <- c(bonusStrength, bonusReflex, bonusCognition, bonusForesight)
        weaponDamage <- c(physicalDamage, energyDamage, nihilDamage, inductionDamage, entropyDamage, radiationDamage)
        
        #define the type of conductor and how the maths have to be done for it
        if (conductorType == "reflex" || conductorType == "strength" || conductorType == "martial"){
            conductorFormula <- "physical"
        }else if(conductorType == "light" || conductorType == "induction" || conductorType == "radiation"){
            conductorFormula <- "elemental"
        }else{
            conductorFormula <- "none"
        }
        
        #calculate variables, formula given by Cradle
        #define the conductor type used and use the formula relevant to it
        if (conductorFormula == "physical"){
            conductorBonus <- conductorLevel * (1 + conductorVariation)
        }else if (conductorFormula == "elemental"){
            conductorBonus <- (conductorLevel * 0.02 + 1) ^ 3
        }else{
            conductorBonus <- 0
        }
        
        #function to calculate bonus stats for the weapon
        b <- clamp((log10(weaponBonusStat + 0.1) * 0.96), 0, 4)
        l <- log10(characterStat + 10) * 1.3 - 1.3
        
        #calculate base damage based on conductor and weapon stats
        if (conductorFormula == "physical"){
            weaponDamage <- weaponDamage + c(conductorBonus, 0, 0, 0, 0, 0)
        }else if(conductorFormula == "elemental"){
            #reduce base physical damage by to 66% of its original value
            weaponDamage <- weaponDamage * c(0.66, 1, 1, 1, 1, 1)
            #get the reduced physical damage that its going to be added to the base elemental damage of the weapon
            weaponPhysToElemental <- weaponDamage[1]
            #do the elemental damage additions
            switch(conductorType,
                   "light" = (weaponDamage <- weaponDamage + c(0, weaponPhysToElemental, 0, 0, 0, 0)),
                   "induction" = (weaponDamage <- weaponDamage + c(0, 0, 0, weaponPhysToElemental, 0, 0)),
                   "radiation" = (weaponDamage <- weaponDamage + c(0, 0, 0, 0, 0, weaponPhysToElemental)))
            #now do the elemental damage multiplications based on conductorBonus
            switch(conductorType,
                   "light" = (weaponDamage <- weaponDamage * c(1, conductorBonus, 1, 1, 1, 1)),
                   "induction" = (weaponDamage <- weaponDamage * c(1, 1, 1, conductorBonus, 1, 1)),
                   "radiation" = (weaponDamage <- weaponDamage * c(1, 1, 1, 1, 1, conductorBonus)))
        }
        
        #calculate the final damage of the weapon
        #get the new b and l values and create a new vector that can be used to work with weaponDamage
        weaponBValue <- c(b[1] + b[2], b[3] + b[4], b[3] + b[4], b[3] + b[4], b[3] + b[4], b[3] + b[4])
        characterLvalue <- c(l[1] + l[2], l[3] + l[4], l[3] + l[4], l[3] + l[4], l[3] + l[4], l[3] + l[4])
        bonusDamage <- characterLvalue * weaponBValue * weaponDamage
        finalDamage <- weaponDamage + bonusDamage
        
        output$finalPhysicalDamage <- renderText({ 
            paste("Physical Damage :", finalDamage[1])
        })
        output$finalEnergyDamage <- renderText({ 
            paste("Energy Damage :", finalDamage[2])
        })
        output$finalNihilDamage <- renderText({ 
            paste("Nihil Damage :", finalDamage[3])
        })
        output$finalInductionDamage <- renderText({ 
            paste("Induction Damage :", finalDamage[4])
        })
        output$finalEntropicDamage <- renderText({ 
            paste("Entropic Damage :", finalDamage[5])
        })
        output$finalRadiationDamage <- renderText({ 
            paste("Radiation Damage :", finalDamage[6])
        })
    })
    
}

shinyApp(ui, server)