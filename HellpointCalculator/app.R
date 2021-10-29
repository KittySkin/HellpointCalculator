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
               p(strong("Weapon Information")),
               selectInput("selectedWeapon", "Select Weapon", choices = c(
                   "Pipe" = "pipe",
                   "Shard" = "shard",
                   "Column" = "column",
                   "Infused Column" = "infusedColumn",
                   "Bone Bat" = "boneBat",
                   "Scary Scooper" = "scaryScooper",
                   "Coffing Crusher" = "coffingCrusher",
                   "Antiquated Glaive" = "antiquatedGlaive",
                   "Officer Glaive" = "officerGlaive",
                   "Officer Tule Glaive" = "officerTuleGlaive",
                   "Heater Spear" = "heaterSpear",
                   "Archon Spear" = "archonSpear",
                   "Victim Poker" = "victimPoker",
                   "Shredding Saber" = "shreddingSaber",
                   "Wasteland Saber" = "wastelandSaber",
                   "Rail Poignard" = "railPoignard",
                   "Sewing Anlace" = "sewingAnlace",
                   "Light Striker" = "lightStriker",
                   "Prying Tool" = "pryingTool",
                   "Tomahawk" = "tomahawk",
                   "Lost Hatchet" = "lostHatchet",
                   "Ceremonial Dagger" = "ceremonialDagger",
                   "Daemon Scythe" = "daemonScythe",
                   "Linked Espadon" = "linkedEspadon",
                   "Thespian Hook" = "thespianHook",
                   "Ozy's Hand" = "ozysHand",
                   "Nemundis Oculus" = "nemundisOculus",
                   "Uthos Gavel" = "uthosGavel",
                   "Kickstarter Linked Espadon" = "kickstarterLinkedEspadon",
                   "Disciple Ferula" = "discipleFerula",
                   "Prodigial Spawn Ferula" = "prodigialSpawnFerula",
                   "Deliberate Burden" = "deliberateBurden",
                   "Thespian Mace" = "thespianMace",
                   "Whalebone Halberd" = "whaleboneHalberd",
                   "Winged Halberd" = "wingedHalberd"
                                                                                )
                           ),
               selectInput("conductorTypeInput", "Conductor Type", choices = c("None" = "none", "Reflex" = "reflex", "Strength" = "strength", "Martial" = "martial", "Light" = "light", "Induction" = "induction", "Radiation" = "radiation")),
               numericInput("conductorLevelInput", "Conductor Level", value = "1")
               
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
        column(12,
               p(strong("Made with love by KittySkin")),
               p(strong("With the help of Cradle Games")),
               p(strong("And the support and help of Patkin"))
        )
    )
)

server <- function(input, output) {
    observeEvent(input$calculateButton,{
        #initial values to start working
        
        switch (input$selectedWeapon,
                "pipe" = stats <- c(12, 0, 0, 0, 0, 0),
                "shard" = stats <- c(6, 0, 0, 0, 0, 0),
                "column" = stats <- c(24, 0, 0, 0, 0, 0),
                "infusedColumn" = stats <- c(24, 8, 0, 0, 0, 0),
                "boneBat" = stats <- c(12, 0, 0, 0, 0, 0),
                "scaryScooper" = stats <- c(6, 0, 0, 0, 0, 0),
                "coffingCrusher" = stats <- c(24, 0, 0, 0, 0, 0),
                "antiquatedGlaive" = stats <- c(16, 0, 0, 0, 0, 0),
                "officerGlaive" = stats <- c(16, 0, 0, 0, 0, 0),
                "officerTuleGlaive" = stats <- c(16, 4, 0, 0, 0, 0),
                "heaterSpear" = stats <- c(24, 0, 0, 0, 0, 0),
                "archonSpear" = stats <- c(20, 0, 0, 0, 0, 0),
                "victimPoker" = stats <- c(16, 0, 0, 0, 0, 0),
                "shreddingSaber" = stats <- c(16, 0, 0, 0, 0, 0),
                "wastelandSaber" = stats <- c(16, 0, 0, 0, 0, 0),
                "railPoignard" = stats <- c(8, 0, 0, 0, 0, 0),
                "sewingAnlace" = stats <- c(8, 0, 0, 0, 0, 0),
                "lightStriker" = stats <- c(16, 20, 0, 0, 0, 0),
                "pryingTool" = stats <- c(12, 0, 0, 0, 0, 0),
                "tomahawk" = stats <- c(8, 4, 0, 0, 0, 0),
                "lostHatchet" = stats <- c(16, 0, 0, 0, 0, 0),
                "ceremonialDagger" = stats <- c(8, 0, 0, 0, 0, 0),
                "daemonScythe" = stats <- c(24, 0, 0, 0, 0, 0),
                "linkedEspadon" = stats <- c(24, 0, 0, 0, 0, 0),
                "thespianHook" = stats <- c(16, 0, 0, 0, 0, 0),
                "ozysHand" = stats <- c(16, 0, 0, 0, 0, 0),
                "nemundisOculus" = stats <- c(20, 0, 0, 0, 0, 0),
                "uthosGavel" = stats <- c(40, 0, 0, 0, 8, 0),
                "kickstarterLinkedEspadon" = stats <- c(20, 0, 0, 0, 0, 0),
                "discipleFerula" = stats <- c(20, 0, 0, 0, 0, 0),
                "prodigialSpawnFerula" = stats <- c(20, 0, 0, 0, 0, 0),
                "deliberateBurden" = stats <- c(24, 0, 0, 0, 0, 0),
                "thespianMace" = stats <- c(24, 0, 0, 0, 0, 0),
                "whaleboneHalberd" = stats <- c(24, 0, 0, 0, 0, 0),
                "wingedHalberd" = stats <- c(20, 0, 0, 0, 0, 0)
        )
        
        #weapon base damage
        physicalDamage <<- stats[1]
        energyDamage <<- stats[2]
        nihilDamage <<- stats[3]
        inductionDamage <<- stats[4]
        entropyDamage <<- stats[5]
        radiationDamage <<- stats[6]
        
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
            paste("Physical Damage :", round(finalDamage[1]))
        })
        output$finalEnergyDamage <- renderText({ 
            paste("Energy Damage :", round(finalDamage[2]))
        })
        output$finalNihilDamage <- renderText({ 
            paste("Nihil Damage :", round(finalDamage[3]))
        })
        output$finalInductionDamage <- renderText({ 
            paste("Induction Damage :", round(finalDamage[4]))
        })
        output$finalEntropicDamage <- renderText({ 
            paste("Entropic Damage :", round(finalDamage[5]))
        })
        output$finalRadiationDamage <- renderText({ 
            paste("Radiation Damage :", round(finalDamage[6]))
        })
    })
    
}

shinyApp(ui, server)