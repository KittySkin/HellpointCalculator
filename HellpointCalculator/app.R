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
        #load selected weapon and add the stats to weaponDamage variable to be operated in the calculations
        switch (input$selectedWeapon,
                "pipe" = weaponDamage <- c(12, 0, 0, 0, 0, 0),
                "shard" = weaponDamage <- c(6, 0, 0, 0, 0, 0),
                "column" = weaponDamage <- c(24, 0, 0, 0, 0, 0),
                "infusedColumn" = weaponDamage <- c(24, 8, 0, 0, 0, 0),
                "boneBat" = weaponDamage <- c(12, 0, 0, 0, 0, 0),
                "scaryScooper" = weaponDamage <- c(6, 0, 0, 0, 0, 0),
                "coffingCrusher" = weaponDamage <- c(24, 0, 0, 0, 0, 0),
                "antiquatedGlaive" = weaponDamage <- c(16, 0, 0, 0, 0, 0),
                "officerGlaive" = weaponDamage <- c(16, 0, 0, 0, 0, 0),
                "officerTuleGlaive" = weaponDamage <- c(16, 4, 0, 0, 0, 0),
                "heaterSpear" = weaponDamage <- c(24, 0, 0, 0, 0, 0),
                "archonSpear" = weaponDamage <- c(20, 0, 0, 0, 0, 0),
                "victimPoker" = weaponDamage <- c(16, 0, 0, 0, 0, 0),
                "shreddingSaber" = weaponDamage <- c(16, 0, 0, 0, 0, 0),
                "wastelandSaber" = weaponDamage <- c(16, 0, 0, 0, 0, 0),
                "railPoignard" = weaponDamage <- c(8, 0, 0, 0, 0, 0),
                "sewingAnlace" = weaponDamage <- c(8, 0, 0, 0, 0, 0),
                "lightStriker" = weaponDamage <- c(16, 20, 0, 0, 0, 0),
                "pryingTool" = weaponDamage <- c(12, 0, 0, 0, 0, 0),
                "tomahawk" = weaponDamage <- c(8, 4, 0, 0, 0, 0),
                "lostHatchet" = weaponDamage <- c(16, 0, 0, 0, 0, 0),
                "ceremonialDagger" = weaponDamage <- c(8, 0, 0, 0, 0, 0),
                "daemonScythe" = weaponDamage <- c(24, 0, 0, 0, 0, 0),
                "linkedEspadon" = weaponDamage <- c(24, 0, 0, 0, 0, 0),
                "thespianHook" = weaponDamage <- c(16, 0, 0, 0, 0, 0),
                "ozysHand" = weaponDamage <- c(16, 0, 0, 0, 0, 0),
                "nemundisOculus" = weaponDamage <- c(20, 0, 0, 0, 0, 0),
                "uthosGavel" = weaponDamage <- c(40, 0, 0, 0, 8, 0),
                "kickstarterLinkedEspadon" = weaponDamage <- c(20, 0, 0, 0, 0, 0),
                "discipleFerula" = weaponDamage <- c(20, 0, 0, 0, 0, 0),
                "prodigialSpawnFerula" = weaponDamage <- c(20, 0, 0, 0, 0, 0),
                "deliberateBurden" = weaponDamage <- c(24, 0, 0, 0, 0, 0),
                "thespianMace" = weaponDamage <- c(24, 0, 0, 0, 0, 0),
                "whaleboneHalberd" = weaponDamage <- c(24, 0, 0, 0, 0, 0),
                "wingedHalberd" = weaponDamage <- c(20, 0, 0, 0, 0, 0)
        )
        
        #need more information on this, so will be set at 0.00 for the time being, supposedly be a value between 0.02 and 0.03
        conductorVariation <- 0.00
        
        #create the vectors containing the character stats and scaling stats
        characterStat <- c(input$characterStrengthInput, input$characterReflexInput, input$characterCognitionInput, input$characterForesightInput)
        weaponBonusStat <- c(input$bonusStrengthInput, input$bonusReflexInput, input$bonusCognitionInput, input$bonusForesightInput)
        
        #define the type of conductor and how the maths have to be done for it
        if (input$conductorTypeInput == "reflex" || input$conductorTypeInput == "strength" || input$conductorTypeInput == "martial"){
            conductorFormula <- "physical"
        }else if(input$conductorTypeInput == "light" || input$conductorTypeInput == "induction" || input$conductorTypeInput == "radiation"){
            conductorFormula <- "elemental"
        }else{
            conductorFormula <- "none"
        }
        
        #calculate variables, formula given by Cradle
        #define the conductor type used and use the formula relevant to it
        if (conductorFormula == "physical"){
            conductorBonus <- input$conductorLevelInput * (1 + conductorVariation)
        }else if (conductorFormula == "elemental"){
            conductorBonus <- (input$conductorLevelInput * 0.02 + 1) ^ 3
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
            switch(input$conductorTypeInput,
                   "light" = (weaponDamage <- weaponDamage + c(0, weaponPhysToElemental, 0, 0, 0, 0)),
                   "induction" = (weaponDamage <- weaponDamage + c(0, 0, 0, weaponPhysToElemental, 0, 0)),
                   "radiation" = (weaponDamage <- weaponDamage + c(0, 0, 0, 0, 0, weaponPhysToElemental)))
            #now do the elemental damage multiplications based on conductorBonus
            switch(input$conductorTypeInput,
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