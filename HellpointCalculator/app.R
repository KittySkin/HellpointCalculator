library(shiny)
library(sp)
library(raster)

ui <- fluidPage(
    titlePanel("Hellpoint Calculator"),
    fluidRow(
        column(3,
               wellPanel(
               h4(strong("Character Stats")),
               numericInput("characterStrengthInput", "Strength", value = "1"),
               numericInput("characterReflexInput", "Reflex", value = "1"),
               numericInput("characterCognitionInput", "Cognition", value = "1"),
               numericInput("characterForesightInput", "Foresight", value = "1"))
        ),
        column(3,wellPanel(
               h4(strong("Weapon Information")),
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
                   "Winged Halberd" = "wingedHalberd",
                   "Kickstarter Railgun" = "kickstarterRailgun",
                   "Railgun" = "railgun",
                   "Entropic Railgun" = "entropicRailgun",
                   "Marine Rifle" = "marineRifle",
                   "Artillery P17" = "artilleryP17",
                   "Artillery OTX" = "artilleryOTX",
                   "Daemon Cannon" = "daemonCannon",
                   "Mouth of Filth" = "mouthOfFilth",
                   "Channeler of Hell" = "channelerOfHell",
                   "Channeler of Light" = "channelerOfLight",
                   "Etek Avos" = "etekAvos",
                   "Hedron of Entropy" = "hedronOfEntropy",
                   "Hedron of Flame" = "hedronOfFlame",
                   "Hedron of Light" = "hedronOfLight",
                   "White Prophet Hand" = "whiteProphetHand",
                   "Amber Prophet Hand" = "amberProphetHand",
                   "Nihil Prophet Hand" = "nihilProphetHand"
                                                                                )
                           ),
               h4(strong("Conductor Information")),
               selectInput("conductorTypeInput", "Conductor Type", choices = c("None" = "none", "Reflex" = "reflex", "Strength" = "strength", "Martial" = "martial", "Light" = "light", "Induction" = "induction", "Radiation" = "radiation", "Firearm" = "firearm", "Catalyst" = "catalyst")),
               numericInput("conductorLevelInput", "Conductor Level", value = "1"))
               
        ),
        column(3,
               wellPanel(
               h4(strong("Weapon Bonus Stats")),
               numericInput("bonusStrengthInput", "Strength", value = "0"),
               numericInput("bonusReflexInput", "Reflex", value = "0"),
               numericInput("bonusCognitionInput", "Cognition", value = "0"),
               numericInput("bonusForesightInput", "Foresight", value = "0"))
        ),
        column(3,
               wellPanel(
               h4(strong("Weapon Damage")),
               p(("Physical Damage: "), textOutput("finalPhysicalDamage", inline = T)),
               p(("Energy Damage: "), textOutput("finalEnergyDamage", inline = T)),
               p(("Nihil Damage: "), textOutput("finalNihilDamage", inline = T)),
               p(("Induction Damage: "), textOutput("finalInductionDamage", inline = T)),
               p(("Entropic Damage: "), textOutput("finalEntropicDamage", inline = T)),
               p(("Radiation Damage: "), textOutput("finalRadiationDamage", inline = T)),
               br(),
               actionButton("calculateButton", "Calculate"))
        )
    ),
    fluidRow(
        column(12, align="center",
               wellPanel(
                   h4(strong("How to use:")),
                   p("Input your character stats on the first column."),
                   p("Then select your weapon and if using a conductor, the conductor type and level."),
                   p("On the weapon bonus section, input the weapon current bonus stats."),
                   p("If you are testing conductors, remember to update the bonus values to reflect the conductor used."),
                   p("Press calculate and see how it goes."),
                   p("Accuracy its 99%, some stats are a bit off not showing the exact same value as in game.")
                   )
        )
    ),
    fluidRow(
        column(12, align="center",
               wellPanel(
               p(strong("Made with love by KittySkin")),
               p(strong("With the help of Cradle Games")),
               p(strong("And the support and help of Patkin")),
               a("Source Here!", href="https://github.com/KittySkin/HellpointCalculator", target="_blank"))
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$calculateButton,{
        
        #initial values to start working
        #load selected weapon and add the stats to weaponDamage variable to be operated in the calculations
        #vector structure: physical, energy, nihil, induction, entropy, radiation
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
                "wingedHalberd" = weaponDamage <- c(20, 0, 0, 0, 0, 0),
                "kickstarterRailgun" = weaponDamage <- c(0, 12, 0, 0, 0, 0),
                "railgun" = weaponDamage <- c(0, 12, 0, 0, 0, 0),
                "entropicRailgun" = weaponDamage <- c(0, 0, 0, 0, 12, 0),
                "marineRifle" = weaponDamage <- c(0, 12, 0, 0, 0, 0),
                "artilleryP17" = weaponDamage <- c(0, 0, 0, 36, 0, 0),
                "artilleryOTX" = weaponDamage <- c(0, 0, 0, 0, 0, 36),
                "daemonCannon" = weaponDamage <- c(0, 0, 0, 36, 0, 0),
                "mouthOfFilth" = weaponDamage <- c(0, 0, 0, 0, 0, 36),
                "channelerOfHell" = weaponDamage <- c(0, 0, 0, 36, 0, 0),
                "channelerOfLight" = weaponDamage <- c(0, 36, 0, 0, 0, 0),
                "etekAvos" = weaponDamage <- c(0, 0, 36, 0, 0, 0),
                "hedronOfEntropy" = weaponDamage <- c(0, 0, 0, 0, 8, 0),
                "hedronOfFlame" = weaponDamage <- c(0, 0, 0, 8, 0, 0),
                "hedronOfLight" = weaponDamage <- c(0, 16, 0, 0, 0, 0),
                "whiteProphetHand" = weaponDamage <- c(0, 24, 0, 0, 0, 0),
                "amberProphetHand" = weaponDamage <- c(0, 0, 0, 24, 0, 0),
                "nihilProphetHand" = weaponDamage <- c(0, 0, 24, 0, 0, 0)
        )
        
        #create the vectors containing the character stats and scaling stats
        #vector structure: strength, reflex, cognition, foresight
        characterStat <- c(input$characterStrengthInput, input$characterReflexInput, input$characterCognitionInput, input$characterForesightInput)
        weaponBonusStat <- c(input$bonusStrengthInput, input$bonusReflexInput, input$bonusCognitionInput, input$bonusForesightInput)
        
        #define the type of conductor to determine how the maths must be done during the base damage and conductor bonus interaction
        if (input$conductorTypeInput == "reflex" || input$conductorTypeInput == "strength" || input$conductorTypeInput == "martial"){
            conductorFormula <- "physical"
        }else if(input$conductorTypeInput == "light" || input$conductorTypeInput == "induction" || input$conductorTypeInput == "radiation"){
            conductorFormula <- "elemental"
        }else if(input$conductorTypeInput == "firearm" || input$conductorTypeInput == "catalyst"){
            conductorFormula <- "special"
        }else{
            conductorFormula <- "none"
        }
        
        #calculate variables, formula given by Cradle
        #((conductor level * 0.02f) + 1f) ^ 3
        #define if a conductor its used and calculate conductorBonus if used
        if (conductorFormula != "none"){
            conductorBonus <- ((input$conductorLevelInput * 0.02) + 1) ^ 3
        }else{
            #here only to remove possible null errors when the app its used multiple times on a single session
            conductorBonus <- 0
        }
        
        #function to calculate bonus stats for the weapon
        #b = bonus from weapons
        #l = stats from character
        b <- clamp((log10((weaponBonusStat * 0.01) + 0.1) + 1) * 0.96, 0, 4)
        l <- log10(characterStat + 10) * 1.3 - 1.3
        
        #to calculate conductor bonuses we need to first get conductor type
        #then, if the conductor its a physical one (reflex, strength or martial), weapon damage its multiplied by 100% of conductorBonus
        #if conductor its elemental, weapon physical damage gets reduced to 66% and the resulting value its added to the weapon as base elemental damage
        #elemental damage its multiplied by 66% of conductorBonus.
        #elemental weapons use 94% of base physical damage (weaponDamage[1]) for physical conductors instead of 100% like regular ones.
        #calculate base damage based on conductor and weapon stats
        if (conductorFormula == "physical"){
            if (weaponDamage[2] > 0 || weaponDamage[3] > 0 || weaponDamage[4] > 0 || weaponDamage[5] > 0 || weaponDamage[6] > 0){
                weaponDamage <- weaponDamage * c(conductorBonus * 0.94, 1, 1, 1, 1, 1)
            }else{
                weaponDamage <- weaponDamage * c(conductorBonus, 1, 1, 1, 1, 1)
            }
        }else if(conductorFormula == "elemental"){
            #reduce base physical damage by to 66% of its original value
            #store original weapon damage to be used later, just in case
            originalWeaponDamage <- weaponDamage
            weaponDamage <- weaponDamage * c(0.66, 1, 1, 1, 1, 1)
            weaponElementalPortion <- originalWeaponDamage[1] * 0.66
            #do the elemental damage additions based on the obtained weaponElementalPortion
            switch(input$conductorTypeInput,
                   "light" = (weaponDamage <- weaponDamage + c(0, weaponElementalPortion, 0, 0, 0, 0)),
                   "induction" = (weaponDamage <- weaponDamage + c(0, 0, 0, weaponElementalPortion, 0, 0)),
                   "radiation" = (weaponDamage <- weaponDamage + c(0, 0, 0, 0, 0, weaponElementalPortion)))
            #now do the elemental damage multiplications based on 66% of conductorBonus
            switch(input$conductorTypeInput,
                   "light" = (weaponDamage <- weaponDamage * c(1, conductorBonus * 0.66, 1, 1, 1, 1)),
                   "induction" = (weaponDamage <- weaponDamage * c(1, 1, 1, conductorBonus * 0.66, 1, 1)),
                   "radiation" = (weaponDamage <- weaponDamage * c(1, 1, 1, 1, 1, conductorBonus * 0.66)))
        }else if(conductorFormula == "special"){
            #formula for firearms and catalysts, checks pending
            weaponDamage <- weaponDamage * c(conductorBonus, conductorBonus, conductorBonus, conductorBonus, conductorBonus, conductorBonus)
        }
        
        #calculate the final damage of the weapon
        #get the new b and l values and create a new vector that can be used to work with weaponDamage for each weapon scaling stat
        #multiply the weaponDamage obtained from conductor operations by the bonuses in order to obtain stat gains per bonus stat
        #physical damage its increased by strength and reflex, elemental damage its increased by cognition and foresight
        bonusDamageByStrength <- c(b[1], 0, 0, 0, 0, 0) * c(l[1], 0, 0, 0, 0, 0) * weaponDamage
        bonusDamageByReflex <- c(b[2], 0, 0, 0, 0, 0) * c(l[2], 0, 0, 0, 0, 0) * weaponDamage
        bonusDamageByCognition <- c(0, b[3], b[3], b[3], b[3], b[3]) * c(0, l[3], l[3], l[3], l[3], l[3]) * weaponDamage
        bonusDamageByForesight <- c(0, b[4], b[4], b[4], b[4], b[4]) * c(0, l[4], l[4], l[4], l[4], l[4]) * weaponDamage
        #add all the stats bonuses obtained from the previous operations to get the final weapon damage
        finalDamage <- weaponDamage + bonusDamageByStrength + bonusDamageByReflex + bonusDamageByCognition + bonusDamageByForesight
        
        #display the final weapon damage on the UI
        output$finalPhysicalDamage <- renderText({ 
            paste(round(finalDamage[1]))
        })
        output$finalEnergyDamage <- renderText({ 
            paste(round(finalDamage[2]))
        })
        output$finalNihilDamage <- renderText({ 
            paste(round(finalDamage[3]))
        })
        output$finalInductionDamage <- renderText({ 
            paste(round(finalDamage[4]))
        })
        output$finalEntropicDamage <- renderText({ 
            paste(round(finalDamage[5]))
        })
        output$finalRadiationDamage <- renderText({ 
            paste(round(finalDamage[6]))
        })
    })
    #kill app once session its closed in order to reduce server usage
    session$onSessionEnded(function() {
        stopApp()
    })
    
}

shinyApp(ui, server)