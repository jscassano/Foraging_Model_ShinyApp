library(shiny)
library(dplyr)
library(tidyverse)
library(cowplot)
library(vctrs)
library(magrittr)


ui <- fluidPage(
          titlePanel("Foraging Model"),
          mainPanel( 
            fluidRow( 
              column(6,
          
          #Nectar load per flower (w) Slider 
          numericInput(inputId = "w", 
                      label = "Nectar load per flower (mg): 50% = 3; 30% = 1.69", 
                      value = 3),
          #Water load per flower (ww) Slider
          numericInput(inputId = "ww", 
                      label = "Water Weight (mg): 50% = 3;  30% = 5.63 ", 
                      value = 3),
            #Time spent from hive to patch; 50m = ~235s; T0
          numericInput(inputId = "tau0", 
                        label = "One-way travel time: FH = 14.2; FL = 14.1; SH = 16.9; SL = 17.4", 
                        value = 14.2),
          #Interflower Time
          numericInput(inputId = "tau",
                    label = "Interflower Time (s): FH = 7.5; FL = 7.7; SH = 8.6; SL = 8.3",
                    value = 7.5),
          # Handling Time: Time Landed (s)
          numericInput(inputId = "h", 
                      label = "Handling Time (s): FH = 12.2; FL = 11.2; SH = 12.1; SL = 11.3", 
                      value = 12.2), 
            #Linear increment of MR Slider
                sliderInput(inputId = "aperj", 
                        label = "Linear increment of MR (W/J)", 
                        min = .00001, max = .0001,  value = .00005),
          #(W) MR in hive; Metabolic rate of active moving bee not in flight
                sliderInput(inputId = "at", 
                      label = "MR in hive (W)", 
                      min = .001, max = .0100,  value = .0042)
              ),
          column(6,
       
              tabsetPanel(
                #Outputs
                tabPanel("Efficiency and Rate Plots", plotOutput(outputId = "Plot.P", width="600px",height="600px")),
                tabPanel("Efficiency Plot: Fast/Slow",plotOutput(outputId = "Plot.E"), tableOutput(outputId = "table.E") ),
                tabPanel("Rate Plot: Fast/Slow", plotOutput(outputId = "Plot.R"), tableOutput(outputId = "table.R"))
                          )
                  )
                 )
                )
              )
                  
   
#########################################################################################                       
server <- function(input, output) {
  # Follow these three rules
  # 1. Save the output that you build to output$
  # 2. Build the output with a render*() fxn
  # 3. Access input values with input$
  #Model Plots
    output$Plot.P <- renderPlot({  
    #Model Code  
            #PARAMETERS
              N <- 1:60     # number flowers
              c <- 16.7         # (J/mg) weight-specific energetic sucrose value (given)
              aperJ <- 0.00005  # (W/J) Linear increment of MR; for every 1 unit increase in Joules, MR increases 
              a <- aperJ*c      # (W) linear increment of MR as a function of load weight
              a0 <- 0.0334      # (W) FMR; Metabolic rate during flight (unloaded) 
              at <- 0.0042      # (W) MR in hive; Metabolic rate of active moving bee not in flight
              ah <- at          # (W) MR during handling; assumed to be the same as at
              w <- .6      # (mg) nectar load per flower
              ww <- .6    #(mg) Water Weight per flower
              wj <- c*w     # (joules) nectar load per flower
              W <- N*w      # Total weight gained by number of flowers 
              h <- 12     # (seconds) time at each flower (handling)
              T0 <- 235         # (seconds) time spent in hive
              tau <- 20   # (seconds) time spent flying between flowers
              tau0 <- 22.8     # (seconds) one-way travel time
                        
              # Set slider inputs   
              w <- input$w
              ww <- input$ww
              tau0 <- input$tau0
              tau <- input$tau
              aperj <- input$aperj
              at <- input$at
              h <- input$h
  
              fmr <- c(0.02018139, 0.0618627) #Imput STephen's FMR values in Watts
          
                long.tib <-  matrix(nrow = vec_size(fmr) * length(N),  # dataframe row length based on fmr and N
                                   ncol = 4) %>%
                  as.data.frame() %>%
                  dplyr::rename(N=V1, Efficiency=V2, Rate=V3, FMR=V4 )
                
                for(i in 1:length(fmr)){
                  a0 <- fmr[i]
                  
                  for(j in N){
                    # Total energy expenditure in patch (arena)
                    # Cp = a0*(N - 1)* tau + a*(N*(N-1)/2)*w*tau + ah*N*h
                    Cp <- round(a0*(N[j]-1)*tau + (a*((N[j]*(N[j]-1))/2)*(w+w)*tau) + ah*N[j]*h, 3) 
                    
                    # Total expenditure during travel to and from
                    # Ct = a0*tau0 + (a0 + a*W)*tau0
                    Ct <- round(a0*tau0 + (a0 + a*N[j]*(w+w))*tau0, 3) 
                    
                    # Total energy expenditure per foraging cycle
                    # C = cp + ct + t0*at
                    C <- round(Cp + Ct + T0*at, 3)  
                    
                    # Gross energy load for N flowers visited
                    # G = N*c*w
                    G <- N[j]*wj 
                    
                    # round-trip time for an entire foraging trip
                    # T = 2*t0 + (N- 1)*t + N*h + t0
                    T <- 2*tau0 + (N[j]-1)*tau + N[j]*h + T0
                    
                    # (G-C)/C
                    Efficiency <- round((G - C)/C, 4)
                    # (G-C)/T
                    Rate <- round((G - C)/T, 4)
                    
                    row.number <- (i-1)*60 + j
                    my.row <- c(N[j], Efficiency, Rate, a0)   #Somethere here is the PROBLEM
                    long.tib[row.number,] <- my.row
                  }
                }
                
                
  
                #create a list of ggplots, where every plot uses a different FMR
                ggplot.list <- list()
                
                #this for loop creates a plot for each unique value of FMR (basically groups plots by FMR as if it were a categorical variable)
                for(i in 1:length(unique(long.tib$FMR))){
                  practice.tib <- long.tib %>%
                    filter(FMR == fmr[i])
                  
                  eff.max.N <- practice.tib %>% 
                    filter(practice.tib$Efficiency == max(practice.tib$Efficiency)) %>%
                    select(N)
                  rate.max.N <- practice.tib %>% 
                    filter(practice.tib$Rate == max(practice.tib$Rate)) %>%
                    select(N)
                  
                  p <- ggplot(data = practice.tib) +
                    geom_point(aes(x = N, y = Efficiency, color= "Efficiency")) +
                    geom_point(aes(x = N, y = Rate*(11/0.25), color = "Rate")) +
                    geom_vline(xintercept =  eff.max.N[[1,1]], color = "blue") +
                    geom_vline(xintercept =  rate.max.N[[1,1]], color = "red") +
                    scale_y_continuous(limits = c(0,20),
                                       sec.axis = sec_axis(~ . *0.025/11, name = "Rate")) +
                    scale_color_manual(values = c("blue", "red")) +
                    labs(y = "Efficiency", x = "Rate", color = NULL) +
                    ggtitle( fmr[i] ) +
                    xlab("Flower Visits (N)") +
                    theme_bw() +
                    theme(legend.key = element_rect(colour = NA, fill = "transparent"),
                          legend.justification = c(1,0), legend.position = c(1,0),
                          legend.title = NULL,
                          legend.text = element_text( size = 10), 
                          legend.background = element_rect(fill = "transparent"),
                          plot.title = element_text(face = "bold", size = 12, hjust = .5))
              
                  ggplot.list[[i]] <- p
                }
                #combine plots onto one page
                cowplot::plot_grid(plotlist =ggplot.list, nrow = 1, ncol = 2)
    }) 

#############################################################################################
  #Efficiency Table
    output$table.E <- renderTable({  
      #Model Code  
      #PARAMETERS
      N <- 1:60     # number flowers
      c <- 16.7         # (J/mg) weight-specific energetic sucrose value (given)
      aperJ <- 0.00005  # (W/J) Linear increment of MR; for every 1 unit increase in Joules, MR increases 
      a <- aperJ*c      # (W) linear increment of MR as a function of load weight
      a0 <- 0.0334      # (W) FMR; Metabolic rate during flight (unloaded) 
      at <- 0.0042      # (W) MR in hive; Metabolic rate of active moving bee not in flight
      ah <- at          # (W) MR during handling; assumed to be the same as at
      w <- .6      # (mg) nectar load per flower
      ww <- .6    #(mg) Water Weight per flower
      wj <- c*w     # (joules) nectar load per flower
      W <- N*w      # Total weight gained by number of flowers 
      h <- 12     # (seconds) time at each flower (handling)
      T0 <- 235         # (seconds) time spent in hive
      tau <- 20   # (seconds) time spent flying between flowers
      tau0 <- 22.8     # (seconds) one-way travel time
      
      # Set slider inputs   
      w <- input$w
      ww <- input$ww
      tau0 <- input$tau0
      tau <- input$tau
      aperj <- input$aperj
      at <- input$at
      h <- input$h
      
      fmr <- c(0.02018139, 0.0618627) #Imput STephen's FMR values in Watts
      
      long.tib <-  matrix(nrow = vec_size(fmr) * length(N),  # dataframe row length based on fmr and N
                          ncol = 4) %>%
        as.data.frame() %>%
        rename(N=V1, Efficiency=V2, Rate=V3, FMR=V4 )
      
      for(i in 1:length(fmr)){
        a0 <- fmr[i]
        
        for(j in N){
          # Total energy expenditure in patch (arena)
          # Cp = a0*(N - 1)* tau + a*(N*(N-1)/2)*w*tau + ah*N*h
          Cp <- round(a0*(N[j]-1)*tau + (a*((N[j]*(N[j]-1))/2)*(w+ww)*tau) + ah*N[j]*h, 3) 
          
          # Total expenditure during travel to and from
          # Ct = a0*tau0 + (a0 + a*W)*tau0
          Ct <- round(a0*tau0 + (a0 + a*N[j]*(w+ww))*tau0, 3) 
          
          # Total energy expenditure per foraging cycle
          # C = cp + ct + t0*at
          C <- round(Cp + Ct + T0*at, 3)  
          
          # Gross energy load for N flowers visited
          # G = N*c*w
          G <- N[j]*wj 
          
          # round-trip time for an entire foraging trip
          # T = 2*t0 + (N- 1)*t + N*h + t0
          T <- 2*tau0 + (N[j]-1)*tau + N[j]*h + T0
          
          # (G-C)/C
          Efficiency <- round((G - C)/C, 4)
          # (G-C)/T
          Rate <- round((G - C)/T, 4)
          
          row.number <- (i-1)*60 + j
          my.row <- c(N[j], Efficiency, Rate, a0)   #Somethere here is the PROBLEM
          long.tib[row.number,] <- my.row
        }
      }
      
      #create wide form tibble for Efficiency data only
      long.tib.E <- select(long.tib, -c(Rate))
      wide.tib.E <-  pivot_wider(long.tib.E, names_from = FMR, values_from = Efficiency)
      
      wide.tib.E <-rename(wide.tib.E, Slow = `0.02018139`)
      wide.tib.E <-rename(wide.tib.E, Fast = `0.0618627`)
      
      wide.tib.E
    })

    output$Plot.E <- renderPlot({
      #Model Code  
      #PARAMETERS
      N <- 1:60     # number flowers
      c <- 16.7         # (J/mg) weight-specific energetic sucrose value (given)
      aperJ <- 0.00005  # (W/J) Linear increment of MR; for every 1 unit increase in Joules, MR increases 
      a <- aperJ*c      # (W) linear increment of MR as a function of load weight
      a0 <- 0.0334      # (W) FMR; Metabolic rate during flight (unloaded) 
      at <- 0.0042      # (W) MR in hive; Metabolic rate of active moving bee not in flight
      ah <- at          # (W) MR during handling; assumed to be the same as at
      w <- .6      # (mg) nectar load per flower
      ww <- .6    #(mg) Water Weight per flower
      wj <- c*w     # (joules) nectar load per flower
      W <- N*w      # Total weight gained by number of flowers 
      h <- 12     # (seconds) time at each flower (handling)
      T0 <- 235         # (seconds) time spent in hive
      tau <- 20   # (seconds) time spent flying between flowers
      tau0 <- 22.8     # (seconds) one-way travel time
      
      # Set slider inputs   
      w <- input$w
      ww <- input$ww
      tau0 <- input$tau0
      tau <- input$tau
      aperj <- input$aperj
      at <- input$at
      h <- input$h
      
      fmr <- c(0.02018139, 0.0618627) #Imput STephen's FMR values in Watts
      
      long.tib <-  matrix(nrow = vec_size(fmr) * length(N),  # dataframe row length based on fmr and N
                          ncol = 4) %>%
        as.data.frame() %>%
        rename(N=V1, Efficiency=V2, Rate=V3, FMR=V4 )
      
      for(i in 1:length(fmr)){
        a0 <- fmr[i]
        
        for(j in N){
          # Total energy expenditure in patch (arena)
          # Cp = a0*(N - 1)* tau + a*(N*(N-1)/2)*w*tau + ah*N*h
          Cp <- round(a0*(N[j]-1)*tau + (a*((N[j]*(N[j]-1))/2)*(w+ww)*tau) + ah*N[j]*h, 3) 
          
          # Total expenditure during travel to and from
          # Ct = a0*tau0 + (a0 + a*W)*tau0
          Ct <- round(a0*tau0 + (a0 + a*N[j]*(w+ww))*tau0, 3) 
          
          # Total energy expenditure per foraging cycle
          # C = cp + ct + t0*at
          C <- round(Cp + Ct + T0*at, 3)  
          
          # Gross energy load for N flowers visited
          # G = N*c*w
          G <- N[j]*wj 
          
          # round-trip time for an entire foraging trip
          # T = 2*t0 + (N- 1)*t + N*h + t0
          T <- 2*tau0 + (N[j]-1)*tau + N[j]*h + T0
          
          # (G-C)/C
          Efficiency <- round((G - C)/C, 4)
          # (G-C)/T
          Rate <- round((G - C)/T, 4)
          
          row.number <- (i-1)*60 + j
          my.row <- c(N[j], Efficiency, Rate, a0)   #Somethere here is the PROBLEM
          long.tib[row.number,] <- my.row
        }
      }
      
      #create wide form tibble for Efficiency data only
      long.tib.E <- select(long.tib, -c(Rate))
      wide.tib.E <-  pivot_wider(long.tib.E, names_from = FMR, values_from = Efficiency)
      
      wide.tib.E <-rename(wide.tib.E, Slow = `0.02018139`)
      wide.tib.E <-rename(wide.tib.E, Fast = `0.0618627`)
      wide.tib.E
     
    #GGPlot  
      #Vert Lines
      eff.maxN.Slow <- wide.tib.E %>% 
        filter(wide.tib.E$Slow == max(wide.tib.E$Slow)) %>%
        select(N)
      
      eff.maxN.Fast <- wide.tib.E %>% 
        filter(wide.tib.E$Fast == max(wide.tib.E$Fast)) %>%
        select(N)

      p2 <- ggplot(data = wide.tib.E) +
        geom_point(aes(x = N, y = Fast, color= "Fast")) +
        geom_point(aes(x = N, y = Slow, color= "Slow")) +
        geom_vline(xintercept =   eff.maxN.Slow[[1,1]], color = "blue") +
        geom_vline(xintercept =   eff.maxN.Fast[[1,1]], color = "red") +
        labs(y = "Efficiency", color = NULL)+
        scale_color_manual(values = c("red", "blue")) +
        xlab("Flower Visits (N)") +
        theme_bw() +
        theme(legend.key = element_rect(colour = NA, fill = "transparent"),
              legend.justification = c(1,0), legend.position = c(1,0),
              legend.title = NULL,
              legend.text = element_text( size = 10), 
              legend.background = element_rect(fill = "transparent"),
              plot.title = element_text(face = "bold", size = 12, hjust = .5),   
              axis.title.x=element_text(size = 20),
              axis.title.y=element_text(size = 20),
              axis.text.x = element_text( size=16),
              axis.text.y = element_text( size=16))
      p2
      
    })
    
################################################################################################
#Rate Table  
    output$table.R <- renderTable({  
    
      #Model Code  
      #PARAMETERS
      N <- 1:60     # number flowers
      c <- 16.7         # (J/mg) weight-specific energetic sucrose value (given)
      aperJ <- 0.00005  # (W/J) Linear increment of MR; for every 1 unit increase in Joules, MR increases 
      a <- aperJ*c      # (W) linear increment of MR as a function of load weight
      a0 <- 0.0334      # (W) FMR; Metabolic rate during flight (unloaded) 
      at <- 0.0042      # (W) MR in hive; Metabolic rate of active moving bee not in flight
      ah <- at          # (W) MR during handling; assumed to be the same as at
      w <- 3      # (mg) nectar load per flower
      ww <- 3     #(mg) Water Weight per flower
      wj <- c*w     # (joules) nectar load per flower
      W <- N*w      # Total weight gained by number of flowers 
      h <- 105.6861     # (seconds) time at each flower (handling)
      T0 <- 235         # (seconds) time spent in hive
      tau <- 74.50042   # (seconds) time spent flying between flowers
      tau0 <- 16.56     # (seconds) one-way travel time
      
      # Set slider inputs   
      w <- input$w
      ww <- input$ww
      tau0 <- input$tau0
      tau <- input$tau
      aperj <- input$aperj
      at <- input$at
      h <- input$h
      
      fmr <- c(0.02018139, 0.0618627) #Imput STephen's FMR values in Watts
      
      long.tib <-  matrix(nrow = vec_size(fmr) * length(N),  # dataframe row length based on fmr and N
                          ncol = 4) %>%
        as.data.frame() %>%
        dplyr::rename(N=V1, Efficiency=V2, Rate=V3, FMR=V4 )
      
      for(i in 1:length(fmr)){
        a0 <- fmr[i]
        
        for(j in N){
          # Total energy expenditure in patch (arena)
          # Cp = a0*(N - 1)* tau + a*(N*(N-1)/2)*w*tau + ah*N*h
          Cp <- round(a0*(N[j]-1)*tau + (a*((N[j]*(N[j]-1))/2)*(w+w)*tau) + ah*N[j]*h, 3) 
          
          # Total expenditure during travel to and from
          # Ct = a0*tau0 + (a0 + a*W)*tau0
          Ct <- round(a0*tau0 + (a0 + a*N[j]*(w+w))*tau0, 3) 
          
          # Total energy expenditure per foraging cycle
          # C = cp + ct + t0*at
          C <- round(Cp + Ct + T0*at, 3)  
          
          # Gross energy load for N flowers visited
          # G = N*c*w
          G <- N[j]*wj 
          
          # round-trip time for an entire foraging trip
          # T = 2*t0 + (N- 1)*t + N*h + t0
          T <- 2*tau0 + (N[j]-1)*tau + N[j]*h + T0
          
          # (G-C)/C
          Efficiency <- round((G - C)/C, 4)
          # (G-C)/T
          Rate <- round((G - C)/T, 4)
          
          row.number <- (i-1)*60 + j
          my.row <- c(N[j], Efficiency, Rate, a0)   #Somethere here is the PROBLEM
          long.tib[row.number,] <- my.row
        }
      }
      
      long.tib.R <- select(long.tib, -c(Efficiency))
      wide.tib.R <-  pivot_wider(long.tib.R, names_from = FMR, values_from = Rate)
      
      wide.tib.R <-rename(wide.tib.R, Slow = `0.02018139`)
      wide.tib.R <-rename(wide.tib.R, Fast = `0.0618627`)
  
      wide.tib.R             
      
    })  
    
      output$Plot.R <- renderPlot({
        
        #Model Code  
        #PARAMETERS
        N <- 1:60     # number flowers
        c <- 16.7         # (J/mg) weight-specific energetic sucrose value (given)
        aperJ <- 0.00005  # (W/J) Linear increment of MR; for every 1 unit increase in Joules, MR increases 
        a <- aperJ*c      # (W) linear increment of MR as a function of load weight
        a0 <- 0.0334      # (W) FMR; Metabolic rate during flight (unloaded) 
        at <- 0.0042      # (W) MR in hive; Metabolic rate of active moving bee not in flight
        ah <- at          # (W) MR during handling; assumed to be the same as at
        w <- 3      # (mg) nectar load per flower
        ww <- 3     #(mg) Water Weight per flower
        wj <- c*w     # (joules) nectar load per flower
        W <- N*w      # Total weight gained by number of flowers 
        h <- 105.6861     # (seconds) time at each flower (handling)
        T0 <- 235         # (seconds) time spent in hive
        tau <- 74.50042   # (seconds) time spent flying between flowers
        tau0 <- 16.56     # (seconds) one-way travel time
        
        # Set slider inputs   
        w <- input$w
        ww <- input$ww
        tau0 <- input$tau0
        tau <- input$tau
        aperj <- input$aperj
        at <- input$at
        h <- input$h
        
        fmr <- c(0.02018139, 0.0618627) #Imput STephen's FMR values in Watts
        
        long.tib <-  matrix(nrow = vec_size(fmr) * length(N),  # dataframe row length based on fmr and N
                            ncol = 4) %>%
          as.data.frame() %>%
          dplyr::rename(N=V1, Efficiency=V2, Rate=V3, FMR=V4 )
        
        for(i in 1:length(fmr)){
          a0 <- fmr[i]
          
          for(j in N){
            # Total energy expenditure in patch (arena)
            # Cp = a0*(N - 1)* tau + a*(N*(N-1)/2)*w*tau + ah*N*h
            Cp <- round(a0*(N[j]-1)*tau + (a*((N[j]*(N[j]-1))/2)*(w+w)*tau) + ah*N[j]*h, 3) 
            
            # Total expenditure during travel to and from
            # Ct = a0*tau0 + (a0 + a*W)*tau0
            Ct <- round(a0*tau0 + (a0 + a*N[j]*(w+w))*tau0, 3) 
            
            # Total energy expenditure per foraging cycle
            # C = cp + ct + t0*at
            C <- round(Cp + Ct + T0*at, 3)  
            
            # Gross energy load for N flowers visited
            # G = N*c*w
            G <- N[j]*wj 
            
            # round-trip time for an entire foraging trip
            # T = 2*t0 + (N- 1)*t + N*h + t0
            T <- 2*tau0 + (N[j]-1)*tau + N[j]*h + T0
            
            # (G-C)/C
            Efficiency <- round((G - C)/C, 4)
            # (G-C)/T
            Rate <- round((G - C)/T, 4)
            
            row.number <- (i-1)*60 + j
            my.row <- c(N[j], Efficiency, Rate, a0)   #Somethere here is the PROBLEM
            long.tib[row.number,] <- my.row
          }
        }
        
        #create wide form tibble for Efficiency data only
        long.tib.R <- select(long.tib, -c(Efficiency))
        wide.tib.R <-  pivot_wider(long.tib.R, names_from = FMR, values_from = Rate)
        
        wide.tib.R <-rename(wide.tib.R, Slow = `0.02018139`)
        wide.tib.R <-rename(wide.tib.R, Fast = `0.0618627`)
  
        wide.tib.R 
        
        #GGPlot      
        #Vert Lines
        rate.maxN.Slow <- wide.tib.R %>% 
          filter(wide.tib.R$Slow == max(wide.tib.R$Slow)) %>%
          select(N)
        
        rate.maxN.Fast <- wide.tib.R %>% 
          filter(wide.tib.R$Fast == max(wide.tib.R$Fast)) %>%
          select(N)

        p2 <- ggplot(data = wide.tib.R) +
          geom_point(aes(x = N, y = Fast, color= "Fast")) +
          geom_point(aes(x = N, y = Slow, color= "Slow")) +
          geom_vline(xintercept = rate.maxN.Slow[[1,1]], color = "blue") +
          geom_vline(xintercept = rate.maxN.Fast[[1,1]], color = "red") +
          labs(y = "Rate", color = NULL) +
          scale_color_manual(values = c("red", "blue")) +
          xlab("Flower Visits (N)") +
          theme_bw() +
          theme(legend.key = element_rect(colour = NA, fill = "transparent"),
                legend.justification = c(1,0), legend.position = c(1,0),
                legend.title = NULL,
                legend.text = element_text( size = 10), 
                legend.background = element_rect(fill = "transparent"),
                plot.title = element_text(face = "bold", size = 12, hjust = .5))
        p2
        
      })   
}
shinyApp(ui = ui, server = server)           
