library(shiny)
library(tidyverse)
library(ggvis)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  observeEvent(input$r_river,{
    updateSliderInput(session, "r_safe", max = 1 - input$r_river)
    disable("r_bay") # putting this here keeps the slider disabled all the time (but still shows updating)
  })
  
  observe({
    updateSliderInput(session, "r_bay", value = 1 - input$r_river - input$r_safe)
  })
  
  observe({
    toggleState("th", condition = input$r_safe > 0)
  })
  
  results <- reactive({
    smolt.total = 8e6
    wild.adults = input$wa
    
    r = c("River" = input$r_river,
          "SAFE" = input$r_safe,
          "Bay" = input$r_bay)
    
    smolt.prod = smolt.total * r
    
    sar = c("River" = 0.01,
            "SAFE" = 0.03,
            "Bay" = 0.03)
    
    oh = input$oh
    
    th = c("River" = 0,
           "SAFE" = input$th,
           "Bay" = 0)
    
    stray = c("River" = 0.05,
              "SAFE" = 0.5,
              "Bay" = 0.5)
    
    ir = input$ir
    
    ocean.harvest = (smolt.total * r * sar * oh)
    
    terminal.harvest = (smolt.total * r * sar * (1 - oh) * th)
    
    straying = smolt.total * r * sar * (1 - oh) * (1 - th) * stray
    
    river.harvest = smolt.total * r * sar* (1 - oh) * (1 - th) * ir
    
    hatchery.return = (smolt.total * r * sar * (1 - oh) * (1 - th) * (1 - stray) * (1 - ir))
    
    straying.river.spawn = smolt.total * r * sar* (1 - oh) * (1 - th) * stray * (1 - ir)
    
    srs.pretty = format(sum(straying.river.spawn), big.mark = ",", nsmall = 0)
    
    phos = round((sum(straying.river.spawn)/(wild.adults + sum(straying.river.spawn)))*100, 1)
    
    out.rs = tibble(ocean.harvest,
                    terminal.harvest,
                    river.harvest,
                    hatchery.return) %>% 
      mutate(release.strategy = c("River", "SAFE", "Bay"),
             release.strategy = factor(release.strategy, levels = c("River", "SAFE", "Bay")))
    return(list(out.rs, phos, srs.pretty))
  })
  
  results.rs <- reactive({
    results()[[1]]
  })
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    paste0(names(x), ": ", format(x), collapse = "<br />")
  }
  
  results.rs %>% 
    ggvis(x = ~release.strategy, y = ~ocean.harvest, fill = ~release.strategy) %>%
    layer_bars() %>%
    add_tooltip(html = function(data){format(data$stack_upr_, big.mark = ",", nsmall = 0)}, on = "hover") %>% 
    add_axis("x", title = "") %>% 
    add_axis("y", title = "") %>% 
    hide_legend("fill") %>% 
    set_options(width = "auto", height = "320px", resizable=FALSE) %>%
    bind_shiny("plot_oh") 
  
  results.rs %>% 
    ggvis(x = ~release.strategy, y = ~river.harvest, fill = ~release.strategy) %>%
    layer_bars() %>%
    add_tooltip(html = function(data){format(data$stack_upr_, big.mark = ",", nsmall = 0)}, on = "hover") %>% 
    add_axis("x", title = "") %>% 
    add_axis("y", title = "") %>% 
    hide_legend("fill") %>% 
    set_options(width = "auto", height = "320px", resizable=FALSE) %>%
    bind_shiny("plot_ir") 
  
  results.rs %>% 
    ggvis(x = ~release.strategy, y = ~terminal.harvest, fill = ~release.strategy) %>%
    layer_bars() %>%
    add_tooltip(html = function(data){format(data$stack_upr_, big.mark = ",", nsmall = 0)}, on = "hover") %>% 
    add_axis("x", title = "") %>% 
    add_axis("y", title = "") %>% 
    hide_legend("fill") %>% 
    set_options(width = "auto", height = "320px", resizable=FALSE) %>%
    bind_shiny("plot_th") 
  
  results.rs %>% 
    ggvis(x = ~release.strategy, y = ~hatchery.return, fill = ~release.strategy) %>%
    layer_bars() %>%
    add_tooltip(html = function(data){format(data$stack_upr_, big.mark = ",", nsmall = 0)}, on = "hover") %>% 
    add_axis("x", title = "") %>%
    add_axis("y", title = "") %>%
    hide_legend("fill") %>% 
    set_options(width = "auto", height = "320px", resizable=FALSE) %>%
    bind_shiny("plot_hr") 
  
  hrTotal <- reactive({
    sum(results.rs()$hatchery.return) * 1000
  })
  
  output$hr_warning <- renderText({
    validate(
      need(hrTotal() > 5000, "Warning: Total hatchery returns below 5,000 fish.")
    )
    " " # return empty text if no validation message
  })
  
  output$phos <- renderText({
    paste0("pHOS: ", results()[[2]], "%")
  })
  
  output$hs <- renderText({
    paste0("Hatchery spawners: ", results()[[3]])
  })
  
  
  observeEvent(input$rsp_info, {
    sendSweetAlert(
      session = session,
      title = "",             # longer paragraphs have long lines b/c returns are interpreted as line breaks (which is useful)
      text = "Total hatchery smolt production is split among 3 release strategies. [See 'About' for more information about the release strategies.]
      
      The release proportions are specified hierarchically to constrain values to sum to one. Changing the value of the 'River' slider changes the max value of the 'SAFE' slider. The 'Bay' slider is set automatically by subtracting the values of the 'River' and 'SAFE' sliders from one.",
      type = "info",
      btn_labels = "OK"
    )
  })
  
  observeEvent(input$phos_info, {
    sendSweetAlert(
      session = session,
      title = "",             # longer paragraphs have long lines b/c returns are interpreted as line breaks (which is useful)
      text = "The proportion of hatchery-origin spawners (pHOS) is calculated as the number of hatchery-origin spawners divided by the total number of spawners (wild + hatchery origin). 
      
      The number of hatchery-origin spawners is calculated based on fixed and user-specified parameter values (see 'About').
      
      pHOS is only calculated for the non-hatchery river. All non-straying hatchery fish that escape harvest are assumed to return to the hatchery.",
      type = "info",
      btn_labels = "OK"
    )
  })
  
}

