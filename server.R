function(input, output, session) {
  
  model_r <- reactive({return(c(input$ar_d |> as.numeric(), input$i_d |> as.numeric(), input$ma_d |> as.numeric()))})
  n_r <- reactive({return(input$n_d |> as.numeric())})
  intr_r <- reactive({return(if_else(str_detect(input$intr_v, "\\D"), 0, input$intr_v |> as.numeric()))})
  sd_r <- reactive({return(if_else(str_detect(input$sd_v, "\\D"), 1, input$sd_v |> as.numeric()))})
  
  observeEvent(input$runculc, {
    
    if(model_r()[1] > 0){
      ar_value <- runif(model_r()[1], -1, 1)    
      while(min(Mod(polyroot(c(1, - ar_value)))) <= 1){
        ar_value <- runif(model_r()[1], -1, 1)
      }
    }

    if(model_r()[3] > 0){
      ma_value <- sum_ma(model_r()[3])
    }
    
    if(model_r()[1] == 0 & model_r()[3] == 0){
      model_ <- list(order=model_r())
    } else if(model_r()[1] == 0){
      model_ <- list(order=model_r(), ma = ma_value)
      
    } else if(model_r()[3] == 0){
      model_ <- list(order=model_r(), ar = ar_value)
    } else{
      model_ <- list(order=model_r(), ar = ar_value, ma = ma_value)
    }
    
    arima_ts <- intr_r() + arima.sim(n=n_r() - 1, model_, sd = sd_r())
    
    d_ts <- 
      arima_ts |> 
      as.data.frame() |> 
      mutate(x = as.numeric(x), time = 1:(n_r()-1 + model_r()[2]))
    
    d_acf <- data.frame(acf = acf(arima_ts, plot = F) |> _$acf) |> mutate(lag = 1:length(acf))
    d_pacf <- data.frame(pacf = pacf(arima_ts, plot = F) |> _$acf) |> mutate(lag = 1:length(pacf))
    
    
    p_ts <- 
      d_ts |> 
      ggplot(aes(x=time, y=x, alpha=0.5))+
      geom_point(size=2)+
      geom_line(linewidth=1)+
      theme(legend.position="none")+
      labs(title="時系列")
    
    p_acf <- 
      d_acf |> 
      ggplot(aes(x=lag, y=acf, ymax=acf, ymin=0, alpha=0.5))+
      geom_point(size=2)+
      geom_linerange(linewidth=1)+
      theme(legend.position="none")+
      labs(title="自己相関")
    
    
    p_pacf <- 
      d_pacf |> 
      ggplot(aes(x=lag, y=pacf, ymax=pacf, ymin=0, alpha=0.5))+
      geom_point(size=2)+
      geom_linerange(linewidth=1)+
      theme(legend.position="none")+
      labs(title="偏自己相関")
    
    output$tsplot <- renderPlot(
      {p_ts / (p_acf + p_pacf)}, height = 800, width = 1200
    )
    
    output$autoarima <- renderPrint({
      auto.arima(arima_ts)
    })
    
    output$ar <- renderPrint({
      ar(arima_ts)
    })
    
    output$adf <- renderPrint({
      adf.test(arima_ts)
    })
    
    output$kpss <- renderPrint({
      ur.kpss(arima_ts) |> summary()
    })
    
    output$model <- renderPrint({
      model_r()
    })
    
    out_ar <- ifelse(
      model_r()[1] == 0, 
      0, 
      paste(ar_value |> round(digits=3) |> as.character(), collapse = ", ")
    )
    
    output$ar_p <- renderText(
      out_ar
    )
    
    out_ma <- ifelse(
      model_r()[3] == 0, 
      0, 
      paste(ma_value |> round(digits=3) |> as.character(), collapse = ", ")
    )
    
    output$ma_p <- renderText(
      out_ma
    )
  })

}
