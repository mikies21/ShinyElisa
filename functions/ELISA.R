ElisaScurve <- function(Conc_DF) {
  CurveFit <- drc::drm(value ~ concentration,
                          data = Conc_DF,
                          fct = drc::LL.4())
  
  
  demo.fits <- expand.grid(concentration=exp(seq(log(0.001),
                                                 log(10),
                                                 length=100)))
  
  pm <-  predict(CurveFit, 
                 demo.fits,
                 interval = "confidence")
  
  demo.fits$p <- pm[,1]
  demo.fits$pmin <- pm[,2]
  demo.fits$pmax <- pm[,3]
  
  plot <- ggplot2::ggplot()+
    ggplot2::geom_line(data = demo.fits, ggplot2::aes(x=concentration, y=p), linetype="dotted", colour="black")+
    ggplot2::geom_ribbon(data=demo.fits, ggplot2::aes(x=concentration, y=p, ymin=pmin, ymax=pmax), alpha=0.2)+
    ggplot2::geom_point(data= Conc_DF, ggplot2::aes(x=concentration, y=value))+
    ggplot2::scale_x_log10(
      limits = c(0.001, 10), 
      breaks = c(0.001, 0.01, 0.1, 1, 10),
      labels = c("0.001", "0.01", "0.1", "1", "10"))+
    ggplot2::labs(x = "concentration (ug/mL)", y = "OD")+
    ggprism::theme_prism()
  
  list("model" = CurveFit,"curve" = demo.fits, "plotCurve" = plot,
       "min" = min(Conc_DF$value), "max" = max(Conc_DF$value))
}

ElisaConcPrediction <- function(curveModel, newData, ODcolumn = "OD"){
  sample_concPred <- drc::ED(object = curveModel$model,
                             respLev = newData |> dplyr::pull(ODcolumn),
                             type = c("absolute"),
                             reference = "control",
                             display = T,
                             interval = "none")
  
  sample_concPreda <- sample_concPred |> 
    as.data.frame()
  
  sample_concPreda <- newData |> 
    dplyr::bind_cols(sample_concPreda) |> 
    dplyr::mutate(Estimate_Conc = Estimate/dilution)
  
  plot <- ggplot2::ggplot(sample_concPreda)+
    ggplot2::geom_line(data=curveModel$curve, 
                       ggplot2::aes(x = concentration,
                  y = p), 
              linetype = "dotted",
              colour = "black")+
    ggplot2::geom_ribbon(data=curveModel$curve,
                         ggplot2::aes(x=concentration,
                    y=p, 
                    ymin=pmin,
                    ymax=pmax),
                alpha=0.2)+
    ggplot2::geom_point(ggplot2::aes(x = Estimate,
                   y = value))+
    ggplot2::geom_hline(yintercept = c(curveModel$min, curveModel$max),
               linetype = "dotted",
               colour = "grey")+
    ggplot2::annotate(geom = "text", x = 0.01, y = curveModel$max, label = paste0(curveModel$max))+
    ggplot2::annotate(geom = "text", x = 1, y = curveModel$min, label = paste0(curveModel$min))+
    ggplot2::scale_x_log10(limits = c(0.001, 10), 
                  breaks = c(0.001, 0.01, 0.1, 1, 10),
                  labels = c("0.001", "0.01", "0.1", "1", "10"))+
    ggplot2::labs(x = "concentration (ug/mL)",
         y = "OD")+
    ggprism::theme_prism()
  
  list("predictions" = sample_concPreda, "plot" = plot)
  
}

