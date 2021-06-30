df_for_plotting <- function(m, fcst) {
  # Make sure there is no y in fcst
  fcst$y <- NULL
  df <- m$history %>%
    dplyr::select(ds, y) %>%
    dplyr::full_join(fcst, by = "ds") %>%
    dplyr::arrange(ds)
  return(df)
}
dyplot.prophet <- function(x, fcst, uncertainty=TRUE,
                           ...)
{
  forecast.label='Predicted'
  actual.label='Actual'
  # create data.frame for plotting
  df <- df_for_plotting(x, fcst)
  
  # build variables to include, or not, the uncertainty data
  if(uncertainty && x$uncertainty.samples && exists("yhat_lower", where = df))
  {
    colsToKeep <- c('y', 'yhat', 'yhat_lower', 'yhat_upper')
    forecastCols <- c('yhat_lower', 'yhat', 'yhat_upper')
  } else
  {
    colsToKeep <- c('y', 'yhat')
    forecastCols <- c('yhat')
  }
  # convert to xts for easier date handling by dygraph
  dfTS <- xts::xts(df %>% dplyr::select_(.dots=colsToKeep), order.by = df$ds)
  
  # base plot
  dyBase <- dygraphs::dygraph(dfTS,...)
  
  presAnnotation <- function(dygraph, x, text) {
    dygraph %>%
      dygraphs::dyAnnotation(x, text, text, attachAtBottom = TRUE)
  }
  
  dyBase <- dyBase %>%
    # plot actual values
    dygraphs::dySeries(
      'y', label=actual.label, color='black', drawPoints=TRUE, strokeWidth=0
    ) %>%
    # plot forecast and ribbon
    dygraphs::dySeries(forecastCols, label=forecast.label, color='blue') %>%
    # allow zooming
    dygraphs::dyRangeSelector() %>%
    # make unzoom button
    dygraphs::dyUnzoom()
  if (!is.null(x$holidays)) {
    for (i in 1:nrow(x$holidays)) {
      # make a gray line
      dyBase <- dyBase %>% dygraphs::dyEvent(
        x$holidays$ds[i],color = "rgb(200,200,200)", strokePattern = "solid")
      dyBase <- dyBase %>% dygraphs::dyAnnotation(
        x$holidays$ds[i], x$holidays$holiday[i], x$holidays$holiday[i],
        attachAtBottom = TRUE)
    }
  }
  return(dyBase)
}