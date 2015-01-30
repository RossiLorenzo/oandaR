oanda_candle = function(df){
  G = ggplot(df) + 
    geom_rect(aes(xmin = time, xmax = next_time, ymax = closeAsk, ymin = openAsk, fill = pos_neg)) +
    geom_errorbar(data = df, aes(x = middle_point, ymax = highAsk, ymin = lowAsk)) +
    ggtitle("CandleStick Ask") +
    scale_x_datetime("") +
    scale_y_continuous("") +
    scale_fill_manual(values = c(muted("red"), muted("green"))) +
    theme_bw(base_size = 21, base_family = "serif") + 
    theme(legend.position = "none", panel.border = element_blank(), panel.grid = element_line(linetype = 2))
  return(G)
}