makelabelsEW <- function(x) {ifelse(x < 0, parse(text=paste0(x,"^o", "*W")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))}

makelabelsNS <- function(x) {ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))}

makebaseplot <- function(){
  wrld <- map_data("world")
  xbreaks <- seq(-180,180,60)
  xlabels <- makelabelsEW(xbreaks)
  ybreaks <- seq(-90,90,30)
  ylabels <- makelabelsNS(ybreaks)
  gg1 <- ggplot() + 
    geom_polygon(data = wrld, aes(x=long, y = lat, group = group), fill = "lightgray", color = "lightgray") + 
    coord_fixed(1.3) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.ticks = element_blank(), legend.position="bottom",legend.title=element_text(size=15), 
          legend.text=element_text(size=12), axis.text = element_text(size=12))+
    scale_x_continuous("", breaks = xbreaks, labels = xlabels) +
    scale_y_continuous("", breaks = ybreaks, labels = ylabels, limits=c(-60,90))
  return(gg1)
}

map_of_df <- function(DATA){
  gg1 <- makebaseplot()
  gg1 + geom_point(data=DATA, aes(x=Longitude, y=Latitude, color=Effect), pch=16, size=1) + scale_color_gradient2(midpoint=0, low=lowcolor, mid=midcolor, high=highcolor, limits=c(-100,140), breaks=c(-100,0,100)) + 
   labs(color="Yield Change (%)") +
    facet_wrap(~Crop)
}

hist_of_df <- function(DATA){
  ggplot(data=DATA, aes(x=Effect)) + 
    geom_histogram() + theme_classic() + scale_fill_continuous() + xlim(-100,140) +
    geom_vline(aes(xintercept = mean(Effect)),col='black', lty=2) + geom_vline(aes(xintercept = median(Effect)),col='gray', lty=3) + xlab("Yield Change (%)") +
    facet_wrap(~Crop)
}

obs_v_pred <- function(ObsPred){
  AllOP <- bind_rows(ObsPred, .id = "column_label")
  FormOP <- AllOP %>%
    tidyr::pivot_longer(cols=c(Predicted, Observed), names_to="Type") %>%
    group_by(Crop, Site, Type) %>%
    
    dplyr::summarise(mean = mean(value),
                     se = sd(value)/sqrt(n()),
                     n = n()) %>%
    tidyr::pivot_wider(names_from = Type, names_sep = ".", values_from = c(mean, se, n))
  
  #mean and se of Observed is redundant, since those don't change over iterations

  ggplot(FormOP, aes(x=mean.Observed, y=mean.Predicted)) +
    geom_point(size=0.5) +
    geom_errorbar(aes(ymin=mean.Predicted-se.Predicted, ymax=mean.Predicted+se.Predicted), width=.2,
                  position=position_dodge(0.05)) +
    ylab("Predicted Yield Change (%)") +
    xlab("Observed Yield Change (%)") +
    facet_wrap(~Crop) + theme_bw()
}



