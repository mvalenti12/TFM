source("scripts/libraries_functions.R")
load("/Users/marcvalenti/TFM_data/data_needed_full.RData")
#load("/Users/marcvalenti/TFM_data/princomp_res.RData")

dt <- dtmr_partner_id %>%
  arrange(year,month) 

month_year_aux <- expand.grid(unique(dt$year),
                              unique(dt$month)) %>%
  set_colnames(c("year","month")) %>%
  arrange(year,month) %>%
  filter(month>=11|year>2008) %>%
  filter(month<=1|year<2018) %>%
  mutate(date = seq.Date(from = as.Date("2008-11-01"), to = as.Date("2018-01-01"), by = "month"),
         SSW = NA,
         SSB = NA) 

# fviz_eig(p_comp) %>%
#   ggsave(file = "/Users/marcvalenti/TFM/Figures/pca_dtmr_eig.png")

rename <- function(x,y){
  if (x < 10) {
    return(name <- paste(getwd(),"_data/img_gif/",y,"-",'0',x,'-plot.png',sep=''))
  }
  return(name <- paste(getwd(),"_data/img_gif/",y,"-",x,'-plot.png', sep=''))
  
}

extract_sample <- function(x){
  if(sum(x)<1000){
    return(x)
  } else {
    return(sample(which(x),1000,replace = FALSE))
  }
}

ptners_high <- names(sort(table(dt$partner_id),decreasing = TRUE)[1:7])

dt <- dt %>%
  filter(partner_id %in% ptners_high) 
dt$partner_id <- factor(dt$partner_id, levels = sort(ptners_high))

# p_comp <- dt %>%
#   dplyr::select(-partner_id,-year,-month) %>%
#   prcomp()
# 
# idx_partner_id_june_2016_numeric <- which(dt$year==2016&dt$month==6,arr.ind = TRUE)
# save(p_comp, file = "/Users/marcvalenti/TFM_data/princomp_res.RData")
# idx_partner_id_june_2016_partner <- dt[idx_partner_id_june_2016_numeric,'partner_id']
# save(idx_partner_id_june_2016_numeric,idx_partner_id_june_2016_partner, file = "/Users/marcvalenti/TFM_data/idx_partners_phil_june_2016.RData")

cols<-brewer.pal(n=7,name="Set2")
aux_matrix <- cbind(levels(dt$partner_id),cols,10:16) %>%
  as.data.frame() %>%
  set_colnames(c("partner_id","color_partner","pch_partner")) %>%
  mutate(color_partner = as.character(color_partner),
         pch_partner = as.numeric(as.character(pch_partner)))

dt_dist_groups <- matrix(rep(NA,49), 
                         nrow = length(ptners_high),
                         byrow = TRUE)
colnames(dt_dist_groups) <- rownames(dt_dist_groups) <- sort(ptners_high)

for (i in 1:nrow(month_year_aux)){
  
  idx <- dt$month==month_year_aux[i,'month']&dt$year==month_year_aux[i,'year']
  
  #idx <- extract_sample(idx)
  
  which_partner <- dt$partner_id[idx]
  dt_plot <- p_comp$x[idx,1:4] %>%
    as.data.frame() %>%
    set_colnames(c("D1","D2","D3","D4")) %>%
    cbind(which_partner) %>%
    left_join(aux_matrix, by = c("which_partner" = "partner_id"))
  
  
  #creates the distance plot###
  dt_dist <- p_comp$x[idx,] %>%
    dist(method = "euclidean", diag = FALSE, upper = FALSE) %>%
    as.matrix() %>%
    as.data.frame()
  SSW <- 0 #sum of squares within
  SSW_n <- 0
  SSB <- 0 #sum of squares between
  SSB_n <- 0
  for (k in 1:nrow(dt_dist_groups)){
    for (j in 1:ncol(dt_dist_groups)){
        dist_partner_i_j <- dt_dist[which_partner==colnames(dt_dist_groups)[k],
                                    which_partner==colnames(dt_dist_groups)[j]]
        dt_dist_groups[k,j] <- sum(dist_partner_i_j[lower.tri(dist_partner_i_j,diag = FALSE)])/sum(lower.tri(dist_partner_i_j,diag = FALSE))    
        if(k==j){
          SSW <- SSW + sum(dist_partner_i_j[lower.tri(dist_partner_i_j,diag = FALSE)])
          SSW_n <- SSW_n + sum(lower.tri(dist_partner_i_j,diag = FALSE))    
        } else {
          SSB <- SSB + sum(dist_partner_i_j[lower.tri(dist_partner_i_j,diag = FALSE)])
          SSB_n <- SSB_n + sum(lower.tri(dist_partner_i_j,diag = FALSE))    
        }
    }
  }
  month_year_aux$SSW[i] <- SSW/SSW_n
  month_year_aux$SSB[i] <- SSB/SSB_n
  
  dt_dist_groups_melted <- melt(dt_dist_groups)
  ###
  
  
  
  # scatterplot3d::scatterplot3d(dt_plot[,1:3],
  #                              color = dt_plot$color_partner,
  #                              pch = dt_plot$pch_partner,
  #                              xlim=c(min(p_comp$x[,1]),max(p_comp$x[,1])), 
  #                              ylim=c(min(p_comp$x[,2]),max(p_comp$x[,2])), 
  #                              zlim=c(min(p_comp$x[,3]),max(p_comp$x[,3])),
  #                              main = paste0( "Month ", month_year_aux[i,'month'],"; ", "Year ", month_year_aux[i,'year']," (N = ",nrow(dt_plot),")"),
  #                              box = FALSE)
  # legend("topleft", 
  #        legend = levels(aux_matrix$partner_id),
  #        col = aux_matrix$color_partner, 
  #        pch = aux_matrix$pch_partner,
  #        ncol=2,cex=0.7)
  
  # p1 <- plot(dt_plot[,1:2], 
  #      col = dt_plot$color_partner,
  #      pch = dt_plot$pch_partner,
  #      xlim=c(min(p_comp$x[,1]),max(p_comp$x[,1])), 
  #      ylim=c(min(p_comp$x[,2]),max(p_comp$x[,2])), 
  #      main = paste0( "Month ", month_year_aux[i,'month'],"; ", "Year ", month_year_aux[i,'year']," (N = ",nrow(dt_plot),")"))
  
  p1 <- ggplot(dt_plot,
         aes(x = D1,
             y = D2,
             color = dt_plot$which_partner)) + 
    scale_x_continuous(limits = c(min(p_comp$x[,1]),max(p_comp$x[,1]))) + 
    scale_y_continuous(limits = c(min(p_comp$x[,2]),max(p_comp$x[,2]))) + 
    scale_color_manual(values = cols) + 
    geom_point() + 
    labs(color = "Partner") + 
    theme_economist_white()
  
  p2 <- ggplot(dt_plot,
               aes(x = D3,
                   y = D4,
                   color = dt_plot$which_partner)) + 
    scale_x_continuous(limits = c(min(p_comp$x[,3]),max(p_comp$x[,3]))) + 
    scale_y_continuous(limits = c(min(p_comp$x[,4]),max(p_comp$x[,4]))) + 
    geom_point() + 
    scale_color_manual(values = cols) + 
    labs(color = "Partner") + 
    theme_economist_white()
  
  p3 <- ggplot(data = dt_dist_groups_melted,
         aes(x=as.factor(X1), y=as.factor(X2), fill=value, label = round(value,1))) + 
    geom_tile(show.legend = FALSE) + 
    geom_text(col="white") + 
    labs(title = "",
         x = "",
         y = "") + 
    theme_economist_white() + 
    theme(axis.text.x = element_text(colour = aux_matrix$color_partner),
          axis.text.y = element_text(colour = aux_matrix$color_partner))
  
  p4 <- melt(month_year_aux[1:i,],id.var = "date") %>%
    filter(variable %in% c("SSW","SSB"),
           value!="Inf",
           value!="NaN",
           value!=0) %>%
    mutate(variable = ifelse(variable=="SSW","Within Groups",
                             "Between Groups")) %>%
    ggplot() +
    aes(x = date,
        y = value,
        col = variable) +
    labs(x = "",
         y= "") +
    geom_line() + 
    scale_x_date(limits = c(as.Date("2009-07-01"),as.Date("2018-01-01"))) + 
    theme_economist_white() + 
    labs(col = "") + 
    theme(legend.position = "bottom",
          legend.text = element_text(size=10))
  
  p1p2 <- grid.arrange(ggarrange(p1,p2,common.legend = TRUE, legend = "bottom"),
                       top = "Plot of Individuals in the first 3 Dimensions using PCA")
  p3 <- grid.arrange(ggarrange(p3),
                     top = "Average Distance Within/Between Groups")
  p4 <- grid.arrange(ggarrange(p4),
                     top = "Average Distance Within/Between Groups Over Time")
  png(filename=rename(month_year_aux[i,'month'],month_year_aux[i,'year']),
      width = 750,
      heigh = 750,
      bg = "#ebebeb")
  grid.arrange(
    p1p2,
    p3,
    p4,
    layout_matrix = rbind(c(1,2),
                          c(1,3)),
    top = text_grob(paste0("Country: Philippines - ", "Month ", month_year_aux[i,'month'],"; ", "Year ", month_year_aux[i,'year']," (N = ",nrow(dt_plot),")"),
                    face = "bold",size = 16)
    # bottom = textGrob(
    #   "this footnote is right-justified",
    #   gp = gpar(fontface = 3, fontsize = 9),
    #   hjust = 1,
    #   x = 1
    # )
  )
  dev.off()
  print(i)
}

my_command <- 'convert -delay 4 -loop 0 *.png animation.gif'
system(my_command)
