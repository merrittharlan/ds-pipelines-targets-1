library(dplyr)

source("1_fetch/src/1_fetch.R")
data <- fetch_data()

source("2_process/src/2_process.R")
eval_data <- process_data()

#' Plot the model data
#' 
#' @param modeldata data processed in 2_fetch
#' @param plotname file name and directory of output plot
#' @param modelfile file name and directory of model diagnostics
#' @param pbColor color of Process-Based model
#' @param dlColor color of Deep Learning model
#' @param pgdlColor color of Process-Guided Deep Learning model
#' @param pbPch plot character of Process-Based model
#' @param dlPch plot character of Deep Learning model
#' @param pgdlPch plot character of Process-Guided Deep Learning model
#' @param plotwidth plot width
#' @param plotheight plot height
#' @param plotres plot resolution in dots per inch
#' @param plotunits width and height units (default inches)
#' @return Figure 1 plot of model comparison, looking at RMSE as a function of the # of training temperature profiles

plot_results <- function(modeldata = eval_data, 
                         plotname = "3_visualize/out/figure_1.png",
                         pbColor = '#1b9e77',
                         dlColor = '#d95f02',
                         pgdlColor = '#7570b3',
                         pbPch = 21,
                         dlPch = 22,
                         pgdlPch = 23,
                         plotwidth = 8,
                         plotheight = 10,
                         plotres = 200,
                         plotunits = 'in',
                         ){
  project_output_dir <- '3_visualize/out'
  if (!dir.exists(project_output_dir)){
    dir.create(project_output_dir)
  }
  png(file = plotname, width = plotwidth, height = plotheight, res = plotres, units = plotunits)
  par(omi = c(0,0,0.05,0.05), mai = c(1,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)
  
  plot(NA, NA, xlim = c(2, 1000), ylim = c(4.7, 0.75),
       ylab = "Test RMSE (°C)", xlab = "Training temperature profiles (#)", log = 'x', axes = FALSE)
  
  n_profs <- c(2, 10, 50, 100, 500, 980)
  
  axis(1, at = c(-100, n_profs, 1e10), labels = c("", n_profs, ""), tck = -0.01)
  axis(2, at = seq(0,10), las = 1, tck = -0.01)
  
  # slight horizontal offsets so the markers don't overlap:
  offsets <- data.frame(pgdl = c(0.15, 0.5, 3, 7, 20, 30)) %>%
    mutate(dl = -pgdl, pb = 0, n_prof = n_profs)
  
  for (mod in c('pb','dl','pgdl')){
    mod_data <- filter(eval_data, model_type == mod)
    mod_profiles <- unique(mod_data$n_prof)
    for (mod_profile in mod_profiles){
      d <- filter(mod_data, n_prof == mod_profile) %>% summarize(y0 = min(rmse), y1 = max(rmse), col = unique(col))
      x_pos <- offsets %>% filter(n_prof == mod_profile) %>% pull(!!mod) + mod_profile
      lines(c(x_pos, x_pos), c(d$y0, d$y1), col = d$col, lwd = 2.5)
    }
    d <- group_by(mod_data, n_prof) %>% summarize(y = mean(rmse), col = unique(col), pch = unique(pch)) %>%
      rename(x = n_prof) %>% arrange(x)
    
    lines(d$x + tail(offsets[[mod]], nrow(d)), d$y, col = d$col[1], lty = 'dashed')
    points(d$x + tail(offsets[[mod]], nrow(d)), d$y, pch = d$pch[1], col = d$col[1], bg = 'white', lwd = 2.5, cex = 1.5)
    
  }
  
  points(2.2, 0.79, col = pgdlColor, pch = pgdlPch, bg = 'white', lwd = 2.5, cex = 1.5)
  text(2.3, 0.80, 'Process-Guided Deep Learning', pos = 4, cex = 1.1)
  
  points(2.2, 0.94, col = dlColor, pch = dlPch, bg = 'white', lwd = 2.5, cex = 1.5)
  text(2.3, 0.95, 'Deep Learning', pos = 4, cex = 1.1)
  
  points(2.2, 1.09, col = pbColor, pch = pbPch, bg = 'white', lwd = 2.5, cex = 1.5)
  text(2.3, 1.1, 'Process-Based', pos = 4, cex = 1.1)
  
  dev.off()
  return(paste0("Figure 1 located here: ", plotname))
}
