#' Visualize behavior in a two-choice decision task
#'
#' This function plots the simulated two-choice task data.
#' @param data simulated task data
#' @param subj subject data to plot. Defaults to subject 100.
#' @param colors color to represent the two choices - choice A or choice B.
#'   Defaults to orange for choice A and purple for choice B.
#' @param plot_type produce either static or animated version of the plot. Defaults to static.
#'
#'
#' @return A plot of individual \code{subj}'s expected value across trials, a plot of individual \code{subj}'s
#'   probability across trials, and a side-by-side plot of both of the previous plots.
#'
#' @export
#'
#' @import ggplot2 ggpubr gganimate
#' @examples
#'
#' # Save simulated task data to a variable, say, data_sim
#' data_sim <- simulate_twochoiceRL(trials_unique = FALSE)
#'
#' # Plot the behavior of individual 100
#' plot_twochoiceRL(data = data_sim, subj = 100, colors = c("#009999","#0000FF"))

plot_twochoiceRL <- function(data = NULL,
                             subj = 100,
                             colors = c("#009999","#0000FF"),
                             plot_type = "static") { # start plotting

 if (plot_type == "static"){
  # Plot expected value across trials
  g1 <- ggplot(data$twochoiceRL[[subj]], aes(x=data$twochoiceRL[[subj]]$Trial,
                                             y=data$twochoiceRL[[subj]]$Value,
                                             fill=data$twochoiceRL[[subj]]$Option)) +
    geom_point() + geom_smooth(span=0.1) +
    xlab("Trial number") + ylab("Expected value") +
    scale_fill_manual(name = "Machine",
                      labels = c("A","B"),
                      values = c(colors[1],colors[2])) +
    theme(#axis.title.y = element_blank(),
          #axis.title.x = element_blank(),
          #axis.text = element_blank(),
          panel.background = element_rect(),
          panel.grid.major = element_line(size=1),
          panel.grid.minor = element_line(size=1),
          axis.ticks = element_line(colour="black", size = 1.5),
          panel.border = element_rect(colour = "black", fill = NA, size = 2.5),
          #legend.text = element_blank(),
          legend.position = "none",
          aspect.ratio = 1)

  # Plot choice probability across trials
  g2 <- ggplot(data$twochoiceRL[[subj]], aes(x=data$twochoiceRL[[subj]]$Trial,
                                             y=data$twochoiceRL[[subj]]$Pr,
                                             fill=data$twochoiceRL[[subj]]$Option)) +
     geom_point() + geom_smooth(span=0.1) +
     xlab("Trial number") + ylab("Choice probability") +
     scale_fill_manual(name = "Machine",
                       labels = c("A","B"),
                       values = c(colors[1],colors[2])) +
    theme(#axis.title.y = element_blank(),
          #axis.title.x = element_blank(),
          #axis.text = element_blank(),
          panel.background = element_rect(),
          panel.grid.major = element_line(size=1),
          panel.grid.minor = element_line(size=1),
          axis.ticks = element_line(colour="black", size = 1.5),
          panel.border = element_rect(colour = "black", fill = NA, size = 2.5),
          #legend.text = element_blank(),
          legend.position = "none",
          aspect.ratio = 1)

  # arrange plots
  plot <- ggarrange(g1,g2,ncol=2,nrow=1,common.legend = TRUE,legend = 'bottom')

  return(list(g1, g2, plot))
 }

 if (plot_type == "animate"){ # start animation plot

   # Plot choice probability across trials
   plot <- ggplot(data = data$twochoiceRL[[subj]],
                   aes(x = data$twochoiceRL[[subj]]$Trial,
                       y = data$twochoiceRL[[subj]]$Pr,
                       color = data$twochoiceRL[[subj]]$Option)) +
     geom_point(aes(group = seq_along(data$twochoiceRL[[subj]]$Trial)),
                size = 4,
                alpha = 0.7) +
     geom_line(aes(lty = data$twochoiceRL[[subj]]$Option),
               alpha = 0.6) +
     labs(y = "Choice probability",
          x = "Trial",
          title = "") +
     scale_linetype_manual(name = "",
                           labels = c("A","B"),
                           values = c("solid","solid"),
                           guide = FALSE) +
     scale_color_manual(#name = "Machine",
                        labels = c("A","B"),
                        values = c(colors[1],colors[2])) +
     theme(#axis.title.y = element_blank(),
           #axis.title.x = element_blank(),
           #axis.text = element_blank(),
           panel.background = element_rect(),
           panel.grid.major = element_line(size=1),
           panel.grid.minor = element_line(size=1),
           axis.ticks = element_line(colour="black", size = 1.5),
           panel.border = element_rect(colour = "black", fill = NA, size = 2.5),
           legend.text = element_blank(),
           legend.position = "none",
           aspect.ratio = 1)



   ## Animated Plot
   animate_plot <- plot + transition_reveal(along = data$twochoiceRL[[subj]]$Trial)

   return(animate_plot)

 } # end animation plot



} # end plotting
