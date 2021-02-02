#' Visualize behavior in a two-choice decision task
#'
#' This function plots the simulated two-choice task data.
#' @param data simulated task data
#' @param subj subject data to plot. Defaults to subject 100.
#' @param colors color to represent the two choices - choice A or choice B. Defaults to orange for choice A and purple for choice B.
#' @param plot_type produce either static or animated version of the plot
#'
#'
#' @return A plot of individual \code{subj} behavior data.
#'
#' @export
#'
#' @import ggplot2 ggpubr gganimate
#' @examples
#'
#' # Save simulated task data to a variable, say, data
#' data <- simulate_twochoiceRL(trials_unique = FALSE)
#'
#' # Plot the behavior of individual 34
#' plot_twochoiceRL(data = data, subj = 34, colors = c("#749dae","#5c1a33"))

plot_twochoiceRL <- function(data = NULL,
                             subj = 100,
                             colors = c("orange", "purple"),
                             plot_type = "static") {

 if (plot_type == "static"){
  # Plot expected value across trials
  g1 <- ggplot(data$twochoiceRL[[subj]], aes(x=Trial, y=Value, fill=Option)) +
    geom_point() + geom_smooth() +
    xlab("Trial number") + ylab("Expected value") +
    scale_fill_manual(name = "Machine",
                      labels = c("A","B"),
                      values = c(colors[1],colors[2]))

  # Plot choice probability across trials
  g2 <- ggplot(data$twochoiceRL[[subj]], aes(x=Trial, y=Pr, fill=Option)) +
     geom_point() + geom_smooth() +
     xlab("Trial number") + ylab("Choice probability") +
     scale_fill_manual(name = "Machine",
                       labels = c("A","B"),
                       values = c(colors[1],colors[2]))

  # arrange plots
  plot <- ggarrange(g1,g2,ncol=2,nrow=1,common.legend = TRUE,legend = 'bottom')

  return(plot)
 }

 if (plot_type == "animate"){

   # Plot choice probability across trials
   plot <- ggplot(data = data$twochoiceRL[[subj]],
                   aes(x = Trial,
                       y = Pr,
                       color = Option)) +
     geom_point(aes(group = seq_along(Trial)),
                size = 4,
                alpha = 0.7) +
     geom_line(aes(lty = Option),
               alpha = 0.6
     ) +
     labs(y = "Choice probability",
          x = "Trial",
          title = "") +
     scale_linetype_manual(name = "",
                           labels = c("A","B"),
                           values = c("solid","solid"),
                           guide = FALSE) +
     scale_color_manual(name = "Machine",
                        labels = c("A","B"),
                        values = c(colors[1],colors[2]))



   ## Animated Plot
   animate.plot <- plot + transition_reveal(along = Trial)

   return(animate.plot)

 }



}
