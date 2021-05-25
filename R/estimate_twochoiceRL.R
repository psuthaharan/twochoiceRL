#' Estimate behavior in a two-choice decision task
#'
#' This function performs parameter estimation to recover behavior in a
#' two-choice decision task
#' @param seed_value user-specified random value that ensures replication of results. Defaults to 528.
#' @param data simulated two-choice task data
#' @param method parameter estimation technique used; either maximum-likelihood
#'   estimation (mle), maximum a posteriori (map) or expectation-maximization
#'   with laplace approximation (eml). Defaults to mle.
#' @param nRes number of restarts. Defaults to 5.
#' @param tr_rad starting and maximum allowed trust region radius. Defaults to 1 and 5.
#' @param prior_mean initial prior means of parameters (x1,x2). Set value if method = "map" or "eml".
#' @param prior_sd initial prior standard deviations of parameters (x1,x2). Set value if method = "map" or "eml".
#' @param plot visualize estimation performance between true parameters vs
#'   estimated parameters. Defaults to FALSE.
#' @param progress_bar track completion time of estimation procedure. Defaults to TRUE.
#'
#'
#' @return A list containing a dataframe of the true and estimated parameter
#'   values, the measures of the bias, rmse and pearson correlation of parameters x1
#'   and x2, the posterior hyperparameters (if method = "eml") per iteration,
#'   plot (if plot = TRUE) of the relationship between the true and estimated
#'   parameter values, and a plot (if method = eml and plot = TRUE) of the posterior
#'   hyperparameters per iteration.
#'
#'
#' @export
#'
#' @import Metrics stats ggthemes grid tidyr ggridges ggjoy
#'
#' @examples
#'
#' # Save simulated task data to a variable, say, data_sim
#' data_sim <- simulate_twochoiceRL(trials_unique = TRUE)
#'
#' # Recover behavioral parameters using maximum-likelihood estimation (MLE)
#' est_mle <- estimate_twochoiceRL(data = data_sim, method = "mle", plot=FALSE)
#'
#' # View the true and MLE-estimated parameter values
#' View(est_mle[[1]]$value)
#'
#' # If plot=TRUE, view correlation plot between the true and MLE-estimated parameter for x1
#' View(est_mle[[2]])
#'
#'
#'

estimate_twochoiceRL <- function(seed_value = 528,
                                 data = NULL,
                                 method = "mle",
                                 nRes = 5,
                                 tr_rad = c(1,5),
                                 prior_mean = c(NULL,NULL),
                                 prior_sd = c(NULL,NULL),
                                 plot = FALSE,
                                 progress_bar = TRUE) { # start estimation

  # ensures replication of result - the rnorm() function
  # used in the next few lines will result in the same
  # sequence of random numbers for the specified seed
  set.seed(seed_value)

  # if specified estimation method is MLE, then run MLE
  if (method == "mle") {

    # randomly generate initial guesses for parameters
    init_x1 <- rnorm(data$subjects,0,5)
    init_x2 <- rnorm(data$subjects,0,5)


    # perform MLE
    mle_data <- mle_twochoiceRL(data = data,                      # simulated task data
                                param = list(init_x1,init_x2),    # initial guesses for parameters
                                fn = "mle.objectiveFunction",     # likelihood function being minimized
                                opt = "TRM",                      # trust-region optimization method
                                radius = c(tr_rad[1],tr_rad[2]),  # starting and maximum allowed trust region radius
                                nRes = nRes,                      # random restarts
                                progress_bar = progress_bar)      # track completion time of estimation


    # save mle results
    x1_hat <- mle_data[[2]] # estimates of x1
    x2_hat <- mle_data[[3]] # estimates of x2

    # store true and estimated parameters
    x1 <- data$x1
    x2 <- data$x2

    df <- data.frame(x1,
                     x2,
                     x1_hat,
                     x2_hat)

    # mle function output
    results <- list(value = df,
                    bias_x1 = bias(x1,x1_hat),
                    bias_x2 = bias(x2,x2_hat),
                    rmse_x1 = rmse(x1,x1_hat),
                    rmse_x2 = rmse(x2,x2_hat),
                    corr_x1 = cor(x1,x1_hat),
                    corr_x2 = cor(x2,x2_hat)
    )

    # if user doesn't want to see the plot, then only return results
    if (plot == FALSE){
      return(results)
    }
    else {


      # generate plot 1
      p1 <- ggplot(df,
                   aes(x=x1,y=x1_hat)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        coord_cartesian(xlim=c(min(x1,x1_hat),
                               max(x1,x1_hat)),
                        ylim=c(min(x1,x1_hat),
                               max(x1,x1_hat))) +
         labs(x = expression("x"[1]),
              y = expression(hat(x)[1])) +
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
              aspect.ratio = 1) +
        annotate("label",
                 label = paste("bias =", round(bias(x1,x1_hat),3), "\n",
                               "rmse =", round(rmse(x1,x1_hat),3),"\n",
                               "r =", round(cor(x1,x1_hat, method = "pearson"),3)),
                 x=max(x1), # adjust position based on plot
                 y=min(x1_hat)+1, # adjust position based on plot
                 size=4) +

      # add reference line
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # generate plot 2
      p2 <- ggplot(df,
                   aes(x=x2,y=x2_hat)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        coord_cartesian(xlim=c(min(x2,x2_hat),
                               max(x2,x2_hat)),
                        ylim=c(min(x2,x2_hat),
                               max(x2,x2_hat))) +
        labs(x = expression("x"[2]),
             y = expression(hat(x)[2])) +
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
              aspect.ratio = 1) +
        annotate("label",
                 label = paste("bias =", round(bias(x2, x2_hat),3),"\n",
                               "rmse =", round(rmse(x2, x2_hat),3),"\n",
                               "r =", round(cor(x2, x2_hat, method = "pearson"),3)),
                 x=max(x2), # adjust position based on plot
                 y=min(x2_hat)+1, # adjust position based on plot
                 size=4) +

      # add reference line
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # combine plots
      plot <- ggarrange(p1,p2, ncol = 2, nrow = 1,common.legend = TRUE,legend = 'bottom')


      # display the plot AND the estimation performance results
      return(list(results, p1, p2, plot))
    }


  } else if (method == "map"){ # if specified estimation method is MAP, then run MAP

    # randomly generate initial guesses for parameters
    init_x1 <- rnorm(data$subjects,0,5)
    init_x2 <- rnorm(data$subjects,0,5)

    if (is.null(prior_mean) == TRUE && is.null(prior_sd) == TRUE){

      message("MAP requires specification of initial priors; defaulting to prior_mean = c(0,0) and prior_sd = c(5,5)")

      # initialize priors for parameters
      m1 = 0
      s1 = 5

      m2 = 0
      s2 = 5

    } else {

      # initialize priors for parameters
      m1 = prior_mean[1]
      s1 = prior_sd[1]

      m2 = prior_mean[2]
      s2 = prior_sd[2]

    }

    map_data <- map_twochoiceRL(data = data,
                               param = list(init_x1,init_x2),
                               prior_mean = c(m1,m2),
                               prior_sd = c(s1,s2),
                               fn = "map.objectiveFunction",
                               opt = "TRM",
                               radius = c(tr_rad[1],tr_rad[2]),
                               nRes = nRes,
                               progress_bar = progress_bar)


  # save map results
  x1_hat <- map_data[[2]] # estimates of x1
  x2_hat <- map_data[[3]] # estimates of x2

  # store true and estimated parameters
  x1 <- data$x1
  x2 <- data$x2

  df <- data.frame(x1,
                   x2,
                   x1_hat,
                   x2_hat)

  # map function output
  results <- list(value = df,
                  bias_x1 = bias(x1,x1_hat),
                  bias_x2 = bias(x2,x2_hat),
                  rmse_x1 = rmse(x1,x1_hat),
                  rmse_x2 = rmse(x2,x2_hat),
                  corr_x1 = cor(x1,x1_hat),
                  corr_x2 = cor(x2,x2_hat)
  )


    if (plot == FALSE){
      return(results)
    }
    else {

      # generate plot 1
      p1 <- ggplot(df,
                   aes(x=x1,y=x1_hat)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        coord_cartesian(xlim=c(min(x1,x1_hat),
                               max(x1,x1_hat)),
                        ylim=c(min(x1,x1_hat),
                               max(x1,x1_hat))) +
        labs(x = expression("x"[1]),
             y = expression(hat(x)[1])) +
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
              aspect.ratio = 1) +
        annotate("label",
                 label = paste("bias =", round(bias(x1,x1_hat),3), "\n",
                               "rmse =", round(rmse(x1,x1_hat),3),"\n",
                               "r =", round(cor(x1, x1_hat, method = "pearson"),3)),
                 x=max(x1), # adjust position based on plot
                 y=min(x1_hat)+1, # adjust position based on plot
                 size=4) +

        # add reference line
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # generate plot 2
      p2 <- ggplot(df,
                   aes(x=x2,y=x2_hat)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        coord_cartesian(xlim=c(min(x2,x2_hat),
                               max(x2,x2_hat)),
                        ylim=c(min(x2,x2_hat),
                               max(x2,x2_hat))) +
        labs(x = expression("x"[2]),
             y = expression(hat(x)[2])) +
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
              aspect.ratio = 1) +
        annotate("label",
                 label = paste("bias =", round(bias(x2, x2_hat),3),"\n",
                               "rmse =", round(rmse(x2, x2_hat),3),"\n",
                               "r =", round(cor(x2, x2_hat, method = "pearson"),3)),
                 x=max(x2),
                 y=min(x2_hat)+1,
                 size=4) +

      # add reference line
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # combine plots
      plot <- ggarrange(p1,p2, ncol = 2, nrow = 1,common.legend = TRUE,legend = 'bottom')


      # display the plot AND the estimation performance results
      return(list(results, p1, p2, plot))
    }

  } else if (method == "eml") {


    d <-numeric()   # initialize vector to store difference of log-likelihood value per iteration
    diff = 10000    # initialize difference in log-likelihood value for convergence
    prev = Inf      # initialize previous log-likelihood value
    iter = 1        # initial iteration of E-M step


    # initialize variable to store posterior hyperparameters
    posterior_hyperparameters <- list()


    # Perform E-M step with Laplace Approximation
    while(diff > 0.001){ # start E-M procedure

      # generate initial guesses for iterations
      if (iter == 1){ # for the first iteration, generate initial guesses randomly
        init_x1 <- rnorm(data$subjects,0,5)
        init_x2 <- rnorm(data$subjects,0,5)

        if (is.null(prior_mean) == TRUE && is.null(prior_sd) == TRUE){

          message("EML requires specification of initial priors; defaulting to prior_mean = c(0,0) and prior_sd = c(5,5)")

          # initialize priors for parameters
          m1 = 0
          s1 = 5

          m2 = 0
          s2 = 5

        } else {

          # initialize priors for parameters
          m1 = prior_mean[1]
          s1 = prior_sd[1]

          m2 = prior_mean[2]
          s2 = prior_sd[2]

        }

      }else{ # for successive iterations, generate initial guesses based on previous MAP estimate
        init_x1 <- eml_data[[2]]
        init_x2 <- eml_data[[3]]

        m1 <- m1_laplace
        s1 <- s1_laplace

        m2 <- m2_laplace
        s2 <- s2_laplace

      }

    # Run E-step
    eml_data <- eml_twochoiceRL(data = data,
                               param = list(init_x1,init_x2),
                               prior_mean = c(m1,m2),
                               prior_sd = c(s1,s2),
                               fn = "eml.objectiveFunction",
                               opt = "TRM",
                               radius = c(tr_rad[1],tr_rad[2]),
                               nRes = nRes,
                               iter = iter,
                               eml_data = eml_data,
                               progress_bar = progress_bar)

    # calculate difference in log-likelihood for convergence
    diff <- abs(eml_data[[1]] - prev)
    d[iter] <- diff
    prev <- eml_data[[1]]

    # print output in R console
    print(paste("iter", iter, ":",
                "LL =", round(eml_data[[1]],3),",",
                "diff =", round(diff,3),",",
                "(m1,m2,s1,s2) =", "(",round(m1,3),",",round(m2,3),",",round(s1,3),",",round(s2,3),")"))
    print("---------------------------------------------------------------------------")

    # store posterior hyperparameters
    posterior_hyperparameters[[iter]] <- c(iter,m1,m2,s1,s2)

    # Run M-step with Laplace approximation: update prior mean and standard deviation
    m1_laplace <- mean(eml_data[[2]])
    s1_laplace <- sqrt(sum((eml_data[[2]]^(2))+(eml_data[[4]])-
                      (2*eml_data[[2]]*m1)+(rep(m1^(2),data$subjects)))/(data$subjects-1))

    m2_laplace <- mean(eml_data[[3]])
    s2_laplace <- sqrt(sum((eml_data[[3]]^(2))+(eml_data[[5]])-
                      (2*eml_data[[3]]*m2)+(rep(m2^(2),data$subjects)))/(data$subjects-1))

    # iterate
    iter = iter+1



  } # end E-M procedure


    # save eml results
    x1_hat <- eml_data[[2]] # estimates of x1
    x2_hat <- eml_data[[3]] # estimates of x2

    # store true and estimated parameters
    x1 <- data$x1
    x2 <- data$x2

    df <- data.frame(x1,
                     x2,
                     x1_hat,
                     x2_hat)

    posterior_hyperparam_vals <- as.data.frame(matrix(unlist(posterior_hyperparameters),length(posterior_hyperparameters),5, byrow = TRUE))
    colnames(posterior_hyperparam_vals) <- c("iter","m1","m2","s1","s2")



    # eml function output
    results <- list(value = df,
                    bias_x1 = bias(x1,x1_hat),
                    bias_x2 = bias(x2,x2_hat),
                    rmse_x1 = rmse(x1,x1_hat),
                    rmse_x2 = rmse(x2,x2_hat),
                    corr_x1 = cor(x1,x1_hat),
                    corr_x2 = cor(x2,x2_hat),
                    posterior_vals = posterior_hyperparam_vals)


    # creating variables to plot posterior hyperparameters
    # read data
    post_hyper_param <- results$posterior_vals


    # x1
    post_hyper_param_x1 <- data.frame(mean = post_hyper_param$m1,
                                      stdev = post_hyper_param$s1,
                                      iter = paste0("Iter_",sprintf("%02.0f", 1:nrow(post_hyper_param))),
                                      stringsAsFactors = F)

    # x2
    post_hyper_param_x2 <- data.frame(mean = post_hyper_param$m2,
                                      stdev = post_hyper_param$s2,
                                      iter = paste0("Iter_",sprintf("%02.0f", 1:nrow(post_hyper_param))),
                                      stringsAsFactors = F)


    # points at which to evaluate the Gaussian densities
    x1_eval <- seq(-20,20, by = 0.01) # adjust to cover the range of the mean posterior hyperparameter values
    x2_eval <- seq(-20,20, by = 0.01) # adjust to cover the range of the mean posterior hyperparameter values


    # compute Gaussian densities based on means and standard deviations
    pdf_x1 <- mapply(dnorm,
                     mean = post_hyper_param_x1$mean,
                     sd = post_hyper_param_x1$stdev,
                     MoreArgs = list(x = x1_eval),
                     SIMPLIFY = FALSE)
    pdf_x2 <- mapply(dnorm,
                     mean = post_hyper_param_x2$mean,
                     sd = post_hyper_param_x2$stdev,
                     MoreArgs = list(x = x2_eval),
                     SIMPLIFY = FALSE)

    # add group names
    names(pdf_x1) <- post_hyper_param_x1$iter
    names(pdf_x2) <- post_hyper_param_x2$iter

    # convert list to dataframe
    pdf_x1 <- do.call(cbind.data.frame, pdf_x1)
    pdf_x1$x1_eval <- x1_eval
    pdf_x2 <- do.call(cbind.data.frame, pdf_x2)
    pdf_x2$x2_eval <- x2_eval

    # convert dataframe to long format
    x1_long <- gather(pdf_x1, iter, density, -x1_eval)
    x2_long <- gather(pdf_x2, iter, density, -x2_eval)



    if (plot == FALSE){
      return(results)
    }
    else {

      # generate plot 1
      p1 <- ggplot(df,
                   aes(x=x1,y=x1_hat)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        coord_cartesian(xlim=c(min(x1,x1_hat),
                               max(x1,x1_hat)),
                        ylim=c(min(x1,x1_hat),
                               max(x1,x1_hat))) +
        labs(x = expression("x"[1]),
             y = expression(hat(x)[1])) +
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
              aspect.ratio = 1) +
        annotate("label",
                 label = paste("bias =", round(bias(x1,x1_hat),3), "\n",
                               "rmse =", round(rmse(x1,x1_hat),3),"\n",
                               "r =", round(cor(x1,x1_hat, method = "pearson"),3)),
                 x=max(x1)-1,
                 y=min(x1_hat)+0.5,
                 size=4) +

        # add reference line
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # generate plot 2
      p2 <- ggplot(df,
                   aes(x=x2,y=x2_hat)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        coord_cartesian(xlim=c(min(x2,x2_hat),
                               max(x2,x2_hat)),
                        ylim=c(min(x2,x2_hat),
                               max(x2,x2_hat))) +
        labs(x = expression("x"[2]),
             y = expression(hat(x)[2])) +
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
              aspect.ratio = 1) +
        annotate("label",
                 label = paste("bias =", round(bias(x2, x2_hat),3),"\n",
                               "rmse =", round(rmse(x2, x2_hat),3),"\n",
                               "r =", round(cor(x2, x2_hat, method = "pearson"),3)),
                 x=max(x2)-1,
                 y=min(x2_hat)+0.5,
                 size=4) +

        # add reference line
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # combine plots
      plot <- ggarrange(p1,p2, ncol = 2, nrow = 1,common.legend = TRUE,legend = 'bottom')



      # posterior hyperparameter joy plot (x1)
      ggjoy_x1 <- ggplot(x1_long,
                         aes(x = x1_eval, y = factor(iter),
                             height = density, fill = factor(iter))) +
        geom_density_ridges(stat="identity", alpha = 0.5, color = "white") +
        theme(#axis.title.y = element_blank(),
              #axis.title.x = element_blank(),
              #axis.text = element_blank(),
              panel.background = element_rect(),
              panel.grid.major = element_line(size=1),
              panel.grid.minor = element_line(size=1),
              axis.ticks = element_line(colour="black", size = 1.5),
              panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
              #legend.text = element_blank(),
              legend.position = "none",
              aspect.ratio = 1) +
            labs(x = "x1", y = "Iteration")

      # posterior hyperparameter joy plot (x2)
      ggjoy_x2 <- ggplot(x2_long,
                         aes(x = x2_eval, y = factor(iter),
                             height = density, fill = factor(iter))) +
        geom_density_ridges(stat="identity", alpha = 0.5, color = "white") +
        theme(#axis.title.y = element_blank(),
              #axis.title.x = element_blank(),
              #axis.text = element_blank(),
              panel.background = element_rect(),
              panel.grid.major = element_line(size=1),
              panel.grid.minor = element_line(size=1),
              axis.ticks = element_line(colour="black", size = 1.5),
              panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
              #legend.text = element_blank(),
              legend.position = "none",
              aspect.ratio = 1) +
            labs(x = "x2", y = "Iteration")

      ggjoy_x1x2 <- ggarrange(ggjoy_x1,ggjoy_x2, ncol = 2, nrow = 1)


      # animated joy plot (x1)
      ggjoy_x1_anim <- ggjoy_x1 +
        transition_states(factor(iter), transition_length = 1, state_length = 1) +
        shadow_mark()
      # animated joy plot (x2)
      ggjoy_x2_anim <- ggjoy_x2 +
        transition_states(factor(iter), transition_length = 1, state_length = 1) +
        shadow_mark()

      # return all results and plots
      return(list(results, p1, p2, plot, ggjoy_x1, ggjoy_x2, ggjoy_x1x2, ggjoy_x1_anim, ggjoy_x2_anim))

    } # end else statement for plotting EML results


  } # end else if statement for EML


} # end estimation









#' Maximum-likelihood estimation (MLE) of behavior in a two-choice decision task
#'
#' This function runs the MLE technique to recover behavior in a two-choice decision task.
#' @param data simulated two-choice task data
#' @param param randomly generated initial parameters
#' @param fn objective function being minimized
#' @param opt optimization algorithm used. Defaults to trust-region method (trm)
#' @param radius starting and maximum allowed trust region radius. Defaults to 1 and 5.
#' @param nRes number of restarts. Defaults to 5
#' @param progress_bar tracks completion time of function. Defaults to TRUE.
#'
#'
#' @return A list containing the sum log-likelihood, mle estimates of parameter 1 per subject,
#'         mle estimates of parameter 2 per subject, laplace values of parameter 1 per subject,
#'         laplace values of parameter 2 per subject.
#'
#' @keywords internal
#'
#' @import trust









##########################################################################

mle_twochoiceRL <- function(data = NULL,
                            param = list(init_x1,init_x2),
                            fn = "mle.objectiveFunction",
                            opt = "TRM",
                            radius = c(tr_rad[1],tr_rad[2]),
                            nRes = nRes,
                            progress_bar = progress_bar){



  # intialize variables as lists
  mle.results <- list()
  value_list.mle <- list()
  param_list.mle <- list()
  hess_list.mle <- list()

  hess_list.mle.per.subj <- list()
  mle.laplace.x1.per.subj <- numeric()
  mle.laplace.x2.per.subj <- numeric()


  # initialize output variables
  mle.est.x1.per.subj <- numeric()
  mle.est.x2.per.subj <- numeric()
  mle.ll.per.subj <- numeric()


  if (progress_bar == TRUE){
    pb <- txtProgressBar(min = 0, max = data$subjects, style = 3)
  }

  for (i in 1:data$subjects){ # start loop for subjects

    if (fn == "mle.objectiveFunction" ){
      mle.objfun <- function(param = c(x1, x2)){

        ## initialization of variables

        # model parameters
        x1 <- param[1]
        x2 <- param[2]
        beta <- exp(x1)
        alpha <- (1)/(1+exp(-x2))

        # data variables
        X <- data$twochoiceRL[[i]]$Action      # participant choices
        r <- data$twochoiceRL[[i]]$Reward      # participant outcomes (i.e., reward)

        # log-likelihood variable, probability variable, expected value variable
        LL <- 0
        pr <- matrix(c(0,0),2,1)  # probability matrix (2x1) where each element corresponds to choice 1 and choice 2, respectively
        Q <- matrix(c(0,0),2,1)   # expected value matrix (2x1) where each element corresponds to choice 1 and choice 2, respectively


        # gradient variables
        dLL.x1 <- 0 # gradient w.r.t x1
        dLL.x2 <- 0 # gradient w.r.t x2
        dQ.dx1 <- matrix(c(0,0),2,1) # differential of Q w.r.t x1
        dQ.dx2 <- matrix(c(0,0),2,1) # differential of Q w.r.t x2


        # hessian variables
        dLL.x1x1 <- 0 # hessian of x1 w.r.t x1
        dLL.x1x2 <- 0 # hessian of x1 w.r.t x2
        dLL.x2x1 <- 0 # hessian of x2 w.r.t x1
        dLL.x2x2 <- 0 # hessian of x2 w.r.t x2
        dQ.dx1x1 <- matrix(c(0,0),2,1) # second differential of Q of x1 w.r.t x1
        dQ.dx1x2 <- matrix(c(0,0),2,1) # second differential of Q of x1 w.r.t x2
        dQ.dx2x1 <- matrix(c(0,0),2,1) # second differential of Q of x2 w.r.t x1
        dQ.dx2x2 <- matrix(c(0,0),2,1) # second differential of Q of x2 w.r.t x2




        # compute log-likelihood of choice, gradient of log-likelihood and hessian of log-likelihood for each trial
        for (t in 1:length(X)){ # start of loop for computing likelihood, gradient and hessian

          # Generate probability of participants' choice using softmax function
          pr <- softmax(Q)


          # Probability/likelihood of "true" simulated choice
          like <- pr[X[t]]

          # Log of like
          LL[t] <- log(like)


          # variable assignment for simplicity in formula (please refer to appendix)
          br <- beta*r[t]

          # computing gradient of log-likelihood w.r.t parameters
          dLL.x1[t] <- dQ.dx1[X[t]] - t(pr)%*%dQ.dx1 # equation 1
          dLL.x2[t] <- dQ.dx2[X[t]] - t(pr)%*%dQ.dx2 # equation 2

          # computing hessian of log-likelihood w.r.t parameters
          dLL.x1x1[t] <- dQ.dx1x1[X[t]] - (t(pr)%*%dQ.dx1x1) - (dLL.x1[t]*pr[X[t]])*(dQ.dx1[X[t]] - dQ.dx1[3-X[t]]) # equation 3
          dLL.x2x2[t] <- dQ.dx2x2[X[t]] - (t(pr)%*%dQ.dx2x2) - (dLL.x2[t]*pr[X[t]])*(dQ.dx2[X[t]] - dQ.dx2[3-X[t]]) # equation 4
          dLL.x1x2[t] <- dQ.dx1x2[X[t]] - (t(pr)%*%dQ.dx1x2) - (pr[X[t]]*pr[3-X[t]])*(dQ.dx1[X[t]] - dQ.dx1[3-X[t]])*(dQ.dx2[X[t]] - dQ.dx2[3-X[t]]) # equation 5
          dLL.x2x1[t] <- dQ.dx2x1[X[t]] - (t(pr)%*%dQ.dx2x1) - (pr[X[t]]*pr[3-X[t]])*(dQ.dx1[X[t]] - dQ.dx1[3-X[t]])*(dQ.dx2[X[t]] - dQ.dx2[3-X[t]]) # equation 6


          # second derivative updating
          dQ.dx1x1[X[t]] <- (1-alpha)*dQ.dx1x1[X[t]] + alpha*br
          dQ.dx1x2[X[t]] <- (1-alpha)*dQ.dx1x2[X[t]] - (alpha)*(1-alpha)*(dQ.dx1[X[t]]) + (alpha)*(1-alpha)*br
          dQ.dx2x1[X[t]] <- (1-alpha)*dQ.dx2x1[X[t]] - (alpha)*(1-alpha)*(dQ.dx1[X[t]]) + (alpha)*(1-alpha)*br
          dQ.dx2x2[X[t]] <- (1-alpha)*dQ.dx2x2[X[t]] - 2*((alpha)*(1-alpha)*(dQ.dx2[X[t]])) + (1-(2*alpha))*(br-Q[X[t]])

          # first derivative updating
          dQ.dx1[X[t]] <- (1-alpha)*dQ.dx1[X[t]] + alpha*br
          dQ.dx2[X[t]] <- (1-alpha)*dQ.dx2[X[t]] + (alpha)*(1-alpha)*(br-Q[X[t]])


          # Updating expected value (Q)
          Q[X[t]] <- Q[X[t]] + alpha * (br - Q[X[t]])


        } # end of loop for computing negative log-likelihood, gradient and hessian


        # return the summed (negative) log-likelihood, the gradient and the hessian
        list(value = sum(-1*LL),
             gradient = -1*c(sum(dLL.x1),sum(dLL.x2)),
             hessian = matrix(-1*c(sum(dLL.x1x1),sum(dLL.x1x2),sum(dLL.x2x1),sum(dLL.x2x2)),2,2)
        )

        }
     }


    if (progress_bar == FALSE){
      print(paste("working on subject", i, "..."))
    }

    # initialize list within a list for the trust results, log-likelihood value and hessian to store results at every restart
    mle.results[[i]] <- list()
    value_list.mle[[i]] <- list()
    hess_list.mle[[i]] <- list()

    # set random initial guesses for x1 and x2
    init_x1 <- param[[1]][i]
    init_x2 <- param[[2]][i]

    r = 1 # restart = 1


    while(r < nRes+1){ # start while loop for restarts

      if (progress_bar == FALSE){
        print(paste("initial guess for x1 and x2 is: ", round(init_x1,3), "and", round(init_x2,3), "on restart", r, "for subject", i))
      }

      out <- tryCatch({ # start catch statement

        if (opt == "TRM"){
          mle.results[[i]][[r]] <- trust(mle.objfun, c(init_x1,init_x2),radius[1],radius[2])
        }

        if (progress_bar == FALSE){
          print(paste("trust was successful on restart", r, "for subject", i ))
          print(paste("estimated x1 and estimated x2 are: ", round(mle.results[[i]][[r]]$argument[1],3), "and", round(mle.results[[i]][[r]]$argument[2],3), "on restart", r, "for subject", i))
          print(paste("true x1 and true x2 are: ", round(data$x1[i],3), "and", round(data$x2[i],3), "on restart", r, "for subject", i))
        }

        1

      }, error = function(e){ # if error has been caught, print "caught error" message

        if (progress_bar == FALSE){
          message(paste("trust has failed on restart", r, "for subject", i, "trying different initial guess"))
        }

        0

      })


      if (out == 1){

        if (progress_bar == FALSE){
          print(paste("checking for positive definite hessian on restart", r,"for subject", i))
        }

        if ((det(mle.results[[i]][[r]]$hessian) > 0) && (diag(mle.results[[i]][[r]]$hessian)[1] > 0 ) && (diag(mle.results[[i]][[r]]$hessian)[2] > 0 )){

          # store results from trust per restart for a subject
          value_list.mle[[i]][[r]] <- mle.results[[i]][[r]]$value              # store objective value per restart for a subject
          hess_list.mle[[i]][[r]] <- solve(mle.results[[i]][[r]]$hessian)      # store hessian value per restart for a subject

          if (progress_bar == FALSE){
            print(mle.results[[i]][[r]]$hessian)

            print(paste("restart", r, "is a good restart with positive definite hessian"))
            print("---------------------------------------------------------------------------------------")
          }

          r = r+1


          init_x1 <-  rnorm(data$subjects,0,5)[i]
          init_x2 <-  rnorm(data$subjects,0,5)[i]



        } else{

          if (progress_bar == FALSE){
            message(paste("caught non-positive definite hessian on restart", r, "for subject", i, "trying different initial guess"))
          }

          init_x1 <- rnorm(data$subjects,0,5)[i]
          init_x2 <- rnorm(data$subjects,0,5)[i]

        }
      }



      if (out == 0){

        init_x1 <- rnorm(data$subjects,0,5)[i]
        init_x2 <- rnorm(data$subjects,0,5)[i]

      }
    } # end while loop for restarts


    # Select the "best" MAP corresponding to the minimum objective value among restarts
    indx.mle <- which.min(value_list.mle[[i]])                                    # for a subject find index of restart that contains minimum objective value
    param_list.mle[[i]] <- mle.results[[i]][[indx.mle]]$argument                  # store "best" MAP estimate for each subject
    hess_list.mle.per.subj[[i]] <- solve(mle.results[[i]][[indx.mle]]$hessian)    # store hessian corresponding to "best" MAP for each subject


    # store output results
    mle.ll.per.subj[i] <- mle.results[[i]][[indx.mle]]$value                # for each subject, store objective values corresponding to "best" MAP estimates
    mle.est.x1.per.subj[i] <- param_list.mle[[i]][1]                        # for each subject, store estimated x1 parameter
    mle.est.x2.per.subj[i] <- param_list.mle[[i]][2]                        # for each subject, store estimated x2 parameter
    mle.laplace.x1.per.subj[i] <- diag(hess_list.mle.per.subj[[i]])[1]      # for each subject, store laplacian for x1 parameter
    mle.laplace.x2.per.subj[i] <- diag(hess_list.mle.per.subj[[i]])[2]      # for each subject, store laplacian for x2 parameter

    if (progress_bar == TRUE){
      Sys.sleep(0.1)
      setTxtProgressBar(pb, i)
    }

  } # End for loop for subjects

  if (progress_bar == TRUE){
    close(pb)
  }

  # output results from function
  list(sum(mle.ll.per.subj),                # sum log-likelihood
       mle.est.x1.per.subj,                 # x1 parameter estimates per subject
       mle.est.x2.per.subj,                # x2 parameter estimates per subject
       mle.laplace.x1.per.subj,             # x1 laplacian value per subject
       mle.laplace.x2.per.subj              # x2 laplacian value per subject
  )


} # End MLE








#' Maximum A Posteriori (MAP) of behavior in a two-choice decision task
#'
#' This function runs the MAP technique to recover behavior in a two-choice decision task.
#' @param data simulated two-choice task data
#' @param param randomly generated initial parameters.
#' @param prior_mean mean priors for x1 and x2, respectively. Defaults to mean 0 for both parameters.
#' @param prior_sd standard deviation priors for x1 and x2, respectively. Defaults to 5 for both parameters.
#' @param fn objective function being minimized
#' @param opt optimization algorithm used. Defaults to trust-region method (trm)
#' @param radius starting and maximum allowed trust region radius. Defaults to 1 and 5.
#' @param nRes number of restarts. Defaults to 5.
#' @param progress_bar track completion time of estimation. Defaults to TRUE.
#'
#' @return A list containing the sum log-likelihood, map estimates of parameter 1 per subject,
#'         map estimates of parameter 2 per subject, laplace values of parameter 1 per subject,
#'         laplace values of parameter 2 per subject.
#'
#' @keywords internal
#'
#' @import trust


##########################################################################

map_twochoiceRL <- function(data = NULL,
                           param = list(init_x1,init_x2),
                           prior_mean = c(m1,m2),
                           prior_sd = c(s1,s2),
                           fn = "map.objectiveFunction",
                           opt = "TRM",
                           radius = c(tr_rad[1],tr_rad[2]),
                           nRes = nRes,
                           progress_bar = progress_bar){



  # initialize prior mean and prior standard deviation for x1 and x2
  m1 <- prior_mean[1]
  s1 <- prior_sd[1]
  m2 <- prior_mean[2]
  s2 <- prior_sd[2]


  # intialize variables as lists
  map.results <- list()
  value_list.map <- list()
  param_list.map <- list()
  hess_list.map <- list()

  hess_list.map.per.subj <- list()
  map.laplace.x1.per.subj <- numeric()
  map.laplace.x2.per.subj <- numeric()


  # initialize output variables
  map.est.x1.per.subj <- numeric()
  map.est.x2.per.subj <- numeric()
  map.ll.per.subj <- numeric()


  if (progress_bar == TRUE){
    pb <- txtProgressBar(min = 0, max = data$subjects, style = 3)
  }


  for (i in 1:data$subjects){ # start loop for subjects

    if (fn == "map.objectiveFunction"){
      map.objfun <- function(param = c(x1, x2)){

        ## initialization of variables

        # model parameters
        x1 <- param[1]
        x2 <- param[2]
        beta <- exp(x1)
        alpha <- (1)/(1+exp(-x2))

        # data variables
        X <- data$twochoiceRL[[i]]$Action      # participants' action
        r <- data$twochoiceRL[[i]]$Reward      # participants' reward

        # log-likelihood variable, probability variable, expected value variable
        LL <- 0
        pr <- matrix(c(0,0),2,1)  # probability matrix (2x1) where each element corresponds to action 1 and 2, respectively
        Q <- matrix(c(0,0),2,1)   # expected value matrix (2x1) where each element corresponds to action 1 and 2, respectively

        # prior mean variables for parameters x1 and x2
        m1 <- m1     # prior mean of x1 (i.e., beta)
        m2 <- m2     # prior mean of x2 (i.e., alpha)
        s1 <- s1     # prior standard deviation of x1
        s2 <- s2     # prior standard deviation of x2


        # gradient variables
        dLL.dx1 <- 0 # gradient w.r.t x1
        dLL.dx2 <- 0 # gradient w.r.t x2
        dQ.dx1 <- matrix(c(0,0),2,1) # differential of Q w.r.t x1
        dQ.dx2 <- matrix(c(0,0),2,1) # differential of Q w.r.t x2


        # hessian variables
        dLL.dx1x1 <- 0 # hessian of x1 w.r.t x1
        dLL.dx1x2 <- 0 # hessian of x1 w.r.t x2
        dLL.dx2x1 <- 0 # hessian of x2 w.r.t x1
        dLL.dx2x2 <- 0 # hessian of x2 w.r.t x2
        dQ.dx1x1 <- matrix(c(0,0),2,1) # second differential of Q of x1 w.r.t x1
        dQ.dx1x2 <- matrix(c(0,0),2,1) # second differential of Q of x1 w.r.t x2
        dQ.dx2x1 <- matrix(c(0,0),2,1) # second differential of Q of x2 w.r.t x1
        dQ.dx2x2 <- matrix(c(0,0),2,1) # second differential of Q of x2 w.r.t x2





        # compute log-likelihood of action, gradient of log-likelihood and hessian of log-likelihood for each trial
        for (t in 1:length(X)){ # start of loop for computing likelihood, gradient and hessian

          # Generate probability of participants' choice using softmax function
          pr <- softmax(Q)


          # Probability/likelihood of "true" simulated action
          like <- pr[X[t]]

          # Log of like
          LL[t] <- log(like)


          # variable assignment for simplicity in formula (please refer to appendix)
          br <- beta*r[t]

          # computing gradient of log-likelihood w.r.t parameters
          dLL.dx1[t] <- dQ.dx1[X[t]] - t(pr)%*%dQ.dx1 # equation 1
          dLL.dx2[t] <- dQ.dx2[X[t]] - t(pr)%*%dQ.dx2 # equation 2

          # computing hessian of log-likelihood w.r.t parameters
          dLL.dx1x1[t] <- dQ.dx1x1[X[t]] - (t(pr)%*%dQ.dx1x1) - (dLL.dx1[t]*pr[X[t]])*(dQ.dx1[X[t]] - dQ.dx1[3-X[t]]) # equation 3
          dLL.dx2x2[t] <- dQ.dx2x2[X[t]] - (t(pr)%*%dQ.dx2x2) - (dLL.dx2[t]*pr[X[t]])*(dQ.dx2[X[t]] - dQ.dx2[3-X[t]]) # equation 4
          dLL.dx1x2[t] <- dQ.dx1x2[X[t]] - (t(pr)%*%dQ.dx1x2) - (pr[X[t]]*pr[3-X[t]])*(dQ.dx1[X[t]] - dQ.dx1[3-X[t]])*(dQ.dx2[X[t]] - dQ.dx2[3-X[t]]) # equation 5
          dLL.dx2x1[t] <- dQ.dx2x1[X[t]] - (t(pr)%*%dQ.dx2x1) - (pr[X[t]]*pr[3-X[t]])*(dQ.dx1[X[t]] - dQ.dx1[3-X[t]])*(dQ.dx2[X[t]] - dQ.dx2[3-X[t]]) # equation 6


          # second derivative updating
          dQ.dx1x1[X[t]] <- (1-alpha)*dQ.dx1x1[X[t]] + alpha*br
          dQ.dx1x2[X[t]] <- (1-alpha)*dQ.dx1x2[X[t]] - (alpha)*(1-alpha)*(dQ.dx1[X[t]]) + (alpha)*(1-alpha)*br
          dQ.dx2x1[X[t]] <- (1-alpha)*dQ.dx2x1[X[t]] - (alpha)*(1-alpha)*(dQ.dx1[X[t]]) + (alpha)*(1-alpha)*br
          dQ.dx2x2[X[t]] <- (1-alpha)*dQ.dx2x2[X[t]] - 2*((alpha)*(1-alpha)*(dQ.dx2[X[t]])) + (1-(2*alpha))*(br-Q[X[t]])

          # first derivative updating
          dQ.dx1[X[t]] <- (1-alpha)*dQ.dx1[X[t]] + alpha*br
          dQ.dx2[X[t]] <- (1-alpha)*dQ.dx2[X[t]] + (alpha)*(1-alpha)*(br-Q[X[t]])


          # Updating expected value (Q)
          Q[X[t]] <- Q[X[t]] + alpha * (br - Q[X[t]])


        } # end of loop for computing likelihood, gradient and hessian

        # Likelihood of x1 and x2 according to log prior distribution
        log.prior1 <- dnorm(x = x1, mean = m1, sd = s1, log=TRUE)
        log.prior2 <- dnorm(x = x2, mean = m2, sd = s2, log=TRUE)
        prior <- sum(log.prior1,log.prior2)

        # gradient of x1 and x2 according to log prior distribution
        gr.prior.x1 <- -(1/s1^(2))*(x1-m1)
        gr.prior.x2 <- -(1/s2^(2))*(x2-m2)

        # hessian of x1 and x2 according to log prior distribution
        hess.prior.x1x1 <- -(1/s1^(2))
        hess.prior.x1x2 <- 0
        hess.prior.x2x1 <- 0
        hess.prior.x2x2 <- -(1/s2^(2))



        # return the summed (negative) log-likelihood with prior information, the gradient and the hessian
        list(value = sum(-1*LL)-prior,
             gradient = -1*c(sum(dLL.dx1),sum(dLL.dx2))-c(gr.prior.x1,gr.prior.x2),
             hessian = matrix(-1*c(sum(dLL.dx1x1),sum(dLL.dx1x2),sum(dLL.dx2x1),sum(dLL.dx2x2))-c(hess.prior.x1x1,hess.prior.x1x2,hess.prior.x2x1,hess.prior.x2x2),2,2))

      } # end of function that computes negative log-likelihood, gradient and Hessian of the function
    }


    if (progress_bar == FALSE){
      print(paste("working on subject", i, "..."))
    }

    # initialize list within a list for the trust results, log-likelihood value and hessian to store results at every restart
    map.results[[i]] <- list()
    value_list.map[[i]] <- list()
    hess_list.map[[i]] <- list()

    # set random initial guesses for x1 and x2
    init_x1 <- param[[1]][i]
    init_x2 <- param[[2]][i]

    r = 1 # restart = 1


    while(r < nRes+1){ # start while loop for restarts

      if (progress_bar == FALSE){
        print(paste("initial guess for x1 and x2 is: ", round(init_x1,3), "and", round(init_x2,3), "on restart", r, "for subject", i))
      }

      out <- tryCatch({ # start catch statement

        if (opt == "TRM"){
          map.results[[i]][[r]] <- trust(map.objfun, c(init_x1,init_x2),radius[1],radius[2])
        }

        if (progress_bar == FALSE){
          print(paste("trust was successful on restart", r, "for subject", i ))
          print(paste("estimated x1 and estimated x2 are: ", round(map.results[[i]][[r]]$argument[1],3), "and", round(map.results[[i]][[r]]$argument[2],3), "on restart", r, "for subject", i))
          print(paste("true x1 and true x2 are: ", round(data$x1[i],3), "and", round(data$x2[i],3), "on restart", r, "for subject", i))
        }

        1

      }, error = function(e){ # if error has been caught, print "caught error" message

        if (progress_bar == FALSE){
          message(paste("trust has failed on restart", r, "for subject", i, "trying different initial guess"))
        }

        0

      })


      #install.packages("numDeriv")
      #      library(numDeriv)
      if (out == 1){

        if (progress_bar == FALSE){
          print(paste("checking for positive definite hessian on restart", r,"for subject", i))
        }

        if ((det(map.results[[i]][[r]]$hessian) > 0) && (diag(map.results[[i]][[r]]$hessian)[1] > 0 ) && (diag(map.results[[i]][[r]]$hessian)[2] > 0 )){

          # store results from trust per restart for a subject
          value_list.map[[i]][[r]] <- map.results[[i]][[r]]$value              # store objective value per restart for a subject
          hess_list.map[[i]][[r]] <- solve(map.results[[i]][[r]]$hessian)      # store hessian value per restart for a subject

          if (progress_bar == FALSE){
            print(map.results[[i]][[r]]$hessian)

            print(paste("restart", r, "is a good restart with positive definite hessian"))
            print("---------------------------------------------------------------------------------------")
          }

          r = r+1


          init_x1 <-  rnorm(data$subjects,0,5)[i]
          init_x2 <-  rnorm(data$subjects,0,5)[i]



        } else{

          if (progress_bar == FALSE){
            message(paste("caught non-positive definite hessian on restart", r, "for subject", i, "trying different initial guess"))
          }

          init_x1 <- rnorm(data$subjects,0,5)[i]
          init_x2 <- rnorm(data$subjects,0,5)[i]

        }
      }



      if (out == 0){

        init_x1 <- rnorm(data$subjects,0,5)[i]
        init_x2 <- rnorm(data$subjects,0,5)[i]

      }
    } # end while loop for restarts


    # Select the "best" MAP corresponding to the minimum objective value among restarts
    indx.map <- which.min(value_list.map[[i]])                                    # for a subject find index of restart that contains minimum objective value
    param_list.map[[i]] <- map.results[[i]][[indx.map]]$argument                  # store "best" MAP estimate for each subject
    hess_list.map.per.subj[[i]] <- solve(map.results[[i]][[indx.map]]$hessian)    # store hessian corresponding to "best" MAP for each subject


    # store output results
    map.ll.per.subj[i] <- map.results[[i]][[indx.map]]$value                # for each subject, store objective values corresponding to "best" MAP estimates
    map.est.x1.per.subj[i] <- param_list.map[[i]][1]                        # for each subject, store estimated x1 parameter
    map.est.x2.per.subj[i] <- param_list.map[[i]][2]                        # for each subject, store estimated x2 parameter
    map.laplace.x1.per.subj[i] <- diag(hess_list.map.per.subj[[i]])[1]      # for each subject, store laplacian for x1 parameter
    map.laplace.x2.per.subj[i] <- diag(hess_list.map.per.subj[[i]])[2]      # for each subject, store laplacian for x2 parameter

    if (progress_bar == TRUE){
      Sys.sleep(0.1)
      setTxtProgressBar(pb, i)
    }

  } # End for loop for subjects

  if (progress_bar == TRUE){
    close(pb)
  }

  # output results from function
  list(sum(map.ll.per.subj),                # sum log-likelihood
       map.est.x1.per.subj,                 # x1 parameter estimates per subject
       map.est.x2.per.subj,                # x2 parameter estimates per subject
       map.laplace.x1.per.subj,             # x1 laplacian value per subject
       map.laplace.x2.per.subj              # x2 laplacian value per subject
  )

  #} # End MAP
}







#' Expectation-Maximization with Laplace approximation (EML) of behavior in a two-choice decision task
#'
#' This function runs the EML technique to recover behavior in a two-choice decision task.
#' @param data simulated two-choice task data
#' @param param randomly generated initial parameters.
#' @param prior_mean mean priors for x1 and x2, respectively. Defaults to mean 0 for both parameters.
#' @param prior_sd standard deviation priors for x1 and x2, respectively. Defaults to 5 for both parameters.
#' @param fn objective function being minimized
#' @param opt optimization algorithm used. Defaults to trust-region method (trm)
#' @param radius starting and maximum allowed trust region radius. Defaults to 1 and 5.
#' @param nRes number of restarts. Defaults to 5.
#' @param iter iteration of algorithm
#' @param eml_data data to be passed for succeeding iterations
#' @param progress_bar track completion time of estimation. Defaults to TRUE.
#'
#' @return A list containing the sum log-likelihood, eml estimates of parameter 1 per subject,
#'         eml estimates of parameter 2 per subject, laplace values of parameter 1 per subject,
#'         laplace values of parameter 2 per subject.
#'
#' @keywords internal
#'
#' @import trust





##########################################################################

eml_twochoiceRL <- function(data = NULL,
                           param = list(init_x1,init_x2),
                           prior_mean = c(m1,m2),
                           prior_sd = c(s1,s2),
                           fn = "eml.objectiveFunction",
                           opt = "TRM",
                           radius = c(tr_rad[1],tr_rad[2]),
                           nRes = nRes,
                           iter = iter,
                           eml_data = eml_data,
                           progress_bar = progress_bar){






  # initialize prior mean and prior standard deviation for x1 and x2
  m1 <- prior_mean[1]
  s1 <- prior_sd[1]
  m2 <- prior_mean[2]
  s2 <- prior_sd[2]


  # intialize variables as lists
  eml.results <- list()
  value_list.eml <- list()
  param_list.eml <- list()
  hess_list.eml <- list()

  hess_list.eml.per.subj <- list()
  eml.laplace.x1.per.subj <- numeric()
  eml.laplace.x2.per.subj <- numeric()


  # initialize output variables
  eml.est.x1.per.subj <- numeric()
  eml.est.x2.per.subj <- numeric()
  eml.ll.per.subj <- numeric()


  if (progress_bar == TRUE){
    pb <- txtProgressBar(min = 0, max = data$subjects, style = 3)
  }


  for (i in 1:data$subjects){ # start loop for subjects

    if (fn == "eml.objectiveFunction"){
      eml.objfun <- function(param = c(x1, x2)){

        ## initialization of variables

        # model parameters
        x1 <- param[1]
        x2 <- param[2]
        beta <- exp(x1)
        alpha <- (1)/(1+exp(-x2))

        # data variables
        X <- data$twochoiceRL[[i]]$Action      # participants' action
        r <- data$twochoiceRL[[i]]$Reward      # participants' reward

        # log-likelihood variable, probability variable, expected value variable
        LL <- 0
        pr <- matrix(c(0,0),2,1)  # probability matrix (2x1) where each element corresponds to action 1 and 2, respectively
        Q <- matrix(c(0,0),2,1)   # expected value matrix (2x1) where each element corresponds to action 1 and 2, respectively

        # prior mean variables for parameters x1 and x2
        m1 <- m1     # prior mean of x1 (i.e., beta)
        m2 <- m2     # prior mean of x2 (i.e., alpha)
        s1 <- s1     # prior standard deviation of x1
        s2 <- s2     # prior standard deviation of x2


        # gradient variables
        dLL.dx1 <- 0 # gradient w.r.t x1
        dLL.dx2 <- 0 # gradient w.r.t x2
        dQ.dx1 <- matrix(c(0,0),2,1) # differential of Q w.r.t x1
        dQ.dx2 <- matrix(c(0,0),2,1) # differential of Q w.r.t x2


        # hessian variables
        dLL.dx1x1 <- 0 # hessian of x1 w.r.t x1
        dLL.dx1x2 <- 0 # hessian of x1 w.r.t x2
        dLL.dx2x1 <- 0 # hessian of x2 w.r.t x1
        dLL.dx2x2 <- 0 # hessian of x2 w.r.t x2
        dQ.dx1x1 <- matrix(c(0,0),2,1) # second differential of Q of x1 w.r.t x1
        dQ.dx1x2 <- matrix(c(0,0),2,1) # second differential of Q of x1 w.r.t x2
        dQ.dx2x1 <- matrix(c(0,0),2,1) # second differential of Q of x2 w.r.t x1
        dQ.dx2x2 <- matrix(c(0,0),2,1) # second differential of Q of x2 w.r.t x2





        # compute log-likelihood of action, gradient of log-likelihood and hessian of log-likelihood for each trial
        for (t in 1:length(X)){ # start of loop for computing likelihood, gradient and hessian

          # Generate probability of participants' choice using softmax function
          pr <- softmax(Q)


          # Probability/likelihood of "true" simulated action
          like <- pr[X[t]]

          # Log of like
          LL[t] <- log(like)


          # variable assignment for simplicity in formula (please refer to appendix)
          br <- beta*r[t]

          # computing gradient of log-likelihood w.r.t parameters
          dLL.dx1[t] <- dQ.dx1[X[t]] - t(pr)%*%dQ.dx1 # equation 1
          dLL.dx2[t] <- dQ.dx2[X[t]] - t(pr)%*%dQ.dx2 # equation 2

          # computing hessian of log-likelihood w.r.t parameters
          dLL.dx1x1[t] <- dQ.dx1x1[X[t]] - (t(pr)%*%dQ.dx1x1) - (dLL.dx1[t]*pr[X[t]])*(dQ.dx1[X[t]] - dQ.dx1[3-X[t]]) # equation 3
          dLL.dx2x2[t] <- dQ.dx2x2[X[t]] - (t(pr)%*%dQ.dx2x2) - (dLL.dx2[t]*pr[X[t]])*(dQ.dx2[X[t]] - dQ.dx2[3-X[t]]) # equation 4
          dLL.dx1x2[t] <- dQ.dx1x2[X[t]] - (t(pr)%*%dQ.dx1x2) - (pr[X[t]]*pr[3-X[t]])*(dQ.dx1[X[t]] - dQ.dx1[3-X[t]])*(dQ.dx2[X[t]] - dQ.dx2[3-X[t]]) # equation 5
          dLL.dx2x1[t] <- dQ.dx2x1[X[t]] - (t(pr)%*%dQ.dx2x1) - (pr[X[t]]*pr[3-X[t]])*(dQ.dx1[X[t]] - dQ.dx1[3-X[t]])*(dQ.dx2[X[t]] - dQ.dx2[3-X[t]]) # equation 6


          # second derivative updating
          dQ.dx1x1[X[t]] <- (1-alpha)*dQ.dx1x1[X[t]] + alpha*br
          dQ.dx1x2[X[t]] <- (1-alpha)*dQ.dx1x2[X[t]] - (alpha)*(1-alpha)*(dQ.dx1[X[t]]) + (alpha)*(1-alpha)*br
          dQ.dx2x1[X[t]] <- (1-alpha)*dQ.dx2x1[X[t]] - (alpha)*(1-alpha)*(dQ.dx1[X[t]]) + (alpha)*(1-alpha)*br
          dQ.dx2x2[X[t]] <- (1-alpha)*dQ.dx2x2[X[t]] - 2*((alpha)*(1-alpha)*(dQ.dx2[X[t]])) + (1-(2*alpha))*(br-Q[X[t]])

          # first derivative updating
          dQ.dx1[X[t]] <- (1-alpha)*dQ.dx1[X[t]] + alpha*br
          dQ.dx2[X[t]] <- (1-alpha)*dQ.dx2[X[t]] + (alpha)*(1-alpha)*(br-Q[X[t]])


          # Updating expected value (Q)
          Q[X[t]] <- Q[X[t]] + alpha * (br - Q[X[t]])


        } # end of loop for computing likelihood, gradient and hessian

        # Likelihood of x1 and x2 according to log prior distribution
        log.prior1 <- dnorm(x = x1, mean = m1, sd = s1, log=TRUE)
        log.prior2 <- dnorm(x = x2, mean = m2, sd = s2, log=TRUE)
        prior <- sum(log.prior1,log.prior2)

        # gradient of x1 and x2 according to log prior distribution
        gr.prior.x1 <- -(1/s1^(2))*(x1-m1)
        gr.prior.x2 <- -(1/s2^(2))*(x2-m2)

        # hessian of x1 and x2 according to log prior distribution
        hess.prior.x1x1 <- -(1/s1^(2))
        hess.prior.x1x2 <- 0
        hess.prior.x2x1 <- 0
        hess.prior.x2x2 <- -(1/s2^(2))



        # return the summed (negative) log-likelihood with prior information, the gradient and the hessian
        list(value = sum(-1*LL)-prior,
             gradient = -1*c(sum(dLL.dx1),sum(dLL.dx2))-c(gr.prior.x1,gr.prior.x2),
             hessian = matrix(-1*c(sum(dLL.dx1x1),sum(dLL.dx1x2),sum(dLL.dx2x1),sum(dLL.dx2x2))-c(hess.prior.x1x1,hess.prior.x1x2,hess.prior.x2x1,hess.prior.x2x2),2,2))

      } # end of function that computes negative log-likelihood, gradient and Hessian of the function
   }

    if (progress_bar == FALSE){
      print(paste("working on subject", i, "..."))
    }

    # initialize list within a list for the trust results, log-likelihood value and hessian to store results at every restart
    eml.results[[i]] <- list()
    value_list.eml[[i]] <- list()
    hess_list.eml[[i]] <- list()

    # set random initial guesses for x1 and x2
    init_x1 <- param[[1]][i]
    init_x2 <- param[[2]][i]

    r = 1 # restart = 1

    # for kth iteration > 1 perform only 1 restart for each subject
    if (iter > 1){
      nRes = nRes
    }

    while(r < nRes+1){ # start while loop for restarts

      if (progress_bar == FALSE){
        print(paste("initial guess for x1 and x2 is: ", round(init_x1,3), "and", round(init_x2,3), "on restart", r, "for subject", i))
      }

      out <- tryCatch({ # start catch statement

        if (opt == "TRM"){
          eml.results[[i]][[r]] <- trust(eml.objfun, c(init_x1,init_x2),radius[1],radius[2])
        }

        if (progress_bar == FALSE){
          print(paste("trust was successful on restart", r, "for subject", i ))
          print(paste("estimated x1 and estimated x2 are: ", round(eml.results[[i]][[r]]$argument[1],3), "and", round(eml.results[[i]][[r]]$argument[2],3), "on restart", r, "for subject", i))
          print(paste("true x1 and true x2 are: ", round(data$x1[i],3), "and", round(data$x2[i],3), "on restart", r, "for subject", i))
        }

        1

      }, error = function(e){ # if error has been caught, print "caught error" message

        if (progress_bar == FALSE){
          message(paste("trust has failed on restart", r, "for subject", i, "trying different initial guess"))
        }

        0
      })


      if (iter == 1){
        if (out == 1){

          if (progress_bar == FALSE){
            print(paste("checking for positive definite hessian on restart", r,"for subject", i))
          }

          if ((det(eml.results[[i]][[r]]$hessian) > 0) && (diag(eml.results[[i]][[r]]$hessian)[1] > 0 ) && (diag(eml.results[[i]][[r]]$hessian)[2] > 0 )){

            # store results from optim per restart for a subject
            value_list.eml[[i]][[r]] <- eml.results[[i]][[r]]$value              # store objective value per restart for a subject
            hess_list.eml[[i]][[r]] <- solve(eml.results[[i]][[r]]$hessian)      # store hessian value per restart for a subject

            if (progress_bar == FALSE){
              print(eml.results[[i]][[r]]$hessian)

              print(paste("restart", r, "is a good restart with positive definite hessian"))
              print("---------------------------------------------------------------------------------------")
            }

            r = r+1

            init_x1 <-  rnorm(data$subjects,0,5)[i]
            init_x2 <-  rnorm(data$subjects,0,5)[i]



          } else{

            if (progress_bar == FALSE){
              message(paste("caught non-positive definite hessian on restart", r, "for subject", i, "trying different initial guess"))
            }

            init_x1 <- rnorm(data$subjects,0,5)[i]
            init_x2 <- rnorm(data$subjects,0,5)[i]

          }
        }
      }




      if (iter > 1){
        if (out == 1){

          if (progress_bar == FALSE){
            print(paste("checking for positive definite hessian on restart", r,"for subject", i))
          }


          if ((det(eml.results[[i]][[r]]$hessian) > 0) && (diag(eml.results[[i]][[r]]$hessian)[1] > 0 ) && (diag(eml.results[[i]][[r]]$hessian)[2] > 0 )){

            # store results from optim per restart for a subject
            value_list.eml[[i]][[r]] <- eml.results[[i]][[r]]$value               # store objective value per restart for a subject
            hess_list.eml[[i]][[r]] <- solve(eml.results[[i]][[r]]$hessian)       # store hessian value per restart for a subject

            if (progress_bar == FALSE){
              print(eml.results[[i]][[r]]$hessian)

              print(paste("restart", r, "is a good restart with positive definite hessian"))
              print("---------------------------------------------------------------------------------------")
            }

            r = r+1

            init_x1 <- eml_data[[2]][i] + rnorm(data$subjects,0,5)[i]
            init_x2 <- eml_data[[3]][i] + rnorm(data$subjects,0,5)[i]



          } else{

            if (progress_bar == FALSE){
              message(paste("caught non-positive definite hessian on restart", r, "for subject", i, "trying different initial guess"))
            }

            init_x1 <- eml_data[[2]][i] + rnorm(data$subjects,0,5)[i]
            init_x2 <- eml_data[[3]][i] + rnorm(data$subjects,0,5)[i]

          }
        }
      }



      if (iter == 1 && out == 0){

        init_x1 <- rnorm(data$subjects,0,5)[i]
        init_x2 <- rnorm(data$subjects,0,5)[i]

      }

      if (iter > 1 && out == 0){

        init_x1 <- eml_data[[2]][i] + rnorm(data$subjects,0,5)[i]
        init_x2 <- eml_data[[3]][i] + rnorm(data$subjects,0,5)[i]

      }

    }


    # Select the "best" MAP corresponding to the minimum objective value among restarts
    indx.eml <- which.min(value_list.eml[[i]])                                    # for a subject find index of restart that contains minimum objective value
    param_list.eml[[i]] <- eml.results[[i]][[indx.eml]]$argument                  # store "best" MAP estimate for each subject
    hess_list.eml.per.subj[[i]] <- solve(eml.results[[i]][[indx.eml]]$hessian)    # store hessian corresponding to "best" MAP for each subject


    # store output results
    eml.ll.per.subj[i] <- eml.results[[i]][[indx.eml]]$value                # for each subject, store objective values corresponding to "best" MAP estimates
    eml.est.x1.per.subj[i] <- param_list.eml[[i]][1]                        # for each subject, store estimated x1 parameter
    eml.est.x2.per.subj[i] <- param_list.eml[[i]][2]                        # for each subject, store estimated x2 parameter
    eml.laplace.x1.per.subj[i] <- diag(hess_list.eml.per.subj[[i]])[1]      # for each subject, store laplacian for x1 parameter
    eml.laplace.x2.per.subj[i] <- diag(hess_list.eml.per.subj[[i]])[2]      # for each subject, store laplacian for x2 parameter

    if (progress_bar == TRUE){
      Sys.sleep(0.1)
      setTxtProgressBar(pb, i)
    }

  } # End for loop for subjects

  if (progress_bar == TRUE){
    close(pb)
  }

  # output results from function
  list(sum(eml.ll.per.subj),                # sum log-likelihood
       eml.est.x1.per.subj,                 # x1 parameter estimates per subject
       eml.est.x2.per.subj,                # x2 parameter estimates per subject
       eml.laplace.x1.per.subj,             # x1 laplacian value per subject
       eml.laplace.x2.per.subj              # x2 laplacian value per subject
  )

  #} # End EML
}











