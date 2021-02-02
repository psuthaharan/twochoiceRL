#' Estimate behavior in a two-choice decision task
#'
#' This function performs parameter estimation to recover behavior in a two-choice decision task
#' @param data simulated two-choice task data
#' @param method parameter estimation technique used; either maximum-likelihood estimation (mle),
#'               maximum a posteriori (map) or expectation-maximization with laplace approximation (eml).
#'               Defaults to mle.
#' @param plot visualize estimation performance between true parameters vs estimated parameters.
#'             Defaults to TRUE.
#'
#'
#' @return A plot illustrating the correlation between the true parameters and estimated parameters, and a list
#'         containing a dataframe of the true and estimated parameter values, the correlation value of
#'         parameter 1, and the correlation value of parameter 2.
#'
#'
#' @export
#'
#' @import Metrics ggthemes grid
#' @examples
#'
#' # Save simulated task data to a variable, say, data
#' data <- simulate_twochoiceRL(trials.unique = TRUE)
#'
#' # Recover behavioral parameters using maximum-likelihood estimation (MLE)
#' estimate_twochoiceRL(data = data, method = "mle", plot=TRUE)

estimate_twochoiceRL <- function(data = NULL,
                                method = "mle",
                                plot = TRUE) { # start estimation


  # if specified estimation method is MLE, then run MLE
  if (method == "mle") {

    # randomly generate initial guesses for parameters
    init.x1 <- rnorm(data$subjects,0,5)
    init.x2 <- rnorm(data$subjects,0,5)


    # perform MLE
    mle_data <- mle_twochoiceRL(data = data,                      # simulated task data
                               param = list(init.x1,init.x2),    # initial guesses for parameters
                               fn = "mle.objectiveFunction",     # likelihood function being minimized
                               opt = "TRM",                      # trust-region optimization method
                               nRes = 5)                         # random restarts


    # save mle results
    x1.hat <- mle_data[[2]] # estimates of x1
    x2.hat <- mle_data[[3]] # estimates of x2

    # store true and estimated parameters
    df <- data.frame(true.x1 = data$x1,
                     true.x2 = data$x2,
                     est.x1  = x1.hat,
                     est.x2  = x2.hat)

    # mle function output
    results <- list(value = df,
                    bias.x1 = bias(df$true.x1,df$est.x1),
                    bias.x2 = bias(df$true.x2,df$est.x2),
                    rmse.x1 = rmse(df$true.x1,df$est.x1),
                    rmse.x2 = rmse(df$true.x2,df$est.x2),
                    corr.x1 = cor(df$true.x1,df$est.x1),
                    corr.x2 = cor(df$true.x2,df$est.x2))

    # if user doesn't want to see the plot, then only return results
    if (plot == FALSE){
      return(results)
    }
    else {

      # generate plot 1
      p1 <- ggplot(df,
                   aes(x=true.x1,y=est.x1)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        ylim(-3,10) +
         labs(x = expression("x"[1]),
              y = expression(hat(x)[1])) +
        theme_pubr() +
        annotate("label",
                 label = paste("bias =", round(bias(data$x1,mle_data[[2]]),3), "\n",
                               "rmse =", round(rmse(data$x1,mle_data[[2]]),3),"\n",
                               "r =", round(cor(data$x1, mle_data[[2]], method = "pearson"),3)),
                 x=0,
                 y=6.5,
                 size=4) +

      # add reference line
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # generate plot 2
      p2 <- ggplot(df,
                   aes(x=true.x2,y=est.x2)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        ylim(-5,5) +
        labs(x = expression("x"[2]),
             y = expression(hat(x)[2])) +
        theme_pubr() +
        annotate("label",
                 label = paste("bias =", round(bias(data$x2, mle_data[[3]]),3),"\n",
                               "rmse =", round(rmse(data$x2, mle_data[[3]]),3),"\n",
                               "r =", round(cor(data$x2, mle_data[[3]], method = "pearson"),3)),
                 x=-1,
                 y=3.6,
                 size=4) +

      # add reference line
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # combine plots
      plot <- ggarrange(p1,p2, ncol = 2, nrow = 1,common.legend = TRUE,legend = 'bottom')


      # display the plot AND the estimation performance results
      print(plot)
      return(results)
    }

  } else if (method == "map"){ # if specified estimation method is MAP, then run MAP

    # randomly generate initial guesses for parameters
    init.x1 <- rnorm(data$subjects,0,5)
    init.x2 <- rnorm(data$subjects,0,5)

    # initialize priors for parameters
    m1 = 0
    s1 = 5

    m2 = 0
    s2 = 5

    map_data <- map_twochoiceRL(data = data,
                               param = list(init.x1,init.x2),
                               prior.mean = c(m1,m2),
                               prior.sd = c(s1,s2),
                               fn = "map.objectiveFunction",
                               opt = "TRM",
                               nRes = 5)


    # save map results
    x1.hat <- map_data[[2]] # estimates of x1
    x2.hat <- map_data[[3]] # estimates of x2

    # store true and estimated parameters
    df <- data.frame(true.x1 = data$x1,
                     true.x2 = data$x2,
                     est.x1  = x1.hat,
                     est.x2  = x2.hat)

    # map function output
    results <- list(value = df,
                    bias.x1 = bias(df$true.x1,df$est.x1),
                    bias.x2 = bias(df$true.x2,df$est.x2),
                    rmse.x1 = rmse(df$true.x1,df$est.x1),
                    rmse.x2 = rmse(df$true.x2,df$est.x2),
                    corr.x1 = cor(df$true.x1,df$est.x1),
                    corr.x2 = cor(df$true.x2,df$est.x2)
    )


    if (plot == FALSE){
      return(results)
    }
    else {

      # generate plot 1
      p1 <- ggplot(df,
                   aes(x=true.x1,y=est.x1)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        ylim(-3,10) +
        labs(x = expression("x"[1]),
             y = expression(hat(x)[1])) +
        theme_pubr() +
        annotate("label",
                 label = paste("bias =", round(bias(data$x1,map_data[[2]]),3), "\n",
                               "rmse =", round(rmse(data$x1,map_data[[2]]),3),"\n",
                               "r =", round(cor(data$x1, map_data[[2]], method = "pearson"),3)),
                 x=0,
                 y=6.5,
                 size=4) +

        # add reference line
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # generate plot 2
      p2 <- ggplot(df,
                   aes(x=true.x2,y=est.x2)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        ylim(-5,5) +
        labs(x = expression("x"[2]),
             y = expression(hat(x)[2])) +
        theme_pubr() +
        annotate("label",
                 label = paste("bias =", round(bias(data$x2, map_data[[3]]),3),"\n",
                               "rmse =", round(rmse(data$x2, map_data[[3]]),3),"\n",
                               "r =", round(cor(data$x2, map_data[[3]], method = "pearson"),3)),
                 x=-1,
                 y=3.6,
                 size=4) +

      # add reference line
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # combine plots
      plot <- ggarrange(p1,p2, ncol = 2, nrow = 1,common.legend = TRUE,legend = 'bottom')


      # return output
      print(plot)
      return(results)
    }


  } else if (method == "eml") {





    d <-numeric()   # store difference values
    diff = 1        # initialize difference for convergence
    prev = Inf      # initialize previous objective value
    iter = 1        # initial iteration of E-M step



    # Perform E-M step with Laplace Approximation
    while(diff > 0.001){ # start E-M procedure

      # generate initial guesses for iterations
      if (iter == 1){ # for the first iteration, generate initial guesses randomly
        init.x1 <- rnorm(data$subjects,0,5)
        init.x2 <- rnorm(data$subjects,0,5)

        # initialize priors for parameters
        m1 = 0
        s1 = 5

        m2 = 0
        s2 = 5
      }else{ # for successive iterations, generate initial guesses based on previous MAP estimate
        init.x1 <- eml_data[[2]]
        init.x2 <- eml_data[[3]]
      }

    # Run E-step
    eml_data <- eml_twochoiceRL(data = data,
                               param = list(init.x1,init.x2),
                               prior.mean = c(m1,m2),
                               prior.sd = c(s1,s2),
                               fn = "eml.objectiveFunction",
                               opt = "TRM",
                               nRes = 5,
                               iter = iter,
                               eml_data = eml_data)

    # Run M-step with Laplace approximation: update prior mean and standard deviation
    m1 <- mean(eml_data[[2]])
    s1 <- sqrt(sum((eml_data[[2]]^(2))+(eml_data[[4]])-
                     (2*eml_data[[2]]*m1)+(rep(m1^(2),data$subjects)))/(data$subjects-1))

    m2 <- mean(eml_data[[3]])
    s2 <- sqrt(sum((eml_data[[3]]^(2))+(eml_data[[5]])-
                     (2*eml_data[[3]]*m2)+(rep(m2^(2),data$subjects)))/(data$subjects-1))

    # calculate difference of objective values for convergence
    diff <- abs(eml_data[[1]] - prev)
    d[iter] <- diff
    prev <- eml_data[[1]]


    # print output in R console
    print(paste("iter", iter, ":",
                "LL =", round(eml_data[[1]],3),",",
                "diff =", round(diff,3),",",
                "(m1,m2,s1,s2) =", "(",m1,",",m2,",",s1,",",s2,")"))
    print("---------------------------------------------------------------------------")


    iter = iter+1

  } # end E-M procedure


    # save eml results
    x1.hat <- eml_data[[2]] # estimates of x1
    x2.hat <- eml_data[[3]] # estimates of x2

    # store true and estimated parameters
    df <- data.frame(true.x1 = data$x1,
                     true.x2 = data$x2,
                     est.x1  = x1.hat,
                     est.x2  = x2.hat)

    # eml function output
    results <- list(value = df,
                    bias.x1 = bias(df$true.x1,df$est.x1),
                    bias.x2 = bias(df$true.x2,df$est.x2),
                    rmse.x1 = rmse(df$true.x1,df$est.x1),
                    rmse.x2 = rmse(df$true.x2,df$est.x2),
                    corr.x1 = cor(df$true.x1,df$est.x1),
                    corr.x2 = cor(df$true.x2,df$est.x2)
    )


    if (plot == FALSE){
      return(results)
    }
    else {

      # generate plot 1
      p1 <- ggplot(df,
                   aes(x=true.x1,y=est.x1)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        ylim(-3,10) +
        labs(x = expression("x"[1]),
             y = expression(hat(x)[1])) +
        theme_pubr() +
        annotate("label",
                 label = paste("bias =", round(bias(data$x1,eml_data[[2]]),3), "\n",
                               "rmse =", round(rmse(data$x1,eml_data[[2]]),3),"\n",
                               "r =", round(cor(data$x1, eml_data[[2]], method = "pearson"),3)),
                 x=0,
                 y=6.5,
                 size=4) +

        # add reference line
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # generate plot 2
      p2 <- ggplot(df,
                   aes(x=true.x2,y=est.x2)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        ylim(-5,5) +
        labs(x = expression("x"[2]),
             y = expression(hat(x)[2])) +
        theme_pubr() +
        annotate("label",
                 label = paste("bias =", round(bias(data$x2, eml_data[[3]]),3),"\n",
                               "rmse =", round(rmse(data$x2, eml_data[[3]]),3),"\n",
                               "r =", round(cor(data$x2, eml_data[[3]], method = "pearson"),3)),
                 x=-1,
                 y=3.6,
                 size=4) +

        # add reference line
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red")

      # combine plots
      plot <- ggarrange(p1,p2, ncol = 2, nrow = 1,common.legend = TRUE,legend = 'bottom')


      # return output
      print(plot)
      return(results)
    } # end else statement for plotting EML results


  } # end else if statement for EML


} # end estimation









#' Maximum-likelihood estimation (MLE) of behavior in a two-choice decision task
#'
#' This function runs the MLE technique to recover behavior in a two-choice decision task.
#' @param data simulated two-choice task data
#' @param param randomly generated initial parameters.
#' @param fn objective function being minimized
#' @param opt optimization algorithm used. Defaults to trust-region method (trm)
#' @param nRes number of restarts. Defaults to 5.
#'
#'
#' @return A list containing the sum log-likelihood, mle estimates of parameter 1 per subject,
#'         mle estimates of parameter 2 per subject, laplace values of parameter 1 per subject,
#'         laplace values of parameter 2 per subject.
#'
#' @keywords internal
#'
#' @import trust
#' @examples
#' mle_twochoiceRL()








##########################################################################

mle_twochoiceRL <- function(data = NULL,
                           param = list(init.x1,init.x2),
                           fn = "mle.objectiveFunction",
                           opt = "TRM",
                           nRes = 5){



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



    print(paste("working on subject", i, "..."))

    # initialize list within a list for the trust results, log-likelihood value and hessian to store results at every restart
    mle.results[[i]] <- list()
    value_list.mle[[i]] <- list()
    hess_list.mle[[i]] <- list()

    # set random initial guesses for x1 and x2
    init.x1 <- param[[1]][i]
    init.x2 <- param[[2]][i]

    r = 1 # restart = 1


    while(r < nRes+1){ # start while loop for restarts

      print(paste("initial guess for x1 and x2 is: ", round(init.x1,3), "and", round(init.x2,3), "on restart", r, "for subject", i))


      out <- tryCatch({ # start catch statement

        if (opt == "TRM"){
          mle.results[[i]][[r]] <- trust(mle.objfun, c(init.x1,init.x2),1,5)
        }

        print(paste("trust was successful on restart", r, "for subject", i ))
        print(paste("estimated x1 and estimated x2 are: ", round(mle.results[[i]][[r]]$argument[1],3), "and", round(mle.results[[i]][[r]]$argument[2],3), "on restart", r, "for subject", i))
        print(paste("true x1 and true x2 are: ", round(data$x1[i],3), "and", round(data$x2[i],3), "on restart", r, "for subject", i))

        1

      }, error = function(e){ # if error has been caught, print "caught error" message
        message(paste("trust has failed on restart", r, "for subject", i, "trying different initial guess"))
        0
      })


      #install.packages("numDeriv")
      #      library(numDeriv)
      if (out == 1){

        print(paste("checking for positive definite hessian on restart", r,"for subject", i))


        if ((det(mle.results[[i]][[r]]$hessian) > 0) && (diag(mle.results[[i]][[r]]$hessian) > 0 )){

          # store results from trust per restart for a subject
          value_list.mle[[i]][[r]] <- mle.results[[i]][[r]]$value              # store objective value per restart for a subject
          hess_list.mle[[i]][[r]] <- solve(mle.results[[i]][[r]]$hessian)      # store hessian value per restart for a subject

          print(mle.results[[i]][[r]]$hessian)

          print(paste("restart", r, "is a good restart with positive definite hessian"))
          print("---------------------------------------------------------------------------------------")

          r = r+1


          init.x1 <-  rnorm(data$subjects,0,5)[i]
          init.x2 <-  rnorm(data$subjects,0,5)[i]



        } else{
          message(paste("caught non-positive definite hessian on restart", r, "for subject", i, "trying different initial guess"))

          init.x1 <- rnorm(data$subjects,0,5)[i]
          init.x2 <- rnorm(data$subjects,0,5)[i]

        }
      }



      if (out == 0){

        init.x1 <- rnorm(data$subjects,0,5)[i]
        init.x2 <- rnorm(data$subjects,0,5)[i]

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

  } # End for loop for subjects

  # output results from function
  list(sum(mle.ll.per.subj),                # sum log-likelihood
       mle.est.x1.per.subj,                 # x1 parameter estimates per subject
       mle.est.x2.per.subj,                # x2 parameter estimates per subject
       mle.laplace.x1.per.subj,             # x1 laplacian value per subject
       mle.laplace.x2.per.subj              # x2 laplacian value per subject
  )

  #} # End MLE
}








#' Maximum A Posteriori (MAP) of behavior in a two-choice decision task
#'
#' This function runs the MAP technique to recover behavior in a two-choice decision task.
#' @param data simulated two-choice task data
#' @param param randomly generated initial parameters.
#' @param prior.mean mean priors for x1 and x2, respectively. Defaults to mean 0 for both parameters.
#' @param prior.sd standard deviation priors for x1 and x2, respectively. Defaults to 5 for both parameters.
#' @param fn objective function being minimized
#' @param opt optimization algorithm used. Defaults to trust-region method (trm)
#' @param nRes number of restarts. Defaults to 5.
#'
#'
#' @return A list containing the sum log-likelihood, map estimates of parameter 1 per subject,
#'         map estimates of parameter 2 per subject, laplace values of parameter 1 per subject,
#'         laplace values of parameter 2 per subject.
#'
#' @keywords internal
#'
#' @import trust
#' @examples
#' map_twochoiceRL()








##########################################################################

map_twochoiceRL <- function(data = NULL,
                           param = list(init.x1,init.x2),
                           prior.mean = c(0,0),
                           prior.sd = c(5,5),
                           fn = "map.objectiveFunction",
                           opt = "TRM",
                           nRes = 5){



  # initialize prior mean and prior standard deviation for x1 and x2
  m1 <- prior.mean[1]
  s1 <- prior.sd[1]
  m2 <- prior.mean[2]
  s2 <- prior.sd[2]


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


    print(paste("working on subject", i, "..."))

    # initialize list within a list for the trust results, log-likelihood value and hessian to store results at every restart
    map.results[[i]] <- list()
    value_list.map[[i]] <- list()
    hess_list.map[[i]] <- list()

    # set random initial guesses for x1 and x2
    init.x1 <- param[[1]][i]
    init.x2 <- param[[2]][i]

    r = 1 # restart = 1


    while(r < nRes+1){ # start while loop for restarts

      print(paste("initial guess for x1 and x2 is: ", round(init.x1,3), "and", round(init.x2,3), "on restart", r, "for subject", i))


      out <- tryCatch({ # start catch statement

        if (opt == "TRM"){
          map.results[[i]][[r]] <- trust(map.objfun, c(init.x1,init.x2),1,5)
        }

        print(paste("trust was successful on restart", r, "for subject", i ))
        print(paste("estimated x1 and estimated x2 are: ", round(map.results[[i]][[r]]$argument[1],3), "and", round(map.results[[i]][[r]]$argument[2],3), "on restart", r, "for subject", i))
        print(paste("true x1 and true x2 are: ", round(data$x1[i],3), "and", round(data$x2[i],3), "on restart", r, "for subject", i))

        1

      }, error = function(e){ # if error has been caught, print "caught error" message
        message(paste("trust has failed on restart", r, "for subject", i, "trying different initial guess"))
        0
      })


      #install.packages("numDeriv")
      #      library(numDeriv)
      if (out == 1){

        print(paste("checking for positive definite hessian on restart", r,"for subject", i))


        if ((det(map.results[[i]][[r]]$hessian) > 0) && (diag(map.results[[i]][[r]]$hessian) > 0 )){

          # store results from trust per restart for a subject
          value_list.map[[i]][[r]] <- map.results[[i]][[r]]$value              # store objective value per restart for a subject
          hess_list.map[[i]][[r]] <- solve(map.results[[i]][[r]]$hessian)      # store hessian value per restart for a subject

          print(map.results[[i]][[r]]$hessian)

          print(paste("restart", r, "is a good restart with positive definite hessian"))
          print("---------------------------------------------------------------------------------------")

          r = r+1


          init.x1 <-  rnorm(data$subjects,0,5)[i]
          init.x2 <-  rnorm(data$subjects,0,5)[i]



        } else{
          message(paste("caught non-positive definite hessian on restart", r, "for subject", i, "trying different initial guess"))

          init.x1 <- rnorm(data$subjects,0,5)[i]
          init.x2 <- rnorm(data$subjects,0,5)[i]

        }
      }



      if (out == 0){

        init.x1 <- rnorm(data$subjects,0,5)[i]
        init.x2 <- rnorm(data$subjects,0,5)[i]

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

  } # End for loop for subjects

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
#' @param prior.mean mean priors for x1 and x2, respectively. Defaults to mean 0 for both parameters.
#' @param prior.sd standard deviation priors for x1 and x2, respectively. Defaults to 5 for both parameters.
#' @param fn objective function being minimized
#' @param opt optimization algorithm used. Defaults to trust-region method (trm)
#' @param nRes number of restarts. Defaults to 5.
#' @param iter iteration of algorithm
#' @param eml_data data to be passed for succeeding iterations
#'
#'
#' @return A list containing the sum log-likelihood, eml estimates of parameter 1 per subject,
#'         eml estimates of parameter 2 per subject, laplace values of parameter 1 per subject,
#'         laplace values of parameter 2 per subject.
#'
#' @keywords internal
#'
#' @import trust
#' @examples
#' eml_twochoiceRL()








##########################################################################

eml_twochoiceRL <- function(data = NULL,
                           param = list(init.x1,init.x2),
                           prior.mean = c(0,0),
                           prior.sd = c(5,5),
                           fn = "eml.objectiveFunction",
                           opt = "TRM",
                           nRes = 5,
                           iter = iter,
                           eml_data = eml_data){






  # initialize prior mean and prior standard deviation for x1 and x2
  m1 <- prior.mean[1]
  s1 <- prior.sd[1]
  m2 <- prior.mean[2]
  s2 <- prior.sd[2]


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


    print(paste("working on subject", i, "..."))

    # initialize list within a list for the trust results, log-likelihood value and hessian to store results at every restart
    eml.results[[i]] <- list()
    value_list.eml[[i]] <- list()
    hess_list.eml[[i]] <- list()

    # set random initial guesses for x1 and x2
    init.x1 <- param[[1]][i]
    init.x2 <- param[[2]][i]

    r = 1 # restart = 1

    # for kth iteration > 1 perform only 1 restart for each subject
    if (iter > 1){
      nRes = 5
    }

    while(r < nRes+1){ # start while loop for restarts

      print(paste("initial guess for x1 and x2 is: ", round(init.x1,3), "and", round(init.x2,3), "on restart", r, "for subject", i))


      out <- tryCatch({ # start catch statement

        if (opt == "TRM"){
          eml.results[[i]][[r]] <- trust(eml.objfun, c(init.x1,init.x2),1,5)
        }

        print(paste("trust was successful on restart", r, "for subject", i ))
        print(paste("estimated x1 and estimated x2 are: ", round(eml.results[[i]][[r]]$argument[1],3), "and", round(eml.results[[i]][[r]]$argument[2],3), "on restart", r, "for subject", i))
        print(paste("true x1 and true x2 are: ", round(data$x1[i],3), "and", round(data$x2[i],3), "on restart", r, "for subject", i))

        1

      }, error = function(e){ # if error has been caught, print "caught error" message
        message(paste("trust has failed on restart", r, "for subject", i, "trying different initial guess"))
        0
      })


      if (iter == 1){
        if (out == 1){
          #  out1 <- tryCatch({

          print(paste("checking for positive definite hessian on restart", r,"for subject", i))


          if ((det(eml.results[[i]][[r]]$hessian) > 0) && (diag(eml.results[[i]][[r]]$hessian) > 0 )){

            # store results from optim per restart for a subject
            value_list.eml[[i]][[r]] <- eml.results[[i]][[r]]$value              # store objective value per restart for a subject
            hess_list.eml[[i]][[r]] <- solve(eml.results[[i]][[r]]$hessian)      # store hessian value per restart for a subject

            print(eml.results[[i]][[r]]$hessian)

            print(paste("restart", r, "is a good restart with positive definite hessian"))
            print("---------------------------------------------------------------------------------------")

            r = r+1

            init.x1 <-  rnorm(data$subjects,0,5)[i]
            init.x2 <-  rnorm(data$subjects,0,5)[i]



          } else{
            message(paste("caught non-positive definite hessian on restart", r, "for subject", i, "trying different initial guess"))

            init.x1 <- rnorm(data$subjects,0,5)[i]
            init.x2 <- rnorm(data$subjects,0,5)[i]

          }
        }
      }




      if (iter > 1){
        if (out == 1){

          print(paste("checking for positive definite hessian on restart", r,"for subject", i))


          if ((det(eml.results[[i]][[r]]$hessian) > 0) && (diag(eml.results[[i]][[r]]$hessian) > 0 )){

            # store results from optim per restart for a subject
            value_list.eml[[i]][[r]] <- eml.results[[i]][[r]]$value               # store objective value per restart for a subject
            hess_list.eml[[i]][[r]] <- solve(eml.results[[i]][[r]]$hessian)       # store hessian value per restart for a subject

            print(eml.results[[i]][[r]]$hessian)

            print(paste("restart", r, "is a good restart with positive definite hessian"))
            print("---------------------------------------------------------------------------------------")

            r = r+1

            init.x1 <- eml_data[[2]][i] + rnorm(data$subjects,0,5)[i]
            init.x2 <- eml_data[[3]][i] + rnorm(data$subjects,0,5)[i]



          } else{
            message(paste("caught non-positive definite hessian on restart", r, "for subject", i, "trying different initial guess"))

            init.x1 <- eml_data[[2]][i] + rnorm(data$subjects,0,5)[i]
            init.x2 <- eml_data[[3]][i] + rnorm(data$subjects,0,5)[i]

          }
        }
      }



      if (iter == 1 && out == 0){

        init.x1 <- rnorm(data$subjects,0,5)[i]
        init.x2 <- rnorm(data$subjects,0,5)[i]

      }

      if (iter > 1 && out == 0){

        init.x1 <- eml_data[[2]][i] + rnorm(data$subjects,0,5)[i]
        init.x2 <- eml_data[[3]][i] + rnorm(data$subjects,0,5)[i]

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

  } # End for loop for subjects

  # output results from function
  list(sum(eml.ll.per.subj),                # sum log-likelihood
       eml.est.x1.per.subj,                 # x1 parameter estimates per subject
       eml.est.x2.per.subj,                # x2 parameter estimates per subject
       eml.laplace.x1.per.subj,             # x1 laplacian value per subject
       eml.laplace.x2.per.subj              # x2 laplacian value per subject
  )

  #} # End EML
}











