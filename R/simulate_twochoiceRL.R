#' Simulate behavior in a two-choice decision task
#'
#' This function allows users to simulate behavior of \code{n_subj} individuals
#' performing a two-choice task.
#' @param seed_value user-specified random value that ensures replication of results. Defaults to 528.
#' @param n_subj number of subjects. Defaults to 100.
#' @param n_tr number of trials. Defaults to 200.
#' @param mean_x mean of the distribution that we are sampling from for the re-parameterized parameters. Defaults to 1 and 0 for x1 and x2, respectively.
#' @param stdev_x standard deviation of the distribution that we are sampling from for the re-parameterized parameters. Defaults to 1 and 1 for x1 and x2, respectively.
#' @param mean_payoff mean payoff for both choices. Defaults to 1 and -1 for choice 1 and choice 2, respectively.
#' @param stdev_payoff standard deviation payoff for both choices. Defaults to 2 and 2 for choice 1 and choice 2, respectively.
#' @param trials_unique keep only the unique trials where an individual made a choice. Defaults to TRUE.
#' @param progress_bar track completion time of estimation. Defaults to TRUE.
#'
#' @return A list containing a list of the two-choice dataset for each subject, the number of simulated
#'   subjects, the 'true' values for re-parameterized parameter 1 (x1) and parameter 2 (x2) for each subject.
#'   The dataset contains the trial number (Trial), the expected value (Value), the choice probability (Pr),
#'   the presented stimulus (Option), the chosen stimulus (Action), and the given reward (Reward).
#'
#'
#' @export
#'
#'
#' @import foreach
#'
#' @examples
#' # Simulate 100 subjects performing 200 trials of a two-choice decision task
#'   data_sim <- simulate_twochoiceRL(n_subj = 100, n_tr = 200)
#'
#' # View behavioral data of subject 100
#'   View(data_sim$twochoiceRL[[100]])

simulate_twochoiceRL <- function(seed_value = 528,
                                 n_subj = 100,
                                 n_tr = 200,
                                 mean_x = c(1,0),
                                 stdev_x = c(1,1),
                                 mean_payoff = c(1,-1),
                                 stdev_payoff = c(2,2),
                                 trials_unique = TRUE,
                                 progress_bar = TRUE) { # start simulation

  # initialize list for storing data for all subjects
  twochoiceRL_data <- list()

  # set seed value for replication
  set.seed(seed_value)

  # randomly generate values
  x1  <- rnorm(n_subj,mean_x[1],stdev_x[1])
  x2  <- rnorm(n_subj,mean_x[2],stdev_x[2])

  # re-parameterize model parameters (see Suthaharan et.al., 2021)
  beta <- exp(x1) # choice randomness
  alpha <- 1/(1+exp(-x2)) # learning rate

  # create progress bar if user specified
  if (progress_bar == TRUE){
    pb <- txtProgressBar(min = 0, max = n_subj, style = 3)
  }

  # simulate trial-by-trial data for all subjects
  for (i in 1:n_subj){ # start loop for subjects

    # initialize expected value to zero for both choices
    Q <- c(0, 0)

    # simulate choice behavior across trials
    simulate.trials <- foreach(t=1:n_tr, .combine = "rbind") %do% { # start loop for trials

      mu    <- c(mean_payoff[1], mean_payoff[2])       # Mean payoff for choices 1 and 2
      sigma <- c(stdev_payoff[1], stdev_payoff[2])     # Standard deviation of payoff for choices 1 and 2

      # Generate action probability using softmax
      action_prob <- softmax(beta[i]*Q)

      # Use action probability to sample participant action
      action <- sample(c(1,2), size = 1, prob = action_prob)

      # Generate reward based on action
      reward <- rnorm(1, mean = mu[action], sd = sigma[action])

      # Q-learning
      Q[action] <- Q[action] + alpha[i] * (reward - Q[action])

      # Save data
      df <- data.frame(Trial   = rep(t, 2),        # trial number
                       Value   = Q,                # expected value
                       Pr      = action_prob,      # choice probability
                       Option  = paste(1:2),       # presented stimuli
                       Action  = rep(action, 2),   # chosen stimuli
                       Reward  = rep(reward, 2))   # reward received

      if (trials_unique == TRUE){
        # only keep the trials where option was selected
        df[which(df$Option == df$Action),]

      } else {
        # keep all trials for plotting purposes
        df

      }


    } # end loop for trials

    # store trial-by-trial data for each subject
    twochoiceRL_data[[i]] <- simulate.trials

    # load progress bar with loop index i
    if (progress_bar == TRUE){
      Sys.sleep(0.1)
      setTxtProgressBar(pb, i)
    }

  } # End for loop for subjects

  # close progress bar
  if (progress_bar == TRUE){
    close(pb)
  }

  # return list containing the data, number of subjects, true x1 values and true x2 values
  MyList <- list("twochoiceRL" = twochoiceRL_data, "subjects" = n_subj, "x1" = x1, "x2" = x2)
  return(MyList)

} # end simulation




#' Softmax decision model
#'
#' This function converts expected value to choice probability.
#' @param x expected value
#'
#' @return choice probability value
#'
#' @keywords internal
#'


softmax <- function (x) {

  y <- x - max(x)
  exp(y)/sum(exp(y))
}
