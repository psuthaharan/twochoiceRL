#' Simulate behavior in a two-choice decision task
#'
#' This function allows users to simulate behavior of \code{n_subj} individuals performing a two-choice task.
#' @param seed_value user-specified random value that stores data information for replication. Defaults to 528.
#' @param n_subj number of subjects. Defaults to 100.
#' @param n_tr number of trials. Defaults to 200.
#' @param x1_mean mean value of the re-parameterized variable for beta. Defaults to 1.
#' @param x1_sd standard deviation of the re-parameterized variable for beta. Defaults to 1.
#' @param x2_mean mean value of the re-parameterized variable for alpha. Defaults to 0.
#' @param x2_sd standard deviation of the re-parameterized variable for beta. Deafults to 1.
#' @param trials_unique keep only the unique trials where an individual made a choice. Defaults to TRUE.
#'
#' @return A list containing the two-choice dataset, the number of simulated subjects,
#'         the 'true' values for parameter 1 (x1) and the 'true' values for parameter 2 (x2). The
#'         dataset contains the trial number (Trial), the expected value (Value),
#'         the choice probability (Pr), the presented stimuli (Option), the chosen stimuli (Action),
#'         and the given reward (Reward).
#'
#'
#' @export
#'
#'
#' @import foreach
#'
#' @examples
#' # Simulate 50 subjects performing 100 trials in a two-choice decision task
#'   data <- simulate_twochoiceRL(n_subj = 50, n_tr = 100)
#'
#' # View behavioral data of simulated subject 10
#'   View(data$twochoiceRL[[10]])

simulate_twochoiceRL <- function(seed_value = 528,
                                 n_subj = 100,
                                 n_tr = 200,
                                 x1_mean = 1,
                                 x1_sd = 1,
                                 x2_mean = 0,
                                 x2_sd = 1,
                                 trials_unique = TRUE) { # start simulation

  # initialize list for storing data for all subjects
  twochoiceRL_data <- list()

  # set seed value for replication
  set.seed(seed_value)

  # randomly generate values
  x1  <- rnorm(n_subj,x1_mean,x1_sd)
  x2  <- rnorm(n_subj,x2_mean,x2_sd)

  # re-parameterize model parameters (see Suthaharan et.al., 2021)
  beta <- exp(x1) # choice randomness
  alpha <- 1/(1+exp(-x2)) # learning rate

  # simulate trial-by-trial data for all subjects
  for (i in 1:n_subj){ # start loop for subjects

    # initialize expected value to zero for both choices
    Q <- c(0, 0)

    # simulate choice behavior across trials
    simulate.trials <- foreach(t=1:n_tr, .combine = "rbind") %do% { # start loop for trials

      mu    <- c(1, -1)           # Mean payoff for choices 1 and 2
      sigma <- 2                  # Standard deviation of payoff for choices 1 and 2

      # Generate action probability using softmax
      action_prob <- softmax(beta[i]*Q)

      # Use action probability to sample participant action
      action <- sample(c(1,2), size = 1, prob = action_prob)

      # Generate reward based on action
      reward <- rnorm(1, mean = mu[action], sd = sigma)

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


  } # end loop for subjects

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
#' @examples
#' softmax()

softmax <- function (x) {

  y <- x - max(x)
  exp(y)/sum(exp(y))
}
