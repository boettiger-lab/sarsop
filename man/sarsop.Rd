% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sarsop.R
\name{sarsop}
\alias{sarsop}
\title{sarsop}
\usage{
sarsop(
  transition,
  observation,
  reward,
  discount,
  state_prior = rep(1, dim(observation)[[1]])/dim(observation)[[1]],
  verbose = TRUE,
  log_dir = tempdir(),
  log_data = NULL,
  cache = TRUE,
  ...
)
}
\arguments{
\item{transition}{Transition matrix, dimension n_s x n_s x n_a}

\item{observation}{Observation matrix, dimension n_s x n_z x n_a}

\item{reward}{reward matrix, dimension n_s x n_a}

\item{discount}{the discount factor}

\item{state_prior}{initial belief state, optional, defaults to uniform
over states}

\item{verbose}{logical, should the function include a message with pomdp
diagnostics (timings, final precision, end condition)}

\item{log_dir}{pomdpx and policyx files will be saved here, along with
a metadata file}

\item{log_data}{a data.frame of additional columns to include in the log,
such as model parameters. A unique id value for each run can be provided
 as one of the columns, otherwise, a globally unique id will be generated.}

\item{cache}{should results from the log directory be cached? Default TRUE.
Identical functional calls will quickly return previously cached alpha
vectors from file rather than re-running.}

\item{...}{additional arguments to \code{\link{appl}}.}
}
\value{
a matrix of alpha vectors. Column index indicates action associated
 with the alpha vector, (1:n_actions), rows indicate system state, x.
 Actions for which no alpha vector was found are included as all -Inf,
 since such actions are not optimal regardless of belief, and thus have no
 corresponding alpha vectors in alpha_action list.
}
\description{
sarsop wraps the tasks of writing the pomdpx file defining the problem,
running the pomdsol (SARSOP) algorithm in C++, and then reading the
resulting policy file back into R.  The returned alpha vectors and
alpha_action information is then transformed into a more generic,
user-friendly representation as a matrix whose columns correspond
to actions and rows to states.  This function can thus be used at
the heart of most pomdp applications.
}
\examples{
\donttest{ ## Takes > 5s
## Use example code to generate matrices for pomdp problem:
source(system.file("examples/fisheries-ex.R", package = "sarsop"))
alpha <- sarsop(transition, observation, reward, discount, precision = 10)
compute_policy(alpha, transition, observation, reward)
}

}
