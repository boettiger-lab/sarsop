

source(system.file("examples/fisheries-ex.R", package = "appl"))
log <- "."
log_data <- data.frame(model = "ricker", r = 0.1, K = 20, C = NA, sigma_g = 0.1, sigma_m = 0.1)
alpha <- sarsop(transition, observation, reward, discount, precision = 5,
                log_dir = log, log_data = log_data)

meta <- meta_from_log(parameters = data.frame(model = "ricker", r = 0.1), log_dir = log)
alphas <- alphas_from_log(meta, log_dir = log)
fs <- f_from_log(meta)
models <- models_from_log(meta)

n_states <- meta$n_states
m <- models[[1]]

## tests for regularize_alpha
file <- paste0(meta[1,"id"],".policyx")

policy <- sapply(1:n_states, function(i) read_policy(m$observation[,i,1], file)[[2]])
policy <- policy+1 # C++ code index starts at 0

df <- compute_policy(alpha, transition, observation, reward)

identical(df$policy, policy)
#########################################

n_obs <- n_states

## Extract vectors from XML
xml <- xml2::read_xml(file)
vectors <- xml2::xml_find_all(xml, "//Vector")
get_vector <- function(v) as.numeric(strsplit(as.character(xml2::xml_contents(v)), " ")[[1]])
alpha <- lapply(vectors, get_vector)
alpha_action <- vapply(vectors, function(v) as.numeric(xml2::xml_attr(v, "action")), double(1))


sapply(1:n_obs, function(i){
  initial <- m$observation[,i,1]
  a <- vapply(alpha, function(x) initial %*% x, double(1))
  alpha_action[which.max(a)] + 1 # indexing of alpha_action starts at 0 in C++
})


sapply(1:n_obs, function(i){
  initial <- m$observation[,i,1]
  a <- vapply(alpha, function(x) initial %*% x, double(1))
  max(a)
  #alpha_action[which.max(a)] + 1 # indexing of alpha_action starts at 0 in C++
})



########

reg_alpha <- alphas[[1]]
belief <- m$observation[,,1]

V <- t(belief) %*% reg_alpha
value <- apply(V, 1, max)
policy <- apply(V, 1, function(x) which.max(x))



####


b <- m$observation[,n_states-1,1]
which.max(b %*% reg_alpha)

a <- vapply(alpha, function(x) b %*% x, double(1))
max(a)
alpha_action[which.max(a)] + 1


state_prior <- rep(1, n_states) / n_states
a_0 <- 1

belief <- vapply(1:n_obs,
                 function(i){
                   b <- state_prior %*% m$transition[, , a_0] * m$observation[, i, a_0]
                   if(sum(b) == 0) numeric(n_states) ## observed state i is impossible
                   else b / sum(b)
                 },
                 numeric(n_states))

alpha <- alphas[[1]]

V <- t(belief) %*% alpha
value <- apply(V, 1, max)
policy <- apply(V, 1, function(x) which.max(x))


