## Generalize this for appl package

# P_Aug Transition matrix
# O_Aug Observation matrix
# R Reward Matrix
# gamma discount factor
# b
# Num_S number of states (compute from P_Aug)
# Num_a number of actions (compute from R matrix)
# Num_z number of possible observations (compute from O_Aug)
# string names for actions? states?
# XX names for?
write_pomdp <- function(P_Aug,O_Aug,R,gamma,b,Num_S,Num_a,Num_z, string,XX, file = "input.pomdp"){

## FIXME adjust numerical precision of paste objects

  header <- paste0(
    '# Simple POMDP Model for test\n\n',
    paste0('discount: ', gamma),
    '\n',
    'values: reward\n',
    paste0('states: ', size(P_Aug,1)),
    '\n',
    'actions: ', paste0(string, collapse= " "),
    '\n',
    paste0('observations: ', size(O_Aug,2)),
    '\n',
    paste0( 'start: ', paste0(b, collapse= " ")),
    '\n\n')

  transition <- ""
  emission <- ""

  for(ii in 1:Num_a){
    c = XX[ii]
    transition <- paste0(transition, 'T: ', c, "\n")
    emission <- paste0(emission, 'O: ', c, '\n')

    for(i in 1:dim(P_Aug)[1]){
      transition <-paste0(transition, paste0(P_Aug[i,,ii], collapse= " "), "\n")
    }

    for(i in 1:dim(O_Aug)[1]){
      emission <- paste0(emission, paste0(O_Aug[i,,ii], collapse = " "), "\n")
    }

    transition <- paste0(transition, '\n')
    emission <- paste0(emission, '\n')

  }
  transition <- paste0(transition, '\n')
  emission <- paste0(emission, '\n')



  ##  Serialize Reward fn
  reward <- ""
  for(ii in 1:Num_a){
    for(i in 1:Num_S){
      c = XX[ii];
      reward <- paste0(reward, 'R: ', c, ' :', i-1, ': * : * ', R[i,ii], '\n')

    }
  }

  out <- paste0(header, transition, emission, reward)

  writeLines(out, file)


}
