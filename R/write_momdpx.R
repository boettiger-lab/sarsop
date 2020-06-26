#' write_momdpx
#' @noRd
#' @inheritParams pomdp
#' @param P_full transition matrix for fully observable states
#' @param P_par transition matrix for partially observable states
#' @param O observation matrix
#' @param R reward
#' @param gamma discount factor
#' @param b_par initial belief of partially observable states
#' @param b_full initial fully observable state
#' @param file pomdpx file to create
#' @param digits precision to round to before normalizing. Leave at 4 since sarsop seems unable to do more?
#' @param digits2 precision to write solution to. Leave at 10, since normalizing requires additional precision
#' @param format floating point format, because sarsop parser doesn't seem to know scientific notation


write_momdpx <- function(P_full, P_par, O, R, gamma, b_full,
                         b_par = rep(1/dim(O)[1], dim(O)[1]),
                         file = "input.pomdpx",
                         digits = 4, digits2 = 10, format = "f"){


  Num_S <- dim(O)[1]
  Num_z <- dim(O)[2]
  Num_a <- dim(O)[3]
  Num_fs <- dim(O)[4]

  S_full = paste0("fs", 1:Num_fs)
  S_par <- paste0("a", 1:Num_S)
  XX <- paste0("a", 1:Num_a)


  # Creates the description, Discount, and Varibale section of POMDPX file
  header <- paste0(
    '<?xml version="1.0" encoding="ISO-8859-1"?>\n\n',
    '<pomdpx version ="1.0" id="sample" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:noNamespaceSchemaLocation="/Users/Milad/Documents/appl-0.96/doc/POMDPX/pomdpx.xsd">\n\n',
    '<Description>POMDP model</Description>\n\n',
    paste0('<Discount>', gamma, '</Discount>'),
    '\n\n',
    '<Variable>',
    '\n\n')

  header_s <- paste0(
    '<StateVar vnamePrev="fs_0" vnameCurr="fs_1" fullyObs="true">',
    '\n',
    paste0('<ValueEnum>', paste0(S_full, collapse= " "),'</ValueEnum>'),
    '\n',
    '</StateVar>',
    '\n',
    '<StateVar vnamePrev="ps_0" vnameCurr="ps_1" fullyObs="false">',
    '\n',
    paste0('<ValueEnum>', paste0(S_par, collapse= " "),'</ValueEnum>'),
    '\n',
    '</StateVar>',
    '\n\n')

  header_zar <- paste0(
    '<ObsVar vname="measurements">',
    '\n',
    paste0('<NumValues>', dim(O)[[2]],'</NumValues>'),
    '\n',
    '</ObsVar>',
    '\n\n',
    '<ActionVar vname="action_control">',
    '\n',
    paste0('<ValueEnum>', paste0(XX, collapse= " "),'</ValueEnum>'),
    '\n',
    '</ActionVar>',
    '\n\n',
    '<RewardVar vname="reward_agent" />',
    '\n',
    '</Variable>')

  # Creates the initial beleif state section of POMDPX file

  header_b <- paste0(
    '<InitialStateBelief>',
    '\n',
    '<CondProb>',
    '\n',
    '<Var>fs_0</Var>',
    '\n',
    '<Parent>null</Parent>',
    '\n',
    '<Parameter type = "TBL">',
    '\n',
    '<Entry>',
    '\n',
    '<Instance>-</Instance>',
    '\n',
    paste0('<ProbTable>', paste0(b_full, collapse= " "),'</ProbTable>'),
    '\n',
    '</Entry>',
    '\n',
    '</Parameter>',
    '\n',
    '</CondProb>',
    '\n\n',
    '<CondProb>',
    '\n',
    '<Var>ps_0</Var>',
    '\n',
    '<Parent>null</Parent>',
    '\n',
    '<Parameter type = "TBL">',
    '\n',
    '<Entry>',
    '\n',
    '<Instance>-</Instance>',
    '\n',
    paste0('<ProbTable>', paste0(b_par, collapse= " "),'</ProbTable>'),
    '\n',
    '</Entry>',
    '\n',
    '</Parameter>',
    '\n',
    '</CondProb>',
    '\n\n',
    '</InitialStateBelief>',
    '\n\n')

  # Creates the transition of fully observable states section of POMDPX file

  header_transition_full <- paste0(
    '<StateTransitionFunction>',
    '\n\n',
    '<CondProb>',
    '\n',
    '<Var>fs_1</Var>',
    '<Parent>action_control fs_0</Parent>',
    '<Parameter type="TBL">')

  transition_full <- ""

  for(ii in 1:Num_a){
    c = XX[ii]
    transition_full <- paste0(transition_full,
                              '<Entry>\n',
                              paste0('<Instance>',c,' - -</Instance>\n','<ProbTable>'))
    for(i in 1:dim(P_full)[1]){
      transition_full <-paste0(transition_full, paste0(P_full[i,,ii], collapse= " "), "\n")
    }
    transition_full <- paste0(transition_full, '</ProbTable>\n</Entry>\n')
  }
  transition_full <- paste0(transition_full,
                            '\n',
                            '</Parameter>\n',
                            '</CondProb>\n\n')

  # Creates the transition of partially observable states section of POMDPX file

  header_transition_par <- paste0(
    '<CondProb>',
    '\n',
    '<Var>ps_1</Var>',
    '<Parent>action_control fs_0 ps_0</Parent>',
    '<Parameter type="TBL">')

  transition_par <- ""

  for(ii in 1:Num_a){
    for(iii in 1:length(S_full)){
      c = XX[ii]
      cc = S_full[iii]
      transition_par <- paste0(transition_par,
                               '<Entry>\n',
                               paste0('<Instance>',c,' ',cc,' - -</Instance>\n','<ProbTable>'))
      for(i in 1:dim(P_par)[1]){
        transition_par <-paste0(transition_par, paste0(P_par[i,,ii,iii], collapse= " "), "\n")
      }
      transition_par <- paste0(transition_par, '</ProbTable>\n</Entry>\n')
    }
  }
  transition_par <- paste0(transition_par,
                           '\n',
                           '</Parameter>\n',
                           '</CondProb>\n',
                           '</StateTransitionFunction>\n\n')


  # Creates the emission section of POMDPX file

  header_emission <- paste0(
    '<ObsFunction>',
    '\n\n',
    '<CondProb>',
    '\n',
    '<Var>measurements</Var>',
    '<Parent>action_control fs_1 ps_1</Parent>',
    '<Parameter type="TBL">')

  emission <- ""

  for(ii in 1:Num_a){
    for(iii in 1:length(S_full)){
      c = XX[ii]
      cc = S_full[iii]
      emission <- paste0(emission,
                         '<Entry>\n',
                         paste0('<Instance>',c,' ',cc,' - -</Instance>\n','<ProbTable>'))
      for(i in 1:dim(P_par)[1]){
        emission <-paste0(emission, paste0(O[i,,ii,iii], collapse= " "), "\n")
      }
      emission <- paste0(emission, '</ProbTable>\n</Entry>\n')
    }
  }
  emission <- paste0(emission,
                     '\n',
                     '</Parameter>\n',
                     '</CondProb>\n',
                     '</ObsFunction>\n\n')

  # Creates the reward section of POMDPX file

  header_reward <- paste0(
    '<RewardFunction>',
    '\n',
    '<Func>',
    '\n',
    '<Var>reward_agent</Var>',
    '<Parent>action_control fs_0 ps_0</Parent>',
    '<Parameter type="TBL">')

  reward <- ""

  for(ii in 1:Num_a){
    for(iii in 1:length(S_full)){
      for(iiii in 1:length(S_par)){
        c = XX[ii]
        cc_f = S_full[iii]
        cc_p = S_par[iiii]
        reward <- paste0(reward,
                         '<Entry>\n',
                         paste0('<Instance>',c,' ',cc_f,' ',cc_p,'</Instance>\n'))
        reward <-paste0(reward, '<ValueTable>',paste0(R[iiii,ii,iii], collapse= " "), "</ValueTable>\n")
        reward <- paste0(reward, '</Entry>\n')
      }
    }
  }
  reward <- paste0(reward,
                   '\n',
                   '</Parameter>\n',
                   '</Func>\n',
                   '</RewardFunction>\n\n',
                   '</pomdpx>')


  out <- paste0(header,header_s,header_zar,header_b,header_transition_full,transition_full,
                header_transition_par,transition_par,header_emission,emission,header_reward,reward)


  writeLines(out, file)


}

