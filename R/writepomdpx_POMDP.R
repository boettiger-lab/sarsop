#' write_pomdpx
#'
#' write the pompdp xml file
#' @inheritParams pomdp
#' @param b belief vector
#' @export
writepomdpx_POMDP <- function(P,O,R,gamma,b){

  Num_S = dim(O)[1]
  Num_z = dim(O)[2]
  Num_a = dim(O)[3]

  XX = paste0("a", 1:Num_a)
  SS = paste0("a", 1:Num_S)
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

  # defining state variables
  header_s <- paste0(
    '<StateVar vnamePrev="ps_0" vnameCurr="ps_1" fullyObs="false">',
    '\n',
    paste0('<ValueEnum>', paste0(SS, collapse= " "),'</ValueEnum>'),
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
    '</Variable>\n\n')

  # Creates the initial beleif state section of POMDPX file
  header_b <- paste0(
    '<InitialStateBelief>',
    '\n',
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
    paste0('<ProbTable>', paste0(b, collapse= " "),'</ProbTable>'),
    '\n',
    '</Entry>',
    '\n',
    '</Parameter>',
    '\n',
    '</CondProb>',
    '\n\n',
    '</InitialStateBelief>',
    '\n\n')

  # Creates the transition section of POMDPX file
  header_transition_full <- paste0(
    '<StateTransitionFunction>',
    '\n\n',
    '<CondProb>',
    '\n',
    '<Var>ps_1</Var>\n',
    '<Parent>action_control ps_0</Parent>\n',
    '<Parameter type="TBL">\n')

  transition_full <- ""

  for(ii in 1:Num_a){
    c = XX[ii]
    transition_full <- paste0(transition_full,
                              '<Entry>\n',
                              paste0('<Instance>',c,' - -</Instance>\n','<ProbTable>'))
    for(i in 1:dim(P)[1]){
      transition_full <-paste0(transition_full, paste0(P[i,,ii], collapse= " "), "\n")
    }
    transition_full <- paste0(transition_full, '</ProbTable>\n</Entry>\n')
  }
  transition_full <- paste0(transition_full,
                            '\n',
                            '</Parameter>\n',
                            '</CondProb>\n',
                            '</StateTransitionFunction>\n\n')

  # Creates the emission section POMDPX file
  header_emission <- paste0(
    '<ObsFunction>',
    '\n\n',
    '<CondProb>',
    '\n',
    '<Var>measurements</Var>\n',
    '<Parent>action_control ps_1</Parent>\n',
    '<Parameter type="TBL">\n')

  emission <- ""

  for(ii in 1:Num_a){

    c = XX[ii]
    emission <- paste0(emission,
                       '<Entry>\n',
                       paste0('<Instance>',c,' - -</Instance>\n','<ProbTable>'))
    for(i in 1:dim(P)[1]){
      emission <-paste0(emission, paste0(O[i,,ii], collapse= " "), "\n")
    }
    emission <- paste0(emission, '</ProbTable>\n</Entry>\n')

  }
  emission <- paste0(emission,
                     '\n',
                     '</Parameter>\n',
                     '</CondProb>\n',
                     '</ObsFunction>\n\n')




  # Creates the reward section POMDPX file
  header_reward <- paste0(
    '<RewardFunction>',
    '\n',
    '<Func>',
    '\n',
    '<Var>reward_agent</Var>\n',
    '<Parent>action_control ps_0</Parent>\n',
    '<Parameter type="TBL">\n')

  reward <- ""

  for(ii in 1:Num_a){
    for(iii in 1:Num_S){

      c = XX[ii]
      cc_f = SS[iii]
      reward <- paste0(reward,
                       '<Entry>\n',
                       paste0('<Instance>',c,' ',cc_f,'</Instance>\n'))
      reward <-paste0(reward, '<ValueTable>',paste0(R[iii,ii], collapse= " "), "</ValueTable>\n")
      reward <- paste0(reward, '</Entry>\n')

    }
  }
  reward <- paste0(reward,
                   '\n',
                   '</Parameter>\n',
                   '</Func>\n',
                   '</RewardFunction>\n\n',
                   '</pomdpx>')


  out <- paste0(header,header_s,header_zar,header_b,header_transition_full,transition_full,
                header_emission,emission,header_reward,reward)

  filename = paste0("input.pomdpx")
  writeLines(out, filename)



}
