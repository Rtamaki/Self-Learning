library(tidyverse)


jokenpo_actions = list("scissor", "rock", "paper")

alternatove_jokenpo_tb = expand.grid(action = jokenpo_actions,
                         reaction = jokenpo_actions,
                         stringsAsFactors = FALSE) %>%
  mutate(benefit = case_when(action == "scissor" & reaction=="scissor" ~ 0.0,
                              action == "scissor" & reaction=="paper" ~ 1,
                              action == "scissor" & reaction=="rock" ~ -.5,
                              
                              action == "paper" & reaction=="scissor" ~ -1,
                              action == "paper" & reaction=="paper" ~ 0.00,
                              action == "paper" & reaction=="rock" ~ 1,
                              
                              action == "rock" & reaction=="scissor" ~ .5,
                              action == "rock" & reaction=="paper" ~-1,
                              action == "rock" & reaction=="rock" ~ 0.00)
         )



prob =  c(1, 0, 0 )
game_sim = function(par, table = alternatove_jokenpo_tb){
  par = par/sum(par)
  t = table %>%
      group_by(action) %>%
      summarise(benefit = min(benefit)) %>%
      ungroup %>%
      mutate(action_prob = case_when(action == "scissor" ~ par[1],
                                     action == "paper" ~ par[2],
                                     action == "rock" ~ par[3],
                                     TRUE ~ 0),
             expected_benefit = action_prob*benefit)
  return(sum(t$expected_benefit))
}

par = c(0, 0, 1)
probabilities = optim (par, game_sim, lower = c(0, 0, 0), upper = c(1, 1, 1), control = list (fnscale = - 1, maxit = 5E3)) [c ('par', 'value')]
probabilities$par
par = probabilities$par
prob = probabilities$par




x = alternatove_jokenpo_tb %>%
  mutate(action_prob = case_when(action == "scissor" ~ par[1],
                                 action == "paper" ~ par[2],
                                 action == "rock" ~ par[3],
                                 TRUE ~ 0),
         reaction_prob = case_when(reaction == "scissor" ~ prob[1],
                                   reaction == "paper" ~ prob[2],
                                   reaction == "rock" ~ prob[3],
                                   TRUE ~ 0),
         expected_benefit = action_prob*reaction_prob*benefit)
sum(x$expected_benefit) 










# # 'Random' action selection
# pick_action = function(l=prob_list){
#   numb = runif(1)
#   if(numb < prob_list[1]){
#     return("scissor")
#   }else{
#     if(numb < prob_list[1] + prob_list[2]){
#       return("paper")
#     }else{
#       return("rock")
#     }
#   }
# }
# 
# 
# # Normalize Function
# normalize_list = function(l=prob_list){
#   return(prob_list/sum(prob_list))
# }
# 
# simulate = function(picked_action, picked_reaction, table){
#   prob = table %>% filter(action == picked_action, reaction ==picked_reaction) %>% .$win_rate
#   return(as.integer(prob > runif(1)))
# }
# 

# size = length(prob_list)
# for (i in seq(100)){
#   picked_action = pick_action(prob_list)
#   counter_action = pick_action(prob_list)
#   is_victory = simulate(picked_action, counter_action, alternatove_jokenpo_tb)
#   
# }