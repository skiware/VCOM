
########################################################################
#=======================================================================
# FeedingCycle.R

# Contains a function to compute the following parameters in presence of interventions
#        1) Prob to survive feeding period   
#        2) Prob to survive, resting and oviposition   
#        3) Prob to survive one day
#        4) Death rate
#        5) Egg laid per day per female mosquito    
# 
#======================================================================


impactFeedingCycleParameters = function(muV_1_Com,tau1,zCom,wCom,muV_2_Com,tau2,deltaCom,e_ov){
    #. impactFeedingCycleParameters: compute the impact of interventions on feeding cycle parameters

    # probability of surviving feeding period in the absence of an intervetion
    p10 <- exp(-muV_1_Com*tau1)
    
    # Probability of surviving the first attempt, second, and so on -eqn 3 (SK)
    p1Com <- p10*wCom/(1 - zCom*p10)
    
    #probabbility of resting (SK)
    p2 <- exp(-muV_2_Com*tau2)

    #probability of surviving one day 
    pCom <- (p1Com*p2)^deltaCom 
    # female mosquito mortality rate due to ITN and IRS (SK)
    muVCom <- -log(pCom)
    # betaCom: Eggs laid per day by female mosquitoes in presence of ITNs & IRS:
    betaCom <- e_ov*muVCom/(exp(muVCom/deltaCom) - 1)
    
    impactFeedingCycle <- c(muVCom,betaCom)
    
    return(impactFeedingCycle)

}