


cITN <- ITNcov_t - ITNcov_t*IRScov_t - ITNcov_t*HOUcov_t + ITNcov_t*IRScov_t*HOUcov_t 
# Coverage with IRS only
cIRS <- IRScov_t - ITNcov_t*IRScov_t - IRScov_t*HOUcov_t + ITNcov_t*IRScov_t*HOUcov_t
# Coverage with house modification only
cHOU <- HOUcov_t - ITNcov_t*HOUcov_t - IRScov_t*HOUcov_t + ITNcov_t*IRScov_t*HOUcov_t
#LLINs and IRS
cITN_IRS <- ITNcov_t*IRScov_t - ITNcov_t*IRScov_t*HOUcov_t
#LLINs and HOU
cITN_HOU <- ITNcov_t*HOUcov_t - ITNcov_t*IRScov_t*HOUcov_t
#IRS and HOU
cITN_HOU <- IRScov_t*HOUcov_t - ITNcov_t*IRScov_t*HOUcov_t
#LLIN and IRS and HOU
cCom <- ITNcov_t*IRScov_t*HOUcov_t
#Neither of them
c0 <- 1 - ITNcov_t - IRScov_t - HOUcov_t + ITNcov_t*IRScov_t + ITNcov_t*HOUcov_t + IRScov_t*HOUcov_t- 2* ITNcov_t*IRScov_t*HOUcov_t 