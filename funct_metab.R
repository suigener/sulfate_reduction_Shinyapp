#metab <- function(J0, SO4_out, H2S, m, b, 
#                  K_As1, K_Ap1, K_Bs1, K_Bs2, K_Bp1, K_Bp2, K_Cs1, K_Cs2, 
#                  K_Cp1, K_Cp2, K_Cp3, K_Ds1, K_Ds2, K_Dp1, K_Dp2,
#                  V_A, V_B, V_C, V_D, 
#                  MK_red, MK_ox, AMP, ATP, APS, H_in_out, nCalc,
#                  G_A0, G_B0, G_C0, G_D0, R, Temp){

funct_metab <- function(J0, K_As1, K_Ap1, K_Bs1, K_Bs2,K_Bp1, K_Bp2, K_Cs1, K_Cs2, 
                        K_Cp1, K_Cp2, K_Cp3, K_Ds1, K_Ds2, K_Dp1, K_Dp2, V_A, V_B, V_C, V_D,
                        SO4_out, H2S, m, b, MK_red, MK_ox, AMP, ATP, H_in_out, nCalc,
                        G_A0, G_B0, G_C0, G_D0, R, Temp) {
  

# SO3; dependent on J************************************************************************
SO3 <- (J0/(V_D*(m*J0+b))*K_Ds1*K_Ds2^3*(1+H2S*MK_ox^3/(K_Dp1*K_Dp2^3))+
          exp(G_D0/(R*Temp))*H2S*MK_ox^3)/(MK_red^3*(1-J0/(V_D*(m*J0+b))))


# SO4_in; dependent on J*********************************************************************
SO4_in <- (SO4_out/K_As1-J0/(V_A*(m*J0+b))*(1+SO4_out/K_As1))/
  (J0/((V_A*(m*J0+b))*K_Ap1)+exp(G_A0/(R*Temp))/K_As1*(H_in_out^nCalc))


# APS; dependent on J, SO3*******************************************************************
APS <- (K_Cs1*K_Cs2*J0/(V_C*(m*J0+b))*(1+SO3*MK_ox*AMP/(K_Cp1*K_Cp2*K_Cp3))+
          exp(G_C0/(R*Temp))*SO3*AMP*MK_ox)/(MK_red*(1-J0/(V_C*(m*J0+b))))


# PPi; dependent on J, SO4_in, and APS*******************************************************
PPi <- (SO4_in*ATP*(1-J0/(V_B*(m*J0+b)))-K_Bs1*K_Bs2*J0/(V_B*(m*J0+b)))/
  (APS*K_Bs1*K_Bs2/(K_Bp1*K_Bp2)*J0/(V_B*(m*J0+b))+APS*exp(G_B0/(R*Temp)))

metab <- list(SO3, SO4_in, APS, PPi)
return(metab)
}