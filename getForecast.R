getForecast <- function(team_name, pt_spread, yd_spread, end){

  
  pt_data <-  pt_spread[[team_name]][1:end]
  yd_data <-  yd_spread[[team_name]][1:end]
  
  pt_yd_data <- as.data.frame(cbind(pt_data, yd_data))
  
  cor_yd_pt <- cor(pt_data, yd_data)
  
  pt_fit <- auto.arima(pt_data)
  yd_fit <- auto.arima(yd_data)
  
  # Spread forecast
  pt_forc <- pt_fit$residuals + pt_fit$x
  yd_forc <- yd_fit$residuals + yd_fit$x
  
  # Yard spread to point spread factor
  
  E_pt_spd <- mean(pt_fit$x)
  E_yd_spd <- mean(yd_fit$x)
  
  yd_pt_conv <- cor(pt_data, yd_data)*E_pt_spd/E_yd_spd 
  
  # Linear combination on point spread and yard spread
  
  beta <- 0.5*cor_yd_pt
  alpha <- 1 - beta
  
  lin_comb <- beta*yd_pt_conv*yd_forc + alpha*pt_forc
  
  lin_reg_data <- as.data.frame(cbind(pt_data, pt_forc, yd_pt_conv*yd_forc))
  lin_reg_nfl <- lm(pt_data ~  pt_forc + lin_reg_data[,3], data = lin_reg_data)
   
  #### New Method Forecasting
  
  data_pt <- pt_data
  pt_fcast <- predict(arima(data_pt, c(2,0,0)), h = 1)
  
  data_yd <- yd_data
  yd_fcast <- predict(arima(data_yd, c(2,0,0)), h = 1)
  
  beta <- 0.7*cor_yd_pt
  alpha <- 1 - beta
  
  lin_comb <- beta*yd_pt_conv*yd_fcast$pred + alpha*pt_fcast$pred
  
  new_fcast <- lin_reg_nfl$coef[1] + lin_reg_nfl$coef[2]*pt_fcast$pred + lin_reg_nfl$coef[3]*cor_yd_pt*yd_fcast$pred
  
  return(c(pt_fcast$pred*4, lin_comb*4, new_fcast*4))
}


