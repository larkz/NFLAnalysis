run2014 <- function(team_name, tr_his_bt){
  
  # Get multivariate Point Spread Data Set
  pt_spread <- c()
  yd_spread <- c()
  c_names <- c()
  for (t in ls(tr_his_bt)){
    c_names <- c(c_names, t)
    pt_spread <- cbind(pt_spread, tr_his_bt[[t]]$PtsFor - tr_his_bt[[t]]$PtsAgs) 
    yd_spread <- cbind(yd_spread, tr_his_bt[[t]]$YdsFor - tr_his_bt[[t]]$YdsAgs)
    
  }
  
  colnames(pt_spread) <- c_names
  colnames(yd_spread) <- c_names
  
  pt_spread <- as.data.frame(pt_spread)
  yd_spread <- as.data.frame(yd_spread)
  
  #Testing for 2014
  
  # Loop through 2014 - Chicago Bears
  
  fcast_pt_arr <- c()
  pt_arr <- c()
  date_arr <- c()
  win_arr <- data.frame()
  los_arr <- data.frame()
  hist_matchup_arr <- c()
  
  bet_arr_5050 <- c()
  bet_arr_lincomb <- c()
  bet_arr_hist <- c()

  st_ind <- getStartIndex(2014, tr_his_bt[[team_name]]$Year)
  
  
  execution <- function(pred_val, act_val){
    
    if (0 <= act_val && act_val <= pred_val){
      return(1)
    }else if (pred_val <= act_val && act_val <= 0){
      return(1)
    }else{
      return(0)
    }
  }
  
  
  
  
  #print(length(pt_spread[[team_name]]))
  #print(dim(pt_spread)[1])  
  
  for(day_count in st_ind:dim(pt_spread)[1]){ 
    
    cur_data <- tr_his_bt[[team_name]][day_count,]
    cur_obsv <- pt_spread[[team_name]][day_count]
    
    fcast_pt <- getForecast(team_name, pt_spread[1:day_count-1,], yd_spread[1:day_count-1,], day_count-1)
    
    
    fcast_pt_arr <- rbind( fcast_pt_arr, fcast_pt)
    pt_arr <- c(pt_arr, cur_obsv)
    date_arr <- c(date_arr, tr_his_bt[[team_name]]$fullDate[day_count])
    #print(cur_data)
    #print(day_count)
    if(is.na(cur_data$Week)){break}
    
    if(team_name == as.character(cur_data$Winner.tie)){
      opp_team = as.character(cur_data$Loser.tie)
    }else{
      opp_team = as.character(cur_data$Winner.tie)
    }
    
    hist_matchup <- get2Team( team_name, opp_team, tr_his_bt)
    hist_spread <- mean(hist_matchup$PtsFor-hist_matchup$PtsAgs)
    
    win_arr <- c(win_arr, as.character(cur_data$Winner.tie))
    los_arr <- c(los_arr, as.character(cur_data$Loser.tie))
    
    hist_matchup_arr <- c(hist_matchup_arr, hist_spread)
    
    bet_arr_5050 <- c(bet_arr_5050, execution(fcast_pt[2], cur_obsv))
    bet_arr_lincomb <- c(bet_arr_lincomb, execution(fcast_pt[3], cur_obsv))
    bet_arr_hist <- c(bet_arr_hist, execution(fcast_pt[1], cur_obsv))
    
  }
  
  result <- cbind(date_arr, win_arr, los_arr, pt_arr, fcast_pt_arr, hist_matchup_arr)
  colnames(result) <- c('Date', 'Winner', 'Loser', 'Actual.Spread', 'AR2_Forecast', '50.50', 'Lin_Comb', 'Hist.Match')
  
  
  result;
  bet_arr_5050;
  res5050 <- length(bet_arr_5050[bet_arr_5050==1])/length(bet_arr_5050)
  bet_arr_lincomb; 
  res_lincomb <- length(bet_arr_lincomb[bet_arr_lincomb==1])/length(bet_arr_lincomb)
  bet_arr_hist; 
  res_hist <- length(bet_arr_hist[bet_arr_hist==1])/length(bet_arr_hist)
  
  bet_result <- as.data.frame(cbind(res5050, res_lincomb, res_hist))
  # Forecast Point Spread using new method
  
  # Forecast Point Spread using historic mean
  colnames(bet_result) <- c('5050', 'lincomb', 'hist')
  # Compare Results
  
  # Create list 
  
  ret_list <- list(cbind(c(1:dim(result)[1]), result), bet_result)
  return(ret_list)
  
}
  
  
