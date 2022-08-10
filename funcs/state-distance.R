state_distance <- function(org.state, compare.state ){
  
  ## Inputs
  # org.state = state abbreviation of input org
  # compare.state = state abbreviation of comparison org
  
  ## Output 
  # 0 if in the same state 
  # 1/3 if in different states but in the same region
  # 2/3 if in difference states but in neighboring regions
  # 1 otherwise
  
  ## Notes
  # region data table is in global.R
  
  if(is.null(compare.state) | is.na(compare.state)){
    return(1)
  }
  
  
  #if they match return 0
  if(org.state == compare.state){
    return(0)
  }
  
  #if they dont match, find distance by region 
  org.region <- region$region[which(region$state == org.state)]
  compare.region <- region$region[which(region$state == compare.state)]
  
  #if they are in the same region, return 1/3
  if(org.region == compare.region){
    return(1/3)
  }
  
  #if they are not in the same region, but are in neighboring regions, return 2/3
  #region 1
  if(org.region == 1 && compare.region %in% c(3, 5)){
    return(2/3)
  }
  
  #region 3
  if(org.region == 3 && compare.region %in% c(1, 4, 6)){
    return(2/3)
    }
  
  #region 4
  if(org.region == 4 && compare.region %in% c(3, 7, 8)){
    return(2/3)
  }
  
  #region 5
  if(org.region == 5 && compare.region %in% c(1, 3, 6)){
    return(2/3)
  }
  
  #region 6
  if(org.region == 6 && compare.region %in% c(3, 5, 7)){
    return(2/3)
  }
  
  #region 7
  if(org.region == 7 && compare.region %in% c(4, 6, 8)){
    return(2/3)
  }
  
  #region 8
  if(org.region == 8 && compare.region %in% c(4, 7, 9)){
    return(2/3)
  }
  
  #region 9
  if(org.region == 9 && compare.region %in% c(8)){
    return(2/3)
  }
  
  #Else return 1
  return(1)

}