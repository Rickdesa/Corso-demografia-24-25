lexis_polygon_dfcohort<-function(df){
  group<-numeric(0)
  x<-numeric(0)
  class(x) <- "Date"
  y<-numeric(0)
  alphas<-numeric(0)
  j<-1
  for(i in 1:nrow(df)){group<-c(group,c(rep(c(j,(j+1)),each=3)))
  j<-j+2
  x<-c(x,c(as.Date(ISOdate(df$year[i], 1, 1)),as.Date(ISOdate(df$year[i]+1, 1, 1)),as.Date(ISOdate(df$year[i]+1, 1, 1)),as.Date(ISOdate(df$year[i]+1, 1, 1)),as.Date(ISOdate(df$year[i]+1, 1, 1)),as.Date(ISOdate(df$year[i]+2, 1, 1))))
  y<-c(y,c(df$age[i],df$age[i],df$age[i]+1,df$age[i],df$age[i]+1,df$age[i]+1))
  alphas<-c(alphas,rep(df$death_prob[i],6))
  }
  return(data.frame(group,x,y,alphas))}




lexis_polygon_dfcont<-function(df,max_alpha=NULL){if(is.null(max_alpha)){max_alpha<-max(df$death_rate)}
  group<-numeric(0)
  x<-numeric(0)
  class(x) <- "Date"
  y<-numeric(0)
  alphas<-numeric(0)
  j<-1
  for(i in 1:nrow(df)){group<-c(group,c(rep(c(j,(j+1)),each=3)))
  j<-j+2
  x<-c(x,c(as.Date(ISOdate(df$year[i], 1, 1)),as.Date(ISOdate(df$year[i]+1, 1, 1)),as.Date(ISOdate(df$year[i]+1, 1, 1)),as.Date(ISOdate(df$year[i], 1, 1)),as.Date(ISOdate(df$year[i], 1, 1)),as.Date(ISOdate(df$year[i]+1, 1, 1))))
  y<-c(y,c(df$age[i],df$age[i],df$age[i+1],df$age[i],df$age[i+1],df$age[i+1]))
  alphas<-c(alphas,rep(df$death_rate[i],6))
  }
  alphas<-alphas/max_alpha
  y[is.na(y)]<-max(y, na.rm = T)+5
  return(data.frame(group,x,y,alphas))}





leslie_matrix_new<-function(dataframe,lifetable){
  mx <- dataframe$Nati/dataframe$Pop  #age-specific fertility rates
  nLx<-lifetable$nLx
  p_x<-numeric(0)
  for (i in 1:(length(nLx)-1)) {p_x<-c(p_x,nLx[i+1]/nLx[i])}
  phi_x<-numeric(0)
  for (i in 1:(length(nLx)-1)) {phi_x<-c(phi_x,nLx[1]/2*(mx[i]+p_x[i]*mx[i+1]))}
  phi_x<-c(phi_x,0)
  lesl_mat<-matrix(0, length(lifetable$nLx),length(lifetable$nLx))
  lesl_mat[1,]<-phi_x
  for (i in 1:(length(lifetable$nLx)-1)) {lesl_mat[(i+1),i]<-p_x[i]}
  return(lesl_mat)}





Predicted_pop_leslie<-function(leslie_mat,Population,starting_year,years){
  pop_prev<-numeric(0)
  time<-years-starting_year
  for (i in time) {pop_prev<-c(pop_prev,sum(matrix.power(lesl_mat,i)%*%Population))}
  return(data.frame(pop_prev,years))}