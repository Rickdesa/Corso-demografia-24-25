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