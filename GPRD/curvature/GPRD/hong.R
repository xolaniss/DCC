# DCC-GARCH+ time-varying Haugh and Hong tests

# using DCC-GARCH to get the correlation coeffecients with lag i, -k<i<k, k; diagonal GARCH model estimation
# M is the lag order in the statistics;
# Barllet functions, M 

#Preliminary ----
library(ccgarch)
library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(zoo)
library(tidyverse)
library(scales)
setwd("/Users/xolanisibande/DCC/GPRD/curvature/GPRD")
rm(list=ls())
Sys.setenv(TZ="Africa/Johannesburg")
theme <- theme_minimal(base_size = 6)  + theme(axis.text.x = element_text(angle = 90))
theme_set(theme)
col1 <- "#ffffff"
col2 <- "#000000"


# Importing Sheets ----
data <-read_csv("../../Clean_Data.csv")

# Defining Variables ----

u = data$Curvature
v = data$GPRD
date = data$Date
u=u-mean(u);
v=v-mean(v);
T=length(date);

##################
##### Define the lag M in DCC-MGARCH Hong and Haugh tests ;
##################
M=16; 

# define the correlations between u and v with lags -M,-M+1,...,-1,0,1,..,M
# the lag is positive;correlation of U and V(-k); V Granger causes U;
corrp=matrix(0,T,M);
# the lag is negative; U Granger causes V;
corrn=matrix(0,T,M);  
# time-varying (contemporary) correlation between U and V;
corr0=matrix(0,T,1);

dcc.data=cbind(u,v);

# the initial value for the estimation
a1=cov(u,u); a2=cov(v,v); # covariance of U and v for the initial values of u and v condional variance
cri=cor(u,v); # initial value for correlation

a <-c(a1, a2);A <- diag(c(0.05,0.05));B <- diag(c(0.9,0.9)); 
uncR <- matrix(c(1,cri,cri, 1),2,2);dcc.para <- c(0.05,0.9);

# Estimating a DCC-GARCH(1,1) model
dcc.results <- dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dcc.para, dvar=dcc.data,
                              model="diagonal")
# model="diagonal"  diagonal GARCH; 
# dcc.results <- dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dcc.para, dvar=dcc.data,model="extended")
# Output Parameter estimates and their robust standard errors??   dcc.results$out
write.table(dcc.results$out,file="dccparam_lag0.txt");

corr0=dcc.results$DCC[,2];


#####lag k is positive    
for (k in 1:M) {
  
  uk=u[(1+k):T]; # the 1st market is lag of the 2nd market;     
  vk=v[1:(T-k)];
  # Market 1 is affected by Market 2. 
  # ???ֶ?????ζ???г?1?ܵ??г?2??Ӱ?? 
  
  dcc.data=cbind(uk,vk);
  
  # the intial value for the estimation
  a1=cov(uk,uk); a2=cov(vk,vk); # covariance of U and v for the initial values of u and v condional variance
  cri=cor(uk,vk); # initial value for correlation
  
  a <-c(a1, a2);A <- diag(c(0.05,0.05));B <- diag(c(0.9,0.9)); 
  uncR <- matrix(c(1,cri, cri, 1),2,2);dcc.para <- c(0.05,0.85);
  
  # Estimating a DCC-GARCH(1,1) model
  dcc.results <- dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dcc.para, dvar=dcc.data,
                                model="diagonal")
  
  out=dcc.results$out;
  # Output Parameter estimates and their robust standard errors??   dcc.results$out
  write.table(out,file="dccparam_positivelag.txt",append=TRUE);
  
  corrp[((k+1):T),k] =dcc.results$DCC[,2];
}


######lag k is negative    
for (k in 1:M) {
  
  uk=u[1:(T-k)]; # the 1st market lead  2nd market;     
  vk=v[(1+k):T]; # Market 2 is affected by Market 1.
  # ???ֶ?????ζ???г?2?ܵ??г?1??Ӱ?? 
  
  dcc.data=cbind(uk,vk);
  
  # the intial value for the estimation
  a1=cov(uk,uk); a2=cov(vk,vk); # covariance of U and v for the initial values of u and v condional variance
  cri=cor(uk,vk); # initial value for correlation
  
  a <-c(a1, a2);A <- diag(c(0.05,0.05));B <- diag(c(0.9,0.9)); 
  uncR <- matrix(c(1,cri, cri, 1),2,2);dcc.para <- c(0.05,0.85);
  
  # Estimating a DCC-GARCH(1,1) model
  dcc.results <- dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dcc.para, dvar=dcc.data,
                                model="diagonal")
  
  out=dcc.results$out;
  # Output Parameter estimates and their robust standard errors??   dcc.results$out
  write.table(out,file="dccparam_negativelag.txt",append=TRUE);
  
  corrn[((k+1):T),k] =dcc.results$DCC[,2];
}

# the correlaion considering contemporary spillover
corrp0=cbind(corr0,corrp[,1:(M-1)]);
corrn0=cbind(corr0,corrn[,1:(M-1)]);

##################################################################################
###########Define time-varying Haugh and Hong tests ????ʱ????Haugh??Hong??Ϣ????ͳ??��
##################################################################################

# ǰ??M???޷????㣬??????Ҫ????:
# (1) ɾ??ǰM???????ڣ?????ʣ????????ΪT-M?? (2) ??Mʱ?̵?ֵ??Ϊǰ??M-1ʱ?̵?????ͳ??��
# ??????ͬһ?????յ???Ϣ??????????


# The uni-directional Granger test
#  
# H1??һ???????ܵ??ĵ???Ӱ??,H2Ϊ?ڶ????????ܵ??ĵ???Ӱ?? 

# H1: Granger casaulity or information spillover from Series 2 to 1;
# ph1 is the p-values of H1, and the following series are similar. 
H1=ph1=matrix(0,T,1);
# H2: Granger causality or information spillover from Series 1 to 2.
# ph2 is the p-values of H2.  
H2=ph2=H1;

# Uni-directional Granger test for nonchroy trading: Considering the same trading day' Granger 
# ???ǵ?ͬһ?????յ???Ϣ?????ڷ??????׷?ͬ??????
# H10: contemprary Granger casaulity from Series 2 to 1;
H10=ph10=H1; 
# H20: contemprary Granger casaulity from Series 1 to 2;
H20=ph20=H1; 

# ˫????Ϣ???? Hb??bi-directional or instaneous Granger test
Hb=phb=H1;

# Kernel function: K Bartlett function K(z)=1-|z|,|z|<=1;

J=1:M; # ???ڿ???Bartlett function???ͺ?M??ʵ??ֻ?ܿ???M-1?ף?Ϊ?˿???M?ף?????ȡM0=M+1,????ʵ??ͳ??��һ?????𲻴???
Kbar=1-abs(J/M);
C1=sum((1-J/T)*Kbar^2);
D1=sum((1-J/T)*(1-J/T-1/T)*Kbar^4);


## Calculate the mean and variance of bidirectional Hong test. ????˫??????ͳ??��?б?׼???ľ?ֵ?뷽??
Jb=(-M):M; 
Kbarb=1-abs(Jb/M);
# ˫??????ͳ??��?ľ?ֵ?뷽???׼??ͳ??��??ʹ????????̬?ֲ?
# C2TM=2*C1TM+1; D2TM=2*D1TM+(1-1/M); #Another calculation method. ????һ?ַ?ʽ?????к˺???K(0)=1;
C2=sum((1-abs(Jb)/T)*Kbarb^2);
D2=sum((1-abs(Jb)/T)*(1-abs(Jb)/T-1/T)*Kbarb^4);

###### Time-varying Hong tests and p-vlaues #############
for (t in 1:T) {
  H1[t]=(T*sum(Kbar^2*corrp[t,]^2)-C1)/sqrt(2*D1);
  ph1[t]=1-pnorm(H1[t]);
  
  H2[t]=(T*sum(Kbar^2*corrn[t,]^2)-C1)/sqrt(2*D1);
  ph2[t]=1-pnorm(H2[t]);
  
  H10[t]=(T*sum(Kbar^2*corrp0[t,]^2)-C1)/sqrt(2*D1);
  ph10[t]=1-pnorm(H10[t]);
  
  H20[t]=(T*sum(Kbar^2*corrn0[t,]^2)-C1)/sqrt(2*D1);
  ph20[t]=1-pnorm(H20[t]);
  
  Hb[t]=( T*sum(Kbar^2*corrn[t,]^2) +T*corr0[t]^2 +T*sum(Kbar^2*corrp[t,]^2) -C2 )/sqrt(2*D2);
  phb[t]=1-pnorm(Hb[t]);
  
}


####################### time-varying Haugh(1976) tests
# Haughͳ??��??pֵ,???Ƶķ?????????
Q1=p1=Q2=p2=Q10=p10=Q20=p20=Qb=pb=H1;

M0=M;
# M0=M is used for time-varying Haugh tests. 

for (t in 1: T) {
  Q1[t]=T*sum(corrp[t,1:M0]^2);p1[t]=1-pchisq(Q1[t],M0);
  Q2[t]=T*sum(corrn[t,1:M0]^2);p2[t]=1-pchisq(Q2[t],M0);
  Q10[t]=T*sum(corrp0[t,1:M0]^2);p10[t]=1-pchisq(Q10[t],M0);
  Q20[t]=T*sum(corrn0[t,1:M0]^2);p20[t]=1-pchisq(Q20[t],M0);
  Qb[t]=T*( sum(corrn[t,1:M0]^2)+corr0[t]^2+sum(corrp[t,1:M0]^2) );pb[t]=1-pchisq(Qb[t],(2*M0+1));
}


#### output the stastics and corresponding p-values 
outhong=cbind(date,H1,ph1,H2,ph2,H10,ph10,H20,ph20,Hb,phb);
outHong=outhong[(M:T),];
outhaugh=cbind(date,Q1,p1,Q2,p2,Q10,p10,Q20,p20,Qb,pb);
outHaugh=outhaugh[(M:T),]; 
write.csv(outHong, file="mgarch_Hong_Granger.csv", sep = " ");

#Importing
results <- read.csv("mgarch_Hong_Granger.csv")

#Renaming ----
results[, 1] <- NULL
colnames(results) <- c("date","H1","ph1","H2","ph2","H10","ph10","H20","ph20","Hb","phb")
results <- results %>% select(date,H1, ph1, H10, ph10)

#Merging dates for graphing
results$date <- data$Date[-c(1:15)]

# Discrete p values
discrete <- function(pvalue) {
  case_when(pvalue < 0.05 ~ "1", pvalue > 0.05 ~ "0", TRUE ~ "Other")
}

results$ph1 <- discrete(results$ph1)
#results$ph2 <- discrete(results$ph2)
results$ph10 <- discrete(results$ph10)
#results$ph20 <- discrete(results$ph20)
#results$phb <- discrete(results$phb)

# Initializing rug plots
results$ph1[1] <- 0
#results$ph2[1] <- 0
results$ph10[1] <- 0
#results$ph20[1] <- 0
#results$phb[1] <- 0 

# Exporting 
write.csv(results, file="results.csv", row.names = FALSE);

# Graphing
rug_plot <- function(data, y.var, z.var, plot.name) {
  y.var <- enquo(y.var)
  z.var <- enquo(z.var)
  plot.name <- rlang::sym(plot.name)
plot<- data %>%  
  ggplot(aes(x = date, y = !! y.var )) +
  geom_line() +
  geom_rug(aes(color= !! z.var), inherit.aes = T, sides = "b", show.legend = F) +
  scale_x_date(labels=date_format("%b-%y")) +
  labs(x = "", y = "Causality Test Statistic", subtitle = "") +
  scale_color_manual(values = c(col1, col2))
  ggsave(plot, file=paste(plot.name,"_graph", ".png", sep=''), scale = 1, width=5, height=5)
}          

rug_plot(results, y.var = H1, z.var = ph1, plot.name = "results_ph1")
#rug_plot(results, y.var = H2, z.var = ph2, plot.name = "results_ph2")
rug_plot(results, y.var = H10, z.var = ph10, plot.name = "results_ph10")
#rug_plot(results, y.var = H20, z.var = ph20, plot.name = "results_ph20")
#rug_plot(results, y.var = Hb, z.var = phb, plot.name = "results_phb")

