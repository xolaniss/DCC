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
library(ggplot2)
setwd("/Users/xolanisibande/Desktop/hong_tests_201912")
rm(list=ls())
Sys.setenv(TZ="Africa/Johannesburg")
theme <- theme_minimal(base_size = 6)  + theme(axis.text.x = element_text(angle = 90))
theme_set(theme)
col1 <- "#ffffff"
col2 <- "#000000"

#Sheet Import Function ---
Sheets_import<- function(path){
  data<- path %>%
    excel_sheets() %>%
    purrr::set_names() %>%
    map(read_excel,  col_names = TRUE, path=path)
}


# Importing Sheets ----
Sheets <-Sheets_import("Xolani.xlsx")


#Converting to date type ----
date_replace1 <- function(list) {
 str_replace_all(list, fixed("M"), "-")
}

date_replace2 <- function(list) {
  str_replace_all(list, fixed(","), "-")
}

Sheets[[1]][1] <- Sheets[[1]][1] %>%  map (date_replace1)
Sheets[[1]][1] <- Sheets[[1]][1] %>%  map (date_replace2)

Sheets$Bonds1$Period <- paste(Sheets$Bonds1$Period,"-01", sep = "")
Sheets$Bonds1$Period <- as.yearmon(as.Date(Sheets$Bonds1$Period, format = "%Y-%m-%d"))


#Defining Variables ----

u = Sheets$Bonds1$`Bond Returns`
v = Sheets$Bonds1$`Oil Returns`
date = Sheets$Bonds1$Period

u=u-mean(u);
v=v-mean(v);

T=length(date);



##################
##### Define the lag M in DCC-MGARCH Hong and Haugh tests ;
##################
M=2; 

# define the correlations between u and v with lags -M,-M+1,...,-1,0,1,..,M
# the lag is positive;correlation of U and V(-k); V Granger causes U;
corrp=matrix(0,T,M);
# the lag is negative; U Granger causes V;
corrn=matrix(0,T,M);  
# time-varying (contemporary) correlation between U and V;
corr0=matrix(0,T,1);

dcc.data=cbind(u,v);

# the intial value for the estimation
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
write.table(dcc.results$out,file="dccparam_lag0_Bond1.txt");

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
  write.table(out,file="dccparam_positivelag_Bond1.txt",append=TRUE);
  
  corrp[((k+1):T),k] =dcc.results$DCC[,2];
}


######lag k is negative    
for (k in 1:M) {
  
  uk=u[1:(T-k)]; # the 1st market lead  2nd market;     
  vk=v[(1+k):T]; 
  # Market 2 is affected by Market 1.
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
  write.table(out,file="dccparam_negativelag_Bond1.txt",append=TRUE);
  
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


write.csv(outHong, file="mgarch_Hong_Granger_Bond1.csv", sep = " ");


#Importing
results <- read.csv("mgarch_Hong_Granger_Bond1.csv")

#Renaming ----

results[, 1] <- NULL
colnames(results) <- c("date","H1","ph1","H2","ph2","H10","ph10","H20","ph20","Hb","phb")
Bonds1 <- results

#Merging dates for graphing

Bonds1$date <- Sheets$Bonds1$Period[-1]

# Discrete p values
discrete <- function(pvalue) {
  case_when(pvalue < 0.05 ~ "1", pvalue > 0.05 ~ "0", TRUE ~ "Other")
}

Bonds1$ph1 <- discrete(Bonds1$ph1)
Bonds1$ph2<- discrete(Bonds1$ph2)
Bonds1$ph10<- discrete(Bonds1$ph10)
Bonds1$ph20<- discrete(Bonds1$ph20)
Bonds1$phb<- discrete(Bonds1$phb)
#Exporting 

write.csv(Bonds1, file="Bonds1.csv", row.names = F);

#Graphing

rug_plot <- function(data, y.var, z.var, plot.name) {
  y.var <- enquo(y.var)
  z.var <- enquo(z.var)
  plot.name <- rlang::sym(plot.name)
plot<- data %>%  
  ggplot(aes(x = date, y = !! y.var )) +
  geom_line() +
  geom_rug(aes(color= !! z.var), inherit.aes = T, sides = "b", show.legend = F) +
  scale_x_yearmon() +
  labs(x = "", y = "Causality", subtitle = "") +
  scale_color_manual(values = c(col1, col2))
  ggsave(plot, file=paste(plot.name,"_graph", ".png", sep=''), scale = 1, width=7, height=7)
}          

rug_plot(Bonds1, y.var = H1, z.var = ph1, plot.name = "Bonds1_ph1")
rug_plot(Bonds1, y.var = H2, z.var = ph2, plot.name = "Bonds1_ph2")
rug_plot(Bonds1, y.var = H10, z.var = ph10, plot.name = "Bonds1_ph10")
rug_plot(Bonds1, y.var = H20, z.var = ph20, plot.name = "Bonds1_ph20")
rug_plot(Bonds1, y.var = Hb, z.var = phb, plot.name = "Bonds1_phb")

