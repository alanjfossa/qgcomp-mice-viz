
#Load packages----
library(tidyverse)
library(mice)
library(qgcomp)

#Alan's color pal----
alans_colors<-function(n){
  
  colors<-c("#18BBF5","#F5BB18","#4588A0","#F58A18","#18F5B6","lightgrey")
  
  selected_colors<-colors[1:n]
  
  return(selected_colors)
  
}

#Set working dir----
setwd("C:\\Users\\afossa\\OneDrive\\repos\\qgcomp-mice-viz")

#Load metals dataset----
data("metals", package="qgcomp")

head(metals)

#Add missingness to metals dataset----
metals_miss<-ampute(metals,prop=0.7)$amp

#Impute----
imp<-mice(metals_miss,m=20)

#Set imputed datasets to a list----
imp_list<-complete(imp,action="all")

#Fit qgcomp models to imputed dataset----
#Adapted from Alex's vignette)

#Set vector of exposure and covariate names----
Xnm <- c(
  'arsenic','barium','cadmium','calcium','chromium','copper',
  'iron','lead','magnesium','manganese','mercury','selenium','silver',
  'sodium','zinc'
)
covars <- c('nitrate','nitrite','sulfate','ph', 'total_alkalinity','total_hardness')

qg_models<-map(
  imp_list,
  ~qgcomp.boot(
    y~. + .^2, 
    expnms=Xnm,
    data=.x[,c(Xnm, 'y')], 
    family=gaussian(), 
    q=4, 
    B=10, 
    seed=125,
    d=2
    )
  )

#Get predictions from all models----
all_predictions<-map(qg_models,~as_tibble(modelbound.boot(.x)))

all_predictions_df<-bind_rows(all_predictions,.id='imputation')

#Plot all predictions in a single figure----
col_pal<-c('Prediction'=alans_colors(3)[1])

fill_pal<-c('Bootstrap 95% CI'='lightgrey')

##Version with CI as lines...not as nice.
# all_predictions_df %>% 
#   ggplot()+
#   geom_line(
#     aes(x=quantile,y=linpred,group = imputation,color='Prediction'),
#     alpha=0.3,
#     linewidth=1
#   )+
#   geom_line(
#     aes(x=quantile,y=ll.simul,group = imputation,color='Bootstrap 95% CI'),
#     alpha=0.3,
#     linewidth=1
#   )+
#   geom_line(
#     aes(x=quantile,y=ul.simul,group = imputation,color='Bootstrap 95% CI'),
#     alpha=0.3,
#     linewidth=1
#   )+
#   scale_color_manual(values=pal)+
#   labs(
#     x="Joint exposure quantile",
#     y="Response variable",
#     color=""
#   )+
#   theme_classic()

pred_plot_mice<-all_predictions_df %>% 
  ggplot()+
  geom_ribbon(
    aes(x=quantile,ymin=ll.simul,ymax=ul.simul,group = imputation,fill='Bootstrap 95% CI'),
    alpha=0.2,
    linewidth=1
  )+
  geom_line(
    aes(x=quantile,y=linpred,group = imputation,color='Prediction'),
    alpha=0.3,
    linewidth=1
  )+
  scale_color_manual(values=col_pal)+
  scale_fill_manual(values=fill_pal)+
  labs(
    x="Joint exposure quantile",
    y="Response variable",
    color="",
    fill=""
  )+
  theme_classic()

pred_plot_mice %>% 
  ggsave(
    filename="output\\pred_plot_mice.tiff",
    plot=.,
    dpi=200,
    scale=1
  )
