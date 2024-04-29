library(tidyverse)
library(HMDHFDplus)
library(here)
library(cowplot)

graphics.off()
rm(list=ls() )

source('fix-HFDparse.R')  # this is necessary for some versions of HFDparse (?)

cases = tribble(
  ~code,~country, ~first_year, ~abb, ~description,
  'JPN'   ,'Japan'      , 1947, 'RR'   , 'rectangle data',
  'AUT'   ,'Austria'    , 1951, 'TR'   , 'triangle data',
  'FRATNP','France'     , 1946, 'TR'   , 'triangle data',
  'HUN'   ,'Hungary'    , 1950, 'TR'   , 'triangle data',
  'NLD'   ,'Netherlands', 1950, 'TR'   , 'triangle data',
  'PRT'   ,'Portugal'   , 1940, 'RR/TR', 'mixed input data',
  'IRL'   ,'Ireland'    , 1955, 'RR'   , 'rectangle data',
  'DNK'   ,'Denmark'    , 1916, 'RR'   , 'rectangle data',
  'CHE'   ,'Switzerland', 1932, 'RR/TR', 'mixed data',
  'SWE'   ,'Sweden'     , 1991, 'RR/TR', 'mixed data'
  )
  
for (i in 1:nrow(cases)) {

  this_code        = cases$code[i]
  this_country     = cases$country[i]
  this_abb         = cases$abb
  this_description = cases$description[i]
  
  # Cohort TFR40 from Lexis triangles ----
  
  BTR = readHFD(here('Data',paste0(this_code,'birthsTR.txt')))  %>% 
    rename(Births=Total)
  
  NTR = readHFD(here('Data',paste0(this_code,'exposTR.txt')))
  
  DTR = full_join(BTR,NTR) 
  
  TFR_triangle = 
        DTR %>% group_by(Cohort,Age) %>% 
               summarize(rate = sum(Births)/sum(Exposure)) %>%
               filter(Age %in% 12:39) %>%
               group_by(Cohort) %>% 
               summarize(TFR=sum(rate,na.rm=TRUE), n=n()) %>% 
               filter(n==28) %>% 
               add_column(method='Cohort')
  
  # Cohort TFR40 from Age-Period rectangles ----
  
  BRR = readHFD(here('Data',paste0(this_code,'birthsRR.txt'))) %>% 
    rename(Births=Total)
  
  NRR = readHFD(here('Data',paste0(this_code,'exposRR.txt')))
  
  DRR = full_join(BRR,NRR) %>% 
       mutate(Cohort = Year-Age,
              rate = Births/Exposure)
  
  TFR_rectangle = 
    DRR %>% 
    filter(Age %in% 12:39) %>%
    group_by(Cohort) %>% 
    summarize(TFR=sum(rate,na.rm=TRUE), n=n()) %>% 
    filter(n==28) %>% 
    add_column(method='AP')
  
  # new procedure: average the lagged and leading "rectangle" 
  # measures for each cohort
  
  TFR_new = TFR_rectangle %>% 
    ungroup() %>% 
    mutate(TFR     = (TFR+ lead(TFR,1))/2,
           method='AP2')
  
  df = bind_rows(
          TFR_triangle, 
          TFR_rectangle,
          TFR_new
          ) %>% 
    mutate(method = factor(method,levels=c('Cohort','AP','AP2') ) )
  
  
  G = ggplot(data=df) + 
    aes(x=Cohort,y=TFR, shape=method,size=method, color=method) + 
    geom_point(stroke=1.2) + 
    geom_line(data=filter(df,method=='AP2'),lwd=0.25) +
    theme_bw() +
    theme(legend.position=c(0.8, 0.7),
          legend.text=element_text(size=12)) +
    scale_shape_manual(values=c(1,4,16)) +
    scale_size_manual(values=c(3,1.5,1.2)) +
    scale_color_manual(values=c('black',
                                'royalblue',
                                'red')) +
    labs(title=paste0(toupper(this_country),': Completed Cohort Fertility by Age 40'),
         y='CFR40',
         shape='Method',
         size ='Method',
         color='Method') +
    guides(color = guide_legend(override.aes = list(linetype = c(0, 0, 1) ) ) )
  
  print(G)
  
  ggsave(filename=here('Plots',paste0(this_code,'-CFR-levels.png')),
         height=8, width=8, units='in')

  if (this_country == 'Japan') {
    top_left_plot = G + scale_x_continuous(limits=c(1933,1983)) 

  } 
  
  if (this_country == 'France') {
    bottom_left_plot = G + scale_x_continuous(limits=c(1933,1983)) 
  } 
  
  # calculate errors
  
  err_df = df %>% 
    group_by(Cohort) %>% 
    summarize(AP  = 100*(TFR[method=='AP']/TFR[method=='Cohort']-1),
              AP2 = 100*(TFR[method=='AP2']/TFR[method=='Cohort']-1)) %>% 
    pivot_longer(cols=starts_with('AP'), 
                 names_to = 'etype', values_to = 'error') %>% 
    mutate(etype=factor(etype))
  
  
  G = ggplot(data=err_df) +
    aes(x=Cohort,y=error,color=etype,shape=etype) +
    geom_hline(yintercept = 0) +
    geom_point(size=1, stroke=1.2) +
    geom_line() +
    theme_bw() +
    labs(y='Percent Error', 
         title=this_country,
         subtitle=this_description) +
    scale_y_continuous(limits = c(-2,4.2)) +
    scale_shape_manual(values=c(4,16)) +
    scale_color_manual(values=c('royalblue','red'))

  print(G)
  
  ggsave(filename=here('Plots',paste0(this_code,'-CFR-errors.png')),       
         height=8, width=8, units='in')
  
  
  tab = err_df %>% 
    group_by(etype) %>% 
    summarize( country = this_country,
               MPE   = mean(error,na.rm=TRUE), 
               MAPE  = mean(abs(error),na.rm=TRUE),
               MIN   = quantile(error,prob=0,na.rm=TRUE),
               Q25   = quantile(error,prob=.25,na.rm=TRUE),
               Q75   = quantile(error,prob=.75,na.rm=TRUE),
               MAX   = quantile(error,prob=1,na.rm=TRUE)
               ) %>% 
       mutate_at(c('MPE','MAPE'),round,3) %>% 
       mutate_at(c('MIN','Q25','Q75','MAX'),round,2) 
  
  print(tab)

  
  z = err_df %>% 
       arrange(etype,error) %>% 
       group_by(etype) %>% 
      mutate(P = seq(error)/length(error)) 
  
  txt_df = tibble(
    error = c(-1.2, 1.5),
    P     = c( 0.8, 0.8),
    etype = factor(c('AP2', 'AP'))
  )
  
  G=  ggplot(data=z) +
    aes(x=error,y=P,color=etype) + 
    geom_step(aes(size=etype)) + 
    geom_vline(xintercept = 0) + 
    theme_bw() +
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous(limits = c(-2,4.2)) +
    scale_size_manual(values=c(0.8,1.2)) +
    labs(x='Percent Error', y='Prob', 
         title=paste0(toupper(this_country),': Cumulative Distribution of Errors')) +
    scale_color_manual(values=c('royalblue','red')) +
    guides(color='none', linewidth='none')

  G = G + geom_text(data=txt_df, size=6,
                    hjust=0,aes(label=etype))  
  
  print(G)

  ggsave(filename=here('Plots',paste0(this_code,'-CFR-cumulative-error-distribution.png')),       
         height=8, width=8, units='in')
  
  ggsave(filename=here('Plots',paste0(this_code,'-CFR-cumulative-error-distribution.eps')),       
         height=8, width=8, units='in', dpi=400)
  

  if (this_country == 'Japan') {
    top_right_plot = G
  } 
  
  if (this_country == 'France') {
    bottom_right_plot = G
  } 
  
    
  G=  ggplot(data=err_df) +
    aes(x=error, color=etype) +
    geom_density(trim=TRUE, linewidth=1,adjust=1.5) +
    geom_vline(xintercept = 0) +
    theme_bw() +
    labs(title=this_country,
         subtitle=this_description) +
    scale_x_continuous(limits = c(-2,4.2)) +
    scale_color_manual(values=c('royalblue','red'))
  
  print(G)

  ggsave(filename=here('Plots',paste0(this_code,'-CFR-error-density.png')),       
         height=8, width=8, units='in')
  
  ggsave(filename=here('Plots',paste0(this_code,'-CFR-error-density.eps')),       
         height=8, width=8, units='in', dpi=400)
  
  
  
} # for i

# final, 4-panel plot WORKING HERE...

G4 = plot_grid( top_left_plot, top_right_plot,
           bottom_left_plot, bottom_right_plot,
           nrow=2, rel_widths = c(3,2))

print(G4)

ggsave(filename=here('Plots','2x2-summary.pdf'),       
       height=12, width=14, units='in')

ggsave(filename=here('Plots','2x2-summary.eps'),       
       height=12, width=14, units='in', dpi=400)
