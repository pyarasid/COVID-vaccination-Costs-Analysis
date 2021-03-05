library(dplyr)
library(ggplot2)
library(ggrepel)
library(magrittr)
library(tidyr)
library(readxl)
library(shiny)
library(leaflet)
library(leafpop)
library(sf)
library(scales)
library(stringr)
library(DT)
library(rsconnect)
library(leaflet.extras)
library(shinyBS)
library(shinyjs)
library(knitr)
library(htmltools)
library(kableExtra)
library(shinyWidgets)
library(shinycssloaders)


## includes code adapted from the following sources:
# https://github.com/eparker12/nCoV_tracker/blob/master/app.R
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

#loading the shapefile created in the COVID-19_data script
world_polygon_app <- st_read("world_polygon_app.shp") %>% 
  filter(!nam_lng %in% c("Swaziland", "Palestine")) %>% 
  mutate(nam_lng=recode(nam_lng, `Democratic Republic of the Congo`="Congo [DRC]"))%>% 
  select(-c("VCstHPB" , "VCstPRB" , "VC75PBC"))

#updating bilateral vaccine price per dose
world_polygon_app$VCstDs_b <- 9.95

#getting world_polygon without the geomtry columns i.e get just the dataframe
world_polygon_nogeom <- st_drop_geometry(world_polygon_app) 

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

mill <-  scales::unit_format(unit = "M", scale = 1e-6, accuracy = .01)

#Creating function for bar chart 
cost_plot <- function(country_name, vaccine_cost_cvx){
  
  # if(type_scales =="Log" & vaccine_cost_cvx==FALSE){
  #   
  #   total_df <- world_polygon_nogeom  %>%
  #     filter(nam_lng==country_name) %>%
  #     mutate(TtCstDB=(DlCstDs+VCstDs_b),
  #            VCstHPB=(NmHthPr*TtCstDB*num_dos),
  #            VCstPRB=(tot_pop*HghRskP*TtCstDB*num_dos),
  #            VC75PBC=(tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)) %>% 
  #     select(Year, VCstHPB, VCstPRB, VC75PBC) %>% 
  #     rename(`Total cost health professional`= `VCstHPB`,
  #            `Total cost population at risk`= `VCstPRB`,
  #            `Total cost 70% population`=`VC75PBC`) %>% 
  #     gather(Total_type, `Total cost`, -Year) %>%
  #     mutate(Vcc_cost= case_when(Total_type=="Total cost health professional"~ "Health \n professionals",
  #                                Total_type=="Total cost population at risk" ~"High risk \n population",
  #                                Total_type=="Total cost 70% population"~"70% \n population")) %>% 
  #     mutate(`type_cost`="Total cost")
  # 
  #   g1 <-  ggplot(total_df ,
  #                 aes(Vcc_cost, `Total cost`))+
  #     geom_point(aes(color=`type_cost`), size=4)+
  #     geom_text_repel(aes(label=mill(`Total cost`)),min.segment.length = Inf, box.padding = 0.5, size=2.5, fontface="bold")+
  #     scale_y_log10(labels = addUnits, n.breaks = 7)+
  #     scale_color_manual(values = c("Total cost"="#FB262A"))+
  #     xlim("70% \n population", "High risk \n population", "Health \n professionals")+
  #     theme_minimal()+
  #     theme(axis.title.y=element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.text.x = element_text(face = "bold", size = 8.5),
  #           legend.title =  element_blank(),
  #           legend.text = element_text(face="bold"),
  #           legend.position = "bottom",
  #           plot.title = element_text(size=11, face="bold", hjust = 0),
  #           plot.title.position = "plot")+
  #     labs(title = paste(country_name, ": Total cost to vaccinate \n","(2020 $US) (M: Million, k: Thousand)"))
  #   
  #   g1
  #   
  #   # procurement_df <- world_polygon_nogeom  %>%
  #   #   filter(nam_lng==country_name) %>%
  #   #   mutate(VCstDs_b=(VCstDs_b),
  #   #          NmHthPr=(NmHthPr),
  #   #          HghRskP=(HghRskP),
  #   #          num_dos=(num_dos),
  #   #          PrcCstHthPrf=(NmHthPr*VCstDs_b*num_dos),
  #   #          PrcCstPpRsk=(tot_pop*HghRskP*VCstDs_b*num_dos),
  #   #          PrcCst75Pp= (tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)) %>%
  #   #   select(Year,
  #   #          PrcCstHthPrf, 
  #   #          PrcCstPpRsk, 
  #   #          PrcCst75Pp) %>%
  #   #   rename(`Proc. cost health professional`= `PrcCstHthPrf`,
  #   #          `Proc. cost population at risk`=`PrcCstPpRsk`,
  #   #          `Proc. cost  70% population`=`PrcCst75Pp`) %>%
  #   #   gather(Procurement, `Procurement cost`, matches("Proc"), -Year) %>% 
  #   #   mutate(Vcc_cost= case_when(Procurement=="Proc. cost health professional"~ "Health \n professionals",
  #   #                              Procurement=="Proc. cost population at risk" ~"High risk \n population",
  #   #                              Procurement=="Proc. cost  70% population"~"70% \n population")) 
  #   # 
  #   # delivery_df <- 
  #   #   world_polygon_nogeom  %>%
  #   #   filter(nam_lng==country_name) %>%
  #   #   mutate(DlCstDs=(DlCstDs),
  #   #          NmHthPr=(NmHthPr),
  #   #          HghRskP=(HghRskP),
  #   #          num_dos=(num_dos),
  #   #          DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
  #   #          DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
  #   #          DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
  #   #   select(Year, 
  #   #          DelCstHthPrf, 
  #   #          DelCstPpRsk,
  #   #          DelCst75Pp) %>%
  #   #   rename(`Delivery cost health professional`= `DelCstHthPrf`,
  #   #          `Delivery cost population at risk`= `DelCstPpRsk`,
  #   #          `Delivery cost 70% population`=`DelCst75Pp`) %>%
  #   #   gather(Delivery, `Delivery cost`, matches("Delivery"), -Year) %>% 
  #   #   mutate(Vcc_cost= case_when(Delivery=="Delivery cost health professional"~ "Health \n professionals",
  #   #                              Delivery=="Delivery cost population at risk" ~"High risk \n population",
  #   #                              Delivery=="Delivery cost 70% population"~"70% \n population")) 
  #   # 
  #   # plot_stack <- procurement_df %>% full_join(delivery_df, by = c("Vcc_cost"="Vcc_cost")) %>%
  #   #   gather(type, type_cost, c(`Delivery cost`, `Procurement cost`)) %>%  
  #   #   select(Year.y, Vcc_cost, type, type_cost) %>% 
  #   #   rename(`Year`=`Year.y`)
  #   
  #   # g1 <-  ggplot(plot_stack ,
  #   #               aes(Vcc_cost, type_cost))+
  #   #   geom_point(aes(color= type),size=4)+
  #   #   geom_text_repel(aes(label=mill(type_cost)),min.segment.length = Inf, box.padding = 0.5, size=2.5, fontface="bold")+
  #   #   scale_y_log10(labels = addUnits, n.breaks = 7)+
  #   #   scale_color_manual(values = c("Procurement cost"="#FB262A",
  #   #                                 "Delivery cost"="#5ab4ac"))+
  #   #   xlim("70% \n population", "High risk \n population", "Health \n professionals")+
  #   #   theme_minimal()+
  #   #   theme(axis.title.y=element_blank(),
  #   #         axis.title.x = element_blank(),
  #   #         axis.text.x = element_text(face = "bold", size = 8.5),
  #   #         legend.title =  element_blank(),
  #   #         legend.text = element_text(face="bold"),
  #   #         legend.position = "bottom",
  #   #         plot.title = element_text(size=11, face="bold", hjust = 0),
  #   #         plot.title.position = "plot")+
  #   #   labs(title = paste(country_name, ": Total cost to vaccinate \n","(2020 $US) (M: Million, k: Thousand)"))
  #   # 
  #   # g1
  #   
  # }else if(type_scales =="Log" & vaccine_cost_cvx!=FALSE){
  #   
  #   total_df <- world_polygon_nogeom  %>%
  #     filter(nam_lng==country_name) %>%
  #     mutate(DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
  #            DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
  #            DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b),
  #            PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
  #            PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
  #            PrcCst75Pp=ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
  #                              ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b))),
  #            VCstHPB=(DelCstHthPrf+ PrcCstHthPrf),
  #            VCstPRB=(DelCstPpRsk+PrcCstPpRsk),
  #            VC75PBC=(DelCst75Pp+PrcCst75Pp)) %>% 
  #     select(Year, VCstHPB, VCstPRB, VC75PBC) %>% 
  #     rename(`Total cost health professional`= `VCstHPB`,
  #            `Total cost population at risk`= `VCstPRB`,
  #            `Total cost 70% population`=`VC75PBC`) %>% 
  #     gather(Total_type, `Total cost`, -Year) %>%
  #     mutate(Vcc_cost= case_when(Total_type=="Total cost health professional"~ "Health \n professionals",
  #                                Total_type=="Total cost population at risk" ~"High risk \n population",
  #                                Total_type=="Total cost 70% population"~"70% \n population")) %>% 
  #     mutate(`type_cost`="Total cost")
  #   
  #   g1 <-  ggplot(total_df ,
  #                 aes(Vcc_cost, `Total cost`))+
  #     geom_point(aes(color=`type_cost`), size=4)+
  #     geom_text_repel(aes(label=mill(`Total cost`)),min.segment.length = Inf, box.padding = 0.5, size=2.5, fontface="bold")+
  #     scale_y_log10(labels = addUnits, n.breaks = 7)+
  #     scale_color_manual(values = c("Total cost"="#FB262A"))+
  #     xlim("70% \n population", "High risk \n population", "Health \n professionals")+
  #     theme_minimal()+
  #     theme(axis.title.y=element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.text.x = element_text(face = "bold", size = 8.5),
  #           legend.title =  element_blank(),
  #           legend.text = element_text(face="bold"),
  #           legend.position = "bottom",
  #           plot.title = element_text(size=11, face="bold", hjust = 0),
  #           plot.title.position = "plot")+
  #     labs(title = paste(country_name, ": Total cost to vaccinate \n","(2020 $US) (M: Million, k: Thousand)"))
  #   
  #   g1
  #   
  #   # procurement_df <- world_polygon_nogeom  %>%
  #   #   filter(nam_lng==country_name) %>%
  #   #   mutate(VCstDs_b=(VCstDs_b),
  #   #          NmHthPr=(NmHthPr),
  #   #          HghRskP=(HghRskP),
  #   #          num_dos=(num_dos),
  #   #          PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
  #   #          PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
  #   #          PrcCst75Pp= ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
  #   #                             ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)))) %>%
  #   #   select(Year,
  #   #          PrcCstHthPrf, 
  #   #          PrcCstPpRsk, 
  #   #          PrcCst75Pp) %>%
  #   #   rename(`Proc. cost health professional`= `PrcCstHthPrf`,
  #   #          `Proc. cost population at risk`=`PrcCstPpRsk`,
  #   #          `Proc. cost  70% population`=`PrcCst75Pp`) %>%
  #   #   gather(Procurement, `Procurement cost`, matches("Proc"), -Year) %>% 
  #   #   mutate(Vcc_cost= case_when(Procurement=="Proc. cost health professional"~ "Health \n professionals",
  #   #                              Procurement=="Proc. cost population at risk" ~"High risk \n population",
  #   #                              Procurement=="Proc. cost  70% population"~"70% \n population")) 
  #   # 
  #   # delivery_df <- 
  #   #   world_polygon_nogeom  %>%
  #   #   filter(nam_lng==country_name) %>%
  #   #   mutate(DlCstDs=(DlCstDs),
  #   #          NmHthPr=(NmHthPr),
  #   #          HghRskP=(HghRskP),
  #   #          num_dos=(num_dos),
  #   #          DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
  #   #          DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
  #   #          DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
  #   #   select(Year, 
  #   #          DelCstHthPrf, 
  #   #          DelCstPpRsk,
  #   #          DelCst75Pp) %>%
  #   #   rename(`Delivery cost health professional`= `DelCstHthPrf`,
  #   #          `Delivery cost population at risk`= `DelCstPpRsk`,
  #   #          `Delivery cost 70% population`=`DelCst75Pp`) %>%
  #   #   gather(Delivery, `Delivery cost`, matches("Delivery"), -Year) %>% 
  #   #   mutate(Vcc_cost= case_when(Delivery=="Delivery cost health professional"~ "Health \n professionals",
  #   #                              Delivery=="Delivery cost population at risk" ~"High risk \n population",
  #   #                              Delivery=="Delivery cost 70% population"~"70% \n population")) 
  #   # 
  #   # plot_stack <- procurement_df %>% full_join(delivery_df, by = c("Vcc_cost"="Vcc_cost")) %>%
  #   #   gather(type, type_cost, c(`Delivery cost`, `Procurement cost`)) %>%  
  #   #   select(Year.y, Vcc_cost, type, type_cost) %>% 
  #   #   rename(`Year`=`Year.y`)
  #   # 
  #   # 
  #   # g1 <-  ggplot(plot_stack ,
  #   #               aes(Vcc_cost, type_cost))+
  #   #   geom_point(aes(color= type),size=4)+
  #   #   geom_text_repel(aes(label=mill(type_cost)),min.segment.length = Inf, box.padding = 0.5, size=2.5, fontface="bold")+
  #   #   scale_y_log10(labels = addUnits, n.breaks = 7)+
  #   #   scale_color_manual(values = c("Procurement cost"="#FB262A",
  #   #                                 "Delivery cost"="#5ab4ac"))+
  #   #   xlim("70% \n population", "High risk \n population", "Health \n professionals")+
  #   #   theme_minimal()+
  #   #   theme(axis.title.y=element_blank(),
  #   #         axis.title.x = element_blank(),
  #   #         axis.text.x = element_text(face = "bold", size = 8.5),
  #   #         legend.title =  element_blank(),
  #   #         legend.text = element_text(face="bold"),
  #   #         legend.position = "bottom",
  #   #         plot.title = element_text(size=11, face="bold", hjust = 0),
  #   #         plot.title.position = "plot")+
  #   #   labs(title = paste(country_name, ": Total cost to vaccinate \n","(2020 $US) (M: Million, k: Thousand)"))
  #   # 
  #   # g1
  #   
  # }
  # 
  
  if(vaccine_cost_cvx==FALSE){
    
    total_df <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name) %>%
      mutate(TtCstDB=(DlCstDs+VCstDs_b),
             VCstHPB=(NmHthPr*TtCstDB*num_dos),
             VCstPRB=(tot_pop*HghRskP*TtCstDB*num_dos),
             VC75PBC=(tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)) %>% 
      select(Year, VCstHPB, VCstPRB, VC75PBC) %>% 
      rename(`Total cost health professional`= `VCstHPB`,
             `Total cost population at risk`= `VCstPRB`,
             `Total cost 70% population`=`VC75PBC`) %>% 
      gather(Total_type, `Total cost`, -Year) %>%
      mutate(Vcc_cost= case_when(Total_type=="Total cost health professional"~ "Health \n professionals",
                                 Total_type=="Total cost population at risk" ~"High risk \n population",
                                 Total_type=="Total cost 70% population"~"70% \n population")) %>% 
      mutate(`type_cost`="Total cost")
    
    g1 <-  ggplot(total_df ,
                  aes(Vcc_cost, `Total cost`))+
      geom_point(aes(color=`type_cost`), size=4)+
      geom_text_repel(aes(label=mill(`Total cost`)),min.segment.length = Inf, box.padding = 0.5, size=3.5, fontface="bold")+
      scale_y_continuous(labels = addUnits, n.breaks = 5)+
      scale_color_manual(values = c("Total cost"="#FB262A"))+
      xlim("70% \n population", "High risk \n population", "Health \n professionals")+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 8.5),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold"),
            legend.position = "bottom",
            plot.title = element_text(size=11, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste(country_name, ": Total cost to vaccinate \n","(2020 $US) (M: Million, k: Thousand)"))
    
    g1
    
    # 
    # procurement_df <- world_polygon_nogeom  %>%
    #   filter(nam_lng==country_name) %>%
    #   mutate(VCstDs_b=(VCstDs_b),
    #          NmHthPr=(NmHthPr),
    #          HghRskP=(HghRskP),
    #          num_dos=(num_dos),
    #          PrcCstHthPrf=(NmHthPr*VCstDs_b*num_dos),
    #          PrcCstPpRsk=(tot_pop*HghRskP*VCstDs_b*num_dos),
    #          PrcCst75Pp= (tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)) %>%
    #   select(Year,
    #          PrcCstHthPrf, 
    #          PrcCstPpRsk, 
    #          PrcCst75Pp) %>%
    #   rename(`Proc. cost health professional`= `PrcCstHthPrf`,
    #          `Proc. cost population at risk`=`PrcCstPpRsk`,
    #          `Proc. cost  70% population`=`PrcCst75Pp`) %>%
    #   gather(Procurement, `Procurement cost`, matches("Proc"), -Year) %>% 
    #   mutate(Vcc_cost= case_when(Procurement=="Proc. cost health professional"~ "Health \n professionals",
    #                              Procurement=="Proc. cost population at risk" ~"High risk \n population",
    #                              Procurement=="Proc. cost  70% population"~"70% \n population")) 
    # 
    # delivery_df <- 
    #   world_polygon_nogeom  %>%
    #   filter(nam_lng==country_name) %>%
    #   mutate(DlCstDs=(DlCstDs),
    #          NmHthPr=(NmHthPr),
    #          HghRskP=(HghRskP),
    #          num_dos=(num_dos),
    #          DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
    #          DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
    #          DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
    #   select(Year, 
    #          DelCstHthPrf, 
    #          DelCstPpRsk,
    #          DelCst75Pp) %>%
    #   rename(`Delivery cost health professional`= `DelCstHthPrf`,
    #          `Delivery cost population at risk`= `DelCstPpRsk`,
    #          `Delivery cost 70% population`=`DelCst75Pp`) %>%
    #   gather(Delivery, `Delivery cost`, matches("Delivery"), -Year) %>% 
    #   mutate(Vcc_cost= case_when(Delivery=="Delivery cost health professional"~ "Health \n professionals",
    #                              Delivery=="Delivery cost population at risk" ~"High risk \n population",
    #                              Delivery=="Delivery cost 70% population"~"70% \n population")) 
    # 
    # plot_stack <- procurement_df %>% full_join(delivery_df, by = c("Vcc_cost"="Vcc_cost")) %>%
    #   gather(type, type_cost, c(`Delivery cost`, `Procurement cost`)) %>%  
    #   select(Year.y, Vcc_cost, type, type_cost) %>% 
    #   rename(`Year`=`Year.y`)
    # 
    # g1 <-  ggplot(plot_stack ,
    #               aes(Vcc_cost, type_cost))+
    #   geom_point(aes(color= type),size=4)+
    #   geom_text_repel(aes(label=mill(type_cost)),min.segment.length = Inf, box.padding = 0.5, size=2.5, fontface="bold")+
    #   scale_y_continuous(labels = addUnits, n.breaks = 5)+
    #   scale_color_manual(values = c("Procurement cost"="#FB262A",
    #                                 "Delivery cost"="#5ab4ac"))+
    #   xlim("70% \n population", "High risk \n population", "Health \n professionals")+
    #   theme_minimal()+
    #   theme(axis.title.y=element_blank(),
    #         axis.title.x = element_blank(),
    #         axis.text.x = element_text(face = "bold", size = 8.5),
    #         legend.title =  element_blank(),
    #         legend.text = element_text(face="bold"),
    #         legend.position = "bottom",
    #         plot.title = element_text(size=11, face="bold", hjust = 0),
    #         plot.title.position = "plot")+
    #   labs(title = paste(country_name, ": Total cost to vaccinate \n","(2020 $US) (M: Million, k: Thousand)"))
    # 
    # g1
    
  }else if (vaccine_cost_cvx!=FALSE){
    
    total_df <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name) %>%
      mutate(DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b),
             PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
             PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
             PrcCst75Pp=ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
                               ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b))),
             VCstHPB=(DelCstHthPrf+ PrcCstHthPrf),
             VCstPRB=(DelCstPpRsk+PrcCstPpRsk),
             VC75PBC=(DelCst75Pp+PrcCst75Pp)) %>% 
      select(Year, VCstHPB, VCstPRB, VC75PBC) %>% 
      rename(`Total cost health professional`= `VCstHPB`,
             `Total cost population at risk`= `VCstPRB`,
             `Total cost 70% population`=`VC75PBC`) %>% 
      gather(Total_type, `Total cost`, -Year) %>%
      mutate(Vcc_cost= case_when(Total_type=="Total cost health professional"~ "Health \n professionals",
                                 Total_type=="Total cost population at risk" ~"High risk \n population",
                                 Total_type=="Total cost 70% population"~"70% \n population")) %>% 
      mutate(`type_cost`="Total cost")
    
    g1 <-  ggplot(total_df ,
                  aes(Vcc_cost, `Total cost`))+
      geom_point(aes(color=`type_cost`), size=4)+
      geom_text_repel(aes(label=mill(`Total cost`)),min.segment.length = Inf, box.padding = 0.5, size=3.5, fontface="bold")+
      scale_y_continuous(labels = addUnits, n.breaks = 5)+
      scale_color_manual(values = c("Total cost"="#FB262A"))+
      xlim("70% \n population", "High risk \n population", "Health \n professionals")+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 8.5),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold"),
            legend.position = "bottom",
            plot.title = element_text(size=11, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste(country_name, ": Total cost to vaccinate \n","(2020 $US) (M: Million, k: Thousand)"))
    
    g1
    
    
    # procurement_df <- world_polygon_nogeom  %>%
    #   filter(nam_lng==country_name) %>%
    #   mutate(VCstDs_b=(VCstDs_b),
    #          NmHthPr=(NmHthPr),
    #          HghRskP=(HghRskP),
    #          num_dos=(num_dos),
    #          PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
    #          PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
    #          PrcCst75Pp= ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
    #                             ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)))) %>%
    #   select(Year,
    #          PrcCstHthPrf, 
    #          PrcCstPpRsk, 
    #          PrcCst75Pp) %>%
    #   rename(`Proc. cost health professional`= `PrcCstHthPrf`,
    #          `Proc. cost population at risk`=`PrcCstPpRsk`,
    #          `Proc. cost  70% population`=`PrcCst75Pp`) %>%
    #   gather(Procurement, `Procurement cost`, matches("Proc"), -Year) %>% 
    #   mutate(Vcc_cost= case_when(Procurement=="Proc. cost health professional"~ "Health \n professionals",
    #                              Procurement=="Proc. cost population at risk" ~"High risk \n population",
    #                              Procurement=="Proc. cost  70% population"~"70% \n population")) 
    # 
    # delivery_df <- 
    #   world_polygon_nogeom  %>%
    #   filter(nam_lng==country_name) %>%
    #   mutate(DlCstDs=(DlCstDs),
    #          NmHthPr=(NmHthPr),
    #          HghRskP=(HghRskP),
    #          num_dos=(num_dos),
    #          DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
    #          DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
    #          DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
    #   select(Year, 
    #          DelCstHthPrf, 
    #          DelCstPpRsk,
    #          DelCst75Pp) %>%
    #   rename(`Delivery cost health professional`= `DelCstHthPrf`,
    #          `Delivery cost population at risk`= `DelCstPpRsk`,
    #          `Delivery cost 70% population`=`DelCst75Pp`) %>%
    #   gather(Delivery, `Delivery cost`, matches("Delivery"), -Year) %>% 
    #   mutate(Vcc_cost= case_when(Delivery=="Delivery cost health professional"~ "Health \n professionals",
    #                              Delivery=="Delivery cost population at risk" ~"High risk \n population",
    #                              Delivery=="Delivery cost 70% population"~"70% \n population")) 
    # 
    # plot_stack <- procurement_df %>% full_join(delivery_df, by = c("Vcc_cost"="Vcc_cost")) %>%
    #   gather(type, type_cost, c(`Delivery cost`, `Procurement cost`)) %>%  
    #   select(Year.y, Vcc_cost, type, type_cost) %>% 
    #   rename(`Year`=`Year.y`)
    # 
    # g1 <-  ggplot(plot_stack ,
    #               aes(Vcc_cost, type_cost))+
    #   geom_point(aes(color= type),size=4)+
    #   geom_text_repel(aes(label=mill(type_cost)),min.segment.length = Inf, box.padding = 0.5, size=2.5, fontface="bold")+
    #   scale_y_continuous(labels = addUnits, n.breaks = 5)+
    #   scale_color_manual(values = c("Procurement cost"="#FB262A",
    #                                 "Delivery cost"="#5ab4ac"))+
    #   xlim("70% \n population", "High risk \n population", "Health \n professionals")+
    #   theme_minimal()+
    #   theme(axis.title.y=element_blank(),
    #         axis.title.x = element_blank(),
    #         axis.text.x = element_text(face = "bold", size = 8.5),
    #         legend.title =  element_blank(),
    #         legend.text = element_text(face="bold"),
    #         legend.position = "bottom",
    #         plot.title = element_text(size=11, face="bold", hjust = 0),
    #         plot.title.position = "plot")+
    #   labs(title = paste(country_name, ": Total cost to vaccinate \n","(2020 $US) (M: Million, k: Thousand)"))
    # 
    # g1
    
  }
}


#Creating immunization plot function=============

imu_cost_plot <- function(country_name_imu,cost_type_vaccine_imu, delivery_cost_imu, cvx_vaccine_cost_imu, blt_vaccine_cost_imu,
                          health_prof_imu, pop_risky_imu, number_vaccine_doses_imu, scale_type_imu){
  
  if(cost_type_vaccine_imu=="Vaccine delivery cost only" & scale_type_imu =="Log" & cvx_vaccine_cost_imu ==FALSE){
    
    delivery_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(DlCstDs=(delivery_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
      select(spendIm, Year,
             DelCstHthPrf, 
             DelCstPpRsk,
             DelCst75Pp) %>%
      rename(`Delivery cost health professional`= `DelCstHthPrf`,
             `Delivery cost population at risk`= `DelCstPpRsk`,
             `Delivery cost 70% population`=`DelCst75Pp`) %>%
      gather(Delivery_type, Delivery_cost, -Year) %>% 
      mutate(Vcc_cost= case_when(Delivery_type=="Delivery cost health professional"~ "Health \n professionals",
                                 Delivery_type=="Delivery cost population at risk" ~"High risk \n population",
                                 Delivery_type=="Delivery cost 70% population"~"70% \n population",
                                 Delivery_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year, ")"))) %>% 
      mutate(Delivery_type=recode(Delivery_type, `Delivery cost health professional`="Delivery cost",
                                  `Delivery cost population at risk`="Delivery cost",
                                  `Delivery cost 70% population`="Delivery cost",
                                  `spendIm`="Immunization cost"))
    
    delivery_df_imu$Delivery_type <- factor(delivery_df_imu$Delivery_type, levels = c("Immunization cost", "Delivery cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", delivery_df_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")
    
    g2 <-  ggplot(delivery_df_imu,
                  aes(Vcc_cost, Delivery_cost))+
      geom_point(aes(color= Delivery_type),size=4)+
      geom_text_repel(aes(label=dollar(Delivery_cost)),min.segment.length = Inf, box.padding = 0.5, size=4, fontface="bold")+
      scale_y_log10(labels = addUnits, n.breaks = 7)+
      scale_x_discrete(limits= positions)+
      scale_color_manual(values = c("Delivery cost"="#5ab4ac",
                                    "Immunization cost"="steelblue"))+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 11),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 11),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))

    g2
    
  } else if(cost_type_vaccine_imu=="Vaccine delivery cost only" & scale_type_imu =="Log" & cvx_vaccine_cost_imu !=FALSE){
    
    delivery_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(DlCstDs=(delivery_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
      select(spendIm, Year,
             DelCstHthPrf, 
             DelCstPpRsk,
             DelCst75Pp) %>%
      rename(`Delivery cost health professional`= `DelCstHthPrf`,
             `Delivery cost population at risk`= `DelCstPpRsk`,
             `Delivery cost 70% population`=`DelCst75Pp`) %>%
      gather(Delivery_type, Delivery_cost, -Year) %>% 
      mutate(Vcc_cost= case_when(Delivery_type=="Delivery cost health professional"~ "Health \n professionals",
                                 Delivery_type=="Delivery cost population at risk" ~"High risk \n population",
                                 Delivery_type=="Delivery cost 70% population"~"70% \n population",
                                 Delivery_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year, ")"))) %>% 
      mutate(Delivery_type=recode(Delivery_type, `Delivery cost health professional`="Delivery cost",
                                  `Delivery cost population at risk`="Delivery cost",
                                  `Delivery cost 70% population`="Delivery cost",
                                  `spendIm`="Immunization cost"))
    
    delivery_df_imu$Delivery_type <- factor(delivery_df_imu$Delivery_type, levels = c("Immunization cost", "Delivery cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", delivery_df_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")
    
    g2 <-  ggplot(delivery_df_imu,
                  aes(Vcc_cost, Delivery_cost))+
      geom_point(aes(color= Delivery_type),size=4)+
      geom_text_repel(aes(label=dollar(Delivery_cost)),min.segment.length = Inf, box.padding = 0.5, size=4, fontface="bold")+
      scale_y_log10(labels = addUnits, n.breaks = 7)+
      scale_x_discrete(limits= positions)+
      scale_color_manual(values = c("Delivery cost"="#5ab4ac",
                                    "Immunization cost"="steelblue"))+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 11),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 11),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))

    g2
    
  }else if(cost_type_vaccine_imu=="Vaccine delivery cost only" & scale_type_imu =="Linear" & cvx_vaccine_cost_imu ==FALSE){
    
    delivery_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(DlCstDs=(delivery_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
      select(spendIm, Year,
             DelCstHthPrf, 
             DelCstPpRsk,
             DelCst75Pp) %>%
      rename(`Delivery cost health professional`= `DelCstHthPrf`,
             `Delivery cost population at risk`= `DelCstPpRsk`,
             `Delivery cost 70% population`=`DelCst75Pp`) %>%
      gather(Delivery_type, Delivery_cost, -Year) %>% 
      mutate(Vcc_cost= case_when(Delivery_type=="Delivery cost health professional"~ "Health \n professionals",
                                 Delivery_type=="Delivery cost population at risk" ~"High risk \n population",
                                 Delivery_type=="Delivery cost 70% population"~"70% \n population",
                                 Delivery_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year, ")"))) %>% 
      mutate(Delivery_type=recode(Delivery_type, `Delivery cost health professional`="Delivery cost",
                                  `Delivery cost population at risk`="Delivery cost",
                                  `Delivery cost 70% population`="Delivery cost",
                                  `spendIm`="Immunization cost"))
    
    delivery_df_imu$Delivery_type <- factor(delivery_df_imu$Delivery_type, levels = c("Immunization cost", "Delivery cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", delivery_df_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")

    g2 <-  ggplot(delivery_df_imu,
                  aes(Vcc_cost, Delivery_cost))+
      geom_point(aes(color= Delivery_type),size=4)+
      geom_text_repel(aes(label=dollar(Delivery_cost)),min.segment.length = Inf, box.padding = 0.5, size=4, fontface="bold")+
      scale_y_continuous(labels = addUnits, n.breaks = 5)+
      scale_x_discrete(limits= positions)+
      scale_color_manual(values = c("Delivery cost"="#5ab4ac",
                                    "Immunization cost"="steelblue"))+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 11),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 11),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))

    g2

  }else if (cost_type_vaccine_imu=="Vaccine delivery cost only" & scale_type_imu =="Linear" & cvx_vaccine_cost_imu !=FALSE){
    
    delivery_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(DlCstDs=(delivery_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
      select(spendIm, Year,
             DelCstHthPrf, 
             DelCstPpRsk,
             DelCst75Pp) %>%
      rename(`Delivery cost health professional`= `DelCstHthPrf`,
             `Delivery cost population at risk`= `DelCstPpRsk`,
             `Delivery cost 70% population`=`DelCst75Pp`) %>%
      gather(Delivery_type, Delivery_cost, -Year) %>% 
      mutate(Vcc_cost= case_when(Delivery_type=="Delivery cost health professional"~ "Health \n professionals",
                                 Delivery_type=="Delivery cost population at risk" ~"High risk \n population",
                                 Delivery_type=="Delivery cost 70% population"~"70% \n population",
                                 Delivery_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year, ")"))) %>% 
      mutate(Delivery_type=recode(Delivery_type, `Delivery cost health professional`="Delivery cost",
                                  `Delivery cost population at risk`="Delivery cost",
                                  `Delivery cost 70% population`="Delivery cost",
                                  `spendIm`="Immunization cost"))
    
    delivery_df_imu$Delivery_type <- factor(delivery_df_imu$Delivery_type, levels = c("Immunization cost", "Delivery cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", delivery_df_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")
    
    g2 <-  ggplot(delivery_df_imu,
                  aes(Vcc_cost, Delivery_cost))+
      geom_point(aes(color= Delivery_type),size=4)+
      geom_text_repel(aes(label=dollar(Delivery_cost)),min.segment.length = Inf, box.padding = 0.5, size=4, fontface="bold")+
      scale_y_continuous(labels = addUnits, n.breaks = 5)+
      scale_x_discrete(limits= positions)+
      scale_color_manual(values = c("Delivery cost"="#5ab4ac",
                                    "Immunization cost"="steelblue"))+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 11),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 11),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))
    
    g2
    
  }else if(cost_type_vaccine_imu=="Vaccine procurement cost only" & scale_type_imu =="Log" & cvx_vaccine_cost_imu==FALSE){
    
    procurement_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(VCstDs_b=(blt_vaccine_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             PrcCstHthPrf=(NmHthPr*VCstDs_b*num_dos),
             PrcCstPpRsk=(tot_pop*HghRskP*VCstDs_b*num_dos),
             PrcCst75Pp= (tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)) %>%
      select(spendIm, Year,
             PrcCstHthPrf, 
             PrcCstPpRsk, 
             PrcCst75Pp) %>%
      rename(`Proc. cost health professional`= `PrcCstHthPrf`,
             `Proc. cost population at risk`=`PrcCstPpRsk`,
             `Proc. cost  70% population`=`PrcCst75Pp`) %>%
      gather(Procurement_type, `Procurement_cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Procurement_type=="Proc. cost health professional"~ "Health \n professionals",
                                 Procurement_type=="Proc. cost population at risk" ~"High risk \n population",
                                 Procurement_type=="Proc. cost  70% population"~"70% \n population",
                                 Procurement_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year, ")"))) %>%  
      mutate(Procurement_type=recode(Procurement_type, `Proc. cost health professional`="Procurement cost",
                                     `Proc. cost population at risk`="Procurement cost",
                                     `Proc. cost  70% population`="Procurement cost",
                                     `spendIm`="Immunization cost"))
    
    procurement_df_imu$Procurement_type <- factor(procurement_df_imu$Procurement_type, levels = c("Immunization cost", "Procurement cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", procurement_df_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")
    
    g2 <-  ggplot(procurement_df_imu,
                  aes(Vcc_cost, Procurement_cost))+
      geom_point(aes(color= Procurement_type),size=4)+
      geom_text_repel(aes(label=dollar(Procurement_cost)),min.segment.length = Inf, box.padding = 0.5, size=4, fontface="bold")+
      scale_y_log10(labels = addUnits, n.breaks = 7)+
      scale_x_discrete(limits= positions)+
      scale_color_manual(values = c("Procurement cost"="orange",
                                    "Immunization cost"="steelblue"))+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 10),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 10),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))
    
    g2
    
  }else if(cost_type_vaccine_imu=="Vaccine procurement cost only" & scale_type_imu =="Log" & cvx_vaccine_cost_imu!=FALSE){
    
    procurement_df_imu <- world_polygon_nogeom %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(VCstDs_b=(blt_vaccine_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
             PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
             PrcCst75Pp=ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
                               ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b))))%>%
      select(spendIm, Year,
             PrcCstHthPrf, 
             PrcCstPpRsk, 
             PrcCst75Pp) %>%
      rename(`Proc. cost health professional`= `PrcCstHthPrf`,
             `Proc. cost population at risk`=`PrcCstPpRsk`,
             `Proc. cost  70% population`=`PrcCst75Pp`) %>%
      gather(Procurement_type, `Procurement_cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Procurement_type=="Proc. cost health professional"~ "Health \n professionals",
                                 Procurement_type=="Proc. cost population at risk" ~"High risk \n population",
                                 Procurement_type=="Proc. cost  70% population"~"70% \n population",
                                 Procurement_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year, ")"))) %>%  
      mutate(Procurement_type=recode(Procurement_type, `Proc. cost health professional`="Procurement cost",
                                     `Proc. cost population at risk`="Procurement cost",
                                     `Proc. cost  70% population`="Procurement cost",
                                     `spendIm`="Immunization cost"))
    
    procurement_df_imu$Procurement_type <- factor(procurement_df_imu$Procurement_type, levels = c("Immunization cost", "Procurement cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", procurement_df_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")

    g2 <-  ggplot(procurement_df_imu,
                  aes(Vcc_cost, Procurement_cost))+
      geom_point(aes(color= Procurement_type),size=4)+
      geom_text_repel(aes(label=dollar(Procurement_cost)),min.segment.length = Inf, box.padding = 0.5, size=4, fontface="bold")+
      scale_y_log10(labels = addUnits, n.breaks = 7)+
      scale_x_discrete(limits= positions)+
      scale_color_manual(values = c("Procurement cost"="orange",
                                    "Immunization cost"="steelblue"))+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 10),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 10),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))
    
    g2
    
  }else if(cost_type_vaccine_imu=="Vaccine procurement cost only" & scale_type_imu =="Linear" & cvx_vaccine_cost_imu==FALSE){
    
    procurement_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(VCstDs_b=(blt_vaccine_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             PrcCstHthPrf=(NmHthPr*VCstDs_b*num_dos),
             PrcCstPpRsk=(tot_pop*HghRskP*VCstDs_b*num_dos),
             PrcCst75Pp= (tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)) %>%
      select(spendIm, Year,
             PrcCstHthPrf, 
             PrcCstPpRsk, 
             PrcCst75Pp) %>%
      rename(`Proc. cost health professional`= `PrcCstHthPrf`,
             `Proc. cost population at risk`=`PrcCstPpRsk`,
             `Proc. cost  70% population`=`PrcCst75Pp`) %>%
      gather(Procurement_type, `Procurement_cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Procurement_type=="Proc. cost health professional"~ "Health \n professionals",
                                 Procurement_type=="Proc. cost population at risk" ~"High risk \n population",
                                 Procurement_type=="Proc. cost  70% population"~"70% \n population",
                                 Procurement_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year, ")"))) %>%  
      mutate(Procurement_type=recode(Procurement_type, `Proc. cost health professional`="Procurement cost",
                                     `Proc. cost population at risk`="Procurement cost",
                                     `Proc. cost  70% population`="Procurement cost",
                                     `spendIm`="Immunization cost"))
    
    procurement_df_imu$Procurement_type <- factor(procurement_df_imu$Procurement_type, levels = c("Immunization cost", "Procurement cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", procurement_df_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")
    
    g2 <-  ggplot(procurement_df_imu,
                  aes(Vcc_cost, Procurement_cost))+
      geom_point(aes(color= Procurement_type),size=4)+
      geom_text_repel(aes(label=dollar(Procurement_cost)),min.segment.length = Inf, box.padding = 0.5, size=4, fontface="bold")+
      scale_y_continuous(labels = addUnits, n.breaks = 5)+
      scale_x_discrete(limits= positions)+
      scale_color_manual(values = c("Procurement cost"="orange",
                                    "Immunization cost"="steelblue"))+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 10),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 10),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))
    
    g2
    
  }else if(cost_type_vaccine_imu=="Vaccine procurement cost only" & scale_type_imu =="Linear" & cvx_vaccine_cost_imu!=FALSE){
    
    procurement_df_imu <- world_polygon_nogeom %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(VCstDs_b=(blt_vaccine_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
             PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
             PrcCst75Pp=ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
                               ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b))))%>%
      select(spendIm, Year,
             PrcCstHthPrf, 
             PrcCstPpRsk, 
             PrcCst75Pp) %>%
      rename(`Proc. cost health professional`= `PrcCstHthPrf`,
             `Proc. cost population at risk`=`PrcCstPpRsk`,
             `Proc. cost  70% population`=`PrcCst75Pp`) %>%
      gather(Procurement_type, `Procurement_cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Procurement_type=="Proc. cost health professional"~ "Health \n professionals",
                                 Procurement_type=="Proc. cost population at risk" ~"High risk \n population",
                                 Procurement_type=="Proc. cost  70% population"~"70% \n population",
                                 Procurement_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year, ")"))) %>%  
      mutate(Procurement_type=recode(Procurement_type, `Proc. cost health professional`="Procurement cost",
                                     `Proc. cost population at risk`="Procurement cost",
                                     `Proc. cost  70% population`="Procurement cost",
                                     `spendIm`="Immunization cost"))
    
    procurement_df_imu$Procurement_type <- factor(procurement_df_imu$Procurement_type, levels = c("Immunization cost", "Procurement cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", procurement_df_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")
    
    g2 <-  ggplot(procurement_df_imu,
                  aes(Vcc_cost, Procurement_cost))+
      geom_point(aes(color= Procurement_type),size=4)+
      geom_text_repel(aes(label=dollar(Procurement_cost)),min.segment.length = Inf, box.padding = 0.5, size=4, fontface="bold")+
      scale_y_continuous(labels = addUnits, n.breaks = 5)+
      scale_x_discrete(limits= positions)+
      scale_color_manual(values = c("Procurement cost"="orange",
                                    "Immunization cost"="steelblue"))+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 10),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 10),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))
    
    g2
    
  }else if(cost_type_vaccine_imu=="Vaccine procurement plus delivery cost" & scale_type_imu =="Log" & cvx_vaccine_cost_imu==FALSE){
    
    delivery_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(DlCstDs=(delivery_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
      select( Year,
              DelCstHthPrf, 
              DelCstPpRsk,
              DelCst75Pp) %>%
      rename(`Delivery cost health professional`= `DelCstHthPrf`,
             `Delivery cost population at risk`= `DelCstPpRsk`,
             `Delivery cost 70% population`=`DelCst75Pp`) %>%
      gather(Delivery_type, `Delivery cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Delivery_type=="Delivery cost health professional"~ "Health \n professionals",
                                 Delivery_type=="Delivery cost population at risk" ~"High risk \n population",
                                 Delivery_type=="Delivery cost 70% population"~"70% \n population")) %>% 
      rename(`type`=`Delivery_type`, `type_cost`=`Delivery cost`) %>% 
      mutate(type=recode(type, `Delivery cost health professional`="Delivery cost",
                         `Delivery cost population at risk`="Delivery cost",
                         `Delivery cost 70% population`="Delivery cost"))
    
    procurement_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(VCstDs_b=(blt_vaccine_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             PrcCstHthPrf=(NmHthPr*VCstDs_b*num_dos),
             PrcCstPpRsk=(tot_pop*HghRskP*VCstDs_b*num_dos),
             PrcCst75Pp= (tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)) %>%
      select(Year,
             PrcCstHthPrf, 
             PrcCstPpRsk, 
             PrcCst75Pp) %>%
      rename(`Proc. cost health professional`= `PrcCstHthPrf`,
             `Proc. cost population at risk`=`PrcCstPpRsk`,
             `Proc. cost  70% population`=`PrcCst75Pp`) %>%
      gather(Procurement_type, `Procurement cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Procurement_type=="Proc. cost health professional"~ "Health \n professionals",
                                 Procurement_type=="Proc. cost population at risk" ~"High risk \n population",
                                 Procurement_type=="Proc. cost  70% population"~"70% \n population")) %>%  
      mutate(Procurement_type=recode(Procurement_type, `Proc. cost health professional`="Procurement cost",
                                     `Proc. cost population at risk`="Procurement cost",
                                     `Proc. cost  70% population`="Procurement cost"))
    
    
    total_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             TtCstDC=(delivery_cost_imu+VCstDs_c),
             TtCstDB=(delivery_cost_imu+blt_vaccine_cost_imu),
             VCstHPB=(NmHthPr*TtCstDB*num_dos),
             VCstPRB=(tot_pop*HghRskP*TtCstDB*num_dos),
             VC75PBC=(tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)) %>% 
      select(spendIm,Year, VCstHPB, VCstPRB, VC75PBC) %>% 
      rename(`Total cost health professional`= `VCstHPB`,
             `Total cost population at risk`= `VCstPRB`,
             `Total cost 70% population`=`VC75PBC`) %>% 
      gather(Total_type, `Total cost`, -Year) %>%
      mutate(Vcc_cost= case_when(Total_type=="Total cost health professional"~ "Health \n professionals",
                                 Total_type=="Total cost population at risk" ~"High risk \n population",
                                 Total_type=="Total cost 70% population"~"70% \n population",
                                 Total_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year,")")))
    
    plot_stack_imu <- procurement_df_imu %>% full_join(total_df_imu, by = c("Vcc_cost"="Vcc_cost")) %>%
      gather(type, type_cost, c(`Total cost`, `Procurement cost`)) %>% 
      select(Year.y, Vcc_cost, type, type_cost) %>% 
      rename(`Year`=`Year.y`)
    
    plot_stack_imu_1 <- full_join(plot_stack_imu, delivery_df_imu) %>% na.omit() 
    
    #recoding the total cost to immunization cost and then adding the df to get final df
    plot_stack_imu_11 <- plot_stack_imu_1 %>% 
      filter(!str_detect(Vcc_cost, "Annual")) 
    
    plot_stack__imu_12 <-plot_stack_imu_1 %>% 
      filter(str_detect(Vcc_cost, "Annual")) %>% 
      mutate(type= recode(type,
                          `Total cost`="Immunization cost")) 
    
    plot_stack_imu_final <- full_join(plot_stack_imu_11, plot_stack__imu_12)
    
    plot_stack_imu_final$type <- factor(plot_stack_imu_final$type, levels = c("Immunization cost", "Total cost", "Procurement cost" ,  "Delivery cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", plot_stack_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")
    
    g2 <-  ggplot(plot_stack_imu_final ,
                  aes(Vcc_cost, type_cost))+
      geom_point(aes(color= type),size=4)+
      geom_text_repel(aes(label=mill(type_cost)),min.segment.length = Inf, box.padding = 0.4, size=4, fontface="bold")+
      scale_y_log10(labels = addUnits, n.breaks = 7)+
      scale_color_manual(values = c("Immunization cost"="steelblue",
                                    "Total cost"="#FB262A",
                                    "Procurement cost"="orange",
                                    "Delivery cost"="#5ab4ac"))+
      scale_x_discrete(limits= positions)+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 10),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 10),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))
    
    g2
    
  }else if(cost_type_vaccine_imu=="Vaccine procurement plus delivery cost" & scale_type_imu =="Log" & cvx_vaccine_cost_imu!=FALSE){
    
    delivery_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(DlCstDs=(delivery_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
      select( Year,
              DelCstHthPrf, 
              DelCstPpRsk,
              DelCst75Pp) %>%
      rename(`Delivery cost health professional`= `DelCstHthPrf`,
             `Delivery cost population at risk`= `DelCstPpRsk`,
             `Delivery cost 70% population`=`DelCst75Pp`) %>%
      gather(Delivery_type, `Delivery cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Delivery_type=="Delivery cost health professional"~ "Health \n professionals",
                                 Delivery_type=="Delivery cost population at risk" ~"High risk \n population",
                                 Delivery_type=="Delivery cost 70% population"~"70% \n population")) %>% 
      rename(`type`=`Delivery_type`, `type_cost`=`Delivery cost`) %>% 
      mutate(type=recode(type, `Delivery cost health professional`="Delivery cost",
                         `Delivery cost population at risk`="Delivery cost",
                         `Delivery cost 70% population`="Delivery cost"))
    
    procurement_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(VCstDs_b=(blt_vaccine_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
             PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
             PrcCst75Pp=ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
                               ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)))) %>%
      select(Year,
             PrcCstHthPrf, 
             PrcCstPpRsk, 
             PrcCst75Pp) %>%
      rename(`Proc. cost health professional`= `PrcCstHthPrf`,
             `Proc. cost population at risk`=`PrcCstPpRsk`,
             `Proc. cost  70% population`=`PrcCst75Pp`) %>%
      gather(Procurement_type, `Procurement cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Procurement_type=="Proc. cost health professional"~ "Health \n professionals",
                                 Procurement_type=="Proc. cost population at risk" ~"High risk \n population",
                                 Procurement_type=="Proc. cost  70% population"~"70% \n population")) %>%  
      mutate(Procurement_type=recode(Procurement_type, `Proc. cost health professional`="Procurement cost",
                                     `Proc. cost population at risk`="Procurement cost",
                                     `Proc. cost  70% population`="Procurement cost"))
    
    total_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             VCstDs_b=(blt_vaccine_cost_imu),
             DlCstDs=(delivery_cost_imu),
             DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b),
             PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
             PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
             PrcCst75Pp=ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
                               ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b))),
             VCstHPB=(DelCstHthPrf + PrcCstHthPrf),
             VCstPRB=(DelCstPpRsk + PrcCstPpRsk),
             VC75PBC=(DelCst75Pp + PrcCst75Pp)) %>% 
      select(spendIm,Year, VCstHPB, VCstPRB, VC75PBC) %>% 
      rename(`Total cost health professional`= `VCstHPB`,
             `Total cost population at risk`= `VCstPRB`,
             `Total cost 70% population`=`VC75PBC`) %>% 
      gather(Total_type, `Total cost`, -Year) %>%
      mutate(Vcc_cost= case_when(Total_type=="Total cost health professional"~ "Health \n professionals",
                                 Total_type=="Total cost population at risk" ~"High risk \n population",
                                 Total_type=="Total cost 70% population"~"70% \n population",
                                 Total_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year,")")))

    plot_stack_imu <- procurement_df_imu %>% full_join(total_df_imu, by = c("Vcc_cost"="Vcc_cost")) %>%
      gather(type, type_cost, c(`Total cost`, `Procurement cost`)) %>% 
      select(Year.y, Vcc_cost, type, type_cost) %>% 
      rename(`Year`=`Year.y`)
    
    plot_stack_imu_1 <- full_join(plot_stack_imu, delivery_df_imu) %>% na.omit() 
    
    #recoding the total cost to immunization cost and then adding the df to get final df
    plot_stack_imu_11 <- plot_stack_imu_1 %>% 
      filter(!str_detect(Vcc_cost, "Annual")) 
    
    plot_stack__imu_12 <-plot_stack_imu_1 %>% 
      filter(str_detect(Vcc_cost, "Annual")) %>% 
      mutate(type= recode(type,
                          `Total cost`="Immunization cost")) 
    
    plot_stack_imu_final <- full_join(plot_stack_imu_11, plot_stack__imu_12)
    
    plot_stack_imu_final$type <- factor(plot_stack_imu_final$type, levels = c("Immunization cost", "Total cost", "Procurement cost" ,  "Delivery cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", plot_stack_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")
    
    g2 <-  ggplot(plot_stack_imu_final ,
                  aes(Vcc_cost, type_cost))+
      geom_point(aes(color= type),size=4)+
      geom_text_repel(aes(label=mill(type_cost)),min.segment.length = Inf, box.padding = 0.4, size=4, fontface="bold")+
      scale_y_log10(labels = addUnits, n.breaks = 7)+
      scale_color_manual(values = c("Immunization cost"="steelblue",
                                    "Total cost"="#FB262A",
                                    "Procurement cost"="orange",
                                    "Delivery cost"="#5ab4ac"))+
      scale_x_discrete(limits= positions)+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 10),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 10),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))
    
    g2
    
  }else if(cost_type_vaccine_imu=="Vaccine procurement plus delivery cost" & scale_type_imu =="Linear" & cvx_vaccine_cost_imu==FALSE){
    delivery_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(DlCstDs=(delivery_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
      select( Year,
              DelCstHthPrf, 
              DelCstPpRsk,
              DelCst75Pp) %>%
      rename(`Delivery cost health professional`= `DelCstHthPrf`,
             `Delivery cost population at risk`= `DelCstPpRsk`,
             `Delivery cost 70% population`=`DelCst75Pp`) %>%
      gather(Delivery_type, `Delivery cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Delivery_type=="Delivery cost health professional"~ "Health \n professionals",
                                 Delivery_type=="Delivery cost population at risk" ~"High risk \n population",
                                 Delivery_type=="Delivery cost 70% population"~"70% \n population")) %>% 
      rename(`type`=`Delivery_type`, `type_cost`=`Delivery cost`) %>% 
      mutate(type=recode(type, `Delivery cost health professional`="Delivery cost",
                         `Delivery cost population at risk`="Delivery cost",
                         `Delivery cost 70% population`="Delivery cost"))
    
    procurement_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(VCstDs_b=(blt_vaccine_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             PrcCstHthPrf=(NmHthPr*VCstDs_b*num_dos),
             PrcCstPpRsk=(tot_pop*HghRskP*VCstDs_b*num_dos),
             PrcCst75Pp= (tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)) %>%
      select(Year,
             PrcCstHthPrf, 
             PrcCstPpRsk, 
             PrcCst75Pp) %>%
      rename(`Proc. cost health professional`= `PrcCstHthPrf`,
             `Proc. cost population at risk`=`PrcCstPpRsk`,
             `Proc. cost  70% population`=`PrcCst75Pp`) %>%
      gather(Procurement_type, `Procurement cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Procurement_type=="Proc. cost health professional"~ "Health \n professionals",
                                 Procurement_type=="Proc. cost population at risk" ~"High risk \n population",
                                 Procurement_type=="Proc. cost  70% population"~"70% \n population")) %>%  
      mutate(Procurement_type=recode(Procurement_type, `Proc. cost health professional`="Procurement cost",
                                     `Proc. cost population at risk`="Procurement cost",
                                     `Proc. cost  70% population`="Procurement cost"))
    
    total_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             TtCstDC=(delivery_cost_imu+VCstDs_c),
             TtCstDB=(delivery_cost_imu+blt_vaccine_cost_imu),
             VCstHPB=(NmHthPr*TtCstDB*num_dos),
             VCstPRB=(tot_pop*HghRskP*TtCstDB*num_dos),
             VC75PBC=(tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)) %>% 
      select(spendIm,Year, VCstHPB, VCstPRB, VC75PBC) %>% 
      rename(`Total cost health professional`= `VCstHPB`,
             `Total cost population at risk`= `VCstPRB`,
             `Total cost 70% population`=`VC75PBC`) %>% 
      gather(Total_type, `Total cost`, -Year) %>%
      mutate(Vcc_cost= case_when(Total_type=="Total cost health professional"~ "Health \n professionals",
                                 Total_type=="Total cost population at risk" ~"High risk \n population",
                                 Total_type=="Total cost 70% population"~"70% \n population",
                                 Total_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year,")")))
    
    plot_stack_imu <- procurement_df_imu %>% full_join(total_df_imu, by = c("Vcc_cost"="Vcc_cost")) %>%
      gather(type, type_cost, c(`Total cost`, `Procurement cost`)) %>% 
      select(Year.y, Vcc_cost, type, type_cost) %>% 
      rename(`Year`=`Year.y`)
    
    plot_stack_imu_1 <- full_join(plot_stack_imu, delivery_df_imu) %>% na.omit() 
    
    #recoding the total cost to immunization cost and then adding the df to get final df
    plot_stack_imu_11 <- plot_stack_imu_1 %>% 
      filter(!str_detect(Vcc_cost, "Annual")) 
    
    plot_stack__imu_12 <-plot_stack_imu_1 %>% 
      filter(str_detect(Vcc_cost, "Annual")) %>% 
      mutate(type= recode(type,
                          `Total cost`="Immunization cost")) 
    
    plot_stack_imu_final <- full_join(plot_stack_imu_11, plot_stack__imu_12)
    
    plot_stack_imu_final$type <- factor(plot_stack_imu_final$type, levels = c("Immunization cost", "Total cost", "Procurement cost" ,  "Delivery cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", plot_stack_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")
    
    g2 <-  ggplot(plot_stack_imu_final ,
                  aes(Vcc_cost, type_cost))+
      geom_point(aes(color= type),size=4)+
      geom_text_repel(aes(label=mill(type_cost)),min.segment.length = Inf, box.padding = 0.4, size=4, fontface="bold")+
      scale_y_continuous(labels = addUnits, n.breaks = 5)+
      scale_color_manual(values = c("Immunization cost"="steelblue",
                                    "Total cost"="#FB262A",
                                    "Procurement cost"="orange",
                                    "Delivery cost"="#5ab4ac"))+
      scale_x_discrete(limits= positions)+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 10),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 10),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))
    
    g2
    
  }else if(cost_type_vaccine_imu=="Vaccine procurement plus delivery cost" & scale_type_imu =="Linear" & cvx_vaccine_cost_imu!=FALSE){
    
    delivery_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(DlCstDs=(delivery_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b)) %>%
      select( Year,
              DelCstHthPrf, 
              DelCstPpRsk,
              DelCst75Pp) %>%
      rename(`Delivery cost health professional`= `DelCstHthPrf`,
             `Delivery cost population at risk`= `DelCstPpRsk`,
             `Delivery cost 70% population`=`DelCst75Pp`) %>%
      gather(Delivery_type, `Delivery cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Delivery_type=="Delivery cost health professional"~ "Health \n professionals",
                                 Delivery_type=="Delivery cost population at risk" ~"High risk \n population",
                                 Delivery_type=="Delivery cost 70% population"~"70% \n population")) %>% 
      rename(`type`=`Delivery_type`, `type_cost`=`Delivery cost`) %>% 
      mutate(type=recode(type, `Delivery cost health professional`="Delivery cost",
                         `Delivery cost population at risk`="Delivery cost",
                         `Delivery cost 70% population`="Delivery cost"))
    
    procurement_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(VCstDs_b=(blt_vaccine_cost_imu),
             NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
             PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
             PrcCst75Pp=ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
                               ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)))) %>%
      select(Year,
             PrcCstHthPrf, 
             PrcCstPpRsk, 
             PrcCst75Pp) %>%
      rename(`Proc. cost health professional`= `PrcCstHthPrf`,
             `Proc. cost population at risk`=`PrcCstPpRsk`,
             `Proc. cost  70% population`=`PrcCst75Pp`) %>%
      gather(Procurement_type, `Procurement cost`, -Year) %>% 
      mutate(Vcc_cost= case_when(Procurement_type=="Proc. cost health professional"~ "Health \n professionals",
                                 Procurement_type=="Proc. cost population at risk" ~"High risk \n population",
                                 Procurement_type=="Proc. cost  70% population"~"70% \n population")) %>%  
      mutate(Procurement_type=recode(Procurement_type, `Proc. cost health professional`="Procurement cost",
                                     `Proc. cost population at risk`="Procurement cost",
                                     `Proc. cost  70% population`="Procurement cost"))
    
    total_df_imu <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name_imu) %>%
      mutate(NmHthPr=(health_prof_imu),
             HghRskP=(pop_risky_imu/100),
             num_dos=(number_vaccine_doses_imu),
             VCstDs_b=(blt_vaccine_cost_imu),
             DlCstDs=(delivery_cost_imu),
             DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b),
             PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
             PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
             PrcCst75Pp=ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
                               ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b))),
             VCstHPB=(DelCstHthPrf+ PrcCstHthPrf),
             VCstPRB=(DelCstPpRsk+PrcCstPpRsk),
             VC75PBC=(DelCst75Pp+PrcCst75Pp)) %>% 
      select(spendIm,Year, VCstHPB, VCstPRB, VC75PBC) %>% 
      rename(`Total cost health professional`= `VCstHPB`,
             `Total cost population at risk`= `VCstPRB`,
             `Total cost 70% population`=`VC75PBC`) %>% 
      gather(Total_type, `Total cost`, -Year) %>%
      mutate(Vcc_cost= case_when(Total_type=="Total cost health professional"~ "Health \n professionals",
                                 Total_type=="Total cost population at risk" ~"High risk \n population",
                                 Total_type=="Total cost 70% population"~"70% \n population",
                                 Total_type=="spendIm"~paste("Annual average spending \n on immunization (in", Year,")")))
    
    
    plot_stack_imu <- procurement_df_imu %>% full_join(total_df_imu, by = c("Vcc_cost"="Vcc_cost")) %>%
      gather(type, type_cost, c(`Total cost`, `Procurement cost`)) %>% 
      select(Year.y, Vcc_cost, type, type_cost) %>% 
      rename(`Year`=`Year.y`)
    
    plot_stack_imu_1 <- full_join(plot_stack_imu, delivery_df_imu) %>% na.omit() 
    
    #recoding the total cost to immunization cost and then adding the df to get final df
    plot_stack_imu_11 <- plot_stack_imu_1 %>% 
      filter(!str_detect(Vcc_cost, "Annual")) 
    
    plot_stack__imu_12 <-plot_stack_imu_1 %>% 
      filter(str_detect(Vcc_cost, "Annual")) %>% 
      mutate(type= recode(type,
                          `Total cost`="Immunization cost")) 
    
    plot_stack_imu_final <- full_join(plot_stack_imu_11, plot_stack__imu_12)
    
    plot_stack_imu_final$type <- factor(plot_stack_imu_final$type, levels = c("Immunization cost", "Total cost", "Procurement cost" ,  "Delivery cost"))
    
    positions <- c(paste("Annual average spending \n on immunization (in", plot_stack_imu$Year[1],")"),"70% \n population", "High risk \n population", "Health \n professionals")
    
    g2 <-  ggplot(plot_stack_imu_final ,
                  aes(Vcc_cost, type_cost))+
      geom_point(aes(color= type),size=4)+
      geom_text_repel(aes(label=mill(type_cost)),min.segment.length = Inf, box.padding = 0.4, size=4, fontface="bold")+
      scale_y_continuous(labels = addUnits, n.breaks = 5)+
      scale_color_manual(values = c("Immunization cost"="steelblue",
                                    "Total cost"="#FB262A",
                                    "Procurement cost"="orange",
                                    "Delivery cost"="#5ab4ac"))+
      scale_x_discrete(limits= positions)+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 10),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold", size = 10),
            legend.position = "bottom",
            plot.title = element_text(size=15, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste("Annual immunization and COVID-19 vaccination cost for", country_name_imu, "(2020 $US) (M: Million, k: Thousand):"))
    
    g2
  }
}    

ui <- tagList(bootstrapPage(
  useShinyjs(),

  navbarPage(title = "", inverse = TRUE, 
             
             tabPanel(title = "COVID-19 Vaccination Cost Map", value = "cmap19",
                      
                      div(class = "outer",
                          tags$head(
                            # Include our custom CSS
                            tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                          ),
                          
                          tags$style("#controls {
                            overflow: auto;
                            max-height: 90vh
                            }"),
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%"),
                          
                          tags$head(
                            # this changes the size of the popovers
                            tags$style(".popover{width:270px;height:325px;}")
                          ),
                          
                          #Floating panel
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, style='background-color: #eaf2f8',
                                        
                                        
                                        tags$h6(tags$i(tags$b("You can click any country on the map to view the total cost to vaccinate and other information based on the currently selected radio button below. The chart below will also update as per your choice
                                            of country on the map."))),
                                        
                                        tags$h6(tags$b(tags$i("Note:")), tags$i("If you want to make changes to the input variables used in the calculations to see how different costs change, please go to", 
                                                                                shinyLink(to="scenario", "Scenario Analysis and Immunization Comparison"),tags$i("tab."))),
                                        
                                        tags$h6(tags$i(("Choose any of the radio buttons below to see the distrbution of respective costs on the map."))),
                                        
                                        div(style = "margin-top:-5px"),
                                        
                                        radioButtons("maptype", list(tags$span(style = "font-weight: bold;font-size: 12px; color:#262626", "Total cost to:")),
                                                     choiceNames =  list(tags$span(style = "font-size: 12px; color:#262626", "Vaccinate 70% population"),
                                                                         tags$span(style = "font-size: 12px; color:#262626", "Vaccinate high risk population"), 
                                                                         tags$span(style = "font-size: 12px; color:#262626", "Vaccinate health professionals")),
                                                     choiceValues = c("Vaccinate 70% population", "Vaccinate high risk population", "Vaccinate health professionals")),
                                        
                                        checkboxInput("cvx_check", h5(tags$span(style="font-size: 12px","Include COVAX procurement pricing"),
                                                                      tags$style(type="text/css", "#q01_1 {vertical-align: top;}"),
                                                                      bsButton("q01_1", label="", icon=icon("question"), style="info", size="extra-small")),
                                                      value = FALSE),
                                        bsPopover(id="q01_1", title="",
                                                  content=paste("Check this box if you want to incorporate COVAX pricing for COVAX-AMC eligible countries.",
                                                                "Checking this box will update the calculation",
                                                                "of total costs to vaccinate 70% population, health professionals, and high-risk population",
                                                                "to include COVAX prices. If this box is left unchecked then only the bilateral",
                                                                "prices will be used in calculating vaccination cost.",
                                                                "Check out the Data Explorer page for the baseline COVAX and bilateral prices used in the calculations."),
                                                  placement = "bottom",
                                                  trigger = "focus",
                                                  options = list(container = "body")
                                        ),
                                        
                                        div(style = "margin-top:-30px"),
                                        
                                        selectInput("country", "",choices = NULL, width = "150px"),
                                        
                                        div(style = "margin-top:-17px"),
                                   
                                        plotOutput("cost_plot", height = "200px", width = "100%")%>% withSpinner(),
                                        
                                        tags$h6(tags$i("For breakdown on Procurement and Delivery costs, please go to",
                                                       shinyLink(to="scenario", "Scenario Analysis and Immunization Comparison"), tags$i("tab."))),
                                        
                                        #tags$p("Visit the", shinyLink(to = "scenario", "Scenario Page"), "to view the magic" ),
                                        
                                        tags$h6(tags$i("*This site is best viewed on Google Chrome, Microsoft Edge, and Microsoft Explorer"))
                                        
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 10, left = 280, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://centerforpolicyimpact.org/', target="_blank",  tags$img(src='CPIGH_Logo.png',height='40',width='120'))),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 10, left = 410, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:10px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://twitter.com/intent/tweet?text=%20Check out the interactive COVID-19 country vaccination cost tracker developed by The Center for Policy Impact in Global Health, based at Duke University, USA:&url= https://centerforpolicyimpact.shinyapps.io/covid-vaccination-costs-analysis/%20%20%20&hashtags=coronavirus @DukeCPIGH @DukeGHI @GYamey @sid_dix @DukeU")))
                          
                      )
             ),
             
             tabPanel(title = "Scenario Analysis and Immunization Comparison", value = "scenario",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          tags$head( tags$style(".well {background-color:#ECF0F1;} "),
                          ),
                          
                          tags$h6(tags$i(tags$b("The chart on the right shows the comparison between average annual immunization
                              cost, and different COVID-19 vaccination costs for the selected country in this panel."))),
                          
                          tags$h6(tags$i(tags$b("Note: You can also input your data for the variables on number of health professionals, population at risk, 
                                                number of doses, bilateral vaccine price, and vaccine delivery cost by clicking on `Click to input data!` button below.
                                                The costs in the chart will automatically update based on your input data. The average annual immunization cost remains fixed as the country has already incurred this cost."))),
                          
                          
                          selectInput("imu_cost_type", h5("Select type of cost borne by the country:",
                                                          tags$style(type="text/css", "#q7 {vertical-align: top;}"),
                                                          bsButton("q7", label="", icon=icon("question"), style="info", size="extra-small")),
                                      choices = c("Vaccine procurement plus delivery cost", "Vaccine procurement cost only",
                                                  "Vaccine delivery cost only"), width = "85%"),
                          
                          bsPopover(id="q7", title="",
                                    content=paste("Please select whether the country you are interested in will incur",
                                                  "only precurement cost or delivery cost, or both vaccine procurement and delivery cost."),
                                    placement = "bottom",
                                    trigger = "focus",
                                    options = list(container = "body")
                          ),
                          
                          div(style = "margin-top:-5px"),
                          
                          checkboxInput("imu_cvx_check", h5("Include COVAX procurement pricing",
                                                            tags$style(type="text/css", "#q07_1 {vertical-align: top;}"),
                                                            bsButton("q07_1", label="", icon=icon("question"), style="info", size="extra-small")),
                                        value = FALSE),
                          bsPopover(id="q07_1", title="",
                                    content=paste("Check this box if you want to incorporate COVAX pricing for COVAX-AMC eligible countries.",
                                                  "Checking this box will update the calculation",
                                                  "of the total costs to vaccinate 70% population, health professionals, and high-risk population",
                                                  "to include the COVAX prices. If this box is left unchecked then only the bilateral",
                                                  "prices will be used in calculating vaccination cost.",
                                                  "Check out the Data Explorer page or the table below the chart for the baseline COVAX and bilateral prices for used in the calculations."),
                                    placement = "bottom",
                                    trigger = "focus",
                                    options = list(container = "body")
                          ),
                          
                          div(style = "margin-top:-20px"),
                          
                          selectInput("imu_country_name", h5("Select a Country",
                                                             tags$style(type="text/css", "#q8 {vertical-align: top;}"),
                                                             bsButton("q8", label="", icon=icon("question"), style="info", size="extra-small")),
                                      choices = NULL, width = "50%"),
                          bsPopover(id="q8", title="",
                                    content=paste("Please select the country for which you want to see the immunization cost, cost to vaccinate",
                                                  "70% population, high-risk population, and all health professionals.",
                                                  "Based on your selection, chart on the right will show the breakdown of different costs."),
                                    placement = "bottom",
                                    trigger = "focus",
                                    options = list(container = "body")
                          ),
             
                          actionButton("imu_button", "Click to input your data!",
                                       icon("paper-plane"),
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          
                          div(style = "margin-top:3px"),
                          
                          
                          hidden(
                            div(id="show_hide",
                                
                                tags$h6(
                                  tags$i("The cost calculations to vaccinate 70% population, health professionals
                                          and high risk population use the following variables. You can change the baseline data for these variables
                                          to update the COVID-19 vacccination cost for the `Country` selected above. Please
                                          read `?` icon shown against each variable to understand which indicator impacts
                                          what cost calculation.")),
                                
                                div(style = "margin-top:-5px"),
                                
                                numericInput("imu_risk_pop", label = h5("High risk population in the country (%):",
                                                                        tags$style(type="text/css", "#q9 {vertical-align: top;}"),
                                                                        bsButton("q9", label="", icon=icon("question"), style="info", size="extra-small")),
                                             min = 0, value = 0, step = 0.1),
                                bsPopover(id="q9", title="",
                                          content=paste("The default value here is the percentage of high risk population",
                                                        "in the selected country based on data by Clark et al. 2020. You can input a different",
                                                        "value to get new cost of vaccinating high-risk population in the selected country.",
                                                        "Changes in the value here will update the vaccination cost for high-risk population in the chart.",
                                                        "The table below the chart will also get updated to the new values."),
                                          placement = "top",
                                          trigger = "focus",
                                          options = list(container = "body")
                                ),
                                
                                numericInput("imu_health_worker", label = h5("Health professionals in the country:",
                                                                             tags$style(type="text/css", "#q10 {vertical-align: top;}"),
                                                                             bsButton("q10", label="", icon=icon("question"), style="info", size="extra-small")),
                                             min = 0, value = 0, step = 1),
                                bsPopover(id="q10", title="",
                                          content=paste("The default value here is the number of health professionals",
                                                        "in the selected country based on our calculations. You can input a different",
                                                        "value to get new cost of vaccinating all health professionals in the selected country.",
                                                        "Changes in the value here will update the vaccination cost for health professionals in the chart.",
                                                        "The table below the chart will also get updated to the new values."),
                                          placement = "top",
                                          trigger = "focus",
                                          options = list(container = "body")
                                ),
                                
                                numericInput("imu_doses_num", label = h5("Number of vaccine doses required:",
                                                                         tags$style(type="text/css", "#q11 {vertical-align: top;}"),
                                                                         bsButton("q11", label="", icon=icon("question"), style="info", size="extra-small")),
                                             min = 0, value = 0, step = 1),
                                bsPopover(id="q11", title="",
                                          content=paste("The default value in the cell is the number doses for the vaccine",
                                                        "that the selected country requires based on our calculations.",
                                                        "You can input a different value to see how it will impact the vaccination cost in the country.",
                                                        "Changes in the value here will update all the costs in the chart.",
                                                        "The table below the chart will also get updated to the new values."),
                                          placement = "top",
                                          trigger = "focus",
                                          options = list(container = "body")
                                ),
                                
                                numericInput("imu_Vac_bilateral",label = h5("Procurement cost per dose (2020 $US):",
                                                                            tags$style(type="text/css", "#q12_2 {vertical-align: top;}"),
                                                                            bsButton("q12_2", label="", icon=icon("question"), style="info", size="extra-small")),
                                             min = 0, value = 0, step = .01),
                                bsPopover(id="q12_2", title="",
                                          content=paste("The default value here is the bilateral procurement cost of vaccine per dose",
                                                        "in the selected country based on our calculations. You can input a different",
                                                        "value to get new vaccination costs for the country. Changes in the value here",
                                                        "will update all costs in the chart. The table below the chart will also get updated to the new values."),
                                          placement = "top",
                                          trigger = "focus",
                                          options = list(container = "body")
                                ),
                                
                                numericInput("imu_delivery_vaccine", label = h5("Delivery cost per dose (2020 $US):",
                                                                                tags$style(type="text/css", "#q13 {vertical-align: top;}"),
                                                                                bsButton("q13", label="", icon=icon("question"), style="info", size="extra-small")),
                                             min = 0, value = 0, step = 0.01),
                                bsPopover(id="q13", title="",
                                          content=paste("The default value here is the delivery cost of vaccine per dose",
                                                        "in the selected country based on our calculations. You can input a", 
                                                        "different value to get new vaccination costs for the selected country.",
                                                        "Changes in the value here will update all the costs in the chart.",
                                                        "The table below the chart will also get updated to the new values."),
                                          placement = "top",
                                          trigger = "focus",
                                          options = list(container = "body")
                                ),
                                
                            )
                          )
                        ),
                        mainPanel(
                          
                          plotOutput("imu_cost_plot")%>% withSpinner(),
                          div(style = "margin-top:-35px"),
                          radioGroupButtons("imu_scales", label = "", choices = c("Linear", "Log"), selected = "Linear", size="xs"),
                          
                          #tags$br(),
                          tags$br(),
                          tableOutput("user_table"))
                      )
             ),
             
             tabPanel(title = "Data Explorer", value = "data",
                      fluidPage(
                        checkboxInput("show_rownames",
                                      label = "Show rownames?"),
                        tags$h5(tags$i(tags$b("Note: Click on `+` symbol to view more information about the country.
                                               If you do not see `+` symbol then you are viewing the complete data for the country."))),
                        tags$br(),
                        DTOutput("data_explorer"))
             ),
              
             tabPanel(title = "About This Site", value = "about",
                      tags$div(
                        tags$br(),tags$h4(tags$b("Background")),
                        "Safe and effective vaccines are essential to ending the COVID-19 pandemic. 
                        However, the development and production of vaccines without subsequent vaccinations in the population achieves zero impact on pandemic control. 
                        Ending the pandemic necessitates effective vaccination programs in all countries across the world. 
                        Evidence on COVID-19 vaccination cost is paramount to inform effective financing, planning, and implementation of these programs by policymakers and funders. 
                        But, this information is not readily available, especially for low-and-middle-income countries (LMICs). 
                        The goal of this platform is to provide national-level cost estimates of COVID-19 vaccinations for LMICs under varying population coverage scenarios. 
                        Because of the rapid changing nature of the pandemic and the uncertainty around our primary input variables, we provide the users with the ability to change the input variables for target populations, vaccine procurement and delivery prices.",
                        tags$br(),tags$br(),tags$h4(tags$b("Code")),
                        "Code and input data used to generate this tool are available on", a(href="https://github.com/pyarasid/COVID-vaccination-Costs-Analysis", target="_blank" ,  "Github."),
                        tags$br(),tags$br(),tags$h4(tags$b("Selected Primary Data References")),
                        "1- Clark A, Jit M, Warren-Gash C, et al. Global, regional, and national estimates of the population at increased risk of severe COVID-19 due to underlying health conditions in 2020: a modelling study. The Lancet Global Health 2020; 8(8): e1003-e17.",tags$br(),
                        "2- Portnoy A, Vaughan K, Clarke-Deelder E, et al. Producing standardized country-level immunization delivery unit cost estimates. PharmacoEconomics 2020; 38(9): 995-1005.",tags$br(),
                        "3- WHO. THE GLOBAL HEALTH OBSERVATORY. June 23, 2020.", a(href="https://www.who.int/data/gho/data/themes/topics/health-workforce",target="_blank" , "https://www.who.int/data/gho/data/themes/topics/health-workforce"),tags$br(),
                        "4- WHO. WHO SAGE values framework for the allocation and prioritization of COVID-19 vaccination. September 14, 2020.",tags$br(),
                        tags$br(),tags$h4(tags$b("Contributors")),
                        a(href="https://globalhealth.duke.edu/people/dixit-siddharth", target="_blank" ,"Siddharth Dixit,"), "Associate in Research, The Center for Policy Impact in Global Health, Duke University",tags$br(),
                        a(href="https://globalhealth.duke.edu/people/diab-mohamed-mustafa", target="_blank",  "Mohamed Mustafa Diab,"), "Policy Associate, The Center for Policy Impact in Global Health, Duke University",tags$br(),
                        "Armand Zimmerman, Research Assistant, The Center for Policy Impact in Global Health, Duke University",tags$br(),
                        a(href="https://globalhealth.duke.edu/people/ogbuoji-osondu", target="_blank", "Osondu Ogbuoji,"),  "Assistant Research Professor, The Center for Policy Impact in Global Health, Duke University",tags$br(),
                        a(href= "https://globalhealth.duke.edu/people/yamey-gavin", target="_blank", "Gavin Yamey,"), "Director, The Center for Policy Impact in Global Health, Duke University",tags$br(),tags$br(),
                        tags$h4(tags$b("Contact")),"cpigh@duke.edu",tags$br(),tags$br(),
                        tags$a(href='https://centerforpolicyimpact.org/', target="_blank" , tags$img(src = "CPIGH_Logo.png", width = "400px")),
                        tags$a(href='https://globalhealth.duke.edu/', target="_blank",tags$img(src = "dghi_logo.jpg", width = "250px"))
                      )
             )
  ),
  tags$script(src = "shinyLink.js")
)
)

server <- function(input, output, session) {
  
  updateSelectInput(session,
                    "country", choices = world_polygon_app$nam_lng)
  
  #leaflet basemap
  output$map <- renderLeaflet({
    leaflet(world_polygon_app) %>% addProviderTiles(providers$CartoDB) %>% 
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addSearchOSM(options = searchOptions(hideMarkerOnCollapse = TRUE)) %>% 
      addResetMapButton()  
  })
  
  #plot function
  output$cost_plot <- renderPlot({
    cost_plot(input$country,  input$cvx_check)
  })
  
  filtered_world_polygon <- reactive({
    
    order_of_75per <- c("Below 65 Million","65 - 145 Million", "145 - 275 Million",
                        "275 - 650 Million", "Above 650 Million")
    order_of_risk <- c("Below 4 Million", "4 - 8 Million", "8 - 20 Million", 
                       "20 - 45 Million", "Above 45 Million")
    order_of_health <- c("Below 200 Thousand", "200 - 700 Thousand", "700 Thousand - 2 Million",
                         "2 - 6 Million",
                         "Above 6 Million")
    
    if(input$cvx_check==FALSE){
      
      country_polygon <- world_polygon_app %>%
        filter(nam_lng==input$country) %>%
        mutate(TtCstDB=(DlCstDs+VCstDs_b),
               VCstHPB=(NmHthPr*TtCstDB*num_dos),
               VCstPRB=(tot_pop*HghRskP*TtCstDB*num_dos),
               VC75PBC= (tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)) 
      
      remaining_polygon <- world_polygon_app %>%
        filter(nam_lng!=input$country) %>% 
        mutate(TtCstDB=(DlCstDs+VCstDs_b),
               VCstHPB=(NmHthPr*TtCstDB*num_dos),
               VCstPRB=(tot_pop*HghRskP*TtCstDB*num_dos),
               VC75PBC= (tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)) 
      
      new_updated_polygon <- rbind(remaining_polygon, country_polygon) %>%
        mutate(cst_rng_75= case_when(VC75PBC<=65000000~ "Below 65 Million",
                                     VC75PBC>65000000 & VC75PBC<=145000000~"65 - 145 Million",
                                     VC75PBC>145000000 & VC75PBC<=275000000~"145 - 275 Million",
                                     VC75PBC>275000000 & VC75PBC<=650000000~"275 - 650 Million",
                                     VC75PBC>650000000~"Above 650 Million")) %>%
        mutate(cst_rng_75=factor(cst_rng_75, levels =  order_of_75per)) %>%
        mutate(cst_rng_rsk=case_when(VCstPRB<=4000000~ "Below 4 Million",
                                     VCstPRB>4000000 & VCstPRB<=8000000~"4 - 8 Million",
                                     VCstPRB>8000000 & VCstPRB<=20000000~"8 - 20 Million",
                                     VCstPRB>20000000 & VCstPRB<=45000000~"20 - 45 Million",
                                     VCstPRB>45000000~"Above 45 Million")) %>%
        mutate(cst_rng_rsk=factor(cst_rng_rsk, levels =  order_of_risk)) %>%
        mutate(cst_rng_hlth=case_when(VCstHPB<=200000~ "Below 200 Thousand",
                                      VCstHPB>200000 & VCstHPB<=700000~"200 - 700 Thousand",
                                      VCstHPB>700000 & VCstHPB<=2000000~"700 Thousand - 2 Million",
                                      VCstHPB>2000001 & VCstHPB<=6000000~"2 - 6 Million",
                                      VCstHPB>6000000~"Above 6 Million")) %>%
        mutate(cst_rng_hlth=factor(cst_rng_hlth, levels =  order_of_health))
      
      return (new_updated_polygon)
      
    }else{
      
      order_of_75per <- c("Below 65 Million","65 - 145 Million", "145 - 275 Million",
                          "275 - 650 Million", "Above 650 Million")
      order_of_risk <- c("Below 4 Million", "4 - 8 Million", "8 - 20 Million", 
                         "20 - 45 Million", "Above 45 Million")
      order_of_health <- c("Below 200 Thousand", "200 - 700 Thousand", "700 Thousand - 2 Million",
                           "2 - 6 Million",
                           "Above 6 Million")
      
      country_polygon <- world_polygon_app %>%
        filter(nam_lng==input$country) %>% 
        mutate(TtCstDC=(DlCstDs+VCstDs_c),
               TtCstDB=(DlCstDs+VCstDs_b),
               VCstHPB=ifelse(Cvx_lgb=="Yes",(NmHthPr*TtCstDC*num_dos),(NmHthPr*TtCstDB*num_dos)),
               VCstPRB=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*TtCstDC*num_dos), (tot_pop*HghRskP*TtCstDB*num_dos)),
               VC75PBC=ifelse(Cvx_lgb=="Yes", ((tot_pop*TtCstDC*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)),
                              ((tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b))))
      
      remaining_polygon <- world_polygon_app %>% 
        filter(nam_lng!=input$country)%>% 
        mutate(TtCstDC=(DlCstDs+VCstDs_c),
               TtCstDB=(DlCstDs+VCstDs_b),
               VCstHPB=ifelse(Cvx_lgb=="Yes",(NmHthPr*TtCstDC*num_dos),(NmHthPr*TtCstDB*num_dos)),
               VCstPRB=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*TtCstDC*num_dos), (tot_pop*HghRskP*TtCstDB*num_dos)),
               VC75PBC=ifelse(Cvx_lgb=="Yes", ((tot_pop*TtCstDC*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)),
                              ((tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b))))
      
      new_updated_polygon <- rbind(remaining_polygon, country_polygon) %>% 
        mutate(cst_rng_75= case_when(VC75PBC<=65000000~ "Below 65 Million",
                                     VC75PBC>65000000 & VC75PBC<=145000000~"65 - 145 Million",
                                     VC75PBC>145000000 & VC75PBC<=275000000~"145 - 275 Million",
                                     VC75PBC>275000000 & VC75PBC<=650000000~"275 - 650 Million",
                                     VC75PBC>650000000~"Above 650 Million")) %>% 
        mutate(cst_rng_75=factor(cst_rng_75, levels =  order_of_75per)) %>% 
        mutate(cst_rng_rsk=case_when(VCstPRB<=4000000~ "Below 4 Million",
                                     VCstPRB>4000000 & VCstPRB<=8000000~"4 - 8 Million",
                                     VCstPRB>8000000 & VCstPRB<=20000000~"8 - 20 Million",
                                     VCstPRB>20000000 & VCstPRB<=45000000~"20 - 45 Million",
                                     VCstPRB>45000000~"Above 45 Million")) %>% 
        mutate(cst_rng_rsk=factor(cst_rng_rsk, levels =  order_of_risk)) %>% 
        mutate(cst_rng_hlth=case_when(VCstHPB<=200000~ "Below 200 Thousand",
                                      VCstHPB>200000 & VCstHPB<=700000~"200 - 700 Thousand",
                                      VCstHPB>700000 & VCstHPB<=2000000~"700 Thousand - 2 Million",
                                      VCstHPB>2000001 & VCstHPB<=6000000~"2 - 6 Million",
                                      VCstHPB>6000000~"Above 6 Million")) %>% 
        mutate(cst_rng_hlth=factor(cst_rng_hlth, levels =  order_of_health))
      
      return (new_updated_polygon)
    }
  })
  
  factpal <- reactive({
    colorFactor("Blues", filtered_world_polygon()$cst_rng_75)
  })
  
  
  factpal_pop <- reactive({
    colorFactor("Oranges", filtered_world_polygon()$cst_rng_rsk)
    
  })
  
  factpal_health <- reactive({
    colorFactor("Greens", filtered_world_polygon()$st_rng_hlth)
  })
  
  my_poup_70p <- reactive({
    
    if(input$cvx_check==FALSE){
      
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng,Cvx_lgb, num_dos,VCstDs_b,
                      DlCstDs, VC75PBC) %>% 
        dplyr::mutate(VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VC75PBC= dollar_format()(VC75PBC)) %>% 
        dplyr::rename(Country= nam_lng,
                      `COVAX eligibility`=Cvx_lgb, 
                      `Number of doses required`= num_dos,
                      `Vaccine price per dose (Bilateral deal)`= VCstDs_b,
                      `Vaccine delivery cost per dose`= DlCstDs,
                      `Total cost to vaccinate 70% population` = (VC75PBC))
      
    }else if (input$cvx_check!=FALSE){
      
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng, Cvx_lgb, num_dos, VCstDs_c, VCstDs_b,
                      DlCstDs, VC75PBC) %>% 
        dplyr::mutate(cvx_price70p=ifelse(Cvx_lgb=="Yes", VCstDs_c, "Not eligible")) %>% 
        dplyr::mutate(VCstDs_c= dollar_format()(VCstDs_c),
                      VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VC75PBC= dollar_format()(VC75PBC)) %>% 
        dplyr::rename(Country= nam_lng,
                      `Number of doses required`= num_dos,
                      `COVAX eligibility`=Cvx_lgb, 
                      `Vaccine price per dose (COVAX)`= cvx_price70p,
                      `Vaccine price per dose (Bilateral deal)`= VCstDs_b,
                      `Vaccine delivery cost per dose`= DlCstDs,
                      `Total cost to vaccinate 70% population` = (VC75PBC)) %>% 
        dplyr::select(-VCstDs_c)
      
    }
    
  })
  
  my_poup_risky <- reactive({
    
    if(input$cvx_check==FALSE){
      
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng,Cvx_lgb, num_dos,  VCstDs_b,
                      DlCstDs, VCstPRB) %>% 
        dplyr::mutate(VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VCstPRB= dollar_format()(VCstPRB)) %>% 
        dplyr::rename(Country= nam_lng,
                      `COVAX eligibility`=Cvx_lgb, 
                      `Number of doses required`= num_dos,
                      `Vaccine price per dose (Bilateral deal)`= VCstDs_b,
                      `Vaccine delivery cost per dose`= DlCstDs,
                      `Total cost to vaccinate population at risk` = (VCstPRB))
      
    }else if (input$cvx_check!=FALSE){
      
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng, Cvx_lgb, num_dos, VCstDs_c, VCstDs_b,
                      DlCstDs, VCstPRB) %>% 
        dplyr::mutate(cvx_pricersk=ifelse(Cvx_lgb=="Yes", VCstDs_c, "Not eligible")) %>% 
        dplyr::mutate(VCstDs_c= dollar_format()(VCstDs_c),
                      VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VCstPRB= dollar_format()(VCstPRB)) %>% 
        dplyr::rename(Country= nam_lng,
                      `COVAX eligibility`=Cvx_lgb, 
                      `Number of doses required`= num_dos,
                      `Vaccine price per dose (COVAX)`= cvx_pricersk,
                      `Vaccine price per dose (Bilateral deal)`= VCstDs_b,
                      `Vaccine delivery cost per dose`= DlCstDs,
                      `Total cost to vaccinate population at risk` = (VCstPRB)) %>% 
        dplyr::select(-VCstDs_c)
      
    }
  })
  
  my_poup_health <- reactive({
    
    if(input$cvx_check==FALSE){
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng,Cvx_lgb, num_dos, VCstDs_b,
                      DlCstDs, VCstHPB) %>% 
        dplyr::mutate(VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VCstHPB= dollar_format()(VCstHPB)) %>% 
        dplyr::rename(Country= nam_lng,
                      `COVAX eligibility`=Cvx_lgb, 
                      `Number of doses required`= num_dos,
                      `Vaccine price per dose (Bilateral deal)`= VCstDs_b,
                      `Vaccine delivery cost per dose`= DlCstDs,
                      `Total cost to vaccinate health professionals` = (VCstHPB)) 
      
    }else if(input$cvx_check!=FALSE){
      
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng,Cvx_lgb, num_dos, VCstDs_c, VCstDs_b,
                      DlCstDs, VCstHPB) %>% 
        dplyr::mutate(cvx_pricehlth=ifelse(Cvx_lgb=="Yes", VCstDs_c, "Not eligible")) %>% 
        dplyr::mutate(VCstDs_c= dollar_format()(VCstDs_c),
                      VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VCstHPB= dollar_format()(VCstHPB)) %>% 
        dplyr::rename(Country= nam_lng,
                      `COVAX eligibility`=Cvx_lgb, 
                      `Number of doses required`= num_dos,
                      `Vaccine price per dose (COVAX)`= cvx_pricehlth,
                      `Vaccine price per dose (Bilateral deal)`= VCstDs_b,
                      `Vaccine delivery cost per dose`= DlCstDs,
                      `Total cost to vaccinate health professionals` = (VCstHPB)) %>% 
        dplyr::select(-VCstDs_c)
    }
  })
  
  observe({
    if(input$maptype=="Vaccinate 70% population" & input$cvx_check==FALSE){
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>%
          clearShapes() %>%
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal()(cst_rng_75),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_70p(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>%
          clearControls() %>%
          addLegend("bottomleft", pal = factpal(), values =~na.omit(cst_rng_75), opacity = 1,
                    title = "Cost to vaccinate<br>70% population (2020 $US)")
      })
    }
    
    if (input$maptype=="Vaccinate 70% population" & input$cvx_check!=FALSE){
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>%
          clearShapes() %>%
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal()(cst_rng_75),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_70p(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>%
          clearControls() %>%
          addLegend("bottomleft", pal = factpal(), values =~na.omit(cst_rng_75), opacity = 1,
                    title = "Cost to vaccinate<br>70% population (2020 $US)")
      })
      
    }
    
    if (input$maptype=="Vaccinate high risk population"& input$cvx_check==FALSE){
      
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>% 
          clearShapes() %>% 
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal_pop()(cst_rng_rsk),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_risky(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>% 
          clearControls() %>% 
          addLegend("bottomleft", pal =  factpal_pop(), values =~na.omit(cst_rng_rsk), opacity = 1,
                    title = "Cost to vaccinate high<br> risk population (2020 $US)")
      })
    }
    
    if (input$maptype=="Vaccinate high risk population"& input$cvx_check!=FALSE){
      
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>% 
          clearShapes() %>% 
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal_pop()(cst_rng_rsk),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_risky(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>% 
          clearControls() %>% 
          addLegend("bottomleft", pal =  factpal_pop(), values =~na.omit(cst_rng_rsk), opacity = 1,
                    title = "Cost to vaccinate high<br> risk population (2020 $US)")
      })
    }
    
    if(input$maptype=="Vaccinate health professionals"& input$cvx_check==FALSE){
      
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>% 
          clearShapes() %>% 
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal_health()(cst_rng_hlth),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_health(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>% 
          clearControls() %>% 
          addLegend("bottomleft", pal = factpal_health(), values =~na.omit(cst_rng_hlth), opacity = 1,
                    title = "Cost to vaccinate<br>health professionals (2020 $US)")
        
      })
    }
    if(input$maptype=="Vaccinate health professionals"& input$cvx_check!=FALSE){
      
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>% 
          clearShapes() %>% 
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal_health()(cst_rng_hlth),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_health(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>% 
          clearControls() %>% 
          addLegend("bottomleft", pal = factpal_health(), values =~na.omit(cst_rng_hlth), opacity = 1,
                    title = "Cost to vaccinate<br>health professionals (2020 $US)")
        
      })
      
    }
  })
  
  observeEvent(input$map_shape_click, {
    if(input$maptype=="Vaccinate 70% population"){
      
      event <- input$map_shape_click
      
      sub <-   filtered_world_polygon()[filtered_world_polygon()$nam_lng==event$id, c("nam_lng", "HghRskP", "NmHthPr",
                                                                                      "num_dos", "VCstDs_b", "VCstDs_b")]
      
      if (is.null(event)){
        updateSelectInput(session,
                          "country", choices = filtered_world_polygon()$nam_lng)
        
      }else{
        updateSelectInput(session, inputId = "country", selected = sub$nam_lng)
      }
      
    }
  })
  
  observeEvent(input$map_shape_click,{
    if(input$maptype=="Vaccinate high risk population"){
      
      event <- input$map_shape_click
      
      sub <-   filtered_world_polygon()[filtered_world_polygon()$nam_lng==event$id, c("nam_lng", "HghRskP", "NmHthPr", 
                                                                                      "num_dos", "VCstDs_b", "VCstDs_b")]
      if (is.null(event)){
        updateSelectInput(session,
                          "country", choices = filtered_world_polygon()$nam_lng)
      }else{
        updateSelectInput(session, inputId = "country", selected = sub$nam_lng)
      }
      
    }
  })
  
  observeEvent(input$map_shape_click, {
    if(input$maptype=="Vaccinate health professionals"){
      
      event <- input$map_shape_click
      
      sub <-   filtered_world_polygon()[filtered_world_polygon()$nam_lng==event$id, c("nam_lng", "HghRskP", "NmHthPr", 
                                                                                      "num_dos", "VCstDs_b", "VCstDs_b")]
      if (is.null(event)){
        updateSelectInput(session,
                          "country", choices = filtered_world_polygon()$nam_lng)
      }else{
        updateSelectInput(session, inputId = "country", selected = sub$nam_lng)
      }
      
    }
  })
  
  long_lat <- reactive({
    subset(filtered_world_polygon(), nam_lng==input$country,
           select=c(long, lat))
  })
  
  observe({
    leafletProxy("map", data = filtered_world_polygon()) %>% 
      setView(lng = long_lat()$long, lat =long_lat()$lat ,zoom = 3.3)
  })
  
  #Rendering the data table===== 
  output$data_explorer <- renderDT({
    baseline_data <- read_xlsx("baseline_data.xlsx") %>% 
      datatable(rownames = input$show_rownames,
                extensions = "Responsive")
  })
  
  #Immunization tab==========
  
  updateSelectInput(session, "imu_country_name", choices = world_polygon_app$nam_lng)
  
  observe({
    country_selected <- input$imu_country_name
    
    #get the value of risky population in the input for the country selected
    risk_value <- world_polygon_nogeom  %>%
      filter(nam_lng==country_selected) %>%
      select(HghRskP)
    
    updateNumericInput(session, "imu_risk_pop", value = (risk_value[1,1]*100))
    
    #get the value health professional in the input for the country selected
    professional_value <- world_polygon_nogeom  %>%
      filter(nam_lng==country_selected) %>%
      select(NmHthPr)
    
    updateNumericInput(session, "imu_health_worker", value = round(professional_value[1,1],0))
    
    #get the value of number of vaccine doses in the input for the selected country
    doses_value <- world_polygon_nogeom  %>%
      filter(nam_lng==country_selected) %>%
      select(num_dos)
    
    updateNumericInput(session, "imu_doses_num", value = round(doses_value[1,1],0))
    
  })
  
  observe({
    country_selected <- input$imu_country_name
    
    if (input$imu_cost_type=="Vaccine delivery cost only"){
      #get the value delivery cost in the input for the country selected
      delivery_value <- world_polygon_nogeom  %>%
        filter(nam_lng==country_selected) %>%
        select(DlCstDs)
      
      updateNumericInput(session, "imu_delivery_vaccine", value = round(delivery_value[1,1], 2))
    }
  })
  
  observe({
    country_selected <- input$imu_country_name
    
    if (input$imu_cost_type=="Vaccine procurement cost only"){
      
      
      #get the value of bilateral vaccine cost in the input for the country selected
      vaccine_value_blt <- world_polygon_nogeom  %>%
        filter(nam_lng==country_selected) %>%
        select(VCstDs_b)
      
      updateNumericInput(session, "imu_Vac_bilateral", value = vaccine_value_blt[1,1])
      
      
    }
  })
  
  observe({
    country_selected <- input$imu_country_name
    
    if(input$imu_cost_type=="Vaccine procurement plus delivery cost"){
      
      #get the value of delivery cost in the input for the country selected
      delivery_value <- world_polygon_nogeom  %>%
        filter(nam_lng==country_selected) %>%
        select(DlCstDs)
      
      updateNumericInput(session, "imu_delivery_vaccine", value = round(delivery_value[1,1], 2))
      
      
      #get the value of bilateral vaccine cost in the input for the country selected
      vaccine_value_blt <- world_polygon_nogeom  %>%
        filter(nam_lng==country_selected) %>%
        select(VCstDs_b)
      
      updateNumericInput(session, "imu_Vac_bilateral", value = vaccine_value_blt[1,1])
      
      
    }
  })
  
  observe({
    if (input$imu_cost_type=="Vaccine procurement plus delivery cost"){
      
      show("imu_Vac_bilateral")
      show("imu_delivery_vaccine")
    }
  })
  
  observe({
    if (input$imu_cost_type=="Vaccine procurement cost only"){
      
      hide("imu_delivery_vaccine")
      show("imu_Vac_bilateral")
    }
  })
  
  observe({
    if (input$imu_cost_type=="Vaccine delivery cost only"){
      
      hide("imu_Vac_bilateral")
      show("imu_delivery_vaccine")
    }
  })
  
  #subsetting the table to show below the chart 
  table_imu <- reactive({
    
    if (input$imu_cvx_check==FALSE){
 
      df_table  <-  world_polygon_nogeom %>%
        dplyr::filter(nam_lng==input$imu_country_name) %>%
        dplyr::mutate(HghRskP=input$imu_risk_pop,
                      NmHthPr=input$imu_health_worker,
                      num_dos=input$imu_doses_num,
                      VCstDs_b=input$imu_Vac_bilateral,
                      DlCstDs=input$imu_delivery_vaccine) %>%
        dplyr::select(nam_lng,Cvx_lgb, HghRskP, NmHthPr, num_dos, VCstDs_b, DlCstDs) %>%
        dplyr::mutate(NmHthPr= comma_format()(NmHthPr),
                      DlCstDs=comma_format(accuracy = .01)(DlCstDs)) %>%
        dplyr::rename(`Country`=nam_lng,
                      `COVAX eligibility`=Cvx_lgb,
                      `Population at risk (%)`= HghRskP,
                      `No. of Health Professionals`= NmHthPr,
                      `No. of doses required`=num_dos,
                      `Vaccine price per dose (Bilateral, US$)`=VCstDs_b,
                      `Delivery cost per dose (US$)`= DlCstDs)
      
      
    }else if(input$imu_cvx_check!=FALSE){

      
      df_table  <-  world_polygon_nogeom %>%
        dplyr::filter(nam_lng==input$imu_country_name) %>%
        dplyr::mutate(HghRskP=input$imu_risk_pop,
                      NmHthPr=input$imu_health_worker,
                      num_dos=input$imu_doses_num,
                      VCstDs_b=input$imu_Vac_bilateral,
                      DlCstDs=input$imu_delivery_vaccine) %>%
        dplyr::select(nam_lng,Cvx_lgb, HghRskP, NmHthPr, num_dos, VCstDs_c, VCstDs_b, DlCstDs) %>%
        dplyr::mutate(cvx_price=ifelse(Cvx_lgb=="Yes", VCstDs_c, "Not eligible")) %>% 
        dplyr::mutate(NmHthPr= comma_format()(NmHthPr),
                      DlCstDs=comma_format(accuracy = .01)(DlCstDs)) %>%
        dplyr::rename(`Country`=nam_lng,
                      `COVAX eligibility`=Cvx_lgb,
                      `Population at risk (%)`= HghRskP,
                      `No. of Health Professionals`= NmHthPr,
                      `No. of doses required`=num_dos,
                      `Vaccine price per dose (COVAX, US$)` = cvx_price,
                      `Vaccine price per dose (Bilateral, US$)`=VCstDs_b,
                      `Delivery cost per dose (US$)`= DlCstDs) %>% 
        dplyr::select(-VCstDs_c)
      
      }
    
  })
  
  output$user_table <- function(){
    table_imu() %>%
      knitr::kable("html", align='r') %>%
      kable_styling("striped", full_width = F)
  }
  
  observeEvent(input$imu_button, {
    toggle('show_hide')
  })
  
  output$imu_cost_plot <- renderPlot({
    imu_cost_plot(input$imu_country_name,input$imu_cost_type, input$imu_delivery_vaccine, input$imu_cvx_check, input$imu_Vac_bilateral,
                  input$imu_health_worker, input$imu_risk_pop, input$imu_doses_num, input$imu_scales)
  })
  
}

shinyApp(ui, server)