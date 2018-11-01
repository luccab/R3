
library(ggplot2)
library(shiny)
library(readxl)

library(maps)

us <- map_data("state")
AssignmentB110_2 <- read_excel("AssignmentB110-2.xlsx", 
                               sheet = "Sheet1", skip = 1)

Prison <- AssignmentB110_2

newdata <- subset(Prison, `Monthly Electronics Spend` < 20 & `Technology Adoption` == 'early')
newdata1 <- subset(Prison, `Monthly Electronics Spend` > 20 & `Technology Adoption` == 'early')
by_state1 <- newdata %>%
  group_by(Location) %>%
  summarize(mean1 = mean(`Annual Spending on Eletronics`)) %>%
  mutate(region = tolower(Location))
head(newdata)
by_state2 <- newdata %>%
  group_by(Location) %>%
  summarize(mean1 = n()) %>%
  mutate(region = tolower(Location))

by_state3 <- newdata1 %>%
  group_by(Location) %>%
  summarize(mean1 = mean(`Annual Spending on Eletronics`)) %>%
  mutate(region = tolower(Location))
head(newdata)
by_state4 <- newdata1 %>%
  group_by(Location) %>%
  summarize(mean1 = n()) %>%
  mutate(region = tolower(Location))

missing_states <- data_frame(
  region = c("wyoming", "louisiana", "mississippi", "iowa", "nebraska", "minnesota", "kentucky", "virginia")
)

by_state1 <- bind_rows(by_state1, missing_states)
by_state2 <- bind_rows(by_state2, missing_states)

function(input, output) {
  
  
  
    output$barplot <- renderPlot({
      
      #ggplot(data=df, aes(x=dose, y=len, group=1)) +
      #  geom_line()+
      #  geom_point()
      
      gg1 <- ggplot()+ geom_map(data=us, map=us,aes(x=long, y=lat, map_id=region), fill="#ffffff", color="#ffffff", size=0.15) +
        geom_map(data=by_state1, map=us, aes(fill=mean1, map_id=region), color="#ffffff", size=0.15) +
        scale_fill_continuous(low='thistle2', high='darkred', guide='colorbar', name = "Mean Spend")+ 
        labs(x=NULL, y=NULL)+ theme(panel.border = element_blank())+ 
        theme(panel.background = element_blank())+ theme(axis.ticks = element_blank())+
        theme(axis.text = element_blank())
      gg1
      
      
    }, height=700)
  
  
  
  
  
}