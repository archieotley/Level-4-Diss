library(vroom)
library(ggplot2)
library(DT)

# Load in data from csv files------------------------------------------------------------------------------

files <- fs::dir_ls(glob = "solution*csv")
files
vroom(files)
vroom(files, col_select = c("dose", "risk", "value"))->risk

# Make data into data frame -------------------------------------------------------------------------------

# Plot graph of the data and perform p value and t test on data -------------------------------------------

# ggplot(data=risk, aes(x=dose, y=risk, group=1)) +
# geom_line()+
#   geom_point()

# output$Risk <- DT::renderDataTable(
  # files()%>%
    ggplot(data=risk, aes(x=dose, y=value, group=1)+
    # geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
    scale_y_log10()+
    geom_line()+
    # t.test(risk)+
    # fisher.test(risk)+
    ylab("Total Risk Recieved")+
    xlab("NUmber of Suseptible Patients after the Infectious Patient")+
  labs(
    title="Total Risk Compared to Number of Suseptible Patients",
    subtitle="Sensitivity Analysis of Risk to Number of Suseptible Patients",
    caption=""
  ) +
    theme(legend.position="none")
    )  
 
