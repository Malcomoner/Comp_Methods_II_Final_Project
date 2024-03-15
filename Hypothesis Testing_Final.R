## Final Project ##

# HO: p1 - p2 = 0
# Ha: p1 - p2 < 0

#read, parse data and prepare for testing

data = read.csv(file.choose())
tdata = as.data.frame(t(data))
colnames(tdata) = c('CA', 'CO', 'CT', 'FL','IL', 'MS', 'NM', 'NY', 'TX')
tdata # 2015-2023 | rows 1-9 = pop , 10-18 = shootings, 19-27 = shootings/100,000 pop. 

#----------------------------------------#
#----------------------------------------#
# 2015-2020
# Shooting proportions per 100,000 with States with immunity (CO, CT, NM)
CO_avg_cap_15 = mean(tdata[19:24,2]) ;
CT_avg_cap_15 = mean(tdata[19:24,3]) ; 
NM_avg_cap_15 = mean(tdata[19:24,7]) ; 
CO_avg_cap_15 ; CT_avg_cap_15 ; NM_avg_cap_15

data_no_15 = c(CO_avg_cap_15,CT_avg_cap_15,NM_avg_cap_15)
mu_no_15 = mean(data_no_15) ; mu_no_15
sd_no_15 = sd(data_no_15); sd_no_15
n_no_15 = length(data_no_15)

#----------------------------------------#
# 2015-2020
# Shooting proportions per 100,000 with States with qualified immunity (CA, FL, IL, MS, NY, TX)
CA_avg_cap_15 = mean(tdata[19:24,1]) ; CA_avg_cap_15
FL_avg_cap_15 = mean(tdata[19:24,4]) ; FL_avg_cap_15
IL_avg_cap_15 = mean(tdata[19:24,5]) ; IL_avg_cap_15
MS_avg_cap_15 = mean(tdata[19:24,6]) ; MS_avg_cap_15
NY_avg_cap_15 = mean(tdata[19:24,8]) ; NY_avg_cap_15
TX_avg_cap_15 = mean(tdata[19:24,9]) ; TX_avg_cap_15

data_im_15 = c(CA_avg_cap_15,FL_avg_cap_15,IL_avg_cap_15,MS_avg_cap_15,NY_avg_cap_15,TX_avg_cap_15)
mu_im_15 = mean(data_im_15) ; mu_im_15
sd_im_15 = sd(data_im_15); sd_im_15
n_im_15 = length(data_im_15)

#Compare 2015-2020
mu_no_15 ; mu_im_15

#----------------------------------------#
#----------------------------------------#

# 2021-2023
# Shooting proportions per 100,000 with States with no qualified immunity (CO, CT, NM)
CO_avg_cap_21 = mean(tdata[25:27,2]) ; CO_avg_cap_21
CT_avg_cap_21 = mean(tdata[25:27,3]) ; CT_avg_cap_21
NM_avg_cap_21 = mean(tdata[25:27,7]) ; NM_avg_cap_21
CO_avg_cap_21 ; CT_avg_cap_21 ; NM_avg_cap_21

data_no_21 = c(CO_avg_cap_21,CT_avg_cap_21,NM_avg_cap_21)
mu_no_21 = mean(data_no_21) ; mu_no_21
sd_no_21 = sd(data_no_21); sd_no_21
n_no_21 = length(data_no_21)

#----------------------------------------#
# 2021-2023
# Shooting proportions per 100,000 with States with qualified immunity (CA, FL, IL, MS, NY, TX)
CA_avg_cap_21 = mean(tdata[25:27,1]) ; CA_avg_cap_21
FL_avg_cap_21 = mean(tdata[25:27,4]) ; FL_avg_cap_21
IL_avg_cap_21 = mean(tdata[25:27,5]) ; IL_avg_cap_21
MS_avg_cap_21 = mean(tdata[25:27,6]) ; MS_avg_cap_21
NY_avg_cap_21 = mean(tdata[25:27,8]) ; NY_avg_cap_21
TX_avg_cap_21 = mean(tdata[25:27,9]) ; TX_avg_cap_21

data_im_21 = c(CA_avg_cap_21,FL_avg_cap_21,IL_avg_cap_21,MS_avg_cap_21,NY_avg_cap_21,TX_avg_cap_21)
mu_im_21 = mean(data_im_21); 
sd_im_21 = sd(data_im_21); sd_im_21
n_im_21 = length(data_im_21)

#Compare 2021-2023
mu_no_21; mu_im_21

#----------------------------------------#
#----------------------------------------#

# Ho: mu_no_15 - mu_no_21 = 0
# Ha: mu_no_15 - mu_no_21 > 0   | Got rid of immunity so should be less shooting. 
z = ((mu_no_15- mu_no_21)-0) / sqrt( ((sd_no_15)^2/n_no_15) + ((sd_no_21)^2/n_no_21));z
pt(z,n_no_15) # 0.8680375 > 0.05 so reject the null

# Ho: mu_im_15 - mu_im_21 = 0
# Ha: mu_im_15 - mu_im_21 != 0  | Immunity still exist so should be fail to reject Ho 
z = ((mu_im_15- mu_im_21)-0) / sqrt( ((sd_im_15)^2/n_im_15) + ((sd_im_21)^2/n_im_21));z
pt(z,n_no_15) # 0.8680375 > 0.05 so reject the null

# Ho: mu_no_21 - mu_im_21 = 0
# Ha: mu_no_21 - mu_im_21 < 0  | Other states have immunity so they should have more shooting. 
z = ((mu_no_21 - mu_no_15)-0) / sqrt( ((sd_no_21)^2/n_no_21) + ((sd_im_21)^2/n_im_21));z
pt(z,n_no_15) # 0.8680375 > 0.05 so reject the null


#########################
#####10 MAR 24

#net_change_2015_2023 <- sapply(1:9, function(col) tdata[23, col] - tdata[19, col])
net_change_2015_2023 <- sapply(1:9, function(col) tdata[27, col] - tdata[19, col])
# Calculate the average change for all nine states combined
avg_change_all_states <- mean(net_change_2015_2023)

# Print average change for all states
print(avg_change_all_states)

# Net change for each state separately
net_change_each_state <- data.frame(State = colnames(tdata), Net_Change = net_change_2015_2023)

# Print net change for each state
print(net_change_each_state)

# Calculate average net change for states with qualified immunity
avg_change_qualified_immunity <- mean(net_change_2015_2023[1:3])

# Calculate average net change for states without qualified immunity
avg_change_no_qualified_immunity <- mean(net_change_2015_2023[4:9])

# Print average net change for states with qualified immunity and states without qualified immunity
print(avg_change_qualified_immunity)
print(avg_change_no_qualified_immunity)

# Calculate the average net change across all states
average_net_change <- mean(net_change_2015_2023)
print(average_net_change)

## now to plot it
# Calculate the average change across all nine states
average_change <- mean(avg_data$Change_in_Shootings)

#will need ggplot package to see these plots
# Now, plot the Change in Shootings per 100,000 Population by Immunity Status
ggplot(avg_data, aes(x = State, y = Change_in_Shootings, fill = Immunity_Status)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = round(Change_in_Shootings, 2)), vjust = -0.3, color = "black") + # Adds change values as labels
  labs(title = "Change in Shootings per 100,000 Population (2015-2023)", 
       y = "Change in Shootings per 100,000", 
       x = "State") +
  scale_fill_manual(name = "Qualified Immunity", values = c("No" = "green", "Yes" = "orange")) +
  theme_minimal() +
  geom_hline(yintercept = average_change, linetype = "dashed", color = "blue") + # Add horizontal line for average change
  annotate("text", x = 1, y = average_change + 0.1, label = paste("Average Change:", round(average_change, 2)), color = "blue") + # Add label for average change
  theme(legend.position = "top") # Place legend on the top of the plot area


#now just compare the two populations-just 2015-2019

# Define the data for the plot
no_immunity_avg <- mean(net_change_2015_2019[4:9])  # Consolidated average for states with no immunity
immunity_avg <- mean(net_change_2015_2019[1:3])  # Consolidated average for states with immunity
overall_avg <- mean(net_change_2015_2019)  # Consolidated average for all nine states

# Create a data frame for plotting
plot_data <- data.frame(
  Group = c("No Immunity", "Immunity"),
  Average_Change = c(no_immunity_avg, immunity_avg)
)

# Create the bar plot
bar_plot <- ggplot(plot_data, aes(x = Group, y = Average_Change, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = overall_avg, linetype = "dashed", color = "blue") +  # Add horizontal line for consolidated average
  annotate("text", x = 1, y = overall_avg + 0.1, label = paste("Overall Average Change:", round(overall_avg, 2)), color = "blue") +  # Add label for consolidated average
  labs(title = "Average Change in Shootings 2015-2019", 
       y = "Average Change", 
       x = NULL) +  # x-axis label not needed
  theme_minimal() +
  theme(legend.position = "none") +  # Remove legend
  scale_fill_manual(values = c("No Immunity" = "green", "Immunity" = "orange")) +  # Match color scheme
  coord_cartesian(ylim = c(0, max(plot_data$Average_Change) * 1.2))  # Adjust y scale

# Print the plot
print(bar_plot)

## now entire period 2015-2023
# Define the data for the plot
no_immunity_avg <- mean(net_change_2015_2023[4:9])  # Consolidated average for states with no immunity
immunity_avg <- mean(net_change_2015_2023[1:3])  # Consolidated average for states with immunity
overall_avg <- mean(net_change_2015_2023)  # Consolidated average for all nine states

# Create a data frame for plotting
plot_data <- data.frame(
  Group = c("No Immunity", "Immunity"),
  Average_Change = c(no_immunity_avg, immunity_avg)
)

# Create the bar plot
bar_plot <- ggplot(plot_data, aes(x = Group, y = Average_Change, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = overall_avg, linetype = "dashed", color = "blue") +  # Add horizontal line for consolidated average
  annotate("text", x = 1, y = overall_avg + 0.1, label = paste("Overall Average Change:", round(overall_avg, 2)), color = "blue") +  # Add label for consolidated average
  labs(title = "Average Change in Shootings 2015-2023", 
       y = "Average Change", 
       x = NULL) +  # x-axis label not needed
  theme_minimal() +
  theme(legend.position = "none") +  # Remove legend
  scale_fill_manual(values = c("No Immunity" = "green", "Immunity" = "orange")) +  # Match color scheme
  coord_cartesian(ylim = c(0, max(plot_data$Average_Change) * 1.2))  # Adjust y scale

# Print the plot
print(bar_plot)





