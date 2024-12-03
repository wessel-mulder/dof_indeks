# Getting started ---------------------------------------------------------

path = 'data/davison_2024/Cleaned_DOF_data/'

# List all CSV files in the folder
file_list <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# Read all files into a list
data_list <- lapply(file_list, read.csv)

for (file in file_list) {
  # Extract the file name without the extension to use as an object name
  object_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the file and assign it to a new object with the file name
  assign(object_name, read.csv(file))
}

rm(data_list)

#inspect
head(cleaned_habitats)
head(cleaned_observations)
head(cleaned_point_survey_route_data)
head(full_spp_list_w_nonbreeders)
head(point_locations_cleaned)
head(route_df_cleaned)
head(survey_df_cleaned)


# effort ------------------------------------------------------------------
library(dplyr)
library(ggplot2)

effort <- cleaned_point_survey_route_data %>%
  group_by(year) %>%
  summarise(
    n_points = length(unique(pid)),
    n_routes = length(unique(rid))
  ) %>%
  ungroup()

head(effort)

plot(data = effort,
     n_routes ~ year)
plot(data = effort,
     n_points ~ year)

p0 <- effort$n_points[effort$year==min(effort$year)]
r0 <- effort$n_routes[effort$year==min(effort$year)]

effort$i_points <- effort$n_points/p0*100
effort$i_routes <- effort$n_routes/r0*100

plot(data = effort,
     i_routes ~ year)
plot(data = effort,
     i_points ~ year)

colors <- c('Points' = 'black','Routes'='red')

ggplot(data = effort) + 
#  geom_point(aes(x = year, y = i_points, color = 'Points'),
 #            col = 'black') +
#  geom_line(aes(x = year, y = i_points, color = 'Points'),
 #           col = 'black') +
  geom_point(aes(x = year, y = i_routes, color = 'Routes'),
             col = 'red') + 
  geom_line(aes(x = year, y = i_routes, color = 'Routes'),
             col = 'red') +
  labs(title = 'Effort',
       x = 'Year',
       y = 'Index',
       fill = 'Legend') +
  #scale_color_manual(values = colors) +
  annotate("text", x = 2015, y = 1100, label = "Routes", size = 3, color = "red") +
  annotate("text", x = 2015, y = 500, label = "Points", size = 3, color = "black") +
  theme_classic() +
  theme(panel.grid.major = element_line(color = 'gray',
                                        size = 0.1,
                                        linetype = 1))

ggsave('effort.pdf',plot = last_plot())

ggplot(data = effort) + 
#  geom_point(aes(x = year, y = n_points, color = 'Points'),
#             col = 'black') +
#  geom_line(aes(x = year, y = n_points, color = 'Points'),
#            col = 'black') +
  geom_point(aes(x = year, y = n_routes, color = 'Routes'),
             col = 'red') + 
  geom_line(aes(x = year, y = n_routes, color = 'Routes'),
            col = 'red') +
  labs(title = 'Number of routes',
       x = 'Year',
       y = '',
       fill = 'Legend') +
  #scale_color_manual(values = colors) +
#  annotate("text", x = 2015, y = 1100, label = "Routes", size = 3, color = "red") +
#  annotate("text", x = 2015, y = 500, label = "Points", size = 3, color = "black") +
  theme_classic() +
  theme(panel.grid.major = element_line(color = 'gray',
                                        size = 0.1,
                                        linetype = 1))

ggsave('n_routes.pdf',plot = last_plot())

# Birds -------------------------------------------------------------------
obs <- cleaned_observations %>%
  left_join(cleaned_point_survey_route_data,by='tid') %>%
  select(tid,pid,rid,year,euring_id,number)

grebe <- obs[obs$euring_id == 70,]
grebe <- grebe %>%
  group_by(year,pid,rid) %>%
  summarize(count = sum(number))


m1 <- trim(data = grebe, 
           count ~ pid + year)

plot(overall(m1))

# RTRIM tutorial ----------------------------------------------------------
library(rtrim)
data(skylark2)
head(skylark2)

# format:
#site = route 
#time = ob
summary(skylark2)

idx <- which(names(skylark2)=="year")      # rename year->season
names(skylark2)[idx] <- "season"
count_summary(skylark2, year_col="season") # show that it works
#> Total number of sites                   55
#> Sites without positive counts (0): 
#> Number of observed zero counts            0
#> Number of observed positive counts      202
#> Total number of observed counts         202
#> Number of missing counts                238
#> Total number of counts                  440
#> 

names(skylark2)[idx] <- "year"             # revert to original name

z1 <- trim(count ~ site + year, data=skylark2, model=3, serialcor=TRUE, overdisp=TRUE)
summary(z1)

