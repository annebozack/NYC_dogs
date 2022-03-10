# dataset from https://data.cityofnewyork.us/Health/NYC-Dog-Licensing-Dataset/nu7n-tubp

# load data
dogs = read.csv('/Users/annebozack/Downloads/NYC_Dog_Licensing_Dataset.csv')[,-1]                                        
dogs = unique(dogs)
dogs = dogs[order(dogs$LicenseIssuedDate),]

# create year variable
dogs$year = substr(dogs$LicenseIssuedDate,7,10)
dogs$year = as.numeric(dogs$year)

# create dataset for names 
dogs_name = dogs[,c('AnimalName', 'year')]
dogs_name$AnimalName = factor(dogs_name$AnimalName)

# summarize by name
dogs_name = dogs_name %>% count(AnimalName, year, sort = TRUE)
colnames(dogs_name)[3] = 'n_name'

# rmeove missing names 2014 and 2021 (limited observations for these years)
dogs_name = dogs_name[dogs_name$AnimalName != 'UNKNOWN',]
dogs_name = dogs_name[dogs_name$AnimalName != 'NAME NOT PROVIDED',]
dogs_name = dogs_name[dogs_name$AnimalName != 'NAME',]
dogs_name = dogs_name[dogs_name$year != 2021,]
dogs_name = dogs_name[dogs_name$year != 2014,]
dogs_name = dogs_name[!is.na(dogs_name$AnimalName),]

# format dataset with rank variable
dogs_name_format <- dogs_name %>%
  group_by(year) %>%
  mutate(rank = rank(-n_name),
         n_rel_name = n_name/n_name[rank==1]) %>%
  group_by(AnimalName) %>% 
  filter(rank <= 20) %>%
  ungroup()

# correct ties
dogs_name_format$rank[dogs_name_format$AnimalName == 'JACK' & dogs_name_format$year == 2015] = 14
dogs_name_format$rank[dogs_name_format$AnimalName == 'TEDDY' & dogs_name_format$year == 2015] = 15
dogs_name_format$rank[dogs_name_format$AnimalName == 'CHLOE' & dogs_name_format$year == 2015] = 16
dogs_name_format$rank[dogs_name_format$AnimalName == 'LILY' & dogs_name_format$year == 2015] = 17
dogs_name_format$rank[dogs_name_format$AnimalName == 'MIA' & dogs_name_format$year == 2020] = 18
dogs_name_format$rank[dogs_name_format$AnimalName == 'PRINCESS' & dogs_name_format$year == 2020] = 19

# create dataset for beeds
dogs_breed = dogs[,c('BreedName', 'year')]
dogs_breed$BreedName = factor(dogs_breed$BreedName)

# summarize by breed
dogs_breed = dogs_breed %>% count(BreedName, year, sort = TRUE)
colnames(dogs_breed)[3] = 'n_breed'

# remove unknowns and 2014 and 2021
dogs_breed = dogs_breed[dogs_breed$BreedName != 'Unknown',]
dogs_breed = dogs_breed[dogs_breed$year != 2021,]
dogs_breed = dogs_breed[dogs_breed$year != 2014,]

# format dataset with rank variable
dogs_breed_format <- dogs_breed %>%
  group_by(year) %>%
  mutate(rank = rank(-n_breed),
         n_rel_breed = n_breed/n_breed[rank==1]) %>%
  group_by(BreedName) %>% 
  filter(rank <= 20) %>%
  ungroup()

# add variable for color
color_range <- colorRampPalette(c("#FFD700", "#FFB14E", '#FA8775', '#EA5F94', '#CD34B5', '#9d02d7'))
col = color_range(27)

name_col_df = data.frame(AnimalName = dog_format_all$AnimalName[dog_format_all$year == 2015][order(dog_format_all$rank[dog_format_all$year == 2015])], name_col = col[c(1:20)])
name_col_df = rbind(name_col_df, data.frame(AnimalName = c('MILO', 'OLIVER', 'COOKIE', 'MIA', 'OREO', 'LEO', 'COOPER'), name_col = col[c(21:27)]))

breed_col_df = data.frame(BreedName = dog_format_all$BreedName[dog_format_all$year == 2015][order(dog_format_all$rank[dog_format_all$year == 2015])], breed_col = col[c(1:20)])
breed_col_df = rbind(breed_col_df, data.frame(BreedName = c('Shih Tzu Crossbreed', 'French Bulldog', 'Chihuahua Crossbreed', 'Poodle Crossbreed', 'Terrier mix', 'Goldendoodle'), breed_col = col[c(21:26)]))

# merge name and breed datasets
dog_format_all = merge(dogs_name_format, dogs_breed_format, by = c('rank', 'year'))

# add color variables
dog_format_all = merge(dog_format_all, name_col_df, by = 'AnimalName', all.x = T)
dog_format_all = merge(dog_format_all, breed_col_df, by = 'BreedName', all.x = T)

# add variable for proportions
dog_format_all$prop_name = NA
dog_format_all$prop_breed = NA
for (i in c(2015:2020)){
    namesum = sum(dog_format_all$n_name[dog_format_all$year == i])
    dog_format_all$prop_name[dog_format_all$year == i] = dog_format_all$n_name[dog_format_all$year == i]/namesum
    breedsum = sum(dog_format_all$n_breed[dog_format_all$year == i])
    dog_format_all$prop_breed[dog_format_all$year == i] = dog_format_all$n_name[dog_format_all$year == i]/namesum
}

# change proportion to negtive values for breed (bars will be on left side of plot)
dog_format_all$prop_breed = -(dog_format_all$prop_breed)

# reformat names to lowower case
dog_format_all$AnimalName = as.character(dog_format_all$AnimalName)
dog_format_all$AnimalName[dog_format_all$AnimalName == 'MAX'] = 'Max'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'BELLA'] = 'Bella'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'LOLA'] = 'Lola'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'LUCY'] = 'Lucy'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'ROCKY'] = 'Rocky'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'COCO'] = 'Coco'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'CHARLIE'] = 'Charlie'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'BUDDY'] = 'Buddy'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'LUCKY'] = 'Lucky'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'PRINCESS'] = 'Princess'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'BAILEY'] = 'Bailey'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'TOBY'] = 'Toby'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'JACK'] = 'Jack'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'TEDDY'] = 'Teddy'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'CHLOE'] = 'Chloe'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'LILY'] = 'Lily'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'MOLLY'] = 'Molly'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'LUNA'] = 'Luna'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'PENNY'] = 'Penny'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'MILO'] = 'Milo'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'OLIVER'] = 'Oliver'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'COOKIE'] = 'Cookie'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'MIA'] = 'Mia'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'OREO'] = 'Oreo'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'LEO'] = 'Leo'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'COOPER'] = 'Cooper'
dog_format_all$AnimalName[dog_format_all$AnimalName == 'DAISY'] = 'Daisy'
dog_format_all$AnimalName = factor(dog_format_all$AnimalName)

# add label variable
dog_format_all$name_lab = paste0(dog_format_all$AnimalName, ', ', round(dog_format_all$prop_name*100, 1), '%')
dog_format_all$breed_lab = paste0(dog_format_all$BreedName, ', ', -round(dog_format_all$prop_breed*100, 1), '%')

# add data for empty bar to keep axes from shifting (can also be done with 'view_follow(fixed_y = TRUE)')
dog_format_all = rbind(dog_format_all, data.frame(BreedName = rep(NA, times = 6), AnimalName = rep(NA, times = 6), rank = rep(21, times = 6), 
    year = c(2015, 2016, 2017, 2018, 2019, 2020), n_name = rep(NA, times = 6), n_rel_name = rep(NA, times = 6), n_lbl_name = rep(NA, times = 6),
    n_breed = rep(NA, times = 6), n_rel_breed = rep(NA, times = 6), n_lbl_breed = rep(NA, times = 6), name_col = rep(NA, times = 6), breed_col = rep(NA, times = 6),
    prop_name = rep(0.1, times = 6), prop_breed = rep(-0.1, times = 6), name_lab = rep(NA, times = 6), breed_lab = rep(NA, times = 6)))

# plot with black background
# add variable for opacity
dog_format_all$alpha = c(rep(1, times = 120), rep(0, times = 6))

# create static plot
# code based on https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da
staticplot = ggplot(dog_format_all, aes(x = rank)) +
  geom_tile(aes(y = prop_name/2, group = AnimalName, 
               height = prop_name,
                width = 0.9), fill = dog_format_all$name_col, alpha = dog_format_all$alpha, size = 0) + geom_text(aes(x = rank, y=0.005,label = dog_format_all$name_lab, hjust=0), size = 6, color = 'white') +
geom_tile(aes(y = prop_breed/2, group = BreedName, 
                height = prop_breed,
                width = 0.9), fill = dog_format_all$breed_col, alpha = dog_format_all$alpha, size = 0) + geom_text(aes(x = rank, y=-0.005,label = dog_format_all$breed_lab, hjust=1), size = 6, color = 'white') +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma, limits = c(-0.12, 0.12)) +
  scale_x_reverse() +
  guides(color = 'none', fill = 'none') +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
         axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=0, color="grey" ),
        panel.grid.minor.x = element_line( size=0, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="white", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_rect(fill = "black", color = "black"),
       plot.margin = margin(1,1,6,1, "cm")) + 
       geom_segment(aes(x = 0, xend = 21, y = 0, yend = 0), color = 'black', size = 1)

# read image 
d = data.frame(x = 25, y = 0, image = "/Users/annebozack/Desktop/dogs3-01.png")

# add image to static plot
staticplot = staticplot + geom_image(data = d, aes(x = x, y = y, image=image), size = 0.9)

# render animation as gif
anim = staticplot + transition_states(year, transition_length = 4, state_length = 4) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'NYC Dogs {closest_state} \n Top Breeds                                    Top Names')

animate(anim, 200, fps = 20,  width = 1200, height = 1400, 
        renderer = gifski_renderer(file = '/Users/annebozack/Desktop/doganimation_black.gif', loop = T))


# white background
# create variable for opacity
dog_format_all$alpha = c(rep(0.8, times = 120), rep(0, times = 6))

# create static plot
staticplot = ggplot(dog_format_all, aes(x = rank)) +
  geom_tile(aes(y = prop_name/2, group = AnimalName, 
               height = prop_name,
                width = 0.9), fill = dog_format_all$name_col, alpha = dog_format_all$alpha, size = 0) + geom_text(aes(x = rank, y=0.005,label = dog_format_all$name_lab, hjust=0), size = 6, color = '#4D4D4D') +
geom_tile(aes(y = prop_breed/2, group = BreedName, 
                height = prop_breed,
                width = 0.9), fill = dog_format_all$breed_col, alpha = dog_format_all$alpha, size = 0) + geom_text(aes(x = rank, y=-0.005,label = dog_format_all$breed_lab, hjust=1), size = 6, color = '#4D4D4D') +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma, limits = c(-0.12, 0.12)) +
  scale_x_reverse() +
  guides(color = 'none', fill = 'none') +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
         axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=0, color="grey" ),
        panel.grid.minor.x = element_line( size=0, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="#4D4D4D", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
       plot.margin = margin(1,1,6,1, "cm")) + 
       geom_segment(aes(x = 0, xend = 21, y = 0, yend = 0), color = 'white', size = 1)

# add image to static plot
staticplot = staticplot + geom_image(data = d, aes(x = x, y = y,image=image), size = 0.9)

# render animation as gif
anim = staticplot + transition_states(year, transition_length = 4, state_length = 4) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'NYC Dogs {closest_state} \n Top Breeds                                    Top Names')

animate(anim, 200, fps = 20,  width = 1200, height = 1400, 
        renderer = gifski_renderer(file = '/Users/annebozack/Desktop/doganimation_white.gif', loop = T))