# This is my working script for the NHS Data Visualisation competition entry
# Unhappy Meals - the extent of fast food in our most deprived communities
# It starts from some pre-cleaned excel files - I will replace these with accompanying R code asap
# Please excuse any very inefficient R code, I am very much still learning R!


################## UNHAPPY MEALS CODE #######################


# Load libraries
library(tidyverse) # I write all my code in tidyverse - there's lots of dplyr and ggplot especially in this code
library(glue) # used to combine one string later on

#devtools::install_github("GuangchuangYu/ggimage") # the most recent version of the ggimage package
library(ggimage) # used this excellent package to put images onto a ggplot
library(svglite) # required to export a nifty svg to edit in inkscape
library(extrafont) # for snazzy fonts - you'll also need to have run extrafont::font_import() to get windows fonts into r
extrafont::loadfonts("win")

# set a working directory one level above all the folders!

# load data (using a working directory one level above the 'data' folder)
ffdensity_rate_by_la = read_csv("./data/Fast_Food_Outlets_Comparative_Rate_LAs.csv")
imd_roar_by_la = read_csv("./data/IMD_Rank_of_average_rank_by_LAs.csv")

# join this data together
imd_ffdensity_by_la = left_join(x = ffdensity_rate_by_la, 
                    y = imd_roar_by_la, 
                    by = c("LA code" = "Local Authority District code (2019)"))

# I had help making a quick checking function - shoutout to Theo at Jumping Rivers!
name_len_bin = function(n){
  if(n <= 13){
    output = 3
  } else if(14 <= n & n <= 15){
    output = 2
  } else{
    output = 1
  }
  return(output)
}

# drop some excess columns - we'll just keep what we need for the burgerplot
imd_ffdensity_by_la = imd_ffdensity_by_la %>%
  select(lacode = "LA code",
         laname = "LA name",
         ffdensityrate = "Comparative Rate",
         imdroar = "IMD - Rank of average rank") %>%
  mutate(name_len = str_length(laname),
         name_len_binned = map_dbl(name_len, name_len_bin))


# load the image filepath - this needs to be fixed to be more rectangle?
burger_icon = "./icons/burger_fill.svg"
burger_outlines = "./icons/burger_outline.svg"

# pipe our dataset into a ggplot
burger_plot = imd_ffdensity_by_la %>%
 
# test line - use for drawing a smaller set to check settings
# filter(imdroar < 30) %>%

  # start the plot, each burger is an icon sat at 0,0 on a dummy graph
  ggplot(imd_ffdensity_by_la, 
         mapping = aes(x = 0,
                       y = 0)) +
  
  # colour gradient - using inbuilt r colours and with 1 being the midpoint
  scale_colour_gradient2(midpoint = 1, # maybe use 0.95 for clarity
                        low = "dodgerblue4",
                        mid = "seashell1",
                        high = "firebrick3",
                        na.value = "grey80", # City of London is NA, so gives it a shade of grey
                        space = "Lab" ) + # This is the only option for space (the method of shading between colours, I think)
  
  # set some dimensions so that we know where to base our size and font calculations
  ylim(c(-1,1)) +
  xlim(c(-1.,1)) +


  # add the burger images to each plot (coloured by the relative density of fast food outlets)
  geom_image(aes(image = burger_icon,
                 colour = ffdensityrate), 
             size = 0.85) +

  
  # # add outlines to each burger
  # # this can get a bit fiddly with ggplot stretching the image, so I resorted to creating outlines in Inkscape
  # # my outlines can be found in the icons folder
  
  # geom_image(aes(image = burger_outlines),
  #            size = 0.85) +

  # # originally I was going to write the relative density 'multiplier' on each burger, but it gets a little noisy
  # # feel free to add back in, especially if you are using for a smaller number of areas

  # # this line gives us the "1.2x" label at the top of the burger  
  # geom_text(aes(label= glue("{ffdensityrate}", "x")),
  #           colour = "black",
  #           size = 8,
  #           nudge_y = +0.4) +

  # labels are the laname, wrapped to avoid very wide labels (although Bournemouth, Christchurch and Poole required manual label rezising in Inkscape)
  geom_text(aes(label= str_wrap(string = laname,
                                width = 15),
                size = name_len_binned,   # the result of the function at the top of this script
                family="Century Gothic"), #this font needs to be installed on your machine
            colour = "grey5", # a very dark grey
            nudge_y = -0.5,   # push the label to the bottom of the burger
            lineheight = 0.75) +  # this is line spacing
  
  # text scale - longer text is a bit smaller to make room in the burger
  scale_size(range = c(3.15,3.75)) +
  
  # define the grid (facet wrap using IMD, with set number of columns)
  facet_wrap(~ imdroar,
             strip.position = 'bottom', # we get rid of this in a second
             scales = 'fixed', # doesn't do anything for us here as all our graphs are 2x2
             ncol = 14) + # 14 columns
  
  # theme void removes ALL the lines and backgrounds - perfect for this vis
  theme_void() +

  #  even theme_void doesn't get rid of everything for us, so here I remove a few more bits
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y=unit(-0.75,"lines"), # nudges the burgers closer together
        panel.spacing.x=unit(-1.3,"lines"), # nudges the burgers closer together
        legend.position = "none")


# save plot - I had to muck around with these settings A LOT - just keep making it
ggsave(
  filename = "./exports/burgers_and_names.svg",
  height = 380,
  width = 390,
  units = "mm",
  dpi = "print",
  scale = 1.33,
  plot = burger_plot,
  device = 'svg',
  limitsize = FALSE
)

# if you run this whole script at once (CTRL + SHIFT + RETURN) this sound will alert you to the run finishing!
beepr::beep(sound = 8)