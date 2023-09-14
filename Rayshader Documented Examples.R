#Here is the `imhof1` palette:
create_texture("#fff673","#55967a","#8fb28a","#55967a","#cfe0a9") %>%
  plot_map()
#Here is the `unicorn` palette:
create_texture("red","green","blue","yellow","white") %>%
  plot_map()

#Create the Red Relief Image Map (RRIM) technique using a custom texture and ambient_shade(),
#with an addition lambertian layer added with lamb_shade() to improve topographic clarity.
## Not run:
bigmb = resize_matrix(montereybay, scale=2, method="cubic")
bigmb %>%
  sphere_shade(zscale=3, texture = create_texture(lightcolor =  "orange",shadowcolor = "brown",leftcolor = "red",rightcolor = "red",cornercolors = "white",centercolor = "white")) %>%
  add_shadow(ambient_shade(bigmb, maxsearch = 100, multicore = TRUE,zscale=1),0) %>%
  add_shadow(lamb_shade(bigmb),0.5) %>%
  plot_map()

water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
bathy_hs = height_shade(montereybay, texture = water_palette)
plot_map(bathy_hs)
#Set everything below 0m to water palette
montereybay %>%
  sphere_shade(zscale=10) %>%
  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0)) %>%
  add_shadow(ray_shade(montereybay,zscale=50),0.3) %>%
  plot_map()
#Add snow peaks by setting `lower = FALSE`
snow_palette = "white"
snow_hs = height_shade(montereybay, texture = snow_palette)
#Set the snow transition region from 500m to 1200m
montereybay %>%
  sphere_shade(zscale=10, texture = "desert") %>%
  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0)) %>%
  add_overlay(generate_altitude_overlay(snow_hs, montereybay, 500, 1200, lower=FALSE)) %>%
  add_shadow(ambient_shade(montereybay,zscale=50,maxsearch=100),0) %>%
  plot_map()  #we can use plot_3d too.


#Create the water palette
water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
bathy_hs = height_shade(montereybay, texture = water_palette)
#Generate flat water heightmap
mbay = montereybay
mbay[mbay < 0] = 0
base_map = mbay %>%
  height_shade() %>%
  add_overlay(generate_altitude_overlay(bathy_hs, montereybay, 0, 0)) %>%
  add_shadow(lamb_shade(montereybay,zscale=50),0.3)
#Plot a compass
base_map %>%
  add_overlay(generate_compass_overlay(heightmap = montereybay)) %>%
  plot_map()
#Change the position to be over the water
base_map %>%
  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15)) %>%
  plot_map()
#Change the text color for visibility
base_map %>%
  add_overlay(generate_compass_overlay(heightmap = montereybay, x = 0.15, text_color="white")) %>%
  plot_map()




