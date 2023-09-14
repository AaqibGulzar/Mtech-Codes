remotes::install_github("tylermorganwall/raybonsai")
library(raybonsai)
generate_tree() %>% 
  render_tree()
par(mfrow=c(3,3))
for(i in 1:9) {
  generate_tree(seed = i) %>% 
    render_tree()
}
par(mfrow=c(3,3))
for(i in 1:9) {
  generate_tree(seed = i, branch_angle_vert = seq(-45,45,by=5)/2, leaf_color = "pink") %>% 
    render_tree()
}
par(mfrow=c(2,3))
for(i in 1:6) {
  generate_tree(seed = i, branch_angle_vert = c(-20,0, 20), leaf_color = "red") %>% 
    render_tree()
}
par(mfrow=c(2,3))
for(i in 1:6) {
  generate_tree(seed = i+1000, branch_depth = 8, leaf_color = "purple") %>% 
    render_tree()
}
par(mfrow=c(1,2))

generate_tree(seed = 1234, branch_depth = 8, leaf_color = "orange") %>% 
  render_tree()

generate_tree(seed = 1234, branch_depth = 8, leaf_color = "orange", leaf_depth_start = 5) %>%
  render_tree()
par(mfrow=c(1,2))
generate_tree(seed = 2020, branch_angle_vert = c(-10, 10, 10, 10, 10, 10),
              branch_depth = 8, leaf_color = "magenta", leaf_depth_start = 5) %>% 
  render_tree()
generate_tree(seed = 2021, branch_angle_vert = c(10,-10,-10,-10,-10,-10),
              branch_depth = 8, leaf_color = "magenta", leaf_depth_start = 5) %>%
  render_tree()


par(mfrow=c(1,2))
generate_tree(seed = 4321, branch_angle_vert = seq(-10,20,by=1),
              branch_depth = 8, leaf_color = "yellow", leaf_depth_start = 5) %>% 
  render_tree()

generate_tree(seed = 4321, branch_angle_vert = -seq(-10,20,by=1),
              branch_depth = 8, leaf_color = "yellow", leaf_depth_start = 5) %>% 
  render_tree()

par(mfrow=c(1,3))
generate_tree(seed = 10, branch_angle = c(-30,0, 30), branch_scale = c(0.6,0.7),
              branch_depth = 7, leaf_color = "green", leaf_depth_start = 5) %>% 
  render_tree()

generate_tree(seed = 11, branch_angle = c(-30,0, 30), branch_scale = c(0.9,1),
              branch_depth = 7, leaf_color = "green", leaf_depth_start = 5) %>% 
  render_tree()

generate_tree(seed = 12, branch_angle = c(-30,0, 30), branch_scale = c(1.1,1.2),
              branch_depth = 7, leaf_color = "green", leaf_depth_start = 5) %>% 
  render_tree()
par(mfrow=c(1,2))
generate_tree(seed = 20, branch_angle = c(-30,0, 30), branch_scale = c(0.9,1), midpoint = TRUE,
              branch_depth = 7, leaf_color = "chartreuse4", 
              leaf_depth_start = 5, branch_color = "tan") %>% 
  render_tree(ground_color1 = "darkgoldenrod4", ground_color2 = "chocolate4")

generate_tree(seed = 20, branch_angle = c(-30,0, 30), branch_scale = c(0.9,1), midpoint = FALSE,
              branch_depth = 7, leaf_color = "chartreuse4", 
              leaf_depth_start = 5, branch_color = "tan") %>% 
  render_tree(ground_color1 = "darkgoldenrod4", ground_color2 = "chocolate4")




library(rayrender)

par(mfrow=c(1,1))
generate_tree(seed = 222, branch_angle = c(-20, 20), branch_scale = c(0.8,0.9), branch_split = 3,
              branch_depth = 6 , leaf_color = "chartreuse4", 
              leaf_depth_start = 5, branch_color = "tan") %>% 
  add_object(sphere(x=5,y=1,radius=1,material=light(color="magenta",intensity = 30))) %>%
  add_object(sphere(x=-5,y=1,radius=1,material=light(color="dodgerblue",intensity = 30))) %>%
  raybonsai::render_tree(lights = FALSE, ground_color1 = "grey50",ground_color2 = "grey50", width=1200,height=800)
generate_tree(seed = 222, branch_angle = c(-20,20), branch_scale = c(0.8,0.9), 
              branch_depth = 10 , leaf_color = "chartreuse4", 
              leaf_depth_start = 5, branch_color = "tan") %>% 
  raybonsai::render_tree(lights = FALSE, environment_light = "d:/photos/Snapseed/iust.jpeg", width=1200, height=800)



tree1 = generate_tree(seed = 1111, branch_angle_vert = c(-45,0,45), 
                      branch_depth = 8 , leaf_color = "green", leaf_depth_start = 5)
tree2 = generate_tree(seed = 2222, branch_angle_vert = seq(-30,30,by=5),
                      branch_depth = 8 , leaf_color = "red", leaf_depth_start = 5)
tree3 = generate_tree(seed = 3333, branch_angle_vert = seq(-30,30,by=5),
                      branch_depth = 8 , leaf_color = "purple", leaf_depth_start = 5)
tree4 = generate_tree(seed = 4444, branch_angle_vert = c(-45,0,45),
                      branch_depth = 8 , leaf_color = "orange", leaf_depth_start = 5)

group_objects(tree1,pivot_point = c(0,-10,0), group_angle = c(0,0,10)) %>% 
  add_object(group_objects(tree2,pivot_point = c(0,-10,0), group_angle = c(0,0,30))) %>% 
  add_object(group_objects(tree3,pivot_point = c(0,-10,0), group_angle = c(0,0,-10))) %>% 
  add_object(group_objects(tree4,pivot_point = c(0,-10,0), group_angle = c(0,0,-30))) %>% 
  raybonsai::render_tree(lights = FALSE, environment_light = "d:/photos/Snapseed/iust.jpeg", 
                         samples=40,
                         aperture=0.5, fov=24, lookfrom=c(0,8,30), width=1200, height=800,
                         ground_color1 = "darkgreen", ground_color2 = "darkgreen")