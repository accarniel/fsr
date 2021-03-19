



## Criando um SPO vazio



# Checando erros:
spo_test <- create_empty_spatial_plateau('point') # OK 

spo_test <- create_empty_spatial_plateau('plateau_point') # OK 

# Criando componentes :

# multi_points....
pt1 =st_multipoint(rbind(c(1,2),c(4,5)))
pt2 = st_multipoint(rbind(c(2,3),c(4,12)))
pt3 = st_multipoint(rbind(c(4,4), c(4,1), c(5,7)))
pt4 = st_multipoint(rbind(c(1,4), c(2,1), c(5,5)))
pt5 = st_multipoint(rbind(c(8,4), c(4,1), c(2,7)))
pt6 = st_multipoint(rbind(c(7,4), c(5,1), c(1,7)))
pt7 = st_multipoint(rbind(c(4,6), c(2,1), c(7,9)))
pt8 = st_multipoint(rbind(c(4,3), c(1,1), c(1,6)))
pt9 = st_multipoint(rbind(c(1,4), c(4,7), c(12,7)))
pt10 = st_multipoint(rbind(c(4,4), c(4,2), c(3,7)))


# components 
comp1 = new("Component", obj = pt1, md =0.5)
comp2 = new("Component", obj = pt2, md =0.7)
comp3 = new("Component", obj = pt3, md =0.5)
comp4 = new("Component", obj = pt4, md =0.3)
comp5 = new("Component", obj = pt5, md =0.6)
comp6 = new("Component", obj = pt6, md =0.9)
comp7 = new("Component", obj = pt7, md =0.1)
comp8 = new("Component", obj = pt8, md =0.2)
comp9 = new("Component", obj = pt9, md =0.4)
comp10 = new("Component", obj = pt10, md =0.5)

components = c(comp1, comp2, comp3, comp4, comp5, comp6, comp7, comp8, comp9, comp10)

spo_test <- create_empty_spatial_plateau('plateau_point') # OK 


for(comp in components){
  spo_test<- spa_plateau_builder(spo_test, comp)
}


