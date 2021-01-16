# Tests and Examples of Use 


# This Script uses Classes and Functions created in the Files:
# Fuzzy_Classes.R 
# Spatial_Fuzzy_Functions.R

#################################################################
#################################################################
########            Spatial Plateau Point                ########
#################################################################
#################################################################


# Creating Points (MultiPoints)
pt1 =st_multipoint(rbind(c(1,2),c(4,5)))
pt2 = st_multipoint(rbind(c(2,3)))
pt3 = st_multipoint(rbind(c(4,4), c(4,1), c(5,7)))


# data frame 
df <- data.frame(md = c(0.4, 0.6, 0.2), st_sfc(pt1, pt2, pt3))

# Lists of components
component1 = new("Component", obj = st_multipoint(rbind(c(1,2),c(4,5))), md =0.4)
component2 = new("Component", obj = st_multipoint(rbind(c(2,3))), md =0.6)
component3 = new("Component", obj = st_multipoint(rbind(c(4,4), c(4,1), c(5,7))), md =0.2)
components = c(component1, component2, component3)



# 
obj1 = list(st_multipoint(rbind(c(1,2),c(4,5))), st_multipoint(rbind(c(2,3))))
supp = st_union(st_sfc(obj1))


var_sup = supp[[1]]

# Spatial Plateau 1 - Test 


SP_1 <- SpatialPlateau(components)


plateau_point = new("PlateauPoint", components = c(component1, component2), supp = var_sup)



#################################################################
#################################################################
########            Spatial Plateau Line                 ########
#################################################################
#################################################################

component1 = new("Component", obj = st_multipoint(rbind(c(1,2),c(4,5))), md =0.4)








#################################################################
#################################################################
########            Spatial Plateau Region               ########
#################################################################
#################################################################



