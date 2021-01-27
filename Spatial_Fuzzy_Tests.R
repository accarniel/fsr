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



#### USING COMPONENT FUNCTION TO CREATE THE COMPONENTS ######

#vector and values:

s0 = c(1,2)

s1 = rbind(c(1,2),c(2,5), c(2,6))
s2 = rbind(c(1,7), c(6,4))
s3 = rbind(c(1,1),c(5,6), c(1,7))
s4 = rbind(c(1,12), c(2,6))
s5 = rbind(c(11,5),c(7,2), c(8,2))
s6 = rbind(c(6,6), c(5,5))

p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
p3 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
p4 <- rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
p5 <- rbind(c(3,3), c(1,2), c(5,6), c(3,3))

list_poly = list(p1,p2)
list_multi_poli = list(list(p1,p2), list(p3,p4), list(p5))




# Check Component Function 
# POINTS
singlePointComponent = create_component(s0, 0.4)

multiPointComponent1 = create_component(s1, 0.5, "POINT")
multiPointComponent2 = create_component(s2, 0.6, "POINT")
multiPointComponent3 = create_component(p1, 0.7, "POINT")


# LINES
singleLineComponent = create_component(s1, 0.6, "LINE")
multiLineComponent1 = create_component(list(s1,s2), 0.7, "LINE")
multiLineComponent2 = create_component(list(s3,s4), 0.7, "LINE")
multiLineComponent3 = create_component(list(s6,s3), 0.7, "LINE")



# REGION (POLYGON)
singlePolygonComponent = create_component(list_poly, 0.3, "REGION")
multiPolygonComponent = create_component(list_multi_poli, 0.6, "REGION")



# Creating a SpatialPlateau Object
list_of_components = c(multiPointComponent1,multiPointComponent2,multiPointComponent3)
SP_1 <- create_spatial_plateau(list_of_components)

# Creating a SpatialPlateau Object - LINE
list_of_line_componenets = c(multiLineComponent1,multiLineComponent2,multiLineComponent3)
SP_2 <- create_spatial_plateau(list_of_line_componenets)


# 
obj1 = list(st_multipoint(rbind(c(1,2),c(4,5))), st_multipoint(rbind(c(2,3))))
supp = st_union(st_sfc(obj1))


var_sup = supp[[1]]

# Spatial Plateau 1 - Test 


SP_1 <- create_spatial_plateau(components)

















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



