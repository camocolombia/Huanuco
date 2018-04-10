#Calling R libraries
require(FactoMineR);require(factoextra);require(cluster);require(NbClust)

#####
dir  <- "D:/AICHI13/Huanuco"
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
trans_process_dir <- paste0(trans_dir,"/","processing");if(!file.exists(trans_process_dir)){dir.create(trans_process_dir)}

trans_results_dir <- paste0(trans_dir,"/","results");if(!file.exists(trans_results_dir)){dir.create(trans_results_dir)}

plot_dir <- paste0(dir,"/","plot");if(!file.exists(plot_dir)){dir.create(plot_dir)}
plot_cluster_dir <- paste0(plot_dir,"/","cluster");if(!file.exists(plot_cluster_dir)){dir.create(plot_cluster_dir)}
plot_full_dir <- paste0(plot_dir,"/","full");if(!file.exists(plot_full_dir)){dir.create(plot_full_dir)}
plot_reg_dir <- paste0(plot_dir,"/","region");if(!file.exists(plot_reg_dir)){dir.create(plot_reg_dir)}
plot_descrip_dir <- paste0(plot_dir,"/","description");if(!file.exists(plot_descrip_dir)){dir.create(plot_descrip_dir)}
################################

#http://factominer.free.fr/course/doc/MFA_course_slides.pdf
tab3 <- readRDS(paste0(trans_results_dir,"/","firstStep.RDS"))

#variables with NAs
#c("alt_mean","wealthindex","inc","per_sale")

tab3_alt <- tab3[,-c(2,19,21,22,25)]
for(i in 1:ncol(tab3_alt)){
  cat("Column: ",i," | ",length(tab3_alt[i][is.na(tab3_alt[,i]),]),"\n")
}
tab3_alt <- tab3_alt[complete.cases(tab3_alt),]
#saving table
write.table(tab3_alt,paste0(trans_process_dir,"/","table_q1_join.csv"),quote=F,row.names = T,sep=",")

# plot(ACP)
# plot.PCA(ACP,choix = "var")
# hCPC <- HCPC(ACP,nb.clust = -1)
# plot.HCPC(hCPC,choice="3D.map")


tab3_alt2 <-tab3_alt

# for( i in c(1,9,10,11,12,13)){
#   tab3_alt2[,i] <- as.character(tab3_alt2[,i]) 
# }


tab3_alt2 <- tab3_alt2[,c(2:8,1,9:27)]
tab3_alt2 <- tab3_alt2[,-c(3,5)]
for(i in 1:ncol(tab3_alt2)){cat(class(tab3_alt2[,i])," | ",i,"\n")}
#corrplot::corrplot(cor(tab3_alt2[,c(1:7,14:27)]))

#ACP <- FactoMineR::PCA(tab3_alt,quali.sup = c(1,9,10,11,12,13),quanti.sup = 25,ncp=10,scale.unit = T,graph = T)

MFA_2 <- FactoMineR::MFA(base=tab3_alt2,
                         # group = c(1,7,5,5,7),
                         group = c(5,6,5,9),
                         # type = c("n","c","n","c","c"),
                         type = c("c","n","c","c"),
                         
                         # c("n",
                         #   "c","c","c","c","c","c","c",
                         #   "n","n","n","n","n",
                         #   "c","c","c","c","c",
                         #   "c","c","c","c","c","c","c"),
                         
                         ncp = 5,
                         # name.group = c("Region",
                         #                "Plants",
                         #                "Socioeconomic",
                         #                "Wealth",
                         #                 "Farm variables")
                         name.group = c("Plants",
                                        "Socioeconomic",
                                        "Wealth",
                                        "Farm variables")
                         #,num.group.sup=c(5)
                         
)
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/
fviz_mfa_var(MFA_2, "group")
# Contribution to the first dimension
fviz_contrib(MFA_2, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(MFA_2, "group", axes = 2)
summary(MFA_2)
print(MFA_2)
MFA_MFA <- get_mfa(MFA_2,element = c("ind", "quanti.var", "quali.var", "group",
                                     "partial.axes")) #GET MFA
#plot(MFA_2,choix="var")
EIG_MFA <- get_eigenvalue(MFA_2) #GET EIGENVALUES
fviz_eig(MFA_2)
group <- get_mfa_var(MFA_2, "group")
##d
quanti.var <- get_mfa_var(MFA_2, "quanti.var")

fviz_mfa_var(MFA_2, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = F,
             geom = c("point", "text"))

fviz_mfa_ind(MFA_2); fviz_mfa_var(MFA_2)#: Visualize the results for individuals and variables, respectively.

fviz_contrib(MFA_2, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")

fviz_contrib(MFA_2, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")

MFA_2$group$Lg

fviz_mfa_var(MFA_2, "quanti.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))

# Color by cos2 values: quality on the factor map
fviz_mfa_var(MFA_2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE)

ind <- get_mfa_ind(MFA_2)
ind

fviz_mfa_ind(MFA_2, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


#http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_mfa.html
fviz_mfa_ind(MFA_2, 
             habillage = c("region"), # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07","#000000"),
             addEllipses = TRUE, ellipse.type = "convex", geom="point",
             repel = TRUE # Avoid text overlapping
) 



fviz_mfa_quali_biplot(MFA_2, repel = FALSE, col.var = "#E7B800",
                      habillage = "region", addEllipses = TRUE, ellipse.level = 0.95)


fviz_mfa_ind(MFA_2, partial = "all") 



fviz_mfa_axes(MFA_2)

MFA_2$group$RV
MFA_2$group$Lg
MFA_2$inertia.ratio
MFA_2$group$contrib
MFA_2$group$cos2

dimdesc(MFA_2)


fviz_mfa_ind(MFA_2,
             label = "none", # hide individual labels
             habillage = final_table$clust, # color by groups
             #  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             ellipse.type = "convex" )# Concentration ellipses)
