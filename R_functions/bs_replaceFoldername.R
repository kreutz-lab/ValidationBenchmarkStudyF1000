# Replacing folder names e.g. for titles of decision trees

bs_replaceFoldername <- function(name){
  name <- sub("../Results/","",name,fixed = T)
  name <- sub("../Results_partReg/","",name,fixed = T)
  #name <- sub("5.2+5.4_","",name,fixed = T)
  name <- sub("5.2","unfiltered",name,fixed = T)
  name <- sub("5.4","filtered",name,fixed = T)
  name <- sub(" Aim2_",", ",name,fixed = T)
  name <- sub("_ancom_Nearing","",name,fixed = T)
  name <- sub("_simEquiv",", EQT-applied",name,fixed = T)
  name <- sub("_sim",", no EQT",name,fixed = T)
  name <- sub("_exp"," templates",name,fixed = T)
  name <- sub("_sparseDOSSA","\nsparseDOSSA2",name,fixed = T)
  name <- sub("metaSPARSim","\nmetaSPARSim",name,fixed = T,ignore.case = T)
  name <- sub("_"," ",name,fixed = T)
  name <- sub("_"," ",name,fixed = T)
  name <- sub(":","\n",name,fixed = T)
  name <- sub("Aim2","\nAim2",name,fixed = T)
  
  return(name)
}