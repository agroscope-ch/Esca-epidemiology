files_png_to_copy <- grep(pattern = ".png", x = list.files(path = "plots/"), value = T)
target_folder = "~/mnt/Data-Work-CH/22_Plant_Production-CH/222.6_Mycologie/05_Données utilisateurs_public/Matthieu_Wilhelm_public/Esca_Epidemiologie/plots/"
if(! dir.exists(target_folder)) dir.create(path = target_folder)
for(i in 1:length(files_png_to_copy))
{
  file.copy(from = paste("plots/",files_png_to_copy, sep="")[i], to = paste0(target_folder,files_png_to_copy[i]), overwrite = TRUE)
}
# Report
file.copy(from ="Esca_Epidemiology.html", to =paste0(target_folder, "Esca_Epidemiology.html" ), overwrite = TRUE)
