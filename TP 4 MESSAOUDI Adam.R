
salaire_net_cadre<- function(salaire){ 
  salaire = salaire*0.75
  return(salaire) }
salaire_net_cadre(1000)
salaire_net_cadre<- function(salaire=3500){ 
  salaire = salaire*0.75
  return(salaire) }
salaire_net_cadre()

salaire_net_cadre = function(salaire = 2500,temps_travail = 1) {
  salaire = salaire * 0.75 * temps_travail
  return(salaire) 
}

salaire_net_cadre(salaire = 3000,
                  temps_travail = 0.8)

salaire_net_cadre = function(salaire = 2500, temps_travail = 1){
  if(!is.numeric(salaire)){
    return("Erreur, entrez un nombre")
  }
  salaire = salaire*0.75*temps_travail
  return(salaire)
}

salaire_net_cadre(2000, 1)
salaire_net_cadre("2000", 1)


salaire_net_cadre = function(salaire = 2500, temps_travail = 1){
  if(!is.numeric(salaire)){
    return("Erreur, entrez un nombre")
  }
  if(!is.numeric(temps_travail)){
    return("Erreur, entrez un nombre")
  }
  if((temps_travail<0) | (temps_travail>1)){
    return("Erreur, le temps de travail doit être compris entre 0 et 1")
  }
  salaire = salaire*0.75*temps_travail
  return(salaire)
}

salaire_net_cadre(2000,"1")
salaire_net_cadre(2000,4)
salaire_net_cadre(2000,0.9)


salaire_net = function(salaire = 2500, temps_travail = 1, statut){
  if(!is.numeric(salaire)){
    return("Erreur, entrez un nombre")
  }
  if(!is.numeric(temps_travail)){
    return("Erreur, entrez un nombre")
  }
  if((temps_travail<0) | (temps_travail>1)){
    return("Erreur, le temps de travail doit être compris entre 0 et 1")
  }
  if(!statut %in% c("cadre", "non cadre")){
    return("Le statut doit être cadre ou non cadre")
  }
  if(statut %in% "cadre"){
    salaire = salaire*0.75*temps_travail
  }
  if(statut %in% "non cadre"){
    salaire = salaire*0.78*temps_travail
  }

  return(salaire)
}

salaire_net(2500, 1, "non cadre")
salaire_net(2500, 1, "cadre")
salaire_net(2500, 1, "etudiant")


salaire_net = function(salaire = 2500, temps_travail = 1, statut){
  if(!is.numeric(salaire)){
    return("Erreur, entrez un nombre")
  }
  if(!is.numeric(temps_travail)){
    return("Erreur, entrez un nombre")
  }
  if((temps_travail<0) | (temps_travail>1)){
    return("Erreur, le temps de travail doit être compris entre 0 et 1")
  }
  if(!statut %in% c("cadre", "non cadre")){
    return("Le statut doit être cadre ou non cadre")
  }
  if(statut %in% "cadre"){
    salaire = salaire*0.75*temps_travail
  }
  if(statut %in% "non cadre"){
    salaire = salaire*0.78*temps_travail
  }
  if (salaire <= 1591) {
    salaire = salaire
  } else if (salaire <= 2006) {
    salaire = salaire * (1 - 0.029)
  } else if (salaire <= 3476) {
    salaire = salaire * (1 - 0.099)
  } else if (salaire <= 8557) {
    salaire = salaire * (1 - 0.20)
  } else {
    salaire = salaire * (1 - 0.43)
  }
  
  return(salaire)
}


salaire_net(4200, 1, "cadre")

shifumi = function(){
  utilisateur = readline(prompt="Choisissez entre pierre, feuille et ciseaux ")
  
  if(utilisateur %in% c("pierre", "feuille", "ciseaux")){
    ordinateur = sample(c("pierre", "feuille", "ciseaux"),1)
    cat("L'ordinateur a choisi", ordinateur, " ")
    
    if(utilisateur==ordinateur){
      cat("Égalité")
    }
      else if((utilisateur=="ciseaux" & ordinateur=="feuille")|
              (utilisateur=="pierre" & ordinateur=="ciseaux")|
              (utilisateur=="feuille" & ordinateur=="pierre")){
        return("Gagné !")
      }
      else{
      return("Perdu")
      }
  }
}

shifumi()

resultat = 0
for (element in c(1,2,3,4,5)) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
}

element = 1
resultat = 0
while (resultat <= 50) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
  print(paste("le programme s'est arrêté à la valeur : ", element))
  element = element + 1
}


for (colonne in colnames(iris)) {
  type_colonne = class(iris[ , colonne])
  print(paste("la colonne ", colonne, " est de type : ", type_colonne))
}




