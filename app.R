### put here your directory 
setwd("C:/Users/Yassmine/Documents/isup/CS3") 

### packages needed
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
library(data.table)
library(shinyjs)
library(forcats)
library(shinythemes)
library(caret)
library(randomForest)
library(Metrics)
library(gridExtra)
library(heatmaply)


### importation of data

data <- read_xlsx("student-mat.xlsx")
data2 <- predict(dummyVars(~ ., data = data), newdata = data)


### explanations of the data


explanations <- list(
  school = "student's school (binary: 'GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira)",
  sex = "student's sex (binary: 'F' - female or 'M' - male)",
  age = "student's age (numeric: from 15 to 22)",
  address = "student's home address type (binary: 'U' - urban or 'R' - rural)",
  famsize = "family size (binary: 'LE3' - less or equal to 3 or 'GT3' - greater than 3)",
  Pstatus = "parent's cohabitation status (binary: 'T' - living together or 'A' - apart)",
  Medu = "mother's education (numeric: 0 - none, 1 - primary education (4th grade), 2 - 5th to 9th grade, 3 - secondary education or 4 - higher education)",
  Fedu = "father's education (numeric: 0 - none, 1 - primary education (4th grade), 2 - 5th to 9th grade, 3 - secondary education or 4 - higher education)",
  Mjob = "mother's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')",
  Fjob = "father's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')",
  reason = "reason to choose this school (nominal: close to 'home', school 'reputation', 'course' preference or 'other')",
  guardian = "student's guardian (nominal: 'mother', 'father' or 'other')",
  traveltime = "home to school travel time (numeric: 1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour)",
  studytime = "weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)",
  failures = "number of past class failures (numeric: n if 1<=n<3, else 4)",
  schoolsup = "extra educational support (binary: yes or no)",
  famsup = "family educational support (binary: yes or no)",
  paid = "extra paid classes within the course subject (Math or Portuguese) (binary: yes or no)",
  activities = "extra-curricular activities (binary: yes or no)",
  nursery = "attended nursery school (binary: yes or no)",
  higher = "wants to take higher education (binary: yes or no)",
  internet = "Internet access at home (binary: yes or no)",
  romantic = "with a romantic relationship (binary: yes or no)",
  famrel = "quality of family relationships (numeric: from 1 - very bad to 5 - excellent)",
  freetime = "free time after school (numeric: from 1 - very low to 5 - very high)",
  goout = "going out with friends (numeric: from 1 - very low to 5 - very high)",
  Dalc = "workday alcohol consumption (numeric: from 1 - very low to 5 - very high)",
  Walc = "weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)",
  health = "current health status (numeric: from 1 - very bad to 5 - very good)",
  absences = "number of school absences (numeric: from 0 to 93)",
  G1 = "first period grade (numeric: from 0 to 20)",
  G2 = "second period grade (numeric: from 0 to 20)",
  G3 = "third period grade (numeric: from 0 to 20, output target)"
)


### ui's code


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "DATA VISUALISATION PROJET"),
  dashboardSidebar(
    sidebarMenu(
      id="tabs",
      menuItem("Sommaire", tabName = "sommaire", icon = icon("list")),
      menuItem("Présentation", tabName = "presentation", icon = icon("info-circle")),
      menuItem("Analyse des variables", tabName = "new_graph", icon = icon("chart-line"),
               menuSubItem("Comparaison des 2 lycées", tabName = "comparaison", icon = icon("angle-right")),
               menuSubItem("Machine Learning", tabName = "ml", icon = icon("angle-right")),
               menuSubItem("Visualisation des résultats", tabName = "comparaison_ensemble", icon = icon("angle-right"))),
      menuItem("Prédiction", tabName = "prediction", icon=icon("star")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("thumbs-up"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sommaire",
              HTML("<h1 style='color: #003f5c; text-align: center;'><strong> Sommaire </strong></h1>"),
              div(
                HTML('<p> Vous trouverez ci-dessous le sommaire de notre projet. Ainsi, vous pouvez découvrir les différentes parties qui constituent notre présentation, en un simple clic sur la partie qui vous intéresse !</p>')
              ),
              div(actionButton("go_to_presentation", "Présentation du sujet et de la base de données", style = "border:none; background-color: transparent; color:#665191; font-size:20px; text-align: left; width:100%;")),
              div(actionButton("go_to_new_graph", "I. Analyse des variables", style = "border:none; background-color: transparent; color:#665191; font-size:20px; text-align: left; width:100%;")),
              div(actionButton("go_to_comparaison", "I.1 Comparaison des deux lycées ", style = "border:none; background-color: transparent; color:#a05195; font-size:18px; text-align: left; width:100%;")),
              div(actionButton("go_to_machine", "I.2 Machine Learning", style = "border:none; background-color: transparent; color:#a05195; font-size:18px; text-align: left; width:100%;")),
              div(actionButton("go_to_ensemble", "I.3 Visualisation des variables du modèle", style = "border:none; background-color: transparent; color:#a05195; font-size:18px; text-align: left; width:100%;")),
              div(actionButton("go_to_prediction_final", "II. Prédiction", style = "border:none; background-color: transparent; color:#665191; font-size:20px; text-align: left; width:100%;")),
              div(actionButton("go_to_conclusion", "Conclusion", style = "border:none; background-color: transparent; color:#665191; font-size:20px; text-align: left; width:100%;"))
              
      ),
      tabItem(
        tabName = "presentation",
        fluidRow(
          column(
            width = 12,
            HTML("<h1 style='color: #003f5c; text-align: center;'><strong>Qui veut peut ?</strong></h1>"),
            HTML("<h2><strong><span style='color: #665191;'>Introduction</span></strong></h2>"),
            HTML("<div class='justify-text'>
<p style='font-size: 18px; text-align: justify;'>Peut-on réellement accomplir ce que l'on souhaite ?</p>
<p style='text-align: justify;'> La question de 'qui veut peut' nous amène à réfléchir sur les ingrédients de la réussite individuelle. Cet adage suggère que la volonté et l'effort personnel sont les moteurs clés de l'épanouissement, mettant en avant l'importance de l'autodétermination. Cependant, une analyse critique nous pousse à considérer l'impact des variables socio-démographiques et environnementales. Est-ce que la réussite dépend uniquement de l'effort individuel, ou est-elle également influencée par le contexte socio-économique et culturel ? Explorer ce sujet de manière approfondie pourrait nous donner une vision plus équilibrée des parcours d'accomplissement individuel.</p>
<p style='text-align: justify;'> Pour examiner cette question, nous ferons une étude des notes de mathématiques de 395 lycéens issus de deux lycées portugais, à savoir Gabriel Pereira et Mousinho da Silveira, basé sur 33 variables scolaire mais aussi socio-démographiques que nous vous invitons à découvrir dans les items suivants :</p>
<p style='text-align: justify;'>
 <ol>
 <li> Le premier vous permettra de découvrir la base de données. Vous pourrez, en fonction de vos préférences, choisir le nombre de lignes affichées (max les 100 premières).</li>
 <li> Le second item vous permettra de mieux connaître les variables. En effet, vous pourrez sélectionner les variables que vous souhaitez et une description de celle(s)-ci apparaîtra.</li>
 <li> Enfin, le dernier item vous permettra d'avoir une première introduction visuelle des variables. De plus, vous pourrez sélectionner les variables en choisissant l'échantillon de note G3 étudié et observer leur distribution.</li>
 </ol>
</p>

</div>")
            
          )
        ),
        hr(),
        tabsetPanel(
          tabPanel("Base de données",
                   DTOutput("table")
          ),
          tabPanel("Explication des variables",
                   selectInput("selected_vars", "Choisir une ou plusieurs variables",
                               choices = colnames(data), multiple = TRUE),
                   verbatimTextOutput("var_explanations")
          ),
          tabPanel("Présentation graphique des variables",
                   HTML('<ul>
<li>Sélectionnez une ou plusieurs variables...</li>
<li>Sélectionnez la plage pour la note G3...</li>
<li>Cliquez sur le bouton "Afficher les graphiques"...</li>
</ul>'),
                   selectInput("variables", "Choisir une ou plusieurs variable(s)", multiple = TRUE, choices = colnames(data)),
                   sliderInput("g3_range", "Sélectionnez la plage pour la note G3", min = 0, max = 20, value = c(0, 20)),
                   actionButton("plot_button", "Afficher les graphiques"),
                   uiOutput("plots_ui")
          )
        )
      ),
      tabItem(
        tabName = "comparaison",
        fluidRow(
          column(
            width = 12,
            HTML("<h1 style='color: #003f5c; text-align: center;'><strong>Comparaison des deux lycées</strong></h1>"),
            HTML("<h2><strong><span style='color: #665191;'>Contexte</span></strong></h2>"),
            HTML("<div class='justify-text'>
 <p style='text-align: justify;'>Dans cette étude, les données ont été collectées auprès de deux écoles, Gabriel Pereira (GP) et Mousinho da Silveira (MS). Il faut garder à l'esprit lors de la comparaison que seuls 46 étudiants sur 395 sont allés à Mousinho da Silveira. Il faut donc être très vigileant à l'interprétation et aux conclusions qu'on pourrait en tirer, pour cela nous ferons les comparaisons uniquement en proportion.</p>
 </div>"),
            HTML("<h3><strong><span style='color: #a05195;'>Distribution des notes</span></strong></h3>"),
            HTML("<div class='justify-text'>
 <p style='text-align: justify;'>Pour comparer les notes dans les deux établissements, nous vous proposons de regarder le barplot et le boxplot ci-dessous. Pour rappel, G1 correspond à la note du T1, G2 du T2, et G3 la note T3. </p>
 <p style='text-align: justify;'>Le boxplot vous permettra de comparer différentes informations statistiques telles que la médiane, les quartiles, l'étendue des notes, tandis que le barplot vous permettra de regarder la proportion d'étudiants dans chaque intervalle de notes.</p>
 </div>"),
            tabsetPanel(
              tabPanel("Barplot",
                       selectInput("y_variable1", "Choisir une variable Y", 
                                   choices = c("G1" = "G1", "G2" = "G2", "G3" = "G3")),
                       plotlyOutput("barplot")
              ),
              tabPanel("Boxplot",
                       selectInput("y_variable", "Choisir une variable Y", 
                                   choices = c("G1" = "G1", "G2" = "G2", "G3" = "G3")), # Choix de G1, G2 ou G3 
                       plotlyOutput("boxplot")
              )
            ),
            hr(), 
            p("Comme vous avez pu le constater avec l'analyse du barplot et du boxplot, le lycée GP semble être plus académique que le lycée MS."),
            p("En outre, si on se focalise sur G3 ( ou bien même G1 et G2 mais cela est moins visible), on remarque que beaucoup plus d'élèves (en proportion) ont des notes supérieures à 10. En effet, le nombre d'élèves avec des faibles notes est plus important à MS qu'à GP, tandis que le nombre d'élèves avec des notes élevées est plus important à GP. Enfin, la médiane et la note maximale sont meilleures à GP."),
            p("Attention, il est vrai qu'on remarque que MS a des profils d'élèves en mathématiques plus homogènes, mais ce constat ne doit pas être pris en compte car cela relève surtout du faible effectif de notes de MS dont on dispose."),
            p(HTML("Pour confirmer notre hypothèse, nous avons donc voulu analyser les variables:
 <ul>
 <li>échec</li>
 <li>durée d'étude par jour</li>
 <li>soutien scolaire</li>
 </ul>")),
            hr(),
            sidebarLayout(
              sidebarPanel(
                selectInput("variable", "Choisissez une variable :",
                            choices = c("schoolsup" = "schoolsup", "studytime" = "studytime", "failures" = "failures"))
              ),
              mainPanel(
                plotOutput("hist_plot")
              )
            ),
            p("L'analyse des ces variables permet de confirmer notre hypothèse car les élèves étudient plus et ont moins d'échecs dans cette école. "),
            p("Mais qu'est ce qui pourrait faire que ce lycée GP ai des éléves qui ont des meilleures profils en mathématiques?"),
            HTML("<h3><strong><span style='color: #a05195;'>Influence des parents ?</span></strong></h2>"),
            p("La première idée qui pourrait nous venir à l'esprit, est probablement l'influence de la famille ie des parents. En outre, il est vrai que si les parents de GP ont plus fait d'étude que MS, alors potentiellement ils pourraient d'avantage les aider ou auraient pu davantager les aider durant leur scolarité, ce qui pourrait expliquer cet écart de résultats. "),
            p("Pour vérifier cette hypothèse nous allons voulu regarder le niveau d'éducation ( et métier ) des parents."),
            p("La première chose à faire est de regarder avec qui vivent la majorité des enfants. Ainsi on peut noter que la plupart des étudiants vivent avec leurs deux parents,et pour ceux qui vivent avec l'un des deux parents, c'est la mère qui est, dans la plupart des cas, le tuteur légal. En effet:")
          ),
          column(
            width = 6,
            plotlyOutput("pie_chart_psta", width = "100%", height = "300px")
          ),
          column(
            width = 6,
            plotlyOutput("pie_chart_guardian", width = "100%", height = "300px"),
          ),
          
          column(
            width = 12,
            hr(),
            p("De ce fait, on va devoir s'intéresser aux deux parents."),
            HTML("<h4><strong><span style='color: #2f4b7c;'> Education des parents </span></strong></h2>"),
            
            selectInput("x_variable", "Choisir la variable x", choices = c("Medu", "Fedu", "Mjob", "Fjob")),
            plotlyOutput("distribution_plotss")
          ),
          hr(),
          column(
            width=12,
            p("On remarque, éffectivement, que les parents de GP que se soit le père ou la mère, sont plus nombreux à avoir fait des études et notamment des études de longue durées. Cela est plus percutant pour la mère que pour le père."),
            HTML("<div class='justify-text'>
 <p style='text-align: justify;'> On peut conclure, par dire que le niveau d'éducation semblerait avoir un impact sur la réussite d'un élève ( en mathématiques). Pour essayer de trouver d'autres facteurs explicatifs, nous poursuiveront notre étude sur l'ensemble du jeu de données, afin d'avoir une place grande confiance en nos indicateurs ( historique de données trop faible pour MS).Par conséquent,dans la prochaine partie nous comparerons les classes de notes G3 au lieu des lycées.</p>
 </div>"),
            HTML("<h4><strong><span style='color: #2f4b7c;'> Association metier/education? </span></strong></h2>"),
            HTML("L'association entre le niveau d'éducation de la mère(resp. père) et l'emploi de la mère(resp. père) a été étudiée en premier lieu, car il est raisonnable de penser qu'il existe une association entre ces deux variables.<br>
  Ce graphique represente la proportion des niveaux d'emploi de la mère(resp. père) est représentée pour chaque niveau d'éducation de la mère(resp. père)."), # Ajouter du texte en utilisant HTML
            sidebarLayout(
              sidebarPanel(
                selectInput("var3", "Choisir le couple de variables",
                            choices = c("Medu vs Mjob","Fedu vs Fjob"
                            )),
                sliderInput("g3_filter2", "Filtrer par G3:", min = 0, max = 20, value = c(0, 20))
                
                
                
              ),
              mainPanel(
                plotOutput("VarPlot2")
              )),
            HTML("Après examination de l'association entre le niveau d'éducation des parents et leur emploi, on remarque une association significative entre le niveau d'éducation des parents (Fedu/Medu) et leur emploi (FjobD/MjobD)."),
            HTML("De plus, une association significative a été observée entre l'emploi de la mère et du père. L'éducation de la mère et le soutien familial (famsup) étaient également associés, suggérant que la mère était souvent responsable du soutien familial."),
            HTML("Ces constatations renforcent notre compréhension des facteurs qui influencent la réussite scolaire des élèves, mettant en évidence le lien entre le métier des parents, leur niveau d'éducation et la réussite de l'élève.") 
          )
        )
      ),
      tabItem(tabName="ml",
              fluidRow(
                column(
                  width = 12,
                  HTML("<h1 style='color: #003f5c; text-align: center;'><strong> Un peu de machine learning ... </strong></h1>"),
                  HTML("<h3><strong><span style='color: #a05195;'> Rappel du contexte </span></strong></h3>"),
                  HTML("<div class='justify-text'>
 <p style='text-align: justify;'> Dans la sous-partie précédente, nous avions commencé par étudier les deux lycées, mais manque de données nous avons conclus qu'il était préférable de faire une étude sur l'ensemble du jeu de données. Cependant, notre base de données contient 32 potentiels variables explicatives. C'est pour cela que nous avons fait appel au machine learning pour étudier les 'vraies' variables explicatives de G3. </p>
 </div>"),
                  HTML("<h2><strong><span style='color: #665191;'> Matrice de corrélation </span></strong></h2>"),
                  HTML("<div class='justify-text'>
 <p style='text-align: justify;'> Pour commencer, nous avons voulu plot une matrice de corrélation. Il faut savoir qu'une matrice de corrélation se fait généralement avec des données numérique, ce qui n'est pas le cas de toutes nos variables. Pour faire face à ce problème nous avons créer data2 : </p>
 </div>"),
                  p("data2 <- predict(dummyVars(~ ., data = data), newdata = data)"),
                  HTML("<div class='justify-text'>
 <p style='text-align: justify;'> Les variables factices ont été créées à partir des données catégorielles initiales ( grace à la fonction dummyVars) pour permettre l'utilisation de ces informations dans des modèles d'analyse( et/ ou d'apprentissage automatique). Cette transformation permet de représenter les catégories sous forme de variables binaires, facilitant ainsi l'intégration des caractéristiques catégorielles dans des analyses statistiques et des prévisions tout en préservant la structure d'origine des données. </p>
 </div>"),
                  
                  mainPanel(
                    plotlyOutput("heatmap")
                  ),
                  HTML("<div class='justify-text'>
 <p style='text-align: justify;'> Aussi jolie soit-elle, le grand nombres de variables rend très difficile l'analyse de cette dernière. Pour cela passons à de l'apprentissage automatique </p>
 </div>")),
                
                column(
                  width = 12,
                  HTML("<h2><strong><span style='color: #665191;'> Random Forest </span></strong></h2>"),
                  HTML("<div class='justify-text'>
 <p style='text-align: justify;'> Nous avions donc testé différents modèle d'apprentissages tels que la regression multiple, SVM etc, mais le random etait celui qui nous donnait les meilleurs résultats. </p>
 </div>"),
                  HTML("<div class='justify-text'>
 <p style='text-align: justify;'>  
 &bull; <strong>Première étape :</strong> on a séparé notre jeu de données en un jeu d'apprentissage avec lequel nous entraînerons notre modèle et un jeu test pour tester notre modèle par la suite en le comparant aux données de prédictions.
 <br>
 &minus; <code>train_percentage <- 0.8</code>
 <br>
 &minus; <code>total_observations <- nrow(data)</code>
 <br>
 &minus; <code>train_size <- round(train_percentage * total_observations)</code>
 <br>
 &minus; <code>train_indices <- sample(1:total_observations, train_size, replace = FALSE)</code>
 <br>
 &minus; <code>train_data <- data[train_indices, ]</code>
 <br>
 &minus; <code>test_data <- data[-train_indices, ]</code>
 </p>
</div>"),
                  HTML("<div class='justify-text'>
 <p style='text-align: justify;'>  
 &bull; <strong> Deuxième étape : </strong> <code>model_rf <- randomForest(G3 ~ ., data = train_data)</code>, qui nous donne les variables importantes que vous pouvez retrouver dans l'item importance des variables. 
 Ainsi, les variables qui semblent expliquer le plus G3 sont :
 <br>
 &minus; G1
 <br>
 &minus; G2
 <br>
 &minus; failures
 <br>
 &minus; absences
 <br>
 &minus; age
 <br>
 &minus; goout
 <br>
 &minus; Fjob
 <br>
 &minus; Mjob
 <br>
 &minus; Medu
 </p>
</div>"),
                  HTML("<div class='justify-text'>
 <p style='text-align: justify;'>  
 &bull; <strong> Troisième étape :</strong> ajustement du modèle
 <code>model_rf2 <- randomForest(G3 ~ G1+G2+failures+absences+age+goout+Fjob+Mjob+Medu, data = train_data)</code>
 </p>
</div>"),
                  HTML("<div class='justify-text'>
 <p style='text-align: justify;'>  
 <strong> Quatrième étape : </strong> comparaison des model_rf & model_rf2.
 <br>
 D'abord, dans l'item 'importance des variables', on a la sortie du modèle 2, et on peut comparer ces deux modèles dans les items Métriques et Prédictions.
 </p>
 <ul>
  <li>Les métriques comparatives pour les modèles 'RF' et 'RF2' sont les suivantes :</li>
  <ul>
  <li>R² (Coefficient de détermination) : RF (0.84) explique 84 % de la variance des données, tandis que RF2 (0.85) est légèrement meilleur.</li>
  <li>RMSE (Erreur quadratique moyenne) : RF (1.97) a une erreur moyenne de 1.97 unités par rapport aux valeurs réelles, tandis que RF2 (1.91) est légèrement plus précis.</li>
  <li>MAE (Erreur moyenne absolue) : RF (1.42) a une erreur moyenne de 1.42 unités par rapport aux valeurs réelles, tandis que RF2 (1.37) est légèrement plus précis.</li>
  </ul>
  <li>Concernant les prédictions, on observe graphiquement que model_rf2 prédit mieux.</li>
 </ul>
</div>"),
                  
                  
                  
                  tabsetPanel(
                    tabPanel("Importance des variables", tableOutput("varImportance")),
                    tabPanel("Métrique", tableOutput("resultsTable")),
                    tabPanel("Prédictions", 
                             fluidRow(
                               column(6, plotlyOutput("plot1")), # Divise l'écran en deux colonnes
                               column(6, plotlyOutput("p2")) # Divise l'écran en deux colonnes
                             )
                    )
                    
                  )
                  
                )
              )
      ),
      tabItem(
        tabName = "comparaison_ensemble",
        HTML("<h1 style='color: #003f5c; text-align: center;'><strong> Visualisation de l'association des variables importantes </strong></h1>"),
        
        HTML("<p>L'analyse des performances scolaires est une tâche complexe qui dépend de divers facteurs. Dans le cadre de notre étude, nous avons choisi de nous concentrer sur plusieurs variables clés afin de mieux comprendre les associations qui pourraient influencer les résultats du troisième trimestre (T3). Ces variables ont été sélectionnées en se basant sur les résultats d'un modèle de forêt aléatoire (random forest), qui a identifié celles-ci comme les plus significatives pour prédire les performances académiques.</p>"),
        
        HTML("<p>Les variables examinées comprennent la relation entre les notes du premier trimestre (T1) et des facteurs tels que le temps consacré à sortir avec des amis, les absences en classe, les échecs académiques, l'âge des étudiants, le métier de la mère, et le niveau d'éducation de la mère. Ces choix ont été guidés par le désir de comprendre comment des éléments de la vie sociale, des aspects familiaux et d'autres facteurs peuvent impacter les performances académiques des étudiants.</p>"),
        
        
        sidebarLayout(
          sidebarPanel(
            selectInput("var", "Choisir le couple de variables",
                        choices = c("G1 vs Go Out","absences vs Go Out","Failures vs Go Out","age vs failure","Mjob vs G2","G1 vs Medu"
                        ))
            
            
            
          ),
          mainPanel(
            plotOutput("VarPlot")
          )
        ),
        HTML("<h3><strong><span style='color: #a05195;'>Notes T1 vs Sortir avec ses amis ?</span></strong></h2>
 <p>Le premier graphique examine la corrélation entre les notes du premier trimestre (T1) et le temps consacré à sortir avec des amis. On observe une tendance intéressante qui suggère que les étudiants passant plus de temps à socialiser ont tendance à obtenir des notes légèrement inférieures. Cette association peut s'expliquer par le fait que le temps consacré aux activités sociales pourrait être du temps perdu pour les études, ce qui pourrait affecter les performances académiques.</p>"),
        
        HTML("<h3><strong><span style='color: #a05195;'>Absences vs Sortir avec ses amis ?</span></strong></h2>
 <p>Le deuxième graphique explore le lien entre les absences en classe et le temps passé à sortir avec des amis. Il révèle une corrélation négative, indiquant que les étudiants qui sortent fréquemment ont tendance à avoir un nombre plus élevé d'absences. Cela pourrait être dû à une éventuelle négligence des responsabilités académiques au profit d'activités sociales.</p>"),
        
        HTML("<h3><strong><span style='color: #a05195;'>Echecs vs Sortir avec ses amis ?</span></strong></h2>
 <p>En ce qui concerne les échecs académiques, le troisième graphique met en lumière une association significative entre le fait de sortir avec des amis et le nombre d'échecs. Les élèves passant beaucoup de temps en activités sociales semblent avoir un risque accru d'échecs. Cette corrélation pourrait être attribuée au manque de concentration et de préparation résultant des sorties fréquentes.</p>"),
        
        HTML("<h3><strong><span style='color: #a05195;'>Age vs Echecs ?</span></strong></h2>
 <p>Le quatrième graphique examine la relation entre l'âge des étudiants et leur nombre d'échecs. Étonnamment, il suggère qu'un âge plus avancé est associé à un risque accru d'échecs. Cela pourrait être expliqué par des défis personnels ou des responsabilités accrues liés à l'âge.</p>"),
        
        HTML("<h3><strong><span style='color: #a05195;'>Metier mère vs Notes T2 ?</span></strong></h2>
 <p>Le cinquième graphique met en évidence une corrélation entre le métier de la mère et les notes du deuxième trimestre (T2). On observe que certaines professions maternelles sont associées à des performances académiques plus élevées. Cette relation peut être influencée par divers facteurs tels que le soutien parental, les ressources éducatives disponibles, etc.</p>"),
        
        HTML("<h3><strong><span style='color: #a05195;'>Niveau education vs mère Notes T1 ?</span></strong></h2>
 <p>Enfin, le dernier graphique explore la relation entre les notes du premier trimestre (T1) et le niveau d'éducation de la mère. On constate une corrélation positive, suggérant que les étudiants dont les mères ont un niveau d'éducation plus élevé ont tendance à obtenir de meilleures notes. Cette association pourrait être attribuée à un environnement familial favorisant l'éducation et l'apprentissage.</p>")
        
        
      ),
      tabItem(
        tabName = "prediction",
        fluidRow(
          column(
            width = 12,
            HTML("<h1 style='color: #003f5c; text-align: center;'><strong> Prédiction </strong></h1>"),
            HTML("<h2><span style='color:#665191;'> Enfin arrivé à la partie prédiction ! </span><h2>"),
            p("Merci de spécifier les valeurs correspondantes pour chacune des variables ci-dessous."),
            sidebarLayout(
              sidebarPanel(
                sliderInput("g1", "G1 (0-20):", min = 0, max = 20, value = 10),
                sliderInput("g2", "G2 (0-20):", min = 0, max = 20, value = 10),
                sliderInput("failures", "Failures (1-4):", min = 1, max = 4, value = 2),
                sliderInput("age", "Age (15-22):", min = 15, max = 22, value = 18),
                sliderInput("goout", "Goout (1-5):", min = 1, max = 5, value = 3),
                sliderInput("absences", "absences (0-93):", min=0, max=93, value=2),
                selectInput("fjob", "Fjob:", choices = c("teacher", "health", "services", "at_home", "other")),
                selectInput("mjob", "Mjob:", choices = c("teacher", "health", "services", "at_home", "other")),
                selectInput("medu", "Medu:", choices = c("0 - none", "1 - primary", "2 - 5th to 9th", "3 - secondary or higher"))
              ),
              mainPanel(
                HTML("<h3><strong><span style='color:#a05195;'> La note que vous obtiendrez sera probablement: </span></strong><h2>"),
                textOutput("valeur_affichee")
                
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "conclusion",
        fluidRow(
          column(
            width = 12,
            HTML("<h1 style='color: #003f5c; text-align: center;'><strong> Conclusion </strong></h1>"),
            HTML("<div class='justify-text'>
 <p style='text-align: justify;'> En conclusion, l'idée 'qui veut peut' a été nuancée par notre analyse. Bien que la volonté et l'effort personnel soient importants, notre examen graphique et le modèle de machine learning révèlent l'influence significative de facteurs socio-démographiques sur la performance académique. La réussite ne dépend pas uniquement de l'effort individuel, mais aussi de variables telles que les échecs antérieurs, l'absentéisme, l'âge, les activités extra-scolaires, le métier des parents et le niveau d'éducation de la mère. Ainsi, qui ne veut pas ne peut pas toujours ! Des facteurs indépendants de notre volonté, tels que le métier des parents, influent directement sur notre parcours. Il est essentiel d'adapter les systèmes éducatifs pour atténuer ces inégalités et éviter leur perpétuation de génération en génération.</p>
 </div>")
          )
        )
      )
    )
  )
)


### server's code


server <- function(input, output, session) {
  
  
  output$table <- renderDataTable({
    
    datatable(data, options = list(pageLength = 5, autoWidth = TRUE, scrollX = TRUE))
    
  })
  
  selected_vars <- reactive({
    req(input$selected_vars)
    return(input$selected_vars)
  })
  
  explanations_output <- renderText({
    if (length(selected_vars()) == 0) {
      return("Sélectionnez au moins une variable.")
    }
    explanation_text <- lapply(selected_vars(), function(var) {
      return(paste("Explication de", var, ":", explanations[[var]]))
    })
    return(paste(explanation_text, collapse = "\n"))
  })
  
  output$var_explanations <- renderText({
    return(explanations_output())
  })
  
  
  observeEvent(input$plot_button, {
    
    selected_variables <- input$variables
    
    g3_range <- input$g3_range 
    
    filtered_data <- data %>%
      
      filter(G3 >= g3_range[1] & G3 <= g3_range[2])
    
    
    
    plots <- lapply(selected_variables, function(variable) {
      
      plot_data <- ggplot(filtered_data, aes(x = .data[[variable]])) +
        
        geom_bar(fill = "#a05195") +
        
        labs(x = variable, y = "Fréquence") +
        
        ggtitle(variable) +
        
        theme(plot.title = element_text(hjust = 0.5, size = 10))
      
      ggplotly(plot_data)
      
    })
    
    
    
    
    output$plots_ui <- renderUI({
      
      if (length(plots) == 0) {
        
        return(NULL)
        
      }
      
      
      
      plot_grid <- list()
      
      
      
      for (i in seq_along(plots)) {
        
        plot_grid <- c(plot_grid, list(column(width = 3, plots[[i]])))
        
        
        
        if (i %% 4 == 0 || i == length(plots)) {
          
          plot_grid <- c(plot_grid, list(column(width = 12, div(style = "height:20px;"))))
          
        }
        
      }
      
      
      
      do.call(fluidRow, plot_grid)
      
    })
    
    
    
    updateTabsetPanel(session, "tabs", selected = "new_graph")
    
  })
  
  
  
  
  observeEvent(input$go_to_presentation, {
    
    updateTabsetPanel(session, "tabs", selected = "presentation")
    
  })
  
  
  observeEvent(input$go_to_analyse, {
    
    updateTabsetPanel(session, "tabs", selected = "new_graph")
    
  })
  
  
  observeEvent(input$go_to_comparaison, {
    
    updateTabsetPanel(session, "tabs", selected = "comparaison")
    
  })
  
  
  observeEvent(input$go_to_ensemble, {
    
    updateTabsetPanel(session, "tabs", selected = "comparaison_ensemble")
    
  })
  
  observeEvent(input$go_to_machine, {
    updateTabItems(session, "tabs", selected = "ml")
  })
  
  
  observeEvent(input$go_to_prediction_final, {
    updateTabItems(session, "tabs", selected = "prediction")
  })
  
  
  
  observeEvent(input$go_to_conclusion, {
    updateTabItems(session, "tabs", selected = "conclusion")
  })
  
  
  
  output$boxplot <- renderPlotly({
    selected_variable <- input$y_variable
    
    p1 <- ggplot(data, aes(x = school, y = .data[[selected_variable]], fill = school)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", selected_variable)) +
      theme_minimal()
    
    p1 <- p1 + scale_fill_manual(values = c("GP" = "#2f4b7c", "MS" = "#a05195")) # Personnalisez les couleurs ici
    
    plotly_plot <- ggplotly(p1)
    
    return(plotly_plot)
  })
  
  
  
  data_MS <- subset(data, school == "MS") 
  data_GP <- subset(data, school == "GP")
  
  output$barplot <- renderPlotly({
    y_variable1 <- input$y_variable1
    
    # Créez une nouvelle colonne catégorielle basée sur les notes
    data_MS$Category <- cut(data_MS[[y_variable1]], breaks = c(0, 5, 10, 15, 20), labels = c("1", "2", "3", "4"),include.lowest = TRUE)
    data_GP$Category <- cut(data_GP[[y_variable1]], breaks = c(0, 5, 10, 15, 20), labels = c("1", "2", "3", "4"),include.lowest = TRUE)
    
    
    # Calculez les proportions pour chaque catégorie
    proportion_classes_MS <- table(data_MS$Category) / nrow(data_MS)
    proportion_classes_GP <- table(data_GP$Category) / nrow(data_GP)
    
    
    
    # Définissez une palette de couleurs pour chaque catégorie
    colors <- c("#003f5c", "#2f4b7c", "#665191", "#a05195")
    
    barplot_MS <- plot_ly(
      x = levels(data_MS$Category),
      y = proportion_classes_MS,
      type = 'bar',
      name = '(École MS)',
      marker = list(color = colors),
      showlegend = FALSE
    ) %>%
      add_annotations(
        text = "Ecole MS",
        showarrow = FALSE,
        x = 0.5,
        y = 1.05,
        xref = "paper",
        yref = "paper",
        showlegend = FALSE
      )
    
    barplot_MS <- barplot_MS %>% layout(xaxis = list(ticktext = c("0-5", "5-10", "10-15", "15-20"), tickvals = c(1, 2, 3, 4)))
    
    
    
    
    barplot_GP <- plot_ly(
      x = levels(data_GP$Category),
      y = proportion_classes_GP,
      type = 'bar',
      name = '(École GP)',
      marker = list(color = colors),
      showlegend = FALSE
    ) %>%
      add_annotations(
        text = "Ecole GP",
        showarrow = FALSE,
        x = 0.5,
        y = 1.05,
        xref = "paper",
        yref = "paper",
        showlegend = FALSE
      )
    barplot_GP <- barplot_GP %>% layout(xaxis = list(ticktext = c("0-5", "5-10", "10-15", "15-20"), tickvals = c(1, 2, 3, 4)))
    
    
    
    
    subplot(
      barplot_GP ,
      barplot_MS
    )
    
    
  })
  
  
  
  
  
  
  
  
  
  output$hist_plot <- renderPlot({
    
    var <- input$variable
    
    table_var <- table(data$school, data[[var]])
    
    table_var <- prop.table(table_var, margin = 1)
    
    table_var <- melt(table_var, id.vars = "school")
    
    data_var <- data.frame(table_var)
    
    
    
    ggplot(data = data_var, aes(x = Var2, y = value, fill = Var1)) +
      
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      
      labs(title = paste("Proportion de", var, "par école"),
           
           x = var, y = "Proportion") +
      
      theme_minimal() +
      
      scale_fill_manual(values = c("GP" = "#003f5c", "MS" = "#a05195"),
                        
                        name = "École",
                        
                        labels = c("GP", "MS")) +
      
      scale_y_continuous(labels = scales::percent)
    
  })
  
  
  
  percentage_Psta <- data %>%
    
    group_by(Pstatus) %>%
    
    summarise(Percentage = n() / nrow(data) * 100)
  
  
  
  pie_chart_psta <- plot_ly(data = percentage_Psta, labels = ~Pstatus, values = ~Percentage, type = "pie", marker = list(colors = c("#003f5c", "#a05195")))
  
  
  
  output$pie_chart_psta <- renderPlotly({
    
    pie_chart_psta
    
  })
  
  
  
  filtered_data <- data %>%
    
    filter(Pstatus == "A")
  
  
  
  percentage_guardian <- filtered_data %>%
    
    group_by(guardian) %>%
    
    summarise(Percentage = n() / nrow(filtered_data) * 100)
  
  
  
  pie_chart_guardian <- plot_ly(data = percentage_guardian, labels = ~guardian, values = ~Percentage, type = "pie", marker = list(colors = c("#003f5c", "#665191", "#a05195")))
  
  
  
  output$pie_chart_guardian <- renderPlotly({
    
    pie_chart_guardian
    
  })
  
  
  library(plotly)
  output$distribution_plotss <- renderPlotly({
    combined_plot <- subplot(
      # Cela crée une cellule vide pour le graphique GP
      {
        filtered_data_GP <- data %>%
          filter(school == "GP")
        
        variable_data_GP <- filtered_data_GP %>%
          group_by(!!sym(input$x_variable)) %>%
          summarize(count = n()) %>%
          mutate(percentage = count / sum(count) * 100)
        
        if (input$x_variable %in% c("Medu", "Fedu")) {
          variable_data_GP <- variable_data_GP %>%
            mutate(!!sym(input$x_variable) := factor(!!sym(input$x_variable),
                                                     levels = c(0, 1, 2, 3, 4),
                                                     labels = c("None", "Primary Education", "5th to 9th Grade", "Secondary Education", "Higher Education")))
        }
        
        gg_GP <- ggplot(variable_data_GP, aes(x = reorder(!!sym(input$x_variable), -percentage), y = percentage)) +
          geom_bar(stat = "identity", fill = "#2f4b7c") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(gg_GP)
      },
      {
        filtered_data_MS <- data %>%
          filter(school == "MS")
        
        variable_data_MS <- filtered_data_MS %>%
          group_by(!!sym(input$x_variable)) %>%
          summarize(count = n()) %>%
          mutate(percentage = count / sum(count) * 100)
        
        if (input$x_variable %in% c("Medu", "Fedu")) {
          variable_data_MS <- variable_data_MS %>%
            mutate(!!sym(input$x_variable) := factor(!!sym(input$x_variable),
                                                     levels = c(0, 1, 2, 3, 4),
                                                     labels = c("None", "Primary Education", "5th to 9th Grade", "Secondary Education", "Higher Education")))
        }
        
        gg_MS <- ggplot(variable_data_MS, aes(x = reorder(!!sym(input$x_variable), -percentage), y = percentage)) +
          geom_bar(stat = "identity", fill = "#a05195") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(gg_MS)
      }
    )
    combined_plot <- layout(
      combined_plot,
      title = "Distribution des données pour l'école GP et MS par variable",
      margin = list(t = 100) # Ajustez la marge supérieure pour faire de la place au titre général
    )
    
    # Ajoutez une légende
    combined_plot <- layout(combined_plot, showlegend = TRUE)
    
    combined_plot
  })
  
  
  
  
  correlation_matrix_data <- cor(data2)
  
  # Affichez la heatmap interactive
  output$heatmap <- renderPlotly({
    heatmaply(correlation_matrix_data, trace = "none", col = colorRampPalette(c("#a05195", "#2f4b7c"))(50))
  })
  
  
  
  set.seed(123)
  train_percentage <- 0.8
  total_observations <- nrow(data)
  train_size <- round(train_percentage * total_observations)
  train_indices <- sample(1:total_observations, train_size, replace = FALSE)
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  
  
  # RANDOM FOREST
  model_rf <- randomForest(G3 ~ ., data = train_data)
  model_rf2 <- randomForest(G3 ~ G1+G2+failures+absences+age+goout+Fjob+Mjob+Medu, data = train_data)
  
  output$varImportance <- renderTable({
    var_importance <- importance(model_rf)
    var_names <- rownames(var_importance)
    
    # Créez un vecteur avec les valeurs d'importance triées
    sorted_importance <- var_importance[order(-var_importance)]
    
    # Créez un vecteur de noms triés correspondant aux valeurs d'importance
    sorted_names <- var_names[order(-var_importance)]
    
    # Créez un dataframe à partir des vecteurs triés
    var_importance_df <- data.frame(Variable = sorted_names, Importance = sorted_importance)
    print(var_importance_df)
  })
  
  
  
  output$plot1 <- renderPlotly({
    predictions_rf <- predict(model_rf, newdata = test_data)
    p1 <- plot_ly(x = test_data$G3, y = predictions_rf, type = 'scatter', mode = 'markers', marker = list(color = "#a05195"))
    p1 <- p1 %>% layout(xaxis = list(title = "Valeurs Réelles (G3)"), yaxis = list(title = "Prédictions (G3)"))
    return(p1)
  })
  
  
  output$p2 <- renderPlotly({
    predictions_rf2 <- predict(model_rf2, newdata = test_data)
    p2 <- plot_ly(x = test_data$G3, y = predictions_rf2, type = 'scatter', mode = 'markers', marker = list(color = "#a05195"))
    p2 <- p2 %>% layout(xaxis = list(title = "Valeurs Réelles (G3)"), yaxis = list(title = "Prédictions (G3)"))
    return(p2)
  })
  
  
  
  # TABLEAU POUR COMPARER LES VALEURS
  output$resultsTable <- renderTable({
    predictions_rf <- predict(model_rf, newdata = test_data)
    r_squared_rf <- 1 - sum((test_data$G3 - predictions_rf)^2) / sum((test_data$G3 - mean(test_data$G3))^2)
    rmse_rf <- sqrt(mean((predictions_rf - test_data$G3)^2))
    mae_rf <- mean(abs(predictions_rf - test_data$G3))
    
    predictions_rf2 <- predict(model_rf2, newdata = test_data)
    r_squared_rf2 <- 1 - sum((test_data$G3 - predictions_rf2)^2) / sum((test_data$G3 - mean(test_data$G3))^2)
    rmse_rf2 <- sqrt(mean((predictions_rf2 - test_data$G3)^2))
    mae_rf2 <- mean(abs(predictions_rf2 - test_data$G3))
    
    results <- data.frame(
      Metric = c("R²", "RMSE", "MAE"),
      RF = c(r_squared_rf, rmse_rf, mae_rf),
      RF2 = c(r_squared_rf2, rmse_rf2, mae_rf2)
    )
    
    results
  })
  
  
  
  output$VarPlot <- renderPlot({
    
    
    if (input$var == "G1 vs Go Out") {
      
      
      prop_data <- data %>%
        group_by(goout, G1) %>%
        
        summarise(count = n()) %>%
        
        mutate(prop = count / sum(count)) %>%
        
        ungroup() %>%
        
        arrange(goout)
      
      
      ggplot(prop_data, aes(x = G1, y = prop, fill = as.factor(goout))) +
        
        geom_bar(stat = "identity", position = position_dodge()) +
        
        scale_y_continuous(labels = scales::percent) +
        
        labs(x = "G1", y = "Proportion", fill = "Go Out") +
        
        scale_fill_manual(values = c("1" = "#003f5c", "2" = "#2f4b7c", "3" = "#665191", "4" = "#a05195", "5" = "#d45087"), 
                          
                          labels = c("Très peu", "Peu", "Moyen", "Haut", "Très haut")) +
        
        theme_minimal() +
        
        theme(legend.title = element_blank())
      
    }
    
    
    else if (input$var == "absences vs Go Out") {
      
      
      prop_da <- data %>%
        
        group_by(goout, absences) %>%
        
        summarise(count = n()) %>%
        
        mutate(prop = count / sum(count)) %>%
        
        ungroup() %>%
        
        arrange(goout)
      
      
      # Créer un barplot avec les proportions
      
      ggplot(prop_da, aes(x = as.factor(absences), y = prop, fill = as.factor(goout))) +
        
        geom_bar(stat = "identity", position = position_dodge()) +
        
        scale_y_continuous(labels = scales::percent) +
        
        labs(x = "absences ", y = "Proportion de sorties entre amis", fill = "Go Out") +
        
        scale_fill_brewer(palette = "Set1") +
        
        
        scale_fill_manual(values = c("1" = "#003f5c", "2" = "#2f4b7c", "3" = "#665191", "4" = "#a05195", "5" = "#d45087"), 
                          
                          labels = c("Très peu", "Peu", "Moyen", "Haut", "Très haut")) +
        
        theme_minimal() +
        
        theme(legend.title = element_blank())
      
    }
    
    else if (input$var == "Failures vs Go Out") {
      
      prop_d <- filtered_data %>%
        
        group_by(goout, failures) %>%
        
        summarise(count = n()) %>%
        
        mutate(prop = count / sum(count)) %>%
        
        ungroup() %>%
        
        arrange(goout)
      
      
      # Créer un barplot avec les proportions
      
      ggplot(prop_d, aes(x = as.factor(failures), y = prop, fill = as.factor(goout))) +
        
        geom_bar(stat = "identity", position = position_dodge()) +
        
        scale_y_continuous(labels = scales::percent) +
        
        labs(x = "Failures", y = "Proportion", fill = "Go Out") +
        
        scale_fill_manual(values = c("1" = "#003f5c", "2" = "#2f4b7c", "3" = "#665191", "4" = "#a05195", "5" = "#d45087"), 
                          
                          labels = c("Très peu", "Peu", "Moyen", "Haut", "Très haut")) +
        
        theme_minimal() +
        
        theme(legend.title = element_blank())
      
    }
    
    
    else if (input$var == "Mjob vs G2") {
      
      prop_d2 <- data %>%
        
        group_by(G2, Mjob) %>%
        
        summarise(count = n()) %>%
        
        mutate(prop = count / sum(count)) %>%
        
        ungroup() %>%
        
        arrange(Mjob)
      
      
      # Créer un barplot avec les proportions
      
      ggplot(prop_d2, aes(x = as.factor(G2), y = prop, fill = as.factor(Mjob))) +
        
        geom_bar(stat = "identity", position = position_dodge()) +
        
        scale_y_continuous(labels = scales::percent) +
        
        labs(x = "G2", y = "", fill = "Mjob") +
        scale_fill_manual(values = c("at_home" = "#003f5c", "health" = "#2f4b7c", "other" = "#665191", "services" = "#a05195", "teacher" = "#d45087"))+
        
        theme_minimal() + theme(legend.title = element_blank())
      
    }
    
    else if (input$var == "G1 vs Medu") {
      
      prop_d2 <- data %>%
        
        group_by(G1, Medu) %>%
        
        summarise(count = n()) %>%
        
        mutate(prop = count / sum(count)) %>%
        
        ungroup() %>%
        
        arrange(Medu)
      
      
      # Créer un barplot avec les proportions
      
      ggplot(prop_d2, aes(x = as.factor(G1), y = prop, fill = as.factor(Medu))) +
        
        geom_bar(stat = "identity", position = position_dodge()) +
        
        scale_y_continuous(labels = scales::percent) +
        
        labs(x = "G1", y = "", fill = "Medu") +
        scale_fill_manual(values = c("0" = "#003f5c", "1" = "#2f4b7c", "2" = "#665191", "3" = "#a05195", "4" = "#d45087"),
                          labels=c("None", "Primary Education", "5th to 9th Grade", "Secondary Education", "Higher Education"))+
        
        theme_minimal() + theme(legend.title = element_blank())
      
    }
    
    else if (input$var == "age vs failure") {
      
      prop_d2 <- data %>%
        
        group_by(age, failures) %>%
        
        summarise(count = n()) %>%
        
        mutate(prop = count / sum(count)) %>%
        
        ungroup() %>%
        
        arrange(failures)
      
      
      # Créer un barplot avec les proportions
      
      ggplot(prop_d2, aes(x = as.factor(age), y = prop, fill = as.factor(failures))) +
        
        geom_bar(stat = "identity", position = position_dodge()) +
        
        scale_y_continuous(labels = scales::percent) +
        
        labs(x = "age", y = "", fill = "failures") +
        
        scale_fill_manual(values = c("0" = "#003f5c", "1" = "#2f4b7c", "2" = "#665191", "3" = "#a05195")
                          
        ) +
        
        theme_minimal() + theme(legend.title = element_blank())
      
    }
  })
  
  output$VarPlot2 <- renderPlot({
    g3_range2 <- input$g3_filter2
    filtered_data2 <- data %>%
      filter(G3 >= g3_range2[1] & G3 <= g3_range2[2])
    
    if (input$var3 == "Medu vs Mjob") {
      
      prop_data2 <- filtered_data2 %>%
        group_by(Medu, Mjob) %>%
        
        summarise(count = n()) %>%
        
        mutate(prop = count / sum(count)) %>%
        
        ungroup() %>%
        
        arrange(Mjob)
      
      
      ggplot(prop_data2, aes(x = as.factor(Medu), y = prop, fill = as.factor(Mjob))) +
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "Mother Education", y = "Proportion du type d'emploi", fill = "Mjob") +
        scale_fill_manual(values = c("at_home" = "#003f5c", "health" = "#2f4b7c", "other" = "#665191", "services" = "#a05195", "teacher" = "#d45087"))+
        scale_x_discrete(
          labels = c("None", "Primary Education", "5th to 9th Grade", "Secondary Education", "Higher Education")) +
        theme_minimal() +
        theme(legend.title = element_blank())
    }
    
    else if (input$var3 == "Fedu vs Fjob") {
      prop_data21 <- filtered_data2 %>%
        group_by(Fedu, Fjob) %>%
        
        summarise(count = n()) %>%
        
        mutate(prop = count / sum(count)) %>%
        
        ungroup() %>%
        
        arrange(Fjob)
      
      
      ggplot(prop_data21, aes(x = as.factor(Fedu), y = prop, fill = as.factor(Fjob))) +
        geom_bar(stat = "identity", position = position_dodge()) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "Father Education", y = "Proportion du type d'emploi", fill = "Fjob") +
        scale_fill_manual(values = c("at_home" = "#003f5c", "health" = "#2f4b7c", "other" = "#665191", "services" = "#a05195", "teacher" = "#d45087"))+
        scale_x_discrete(
          labels = c("None", "Primary Education", "5th to 9th Grade", "Secondary Education", "Higher Education")) +
        theme_minimal() +
        theme(legend.title = element_blank())
    }
  })
  
  
  
  
  
  observe({
    # Créer un dataframe à partir des valeurs saisies par l'utilisateur
    new_data_utilisateur <- data.frame(
      G1 = input$g1,
      G2 = input$g2,
      failures = input$failures,
      age = input$age,
      goout = input$goout,
      absences=input$absences,
      Fjob = input$fjob,
      Mjob = input$mjob,
      Medu = match(input$medu, c("0 - none", "1 - primary", "2 - 5th to 9th", "3 - secondary or higher")) - 1
    )
    
    valeur_a_afficher <- predict(model_rf2, newdata = new_data_utilisateur)
    
    output$valeur_affichee <- renderText({
      paste0( valeur_a_afficher)
    })
    
  })
  
  
  
  
  
  
  
}


shinyApp(ui, server)