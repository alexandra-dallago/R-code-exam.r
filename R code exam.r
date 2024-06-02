#Codice per l'analisi dei danni causati dalla Tempesta Vaia in Carnia (UD) i ntre anni (2015,2018,2023).L'obiettivo principale è analizzare la perdita della copertura forestale, le precipitazioni nevose, e osservare eventuali miglioramenti e il recupero della vegetazione.
##Il seguente codice è stato scritto utilizzando questi pacchetti precedentemente scaricati.
###Dopo aver installato i seguenti pacchetti con la funzione "install.packages("")" (NB: per installare il pacchetto bisogna inserirlo tra le virgolette), è necessario richiamare la funzione tramite il codice "library()" (richiamando la funzione non è necessario scrivere il pacchetto con le virgolette perchè è già presente in R una volta installato)
library(terra)       #Pacchetto R specializzata per l'analisi geospaziale e la manipolazione di dati raster.
library(imageRy)     #Pacchetto R usato per gestire dati raster, la visualizzazione, l'importazione la modifica delle immagini. Inoltre, facilita la condivisione delle immagini.
library(ggplot2)     #Pacchetto R per la creazione di grafici statistici.
library(patchwork)   #Pacchetto R usato per organizzare e personalizzare la disposizione di più grafici. 

#"setwd()" è un comando usato per impostare la directory del lavoro. In questo caso è stata impostata nella cartella "immagini d'esame" nel desktop del computer.
setwd("C:/Users/alexa/Desktop/Immagini esame")

#IMPORTAZIONE DELLE IMMAGINI
##Le immagini sono state prese dal sito "Copernicus Browser" nei dintorni di Sappada e Forni Avoltri (UD), Friuli Venezia Giulia; per 3 anni diversi (2015-2018-2023) nei mesi di ottobre per confrontare i danni causati dalla tempesta Vaia del 2018. Viene anche analizzala la copertura nevosa.
###Per ciascun anno sono state scaricate due immagini dal satellite "sentinel 2", rispettivamente immagini "true color" aventi le bande b4, b3 e b2 (red, green, blue); e una seconda immagine "false color" aventi le bande b8, b4 e b3 (nir, red, green). Lo scopo è quello di prelevare la banda b8, ovvero quella del nir, per importarla in un unico oggetto assieme alle altre bande b4, b3 e b2.
####Importiamo le immagini dalla cartella indicata nella directory tramite la funzione "rast()" del pacchetto "terra", che ci consente di importare delle immagini creando un oggetto Spatraster.

TC2015<-rast("tc2015vaia.jpg")      #Importazione dell'immagine jpg nell'oggetto chiamato "TC2015" per l'anno 2015. Le bande presenti in questo oggetto sono b4, b3 e b2.  
NIR2015<-rast("fc2015vaia.jpg")     #Importazione dell'immagine jpg nell'oggetto chiamato "NIR2015" per l'anno 2015. Le bande presenti in questo oggetto sono b8, b4 e b3.  

TC2018<-rast("tc2018vaia.jpg")      #Importazione dell'immagine jpg nell'oggetto chiamato "TC2018" per l'anno 2018. Le bande presenti in questo oggetto sono b4, b3 e b2.  
NIR2018<-rast("fc2018vaia.jpg")     #Importazione dell'immagine jpg nell'oggetto chiamato "NIR2018" per l'anno 2015. Le bande presenti in questo oggetto sono b8, b4 e b3.

TC2023<-rast("tc2023vaia.jpg")      #Importazione dell'immagine jpg nell'oggetto chiamato "TC2023" per l'anno 2023. Le bande presenti in questo oggetto sono b4, b3 e b2.
NIR2023<-rast("fc2023vaia.jpg")     #Importazione dell'immagine jpg nell'oggetto chiamato "NIR2023" per l'anno 2023. Le bande presenti in questo oggetto sono b8, b4 e b3.

#Warning unknown extend: per la mancata georeferenziazione delle immagini. Un avviso di "Warning" non implica un "Error" ma ci avvisa di un potenziale problema.

par(mfrow=c(1,3))     #Funzione usata per creare una finestra grafica in una griglia di righe e colonne specificandone il numero che in questo caso sono 1 riga e 3 colonne. Ci permette quindi di disporre più grafici in una sola finestra. Dopo il comando "par()" si inseriscono tutti i grafici che si vogliono visualizzare tramite il comando "plot()".
plot(TC2015)          #Le immagini vengono visualizzate tramite la funzione "plot()".
plot(TC2018)
plot(TC2023)

b15r<-TC2015[[1]]      #Tramite le parentesi quadre si specifica l'oggetto da estrarre (in questo caso l'oggetto 1, ovvero la banda 4 del rosso, di "TC2015") per assegnarlo ad un nuovo oggetto chiamato "b15r".
b15g<-TC2015[[2]]      #Tramite le parentesi quadre si specifica l'oggetto da estrarre (in questo caso l'oggetto 2, ovvero la banda 3 del verde, di "TC2015") per assegnarlo ad un nuovo oggetto chiamato "b15v".
b15b<-TC2015[[3]]      #Tramite le parentesi quadre si specifica l'oggetto da estrarre (in questo caso l'oggetto 3, ovvero la banda 2 del blu, di "TC2015") per assegnarlo ad un nuovo oggetto chiamato "b15b".
b15nir<-NIR2015[[1]]   #Tramite le parentesi quadre si specifica l'oggetto da estrarre (in questo caso l'oggetto 1, ovvero la banda 8 del nir, di "NIR2015") per assegnarlo ad un nuovo oggetto chiamato "b15nir".

b18r<-TC2018[[1]]    #Viene fatto lo stesso procedimento sopra descritto per tutti gli anni.
b18g<-TC2018[[2]]
b18b<-TC2018[[3]]
b18nir<-NIR2018[[1]]

b23r<-TC2023[[1]]
b23g<-TC2023[[2]]
b23b<-TC2023[[3]]
b23nir<-NIR2023[[1]]

vaia15<-c(b15r,b15g,b15b,b15nir)      #Uniamo in un unico oggetto, ovvero "vaia15", tutte le bande estratte precedentemente ottenendo così un unico oggetto avente tutte le 4 bande d'interesse, ovvero rosso, verde, blu e nir; tramite l'assegnazione "c" concatenate essendo elementi di un vettore.
vaia18<-c(b18r,b18g,b18b,b18nir)
vaia23<-c(b23r,b23g,b23b,b23nir)

#Attraverso la funzione "im.plotRGB()" del pacchetto "imageRy", sostituitamo il nir (ovvero la banda b8 e 4° elemento dell'oggetto "vaia15/vaia18/vaia23") con le altre componenti (le bande rosso b4, verde b3 e blu b2) dell'immagine per vedere come i colori di queste cambiano.

##Nir sul red: questo comporta una visualizzazione che evidenzia le caratteristiche della vegetazione in quanto la vegetazione riflette molto di più nel nir rispetto alle altre superfici, facilitandone l'identificazione. E' possibile anche capirne lo stato di salute in quanto più queste sono in salute, più il nir verrà riflesso, e più l'immagine sarà luminosa. Di conseguenza, la vegetazione assumerà il colore rosso. Bisogna considerare anche che l'intensità della riflettanza cambia in base al tipo di vegetazione: un prato rifletterà di più di una pineta. 
par(mfrow=c(1,3))
im.plotRGB(vaia15, 4,2,3)      #4,2,3 specificano rispettivamente nir, green, blu dell'oggetto "vaia15"
im.plotRGB(vaia18, 4,2,3)  
im.plotRGB(vaia23, 4,2,3) 

##Nir sul blu: la vegetazione apparirà di colore blu. Inoltre, il suolo diventerà giallo fornendo un ottimo contrasto tra vegetazione e suolo. 
par(mfrow=c(1,3))
im.plotRGB(vaia15, 1,2,4)      #1,2,4 specificano rispettivamente red, green, nir dell'oggetto "vaia15"
im.plotRGB(vaia18, 1,2,4) 
im.plotRGB(vaia23, 1,2,4) 

##Nir sul green: la vegetazione apparirà verde, dando un aspetto più naturale. 
par(mfrow=c(1,3))
im.plotRGB(vaia15, 1,4,3)      #1,4,3 specificano rispettivamente red, nir, blu dell'oggetto "vaia15"
im.plotRGB(vaia18, 1,4,3) 
im.plotRGB(vaia23, 1,4,3) 

#CORRELAZIONE TRA LE IMMAGINI
##Tramite la funzione "pairs()" viene creata una matrice di grafici a dispersione mostrando le relazioni bivariate tra ogni coppia di banda presente nell'oggetto. Ogni cella della matrice contiene un grafico a dispersione visualizzando la correlazione di Pearson tra le bande. 
pairs(vaia15)    
pairs(vaia18)
pairs(vaia23)

#CLASSIFICAZIONE DELLE IMMAGINI E CALCOLO DELLA FREQUENZA
##Viene fatta la classificazione delle immagini attraverso la funzione "im.classify()" e successivo calcolo della relativa frequenza, proporzione e percentuale del numero dei pixel. La funzione fa parte del pacchetto "imageRy".
###Classifica le immagini tramite un algoritmo che permette di creare dei gruppi/cluster basandosi sulla riflettanza del pixel (che a sua volta è in funzione di che oggetto si trova su quel pixel, quindi se una pianta o del suolo ad esempio). La funzione prende i pixel dell'immagine in maniera casuale, pertanto i colori dei cluster delle classificazioni sono diversi. 

vaia15c<-im.classify(vaia15,3)            #"im.classify()" è la funzione che permette la classificazione. Tra le parentesi si specifica l'oggeto da cui si vuole fare la classificazione (vaia15), seguito dal numero di cluster/classi che si vuole avere (3).      
vaia18c<-im.classify(vaia18,3) 
vaia23c<-im.classify(vaia23,3) 

f2015<-freq(vaia15c)         #Per calcolare il numero di pixel presenti tra le diverse classificazioni da noi scelte, si calcola la frequenza delle classi con la funzione "freq()".
f2018<-freq(vaia18c)
f2023<-freq(vaia23c)

tot2015<-ncell(vaia15c)      #Calcolo del numero totale di celle nell'immagine attraverso la funzione "ncell()". 
tot2018<-ncell(vaia18c)
tot2023<-ncell(vaia23c)

prop2015=f2015/tot2015      #Calcolo della proporzione rispetto al totale.
prop2018=f2018/tot2018      #Usiamo = perchè è una funzione matematica.      
prop2023=f2023/tot2023

perc2015= prop2015*100      #Calcolo della percentuale dei cluster/classi rispetto al totale delle celle.
perc2018= prop2018*100             
perc2023= prop2023*100   

perc2015      #Riscrivo il nome dell'oggetto aventi le percentuali per visualizzare il dato. I risultati ottenuti sono: [1] 19.98092, [2] 54.42268, [3]25.59640; i numeri tra le parentesi quadre indicano la percentuale appartenente alla relativa classe/cluster 
perc2018      #I risultati ottenuti sono: [1] 31.83124, [2] 34.10886, [3] 34.05817
perc2023      #I risultati ottenuti sono: [1] 18.23045, [2] 50.31034, [3] 31.45921

#GGPLOT E DATAFRAME 

class<-c("snow", "forest", "soil")       #Creazione del Vettore tramite "c()" concatenate a cui vengono attribuite 3 nomi, ovvero "snow", "forest", "soil". I nomi devono essere messi tra virgolette in quanto tali. Questi nomi rappresenteranno i cluster ottenuti durante la classificazione. 
y2015<-c(19.98092,54.42268,25.59640)     #Vettore a cui vengono attribuite le relative percentuali calcolate precedentemente (vengono quindi inserite le percentuali ottenute in "perc2015" che ricordiamo essere il numero di pixel corrispondenti a ciascuna classe per le due immagini; quindi otterremo 3 percentuali essendo le classi 3).
y2018<-c(31.83124,34.10886, 34.05817)
y2023<-c(18.23045,50.31034,31.45921)

##Creiamo un dataframe contenente le classi e gli oggetti "y2015", "y2018", "y2023", contenenti le percentuali della classificazione calcolate precedentemente (quindi i risultati ottenuti in "perc2015" ad esempio). I dati vengono poi usati per creare tre grafici a barre per confrontare le distribuzioni delle classi nelle immagini.  

DATAFRAME<-data.frame(class,y2015,y2018,y2023)       #Dataframe dove vene inserito l'oggetto "class" (quindi "snow", "forest", "soil"), e gli  oggetti "y2015", "y2018", "y2023" con le Percentuali delle classificazioni. 
anno2015 <-ggplot(DATAFRAME,aes(x=class, y=y2015, fill=class))+ geom_bar(stat="identity", color="black") + ylim(c(0, 55)) + scale_fill_manual(values = c("forest" = "#77dd77", "snow" = "#59b7d8", "soil" = "#ffb347"))       #Creazione di un grafico a barre utilizzando ggplot (dal paccketto "ggplot2") per rappresentare la distribuzione delle percentuali nelle 3 classi ("snow", "forest", "soil"). "aes()" specifica l'estetica del grafico, quindi si specifica che nell'asse x viene inserito l'oggetto "classi" e in y l'oggetto "y2015". "fill=" specifica che il riempimento delle barre è determinato dall'oggetto "class". "geom_bar()" specifica che il tipo di grafico, ovvero quello a barre.stat="identyty" usa i valori forniti dall'oggetto "y2015" per l'altezza delle barre. "color=black" indica che il bordo delle barre sono colorate di nero. "ylim(c(0,45))" indica che la scala sull'asse y è impostata da 0 a 45. "scale_fill_manual" permette di specificare manualmente i colori di riempimento delle barre tramite "values=c" che è un vettore che associa ai valori contenuti in "class" i colori. I codici dei colori sono stati presi dal sito "tacolor.com" 
anno2018 <-ggplot(DATAFRAME,aes(x=class, y=y2018, fill=class))+ geom_bar(stat="identity", color="black") + ylim(c(0, 55)) + scale_fill_manual(values = c("forest" = "#77dd77", "snow" = "#59b7d8", "soil" = "#ffb347")) 
anno2023 <-ggplot(DATAFRAME,aes(x=class, y=y2023, fill=class))+ geom_bar(stat="identity", color="black") + ylim(c(0, 55)) + scale_fill_manual(values = c("forest" = "#77dd77", "snow" = "#59b7d8", "soil" = "#ffb347"))      
anno2015+anno2018+anno2023                           #Unisce i due grafici a barre creati precedentemente in una sola finestra.

#TIMES SERIES 
##Visualizziamo la differenza pixel per pixel tra le due immagini usando una palette di colori per evidenziare le variazioni tra le due. Osserviamo le differenze effettive tra le immagini in termini di intensità dei pixel. Otteniamo visivamente i cambiamenti di una certa zona. 
###Differenza tra le immagini della banda del Nir  
diff1518<-vaia15[[4]] - vaia18[[4]]    #Calcola la differenza pixel per pixel tra le due immagini considerando solo il quarto elemento selezionato tramite "[[4]]" che in questo caso rappresenta il nir. 
diff1823<-vaia18[[4]] - vaia23[[4]]
diff1523<-vaia15[[4]] - vaia23[[4]]

clblu<- colorRampPalette(c("blue","white", "red")) (100)      #Crea una rampa di colori che va dal blu al bianco al rosso con 100 gradazioni.

#I pixel blu indicano una minore intensità nella prima immagine rispetto alla seconda, quindi una probabile diminuzione dell'intensità del nir e quindi una probabile diminuzione della vegetazione. I pixel bianchi indicano una assenza di intensità. I pixel rossi indicano una maggiore intensità nella prima immagine rispetto alla seconda, ovvero un aumento dell'intensità del nir e quindi una crescita della vegetazione. I valori variano da +255 a -255 perchè la risoluzione radiometrica è di 8bit, quindi 255 possibilità. 
par(mfrow=c(1,3))
plot(diff1518, col=clblu)  #Visualizzazione dell'oggetto "diff1518" con la rampa di colori "clblu" precedentemente creata. Osserviamo quindi la differenza tra le immagini dal 2015 al 2018.
plot(diff1823, col=clblu) 
plot(diff1523, col=clblu) 

#A causa del quantitativo maggiore di neve presente nel 2018, i risultati vengono sfalsati e contrastanti rispetto ai risultati ottenuti dalla classificazione calcolata precedentemente perchè la neve riflette fortemente il nir. Di conseguenza si provvede a calcolare DVI  e NDVI e successiva differenza tra gli anni per evidenziare sempre eventuali cambiamenti negli anni. 

#CALCOLO INDICE DI VARIABILITA': DVI E NDVI 
##Calcolo DVI: Indice di differenza di vegetazione. 
###Questo indice sfrutta l'alta capacità di riflettanza dell'infrarosso e l'alta capacità di assorbimento del rosso per determinare, tramite una differenza nir-red, la biomassa/densità di vegetazione presente. Possiamo anche determianre lo stato di salute della pianta in base all'accrescimento del rosso o infrarosso (se aumenta da rosso a nir la pianta è sana). Si può usare anche la banda del blu per il calcolo. 
####Il valore DVI varia da +255 a -255 perchè la risoluzione radiometrica è di 8bit, quindi 255 possibilità. 

dvi2015=vaia15[[4]] - vaia15[[1]]     #Differenza tra le bande del vicino infrarosso nir [[4]] e del rosso red [[1]]. I risultati sono: min -220, max 225
dvi2018=vaia18[[4]] - vaia18[[1]]     #I risultati del DVI del 2018 sono: min -205, max 198
dvi2023=vaia23[[4]] - vaia23[[1]]     #I risultati del DVI del 2023 sono: min -215, max 223 
clyellow<-colorRampPalette(c("darkblue", "yellow", "red", "black"))(100)     #Crea una rampa di colori che va dal blu scuro, al giallo, al rosso e al nero con 100 gradazioni.

par(mfrow=c(1,3))              #Funzione usata per creare una finestra grafica in una griglia di righe e colonne specificando il numero di righe e colonne che in questo caso sono 1 riga e 3 colonne.
plot(dvi2015,col=clyellow)     #Le immagini vengono visualizzate tramite la funzione "plot()" e rappresentano l'indice di differenza di vegetazione per l'immagine del 2015 in questo caso.
plot(dvi2018,col=clyellow)  
plot(dvi2023,col=clyellow)  

#Calcolo NDVI: indice di differenza normalizzata delle vegetazioni utilizzando i valori delle bande rosse e infrarosse delle immagini. Viene calcolata con la formula NDVI=(nir-red/nir+red). Permette di confrontare i valori tra immagini che hanno una risoluzione radiometrica diversa (ad esempio una immagine a 8 bit con una a 16 bit).
##Il valore NDVI va da +1 a -1  perchè se il nir è massimo e red minimo, inserendo nella formula otteniamo: 255-0/255+0=1 (valore massimo). Otteniamo -1 quando il nir è minimo. 
###Interpretazione dei dati NDVI: valori alti, vicini a +1= vegetazione densa e sana; valori vicini 0= vegetazione scarsa, neve/ghiaccio o suolo nudo; valori bassi vicini a -1= superfici non vegetate o urbanizzate come costruzioni o strade. 

ndvi2015<-dvi2015/(vaia15[[4]]+vaia15[[1]])    #"dvi2015" è il numeratore nir-red. "vaia15[[4]]+vaia15[[1]" rappresenta il denominatore, ovvero nir+red; dove le bande sono nir=4 e red=1.
ndvi2018<-dvi2018/(vaia18[[4]]+vaia18[[1]])
ndvi2023<-dvi2023/(vaia23[[4]]+vaia23[[1]])   

par(mfrow=c(1,3))                 #Funzione usata per creare una finestra grafica in una griglia di righe e colonne specificando il numero di righe e colonne che in questo caso sono 1 riga e 3 colonne.
plot(ndvi2015, col=clyellow)      #Le immagini vengono visualizzate tramite la funzione "plot()" con la relativa palette di colori "clyellow". 
plot(ndvi2018, col=clyellow)
plot(ndvi2023, col=clyellow)

#Calcolo delle differenze tra i vari anni del NDVI per determinare eventuali cambiamenti. Valori positivi indicano una maggiore presenza di vegetazione nel primo anno inserito, valori negativi indicano quantitativi maggiori di vegetazione nel secondo anno inserito nella differenza. 
diffnir1518=ndvi2015 - ndvi2018
diffnir1823=ndvi2018 - ndvi2023
diffnir1523=ndvi2015 - ndvi2023

par(mfrow=c(1,3))                   #Funzione usata per creare una finestra grafica in una griglia di righe e colonne specificando il numero di righe e colonne che in questo caso sono 1 riga e 3 colonne.
plot(diffnir1518, col=clyellow)     #Le immagini vengono visualizzate tramite la funzione "plot()" con la relativa palette di colori "clyellow".        
plot(diffnir1823, col=clyellow)
plot(diffnir1523, col=clyellow)

