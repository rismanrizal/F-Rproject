#import data: Kuesioner Humor Response
str(humor)

#library yang digunakan
library(lavaan)
library(psych)
library(REdaS)
library(grid)

#membuat dataframe untuk setiap variabel
AFF <- data.frame(humor$af1, humor$af2, humor$af3, humor$af4,
                  humor$af5, humor$af6, humor$af7, humor$af8)
ENH <- data.frame(humor$se1, humor$se2, humor$se3, humor$se4,
                  humor$se5, humor$se6, humor$se7, humor$se8)
AGR <- data.frame(humor$ag1, humor$ag2, humor$ag3, humor$ag4, 
                  humor$ag5, humor$ag6, humor$ag7, humor$ag8)
DFT <- data.frame(humor$sd1, humor$sd2, humor$sd3, humor$sd4, 
                  humor$sd5, humor$sd6, humor$sd7, humor$sd8)

#memanggil tes Bartlett Test of Sphericity untuk masing2 variabel
bart_spher(AFF)
bart_spher(ENH)
bart_spher(AGR)
bart_spher(DFT)

#memanggil tes KMO untuk masing2 variabel
KMOS(AFF)
KMOS(ENH)
KMOS(AGR)
KMOS(DFT)

#memanggil alpha cronbach untuk analisis
alpha(AFF, check.keys = T) #apabila ada yang negative correlation, akan otomatis direverse
alpha(ENH, check.keys = T)
alpha(AGR, check.keys = T)
alpha(DFT, check.keys = T)
