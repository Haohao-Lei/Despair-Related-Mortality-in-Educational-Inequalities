####combine two data
RAND$hhid
G2A$hhid
data <- merge(RAND, G2A, by="hhidpn")

################################################################################################################
################################################################################################################
##################################       Cleaning the data from wide to long  ##############################
################################################################################################################
################################################################################################################

######I cannot use a single age, what I need to do is to use the random effect model
#####select the variables I needed for the model
data <- dplyr::select(data, hhidpn, rabyear, hacohort, r1wtresp:r14wtcrnh, r1proxy:r15proxy, r1agey_b:r15agey_b,
                      ragender, raracem, raedegrm, rameduc, rafeduc, rarelig, rabplace, r1shlt:r15shlt, r1deprex,
                      r2depres:r15depres, r1efforx, r2effort:r15effort, r1sleepx, r2sleepr:r15sleepr, r1whappx, 
                      r2whappy:r15whappy, r1flonex, r2flone:r15flone, r1fsadx, r2fsad:r15fsad, r1goingx, r2going:r15going,
                      r1enlifx, r2enlife:r15enlife, r2cesd:r15cesd, r1cesdm:r15cesdm, r8lblonely3:r15lblonely3,
                      r9lblonely11:r15lblonely11, r8lbonchrstr:r15lbonchrstr, r9lbposaffect:r15lbposaffect, r9lbnegaffect:r15lbnegaffect,
                      r8lbsatwlf:r15lbsatwlf, radadoccup, ramischlth, r8pabused:r11pabused, r8padrug:r11padrug,
                      r9police:r11police, racsevent, racseventm, rachshlt, ralivdiffch, rapadivch, rapadiech, rasepmom,
                      rasepdad, rapwarm, rapwarmm, r1painfr:r15painfr, r1painlv:r15painlv, r2paina:r15paina, r8rxpain:r15rxpain,
                      r3backp:r15backp, r3headache:r15headache, r1hrtatte:r15hrtatte, r10angine:r15angine, r1conhrtfe:r15conhrtfe,
                      r10hrtrhme:r15hrtrhme, r11osteoe:r15osteoe, r12hchole:r15hchole, r1rxhibp:r15rxhibp, r1rxdiabo:r15rxdiabo,
                      r1rxdiabi:r15rxdiabi, r1rxdiab:r15rxdiab, r1rxstrok:r15rxstrok, r1rxangina:r15rxangina, r1rxchf:r15rxchf,
                      r1rxarthr:r11rxarthr, r1rxlung:r15rxlung, r1rxpsych:r11rxpsych, r1trpsych:r15trpsych, r3rxhrtat:r15rxhrtat,
                      r3rxheart:r15rxheart, r3drinkb:r15drinkb, r3binged:r15binged, radadeducl, ramomeducl, ravetrn, 
                      r1drinknr:r15drinknr, r1smokef:r15smokef, radage_y, respagey_b, raeduc, ragender, raracem, rarelig, ravetrn, rabplace,
                      radadoccup, rachshlt, racsevent, rafinanch, ralhchild, rapwarm)