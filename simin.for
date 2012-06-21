      PROGRAM SIMIN2
C         
C     SIMIN2 WITH ECONOMICS FOR DFSIM VERS 1.3  
C         
C        FORTRAN V (ANSI '77)   IBM-PC or compatible
C         
C     THIS INTERACTIVE PROGRAM SETS UP A CONTROL FILE FOR 'DFSIM',    
C     (CURTIS).  IT WAS WRITTEN BY STEPHEN GASTON, INTERNATIONAL      
C     PAPER CO., WFRC, IN JULY, 1981.  IT SHOULD FACILITATE USING     
C     'DFSIM' WHILE ALLOWING ALL OPTIONS AVAILABLE.         
C         
C     PROGRAM MODIFIED TO ACCEPT ECONOMICS DATA INPUT, PNW 6/82,3/84  
C     ANY QUESTIONS SHOULD BE DIRECTED TO ROGER FIGHT (503) 231-2086  
C     OR BIOMETRICS UNIT (503) 231-2063 
C     COPIES OF DFSIM WITH ECON0MICS AND SIMIN2 WITH ECONOMICS        
C     CAN BE OBTAINED FROM:   
C     U.S.FOREST SERVICE      
C     PNW FOREST AND RANGE EXPERIMENT STATION     
C     BIOMETRICS UNIT         
C     P.O.BOX 3890  
C     PORTLAND,OR 97208       
C         
C     IUIN IS INTERACTIVE INPUT FILE - TAPE5      
C     IUOUT IS INTERACTIVE OUTPUT FILE - TAPE6    
C     IOUT IS CONTROL FILE - TAPE3      
C     IIN IS LOG COST TABLE INPUT FILE - TAPE13   
C         
C         
C         
C        I/O DEFINITION:      
C           TAPE3 = CONTROL FILE (INPUT INTO DFSIM)         
C           TAPE5 = INTERACTIVE INPUT   
C           TAPE6 = INTERACTIVE OUTPUT  
C           TAPE13= OPTIONAL LOG COST TABLES INPUT
C         
C        VARIABLE DEFINITION: 
C           VARIABLE NAMES ARE USED SYNONYMOUSLY WITH DFSIM 
C         
C        AUTHOR:    
C         STEPHEN GASTON, DATA MANAGEMENT AND RESEARCH SPECIALIST,    
C         INTERNATIONAL PAPER COMPANY, WESTERN FOREST RESEARCH        
C         CENTER.   
C         
C         PROGRAMMING REVISION: JUDITH CHITTESTER PNW/BIOMETRICS      
C         
C         
C         
C***********************************************************************        
C         
C        VARIABLE SPECIFICATION         
C         
      IMPLICIT INTEGER (A-Z)  
      CHARACTER SHORT*1,OBSFLG*1,CNTNUE*1,ECON*1,TABLE*1,SAME*1,INFL*1
      CHARACTER NAME*5,EUNITS*3,INAME*5,RPTSTD*1,RPTCT*1,RPTFRT*1     
      CHARACTER ICV4*3,ICV6*3,IV6*3,ISV6*3,RPTLP*1,RPTOBS*1,TITLE*75  
      CHARACTER LF*1,CR*1,yes*1
      REAL DR(9),SDIA,SBA,DLIM,OBSHTS(2,31)       
      DIMENSION AGET(9),AGEF(9),HY40(9),GRESD(9),TARESD(9), 
     $ PNIT(9),RAN(76),OBSAGE(31),INTDR(9),IOBSHT(2,31)     
C         
C  EEEEEEEEEE       
C         
      INTEGER COSTAB,ECO,PONTAB,NIT(15,2)         
      REAL LOGCF(12,12),LOGCI(12,12),LOGDF(12),LOGDI(12),LOGVF(12),   
     1  LOGVI(12),PONDVL(8,2),CNIT(15,2)
      REAL A,B,C,R,REGENC,PCTC,FC,LCM,FHRC,OATC,OVTC,OAHC,OVHC,HC     
     1 ,PVI,RCI,PCTCI,FCI,LCI,FHRCI,OAHCI,OVHCI,HCI,FALL    
      EQUIVALENCE (NIT(1,1),CNIT(1,1))  
      DATA ICV4,ICV6,IV6,ISV6/'CV4','CV6','IV6','SV6'/      

      OPEN(6,FILE='TAPE6',status = 'unknown')
      WRITE(*,9993)
      OPEN(5,FILE='TAPE5',status = 'unknown')

9993  FORMAT(1X,'REPLACE SIMIN DISK WITH DFSIM DISK')
      WRITE(*,9994)
9994  FORMAT(1X,/,1X,'ARE THEY REPLACED? Y OR N?')
      READ (*,9995)  REZ
9995  FORMAT(A)
C         
      OPEN(3,FILE='TAPE3',status = 'unknown')
      OPEN(13,FILE='TAPE13',status = 'unknown')
C  
C     INITIALIZE ECON VARIABLES         
C         
      COSTAB=0      
      ECO=0         
      LDF=0         
      LDI=0         
      LVF=0         
      LVI=0         
      PONTAB=0      
      IMORT=0       
      A=0.0         
      B=0.0         
      C=0.0         
      FC=0.0        
      FCI=0.0       
      FHRC=0.0      
      FHRCI=0.0     
      HC=0.0        
      HCI=0.0       
      LCI=0.0       
      LCM=0.0       
      OAHC=0.0      
      OAHCI=0.0     
      OATC=0.0      
      OVHC=0.0      
      OVHCI=0.0     
      OVTC=0.0      
      PCTC=0.0      
      PCTCI=0.0     
      PVI=0.0       
      R=0.0         
      RCI=0.0       
      REGENC=0.0    
      FALL=0.0      
      DO 400 I=1,12 
      DO 400 J=1,12 
      LOGCF(I,J)=0.0
      LOGCI(I,J)=0.0
      LOGDF(I)=0.0  
      LOGDI(I)=0.0  
      LOGVF(I)=0.0  
      LOGVI(I)=0.0  
  400 CONTINUE      
      DO 410 I=1,8  
      DO 410 J=1,2  
  410 PONDVL(I,J)=0.0         
      DO 420 I=1,15 
      DO 420 J=1,2  
      NIT(I,J)=0    
  420 CNIT(I,J)=0.0 
C         
C  EEEEEEEEEE       
C         
C        INITIALIZE THE VARIABLES       
       IUOUT= 6
       IOUT=3
       IUIN= 5
       IIN=13
C         
      SI=0
      IORG=0        
      PCT=0         
      EXIST=0       
      IUT=0         
      FTEST=0       
      FERTAS=0      
      FERTCT=0      
      SAGE=0        
      STA=0         
      TAST=0        
      KRD=0         
      SBA=0.0       
      SDIA=0.0      
      DLIM=0.0      
      TTEST=0       
      TI=0
      FIY=0         
      DH=0
      FIH=0         
      CTAS=0        
      NCT=0         
      SCTHT=0       
      IDR=0         
      IGS=0         
      LTREE=0       
      ICONT=0       
      FCT=0         
      GLIM=0        
      GCLIM=0       
      AHARR=0       
      HTHARV=0      
      IMV=0         
      NRA=0         
      NTB=0         
      IOBS=0        
      NFERT=0       
      MULT=0        
      DO 100 I=1,9  
      AGET(I)=0     
      HY40(I)=0     
      GRESD(I)=0    
      TARESD(I)=0   
      AGEF(I)=0     
      PNIT(I)=0     
      DR(I)=0.0     
100   CONTINUE      
      DO 200 I=1,31 
      OBSAGE(I)=0   
      OBSHTS(1,I)=0.0         
      OBSHTS(2,I)=0.0         
      RAN(I)=0      
200   CONTINUE      
      DO 300 I=31,76
      RAN(I)=0      
300   CONTINUE      
C         
C  EEEEEEEEEE       
C     ECONOMICS FORMATS       
 7000 FORMAT (' DO YOU WANT ECONOMICS TABLES FOR THIS RUN?',3X)      
 7010 FORMAT (' INDICATE DFSIM UNITS (CV4,CV6,IV6,SV6)  ')  
 7020 FORMAT (' ARE YOU USING A TABLE OF PONDVL PRICES?',3X)         
 7030 FORMAT (' ENTER # OF DBH VALUES  ')         
 7032 FORMAT (' NO MORE THAN 8 DBH VALUES ARE ALLOWED  ')   
 7034 FORMAT (' ENTER DBH VALUES (XX.X)  ',/)     
 7036 FORMAT (' ENTER PONDVL VALUES (XXXX.)  ',/) 
 7040 FORMAT (' ENTER PONDVL COEFFICIENTS (A,B,C) (8 SPACES WITH (.))'
     1 ,/)         
 7050 FORMAT (' ARE YOU USING A TABLE OF LOGGING COSTS?',3X)         
 7060 FORMAT (' ARE YOU USING A SEPARATE TABLE FOR FINAL',  
     1 ' HARVEST LOGGING COST VALUES?',3X)       
 7070 FORMAT (' NOTE: THE TABLE FILE MUST BE PREDEFINED AS LOGICAL ', 
     1 'UNIT 13 BEFORE EXECUTING',/,' SIMIN. ')   
 7080 FORMAT (' ENTER A 5-CHAR NAME OF INTERMEDIATE HARVEST TABLE  ') 
 7084 FORMAT (' ENTER # OF DBH VALUES, # OF VOLUMES  ')     
 7090 FORMAT (' ENTER A 5-CHAR NAME OF FINAL HARVEST TABLE  ')        
 7095 FORMAT (' CANNOT FIND ',A5,' ON TAPE13. '    
     1 ,' PROGRAM WILL PROCEED WITH DEFAULT REVNOW EQUATIONS')        
 7100 FORMAT (' THE INTERMEDIATE HARVEST TABLE WILL ALSO BE USED',/,  
     1 ' FOR FINAL HARVEST LOGGING COSTS',/)      
 7110 FORMAT (' ENTER THE FOLLOWING PER ACRE COSTS:',/,     
     1 5X,'REGEN COST PER ACRE (XXXX.),',/,       
     2 5X,'PRECOM THIN COST PER ACRE (XXXX.),',/, 
     4 5X,'FIRST HARVEST ROAD COST PER ACRE (XXXX.),',/,    
     5 5X,'OTHER PER ACRE THIN COST (XXXX.),',/,  
     6 5X,'OTHER PER ACRE FINAL HARVEST COST (XXXX.).',/)   
 7115 FORMAT (' ENTER THE FOLLOWING VARIABLES:',/,
     $ 5X,'VOLUME ADJUSTMENT MULTIPLIER (X.XX),',/,         
     1 5X,'INTEREST RATE (XX.X),',/,    
     2 5X,'LOGGING COST MULTIPLIER (X.XX),',/,    
     3 5X,'OTHER PER M UNIT VOL THIN COST (XXXX.),',/       
     4 5X,'OTHER PER M UNIT VOL FINAL HARVEST COST (XXXX.),',/,       
     5 5X,'HAULING COST PER M UNIT VOL (XXXX.).  ',/)       
 7120 FORMAT (' DO YOU WANT TO CHANGE PRICES OVER TIME?',3X)         
 7130 FORMAT (' ENTER POND VALUE PRICE TREND (XX.X)  ')     
 7140 FORMAT (' DO YOU WANT TO CHANGE CULTURAL TREATMENT',  
     1 ' COSTS OVER TIME?',3X)         
 7150 FORMAT (' ENTER THE FOLLOWING RATES: (XX.X)',/,       
     1 5X,'REGEN COST TREND,',/,        
     2 5X,'PRECOM THIN COST TREND,',/,  
     3 5X,'FERT COST TREND.  ',/)       
 7160 FORMAT (' DO YOU WANT TO CHANGE HARVEST COSTS OVER TIME?',3X)  
 7170 FORMAT (' ENTER THE FOLLOWING RATES: (XX.X)',/,       
     1 5X,'LOGGING COST TREND,',/,      
     2 5X,'FIRST HARVEST ROAD COST TREND,',/,     
     3 5X,'OTHER PER ACRE HARVEST COST TREND,',/, 
     4 5X,'OTHER PER M UNIT VOL HARVEST COST TREND,',/,     
     5 5X,'HAULING COST TREND.  ',/)    
C         
 8085 FORMAT (/,' DO YOU WANT TO INCLUDE SALVAGE OF MORTALITY ?',3X) 
C         
 8065 FORMAT (' CAUTION: REVNOW LOGGING COSTS ARE APPROPRIATE FOR'    
     1 ,' CUBIC VOLUME UNITS (CV4) ONLY.')        
C         
C  EEEEEEEEEE       
C         
C        ACKNOWLEDGEMENTS     
C         
      WRITE(*,1000)       
1000  FORMAT(//' THIS INTERACTIVE PROGRAM GENERATES THE CONTROL FILE ',         
     $ 'INPUT TO RUN DFSIM'//' QUESTIONS CONCERNING THIS PROGRAM MAY ',         
     1 'BE DIRECTED TO THE U.S.FOREST SERVICE,'/' PNW FOREST AND RANGE',        
     2 ' EXPERIMENT STATION',', BIOMETRICS UNIT (503) 231-2063')      
C         
      WRITE (6,1050)
 1050 FORMAT (' ANY QUESTIONS ABOUT ECONOMICS DATA SHOULD BE DIRECTED',         
     1 ' TO:',/,' ROGER FIGHT - PNW (503) 231-2086. ')      
C         
C        TITLE AND INTRODUCTORY INFORMATION       
C         
10    WRITE(*,6000)       
6000  FORMAT(////' THIS PROGRAM REQUESTS USER SPECIFIED ',  
     $ 'INPUT TO INITIALIZE A DFSIM RUN'/' PLEASE ENTER INTEGER VALUES',        
     2 ' (UNLESS OTHERWISE SPECIFIED)', 
     3 ' IN THE ORDER '/' REQUESTED, SEPARATED BY COMMAS, AND ',      
     4 'FOLLOWED BY A CARRIAGE RETURN.',/,' IF YES OR NO ARE REQUIRED,'         
     5 ,'DO NOT INSERT LEADING BLANKS.',//' ENTER A TITLE FOR THIS',  
     6 ' DFSIM RUN (UP TO 75 COLUMNS)',/)         
      READ(*,5000,END=10000)TITLE    
10000 CONTINUE      
c
c    Determine user specified maximun relative density limit
c
      write (*,*) 'Do your want to specify the maximun relative density 
     1limit?'
      read(*,'(a1)') Yes
       if (yes.eq.'y'.or.yes.eq.'Y') go to 5
       ird = 70
       go to 6
    5 Write (*,*) 'Enter the maximun Curtis Relative Density for stands 
     1to build up to.'
      read (*,*) Ird
    6 ird = ird * 10
c
c     Determine whether Scribner 16-foot or 32-foot log rule is used
c     for Scribner volumes
c
      write (*,*) 'Do you want Scribner Board-Foot Volume for 16-foot lo
     1gs or for 32-foot logs?'
      write (*,*) 'Enter "1" for 16-foot or "0" for 32-foot.'
      read (*,*) isv
5000  FORMAT(BZ,A)  
       IF(MULT.EQ.1) GO TO 11 
C         
C        CHECK FOR QUICK (SHORT) VERSION OF THE PROGRAM     
C           'SHORT' VERSION ASSUMES USER WILL SPECIFY COMMERCIAL      
C           THINNING AGES, AND THE OBSERVED HEIGHTS OPTION WILL       
C           NOT BE REQUESTED  
C         
      WRITE(*,6200)       
6200  FORMAT(/' WOULD YOU PREFER A LIMITED (SHORTER) CONTROL FILE ?',
     $ 3X)
      READ(*,5200,END=11   ) SHORT   
5200  FORMAT(BZ,A1) 
C         
C        MAIN CONTROL OPTIONS AND INITIAL INFORMATION       
C         
11    WRITE(*,6001)       
6001  FORMAT(/' ENTER THE FOLLOWING VARIABLES:'/5X,'SITE INDEX (50 YR),'        
     $ /5X,'STAND ORIGIN (0=NATURAL OR SEEDED, 1=PLANTED),'/
     2 5X,'AGE OF PRECOMMERCIAL THINNING (0=NO PCT),'/      
     3 5X,'STAND OPTION (0=REGIONAL AVERAGE, 1=USER SUPPLIED STAND ', 
     4 'CONDITIONS),'/5X,'COMMERCIAL THINNING (0=NO CT, 1=CT OPTION),'/         
     5 5X,'FERTILIZATION (0=NO FERT, 1=FERT DURING EACH CT, 2=FERT ', 
     6 'AT USER '/10X,'SPECIFIED AGES)')
      IF(MULT.EQ.1) THEN      
       FERTAS=0
       FERTCT=0
       WRITE(*,6500)      
6500   FORMAT(/' ARE ALL VALUES REPEATED FROM THE PREVIOUS RUN?',    
     $ 3X)
 6506 FORMAT (' ENTER VALUES REQUESTED')
      READ (*,5200,END=91) SAME         
   91 CONTINUE      
       IF (SAME.EQ.'Y'.or.same.eq.'y') THEN  
       FERTAS=0
       FERTCT=0
      ELSE
        WRITE (*,6506)    
       READ(*,*,END=92) SI,IORG,PCT,EXIST,IUT,FTEST      
   92 CONTINUE      
       ENDIF        
      ELSE
       READ(*,*,END=93) SI,IORG,PCT,EXIST,IUT,FTEST      
   93 CONTINUE      
      ENDIF         
      IF(IUT.EQ.0.AND.FTEST.EQ.1) FTEST=2         
      IF(FTEST.EQ.1)FERTCT=1  
      IF(FTEST.EQ.2)FERTAS=1  
C         
C        'EXISTING STAND' AND 'STAND ORIGIN' OPTIONS        
C         
      IF(EXIST.EQ.1) THEN     
      IF(MULT.NE.1) GO TO 15  
      WRITE(*,6501)       
6501  FORMAT(/' ARE THE EXISTING STAND PARAMETERS REPEATED FROM THE ',
     $ 'PREVIOUS SETUP?',3X) 
      READ(*,5200,END=16) RPTSTD     
   16 CONTINUE      
       IF (RPTSTD.EQ.'Y') GO TO 20      
15     IF(IORG.EQ.1.AND.PCT.GT.0) THEN  
        WRITE(*,6002)     
6002    FORMAT(/' IF STAND HEIGHT < 30 FT OR SBA AND QMD ARE ',       
     $   'UNAVAILABLE,'/5X,'ENTER TOTAL AGE AND TPA OF THE EXISTING', 
     2   ' STAND'/3X,'OTHERWISE;'/5X,'ENTER TOTAL AGE AND ANY TWO OF ',         
     3   'THE FOLLOWING:'/5X,'TPA, SBA(NEAREST .1), QMD(NEAREST .01)'//         
     4   3X,'NOTE: ENTER IN SPECIFIED ORDER USING ZEROS FOR ',        
     5   'UNSELECTED VARIABLES')        
        READ (*,*,END=10002)SAGE,STA,SBA,SDIA     
10002 CONTINUE      
       ELSE         
        WRITE(*,6003)     
6003    FORMAT(/' IF STAND HEIGHT < 30 FT OR SBA AND QMD ARE ',       
     $   'UNAVAILABLE,'/5X,'ENTER TOTAL AGE AND TPA (>1.5 DBH)'/3X,   
     2   'OTHERWISE;'/5X,'ENTER TOTAL AGE AND TWO OF THE FOLLOWING ', 
     3   '(FOR TREES >1.5 DBH)'/5X,'TPA, SBA(NEAREST .1), QMD(NEAREST',         
     4   ' .01)'//3X,'NOTE: ENTER IN SPECIFIED ORDER USING ZEROS FOR',
     5   ' UNSELECTED VARIABLES'/)      
        READ (*,*,END=20   )SAGE,STA,SBA,SDIA     
       ENDIF        
      ENDIF         
20    REWIND iuin
      IF(PCT.GT.0) THEN       
       WRITE(*,6004)      
6004   FORMAT(/' ENTER THE TOTAL STAND TPA AFTER PCT')      
      ELSEIF(IORG.EQ.1) THEN  
       WRITE(*,6005)      
6005   FORMAT(/' ENTER THE TOTAL STAND TPA AFTER PLANTATION ',        
     $  'ESTABLISHMENT')      
      ELSE
       GO TO 25     
      ENDIF         
       READ(*,*,END=10003) TAST      
10003 CONTINUE      
C         
C        COMMERCIAL THINNING OPTIONS    
C         
25    IF(IUT.EQ.0) THEN       
       IUT=1        
      ELSE
       IUT=0        
       IF(MULT.NE.1) GO TO 35 
       WRITE(*,6502)      
6502   FORMAT(/' ARE THE COMMERCIAL THINNING PARAMETERS REPEATED ',   
     $ 'FROM THE LAST SETUP?',3X)      
       READ(*,5200,END=26) RPTCT     
   26 CONTINUE      
       IF (RPTCT.EQ.'Y') GO TO 40       
        TI=0
       FIY=0
       NCT=0
       DH=0
       FIH=0
       CTAS=0
       SCTHT=0
       DO 30 I=1,9  
       AGET(I)=0
       HY40(I)=0
       GRESD(I)=0
       TARESD(I)=0
       DR(I)=0.0    
30     CONTINUE     
35     IF(SHORT.EQ.'Y')THEN   
        TTEST=3     
       ELSE         
C         
C        COMMERCIAL THINNING -- TIMING OPTION     
C         
        WRITE(*,6010)     
6010    FORMAT(/' ENTER A COMMERCIAL THINNING INTERVAL OPTION:'/      
     $   5X,'1=THINING INTERVAL IN YEARS'/5X,'2=THINING INTERVAL ',   
     2   'IN HEIGHT GROWTH (FEET)'/5X,'3=THINNING AT USER SPECIFIED ',
     3   'AGES'/5X,'4=THINNING AT USER SPECIFIED HEIGHTS')  
        READ (*,*,END=10004)TTEST       
10004 CONTINUE      
       ENDIF        
       IF(TTEST.EQ.1)THEN     
C         
C        COMMERCIAL THINNING -- INTERVAL IN YEARS 
C         
        WRITE(*,6011)     
6011    FORMAT(/' ENTER THE THREE FOLLOWING VALUES:'/5X,'MINIMUM NUM',
     $   'BER OF YEARS BETWEEN EACH THINNING'/5X,'NUMBER OF YEARS ',  
     2   'BETWEEN FINAL THINNING AND HARVEST'/5X,'AND, MAX NUMBER OF',
     3   ' THINNINGS (UP TO 9)')        
        READ (*,*,END=10005)TI,FIY,NCT  
10005 CONTINUE      
        IF(FIY.EQ.TI) FIY=0   
       ELSEIF(TTEST.EQ.2) THEN
C         
C        COMMERCIAL THINNING -- INTERVAL IN HEIGHT GROWTH   
C         
        WRITE(*,6012)     
6012   FORMAT(/' ENTER THE THREE FOLLOWING VALUES:'/5X,'MINIMUM HEIGHT',        
     $   ' GROWTH BETWEEN EACH THINNING'/5X,'HEIGHT GROWTH BETWEEN ', 
     2   'FINAL THINNING AND HARVEST'/5X,'AND, MAX NUMBER OF THINNINGS',        
     3   ' (UP TO 9)')        
        READ (*,*,END=10006)DH,FIH,NCT  
10006 CONTINUE      
        IF(FIH.EQ.DH) FIH=0   
       ELSEIF(TTEST.EQ.3) THEN
C         
C        COMMERCIAL THINNING -- USER SPECIFIED AGES         
C         
        CTAS=1      
        WRITE(*,6013)     
6013    FORMAT(/' ENTER THE NUMBER OF COMMERCIAL THINNINGS (UP TO 9)')
        READ (*,*,END=10007)NCT         
10007 CONTINUE      
        WRITE(*,6014)     
6014    FORMAT(/' ENTER A TOTAL AGE FOR EACH COMMERCIAL THIN')        
        READ (*,*,END=10008)(AGET(I),I=1,NCT)     
10008 CONTINUE      
       ELSE         
C         
C        COMMERCIAL THINNING -- USER SPECIFIED HEIGHTS      
C         
        SCTHT=1     
        WRITE(*,6013)     
        READ (*,*,END=10009)NCT         
10009 CONTINUE      
        WRITE(*,6015)     
6015    FORMAT(/' ENTER A STAND HEIGHT FOR EACH COMMERCIAL THIN')     
        READ (*,*,END=10010)(HY40(I),I=1,NCT)     
10010 CONTINUE      
       ENDIF        
C         
C        THINNING PARAMETERS  
C         
       WRITE(*,6017)      
6017   FORMAT(/' SELECT NONE, ONE, OR TWO OF THE FOLLOWING CT OPTIONS:',        
     $  /5X,'USER SUPPLIED D/D RATIO FOR EACH CT'/10X,'0=NO'/10X,     
     2  '1=USER SPECIFICATIONS BETWEEN .80 & 1.25'/10X,'2=UNRESTRICTED',        
     3  ' USER SPECIFICATIONS'/5X,'USER SUPPLIED RESIDUAL SBA FOR EACH',        
     4  ' CT'/10X,'0=NO'/10X,'1=YES'/5X,'USER SUPPLIED RESIDUAL TPA ',
     5  'FOR EACH CT'/10X,'0=NO'/10X,'1=YES'//3X,'NOTE: A VALUE MUST ',         
     6  'BE ENTERED FOR EACH OPTION')   
       READ (*,*,END=10011)IDR,IGS,LTREE
10011 CONTINUE      
       IF(IDR.GT.0) THEN      
        WRITE(*,6020) NCT 
6020    FORMAT(/' ENTER A D/D RATIO FOR',I2,' COMMERCIAL THINNINGS ', 
     $   '(NEAREST .01)')     
        READ (*,*,END=10012)(DR(I),I=1,NCT)       
10012 CONTINUE      
       ENDIF        
       IF(IGS.EQ.1) THEN      
        WRITE(*,6021) NCT 
6021    FORMAT(/' ENTER RESIDUAL SBA FOR',I2,' COMMERCIAL THINNINGS') 
        READ (*,*,END=10013)(GRESD(I),I=1,NCT)    
10013 CONTINUE      
       ENDIF        
       IF(LTREE.EQ.1) THEN    
        WRITE(*,6022) NCT 
6022    FORMAT(/' ENTER RESIDUAL TPA FOR',I2,' COMMERCIAL THINNINGS') 
        READ (*,*,END=10014)(TARESD(I),I=1,NCT)   
10014 CONTINUE      
       ENDIF        
C         
C        STAND REQUIREMENTS FOR THINNING
C         
       WRITE(*,6025)      
6025   FORMAT(/' ENTER THE FOLLOWING FOUR VARIABLES:'/5X,'TOTAL STAND ',        
     $  'AGE FOR EARLIEST POSSIBLE CT'/5X,'MINIMUM SBA (TREES > 5.5', 
     2  ' DBH) FOR EARLIEST POSSIBLE CT (0=100)'/5X,'MINIMUM QMD',    
     3  '(NEAREST .1) OF TREES REMOVED (0=8.0)'/5X,'MINIMUM BASAL AREA',        
     4  ' TO BE REMOVED (0=20)')        
       READ (*,*,END=40   )FCT,GLIM,DLIM,GCLIM    
      ENDIF         
C         
C        FERTILIZATION OPTIONS
C         
40    REWIND iuin
      IF(FTEST.GT.0) THEN     
       IF(MULT.NE.1) GO TO 50 
       WRITE(*,6503)      
6503   FORMAT(/' ARE THE FERTILIZATION PARAMETERS REPEATED FROM THE ',
     $ 'LAST SETUP?',3X)     
       READ(*,5200,END=42) RPTFRT    
   42 CONTINUE      
      IF (FC.LE.0) RPTFRT='N' 
       IF (RPTFRT.EQ.'Y') GO TO 60      
       DO 45 I=1,9  
      AGEF(I)=0     
      PNIT(I)=0     
45     CONTINUE     
50     IF(FERTCT.EQ.1) THEN   
        WRITE(*,6030) NCT 
6030    FORMAT(/' ENTER THE RATE(LBS/ACRE NITROGEN) FOR',I2,
     $   ' FERTILIZER APPLICATIONS')    
        READ (*,*,END=10015)(PNIT(I),I=1,NCT)     
10015 CONTINUE      
       ELSE         
        WRITE(*,6031)     
6031   FORMAT(/' ENTER THE NUMBER OF FERTILIZER APPLICATIONS (UP TO 9)')        
        READ (*,*,END=10016)NFERT       
10016 CONTINUE      
        WRITE(*,6032)     
6032    FORMAT(/' ENTER THE TOTAL AGE AND RATE(LBS/ACRE NITROGEN) FOR ',        
     2   'EACH FERTILIZER APPLICATION',/)         
        READ (*,*,END=60   )(AGEF(I),PNIT(I),I=1,NFERT)     
       ENDIF        
      ENDIF         
C         
C        ROTATION LENGTH OPTION         
C         
60    REWIND iuin
      WRITE(*,6040)       
6040  FORMAT(/' ENTER THE ROTATION LENGTH:'/5X,'ROTATION AGE (0=',    
     $ 'ROTATION HEIGHT SUPPLIED)'/5X,'AND, HEIGHT AT FINAL HARVEST ',
     2 '(0=ROTATION AGE SUPPLIED)')     
      READ (*,*,END=10017)AHARR,HTHARV  
10017 CONTINUE      
C         
C        OUTPUT OPTIONS       
C         
      IF(MULT.NE.1) GO TO 75  
      WRITE(*,6504)       
6504  FORMAT(/' ARE THE OUTPUT TABLE PARAMETERS REPEATED FROM ',      
     $ 'THE LAST SETUP?',3X) 
      READ(*,5200,END=62) RPTLP      
   62 CONTINUE      
       IF (RPTLP.EQ.'Y') GO TO 80       
      DO 70 I=1,76  
      RAN(I)=0      
70    CONTINUE      
75    WRITE(*,6050)       
6050  FORMAT(/' ENTER THREE OUTPUT PARAMETERS:'/5X,'OUTPUT TABLE TYPES'/        
     $ 10X,'0=TREES > 1.5 DBH'/10X,'1=ALL TYPES (TREES > 1.5, 5.5, 7.5',        
     2 ' DBH)'/10X,'2=TWO TYPES (TREES > 1.5 AND 5.5 DBH)'/10X,       
     3 '3=TWO TYPES (TREES > 1.5 AND 7.5 DBH)'/5X,'OUTPUT TIMING'/    
     4 10X,'0=OUTPUT TABLES AT CUTTING AGES'/10X,'1=OUTPUT TABLES AT ',         
     5 'CUTTING AGES AND USER SPECIFIED AGES (UP TO 76)'/10X,         
     6 '5=OUTPUT TABLES EVERY YEAR'/5X,'NUMBER OF USER SPECIFIED AGES',         
     7 ' (0=NONE)') 
      READ (*,*,END=10018)IMV,NRA,NTB   
10018 CONTINUE      
      IF(NTB.GE.20.AND.NTB.LE.38) NRA=2 
      IF(NTB.GE.39.AND.NTB.LE.57) NRA=3 
      IF(NTB.GE.58) NRA=4     
      IF(NRA.GT.0.AND.NRA.LT.5) THEN    
       WRITE(*,6051) NTB  
6051   FORMAT(/' ENTER',I3,' TOTAL STAND AGES FOR OUTPUT TABLES')     
       READ (*,*,END=80   )(RAN(I),I=1,NTB)       
      ENDIF         
C         
C        OBSERVED HEIGHTS OPTION (USER SUPPLIED HT/AGE DATA)
C         
80    REWIND iuin
      IF(SHORT.EQ.'Y') GO TO 90         
      IF(MULT.NE.1) GO TO 85  
      WRITE(*,6505)       
6505  FORMAT(/' ARE THE OBSERVED HEIGHT PARAMETERS REPEATED FROM THE',
     $  ' LAST SETUP?')      
      READ(*,5200,END=81) RPTOBS     
   81 CONTINUE      
       IF (RPTOBS.EQ.'Y') GO TO 90      
      DO 82 I=1,31  
      OBSAGE(I)=0   
      OBSHTS(1,I)=0.0         
      OBSHTS(2,I)=0.0         
82    CONTINUE      
85    WRITE(*,6060)       
6060  FORMAT(/' DO YOU HAVE HT/AGE DATA TO ADJUST THE GROWTH CURVES?')         
      READ(*,5200,END=10019) OBSFLG  
10019 CONTINUE      
      IOBS=0        
      IF(OBSFLG.EQ.'Y') IOBS=1
      IF(IOBS.EQ.1) THEN      
       WRITE(*,6061)      
6061   FORMAT(/' UP TO 30 SETS OF THE FOLLOWING INFO MAY BE ENTERED:'/
     $  5X,'TOTAL STAND AGE AT HEIGHT OBSERVATION'/5X,      
     2  'STAND TOP HEIGHT (BEFORE THINNING IF APPROPRIATE)'/5X,       
     3  'STAND TOP HEIGHT (AFTER THINNING, OR 0 IF HT AFTER = HT ',   
     4  'BEFORE)'//3X,'NOTE: ENTER HEIGHTS TO THE NEAREST .1 FT;')    
  495 CONTINUE      
      WRITE (*,6062)      
 6062 FORMAT (' HOW MANY SETS WILL BE ENTERED?') 
      READ (*,*,END=490) NUM         
  490 CONTINUE      
      IF (NUM.LE.0.OR.NUM.GT.31) THEN   
      WRITE (*,6066) NUM  
 6066 FORMAT (' ERROR INVALID NUMBER OF SETS' ,I5)
      GO TO 495     
      ENDIF         
      WRITE (*,6064)      
 6064 FORMAT (' ENTER DATA:',/)         
       READ(*,*,END=500)(OBSAGE(N),OBSHTS(1,N),OBSHTS(2,N),N=1,NUM)
500   REWIND IUIN   
       IF(N.GT.5.AND.N.LE.10) IOBS=2    
       IF(N.GT.10.AND.N.LE.15) IOBS=3   
       IF(N.GT.15.AND.N.LE.20) IOBS=4   
       IF(N.GT.20.AND.N.LE.25) IOBS=5   
       IF(N.GT.25) IOBS=6     
      ENDIF         
C         
C        PREPARE REAL VARIABLES FOR OUTPUT (CONVERT TO NEAREST INTEGER)         
C         
90    REWIND IUIN   
      IDLIM=NINT(DLIM*10.0)   
      ISDIA=NINT(SDIA*100.0)  
      ISBA=NINT(SBA*10.0)     
      DO 600 I=1,9  
      INTDR(I)=NINT(DR(I)*100.0)        
600   CONTINUE      
      DO 700 I=1,NUM
      IOBSHT(1,I)=NINT(OBSHTS(1,I)*10.0)
      IOBSHT(2,I)=NINT(OBSHTS(2,I)*10.0)
700   CONTINUE      
C         
C  EEEEEEEEEE       
C         
C       DETERMINE IF ECONOMICS TABLES WANTED      
C         
      WRITE (*,7000)      
      READ (*,8000,END=790) ECON        
 8000 FORMAT (BZ,A1)
      IF (ECON.EQ.'Y'.or.econ.eq.'y') THEN   
      ECO=1         
C         
      ELSE
       ECO=0        
       GO TO 790    
      ENDIF         
C       DETERMINE ECONOMICS UNITS       
C         
 7003 CONTINUE      
      WRITE (*,7010)      
      READ (*,8010,END=790) EUNITS      
C         
C       CHECK VALIDITY OF EUNITS        
C         
      IF (.NOT.(EUNITS.EQ.ICV4.OR.EUNITS.EQ.ICV6.OR.EUNITS.EQ.IV6.OR. 
     1 EUNITS.EQ.ISV6)) THEN  
       WRITE (*,7015)     
 7015  FORMAT (' INVALID ECONOMICS UNITS  ')      
       GO TO 7003   
      END IF        
 8010 FORMAT (BZ,A3)
C         
C       DETERMINE IF ALL ECONOMICS PARAMETERS SAME
C         
      IF (MULT.EQ.1) THEN     
       WRITE (*,7190)     
 7190 FORMAT(' ARE ALL ECONOMICS VALUES THE SAME AS THE PREVIOUS',/,  
     1 ' SET-UP?',3X)        
      READ (*,8000,END=7195) SAME       
 7195 CONTINUE      
       IF (SAME.EQ.'Y'.or.same.eq.'y') GO TO 790       
       REWIND IUIN  
      END IF        
 7200 FORMAT (' ARE VALUES THE SAME AS THE PREVIOUS SET-UP?',3X)     
C         
C       CHECK IF USE POND VALUES FROM PREVIOUS SET-UP       
C         
       IF (MULT.EQ.1) THEN    
        WRITE(*,8047)     
 8047 FORMAT(' ARE POND VALUES THE SAME AS THE PREVIOUS',   
     1 ' SET-UP?') 
        READ (*,8000,END=8045) SAME     
 8045 CONTINUE      
       IF (SAME.EQ.'Y'.or.same.eq.'y') GO TO 8050      
      A=0.0         
      B=0.0         
      C=0.0         
      PONTAB=0      
      DO 8055 I=1,8 
      DO 8055 J=1,2 
 8055 PONDVL(I,J)=0.0         
       END IF       
C         
C       DETERMINE IF POND VALUE TABLE PROVIDED    
C         
      WRITE (*,7020)      
      READ (*,8000,END=8030) TABLE      
 8030 CONTINUE      
C         
C       ENTER PONDVL COEFFICIENTS A,B,C IF NO TABLE         
C         
      IF (TABLE.NE.'Y'.and.table.ne.'y') THEN  
       WRITE (*,7040)     
       READ (*,*,END=8040) A,B,C        
 8040  CONTINUE     
      PONTAB=0      
      GO TO 8050    
      ELSE
C         
C       ENTER POND VALUE TABLE
C         
 7028 CONTINUE      
       WRITE (*,7030)     
       WRITE (*,7032)     
       READ (*,*,END=8050) PONTAB       
C         
C       CHECK VALIDITY OF POND VALUE TABLE SUBSCRIPT        
C         
      IF (PONTAB.LT.1.OR.PONTAB.GT.8) THEN        
       WRITE (*,7033)     
 7033  FORMAT (' INVALID NUMBER  ')     
       GO TO 7028   
      END IF        
       WRITE (*,7034)     
       READ (*,*,END=8050) (PONDVL(I,1),I=1,PONTAB)         
       WRITE (*,7036)     
       READ (*,*,END=8050) (PONDVL(I,2),I=1,PONTAB)         
      END IF        
 8050 CONTINUE      
C         
C       CHECK IF USE LOG COST FROM PREVIOUS SET-UP
C         
      IF (MULT.EQ.1) THEN     
       WRITE (*,8073)     
 8073 FORMAT (' ARE LOG COST VALUES THE SAME AS THE PREVIOUS',/       
     1 ' SET-UP?',3X)        
       READ (*,8000,END=8075) SAME      
 8075 CONTINUE      
       IF (SAME.EQ.'Y'.or.same.eq.'y') GO TO 8080      
      COSTAB=0      
      LDI=0         
      LVI=0         
      LDF=0         
      LVF=0         
      DO 8076 I=1,12
      DO 8077 J=1,12
      LOGCF(I,J)=0.0
 8077 LOGCI(I,J)=0.0
      LOGDI(I)=0.0  
 8076 LOGDF(I)=0.0  
      END IF        
C         
C       DETERMINE IF LOG COST TABLE(S) PROVIDED   
C         
      WRITE (*,7050)      
      WRITE (*,7070)      
      READ (*,8000,END=8060) TABLE      
 8060 CONTINUE      
      IF (TABLE.NE.'Y'.and.table.ne.'y') THEN  
       COSTAB=0     
      WRITE (*,8065)      
       GO TO 8080   
      ENDIF         
C         
C       DETERMINE IF DIFFERENT FINAL HARVEST LOG COST TABLE PROVIDED  
C         
      WRITE (*,7060)      
      READ (*,8000,END=8070) TABLE      
 8070 CONTINUE      
      IF (TABLE.NE.'Y'.and.table.ne.'y') THEN  
       COSTAB=1     
       WRITE (*,7100)     
      ELSE
       COSTAB=2     
      END IF        
C         
C       STORE INTERMEDIATE LOG COST TABLE         
C         
 8078 CONTINUE      
      WRITE (*,7080)      
      READ (*,9000,END=9020) NAME       
      WRITE (*,7084)      
      READ (*,*,END=8079) LDI,LVI       
 8079 CONTINUE      
C         
C       CHECK VALIDITY OF LOG COST SUBSCRIPTS     
C         
      IF (LDI.LT.0.OR.LDI.GT.12.OR.LVI.LT.0.OR.LVI.GT.12) THEN        
       WRITE (*,7082)     
 7082  FORMAT (' AT LEAST ONE VALUE IS INVALID  ')
       GO TO 8078   
      END IF        
      REWIND IIN    
 8090 CONTINUE      
      READ (IIN,9000,END=9020) INAME    
 9000 FORMAT (BZ,A5)
      IF (INAME.NE.NAME) GO TO 8090     
      READ (IIN,*,END=9020) (LOGVI(I),I=1,LVI)    
      DO 9010 I=1,LDI         
 9010 READ (IIN,*,END=9020) LOGDI(I),(LOGCI(I,J),J=1,LVI)   
      GO TO 9030    
C  ERROR MESSAGE    
 9020 CONTINUE      
      WRITE (*,7095) NAME 
      WRITE (*,8065)      
      COSTAB=0      
      GO TO 8080    
C         
C       STORE FINAL LOG COST TABLE      
C         
 9030 IF (COSTAB.NE.2) GO TO 8080       
      WRITE (*,7090)      
      READ (*,9000,END=9050) NAME       
      WRITE (*,7084)      
      READ (*,*,END=9055) LDF,LVF       
 9055 CONTINUE      
C         
C       CHECK VALIDITY OF LOG COST SUBSCRIPTS     
C         
      IF (LDF.LT.0.OR.LDF.GT.12.OR.LVF.LT.0.OR.LVF.GT.12) THEN        
       WRITE (*,7082)     
       GO TO 9030   
      END IF        
      REWIND IIN    
 9060 CONTINUE      
      READ (IIN,9000,END=9050) INAME    
      IF (INAME.NE.NAME) GO TO 9060     
      READ (IIN,*,END=9050) (LOGVF(I),I=1,LVF)    
      DO 9040 I=1,LDF         
 9040 READ (IIN,*,END=9050) LOGDF(I),(LOGCF(I,J),J=1,LVF)   
      GO TO 8080    
C         
C       ERROR MESSAGE         
C         
 9050 CONTINUE      
      WRITE (*,7095) NAME 
      WRITE (*,8065)      
      COSTAB=0      
 8080 CONTINUE      
C         
C         MORTALITY OPTION    
C         
      WRITE (*,8085)      
      READ (*,8000,END=8086) SAME       
 8086 CONTINUE      
      IF (SAME.EQ.'Y'.or.same.eq.'y') IMORT=1
C         
C       STORE PER ACRE COSTS  
C         
      WRITE (*,7110)      
      IF (MULT.EQ.1) THEN     
       WRITE (*,7200)     
       READ (*,8000,END=9065) SAME      
 9065 CONTINUE      
       IF (SAME.EQ.'Y'.or.same.eq.'y') GO TO 9070      
       REWIND IUIN  
       WRITE (*,6506)     
      END IF        
      READ (*,*,END=9070) REGENC,PCTC,FHRC,OATC,OAHC        
 9070 CONTINUE      
C         
C  FERTILIZATION OPTION       
C         
      IF (FTEST.GT.0)THEN     
          IF (MULT.EQ.1) THEN 
              WRITE (*,9081)        
 9081     FORMAT (' ARE THE FERTILIZATION RATES AND COSTS REPEATED ', 
     1    'FROM THE LAST SETUP?')      
              READ (*,5200,END=9085) SAME      
              IF (SAME.EQ.'Y'.or.same.eq.'y') GO TO 9085         
C         
          DO 9082 I=1,15      
          DO 9082 J=1,2       
              NIT(I,J)=0      
 9082         CNIT(I,J)=0.0   
          END IF    
C         
 9089     WRITE (*,9083)  
 9083     FORMAT (' ENTER NUMBER OF RATE/COST PAIRS ' )     
          READ (*,*,END=9085) FC        
          IF (FC.GT.0.AND.FC.LE.9) THEN 
              WRITE(*,9086)         
 9086         FORMAT(' ENTER RATE/COST PAIRS XXX,XXXX '/'-')
              READ (*,*,END=9085) ((CNIT(I,J),J=1,2),I=1,FC)
          ELSE IF (FC.GT.9.AND.FC.LE.15) THEN     
              WRITE (*,9088)        
 9088         FORMAT (' ENTER FIRST 9 RATE/COST PAIRS ON ONE LINE'/   
     1        ' THEN ENTER REMAINING PAIRS ON A NEW LINE XXX,XXXX '/'-')        
              READ (*,*,END=9085) ((CNIT(I,J),J=1,2),I=1,9) 
              READ (*,*,END=9085) ((CNIT(I,J),J=1,2),I=10,FC)         
          ELSE      
              WRITE (*,9087)        
 9087         FORMAT (' ERROR FC MUST BE GT 0 AND LE 15 ')  
              GO TO 9089      
          END IF    
      END IF        
 9085 CONTINUE      
C         
C       STORE VARIABLES       
C         
      WRITE (*,7115)      
      IF (MULT.EQ.1) THEN     
       WRITE (*,7200)     
       READ (*,8000,END=9075) SAME      
 9075 CONTINUE      
       IF (SAME.EQ.'Y'.or.same.eq.'y') GO TO 9080      
       REWIND IUIN  
       WRITE (*,6506)     
      END IF        
      READ (*,*,END=9080) FALL,R,LCM,OVTC,OVHC,HC 
 9080 CONTINUE      
C         
C       STORE INFLATION PER CENTS       
C         
      IF (MULT.EQ.1) THEN     
       WRITE (*,9090)     
       READ (*,8000,END=9095) SAME      
 9095 CONTINUE      
       IF (SAME.EQ.'Y'.or.same.eq.'y') GO TO 790       
 9090 FORMAT (' ARE ALL TRENDS THE SAME AS THE PREVIOUS',/, 
     1 ' SET-UP?',3X)        
      END IF        
      INFLAT=0      
      WRITE (*,7120)      
      READ (*,8000,END=9100) INFL       
      IF (INFL.EQ.'Y'.or.infl.eq.'y') THEN   
       WRITE (*,7130)     
       READ (*,*,END=9100) PVI
       INFLAT=1     
      END IF        
 9100 CONTINUE      
      WRITE (*,7140)      
      READ (*,8000,END=9110) INFL       
      IF (INFL.EQ.'Y'.or.infl.eq.'y') THEN   
       WRITE (*,7150)     
       READ (*,*,END=9110) RCI,PCTCI,FCI
       INFLAT=1     
      END IF        
 9110 CONTINUE      
      WRITE (*,7160)      
      READ (*,8000,END=790) INFL        
      IF (INFL.EQ.'Y'.or.infl.eq.'y') THEN   
       WRITE (*,7170)     
       READ (*,*,END=790) LCI,FHRCI,OAHCI,OVHCI,HCI         
       INFLAT=1     
      END IF        
  790 CONTINUE      
C         
C  EEEEEEEEEE       
C         
C        OUTPUT THE CONTROL FILE        
C          TITLE CARD AND MASTER CONTROL CARD     
C         
      LF=CHAR(10)
      CR=CHAR(13)
      DO 1500 I=1,LEN(TITLE)
 1500 IF (TITLE(I:I).EQ.LF.OR.TITLE(I:I).EQ.CR) TITLE(I:I)=' '
      WRITE(IOUT,'(A)') TITLE
      WRITE(IOUT,3000) SI,IORG,PCT,TAST,IOBS,EXIST,IUT,NCT,FCT,  
     $ TI,FIY,CTAS,AHARR,DH,FIH,SCTHT,HTHARV,IDR,IGS,LTREE,IDLIM,     
     2 GCLIM,GLIM,FERTCT,FERTAS,IMV,NRA,ECO,isv,ird       
3000  FORMAT( I3,I2,I3,I5,3I2,4I3,I2,I4,2I3,I2,I4,3I2,I4,I3,I4,6I2,i5) 
C         
C        EXISTING STAND CARD  
C          CARD TYPE 1        
C         
      IF(EXIST.EQ.1) WRITE(IOUT,3001) SAGE,ISDIA,ISBA,STA   
3001  FORMAT(I3,2I5,I6,T79,'10')        
C         
C        SPECIFIED COMMERCIAL THINNING AGE CARD   
C          CARD TYPE 2        
C         
      IF(CTAS.EQ.1) THEN      
       KRD=2        
       WRITE(IOUT,3002)(AGET(I),I=1,9),KRD,ICONT  
3002   FORMAT(9(2X,I3),T79,2I1)         
C         
C        SPECIFIED COMMERCIAL THINNING HEIGHT CARD
C          CARD TYPE 3        
C         
      ELSEIF(SCTHT.EQ.1) THEN 
       KRD=3        
       WRITE(IOUT,3002)(HY40(I),I=1,9),KRD,ICONT  
      ENDIF         
C         
C        D/D RATIO CARD       
C          CARD TYPE 4        
C         
      IF(IDR.GT.0) THEN       
       KRD=4        
       WRITE(IOUT,3005)(INTDR(I),I=1,9),KRD,ICONT 
3005   FORMAT(9(1X,I4),T79,2I1)         
      ENDIF         
C         
C        RESIDUAL BASAL AREA CARD       
C          CARD TYPE 5        
C         
      IF(IGS.EQ.1) THEN       
       KRD=5        
       WRITE(IOUT,3005)(GRESD(I),I=1,9),KRD,ICONT 
      ENDIF         
C         
C        RESIDUAL NUMBER OF TREES CARD  
C          CARD TYPE 6        
C         
      IF(LTREE.EQ.1) THEN     
       KRD=6        
       WRITE(IOUT,3005)(TARESD(I),I=1,9),KRD,ICONT
      ENDIF         
C         
C        FERTILIZATION CARD   
C          CARD TYPE 7        
C         
      IF(FTEST.GT.0) THEN     
       KRD=7        
       WRITE(IOUT,3010)(AGEF(I),PNIT(I),I=1,9),KRD,ICONT    
3010   FORMAT(18(1X,I3),T79,2I1)        
      ENDIF         
C         
C        REPORT AGES CARD     
C          CARD TYPE 8        
C         
      IF(NRA.GT.0.AND.NRA.LT.5) THEN    
       KRD=8        
       DO 800 J=1,NRA         
       INDX=(19*J - 18)       
       ICONT=J-1    
       WRITE(IOUT,3015)(RAN(I),I=INDX,INDX+18),KRD,ICONT    
3015   FORMAT(19I4,T79,2I1)   
800    CONTINUE     
      ENDIF         
C         
C  EEEEEEEEEE       
C         
      IF (ECO.NE.1) GO TO 990 
C         
C       REPORT ECON CARD TYPE 10        
C         
      WRITE (IOUT,9120) EUNITS,PONTAB,COSTAB,LDI,LVI,LDF,LVF,A,B,C    
     1 ,IMORT,FALL  
 9120 FORMAT (1X,A3,I3,I2,2X,4I3,3(1X,G14.8),I2,F5.2)       
C         
C       REPORT ECON CARD TYPE 11        
C         
C       PREPARE REAL VARIABLES FOR OUTPUT (CONVERT TO NEAREST INTEGER)
C         
      IR=NINT(R*10.0)         
      IREGEN=NINT(REGENC)     
      IPCTC=NINT(PCTC)        
      IFC=NINT(FC)  
      ILCM=NINT(LCM*100.0)    
      IFHRC=NINT(FHRC)        
      IOATC=NINT(OATC)        
      IOVTC=NINT(OVTC)        
      IOAHC=NINT(OAHC)        
      IOVHC=NINT(OVHC)        
      IHC=NINT(HC)  
C         
      WRITE (IOUT,9130) IR,IREGEN,IPCTC,IFC,ILCM,IFHRC,IOATC,IOVTC,   
     1 IOAHC,IOVHC,IHC,INFLAT 
 9130 FORMAT (I5,3I6,I5,6I6,I2)         
C         
C       REPORT ECON CARD TYPE 12        
C         
      IF (INFLAT.NE.1) GO TO 9150       
      WRITE (IOUT,9140) PVI,RCI,PCTCI,FCI,LCI,FHRCI,OAHCI,OVHCI,HCI   
 9140 FORMAT (9F5.1)
 9150 CONTINUE      
C         
C       REPORT ECON CARD TYPE 13 AND 14 
C         
      IF (PONTAB.LE.0) GO TO 9180       
      WRITE (IOUT,9160) (PONDVL(J,1),J=1,PONTAB)  
 9160 FORMAT (8F5.1)
      WRITE (IOUT,9170) (PONDVL(J,2),J=1,PONTAB)  
 9170 FORMAT (8F6.0)
 9180 CONTINUE      
C         
C       REPORT ECON CARD TYPE 15 AND 16 
C         
      IF (COSTAB.LE.0) GO TO 9230       
      WRITE (IOUT,9190) (LOGVI(I),I=1,LVI)        
 9190 FORMAT (5X,12F6.2)      
      DO 9200 I=1,LDI         
 9200 WRITE (IOUT,9210) LOGDI(I),(LOGCI(I,J),J=1,LVI)       
 9210 FORMAT (1X,F4.1,12F6.0) 
      IF (COSTAB.NE.2) GO TO 9230       
      WRITE (IOUT,9190) (LOGVF(I),I=1,LVF)        
      DO 9220 I=1,LDF         
 9220 WRITE (IOUT,9210) LOGDF(I),(LOGCF(I,J),J=1,LVF)       
 9230 CONTINUE      
C         
C         
C        REPORT ECON CARD TYPE 17       
C         
      IF (FTEST.GT.0) THEN    
         IF (MULT.EQ.1.AND.CNIT(1,1).LE.0.0) GO TO 9260     
         DO 9250 I=1,15       
         DO 9250 J=1,2        
 9250        NIT(I,J)=CNIT(I,J)         
 9260    IF (FC.GT.0.AND.FC.LE.15) THEN 
         WRITE(IOUT,9240) ((NIT(I,J),J=1,2),I=1,FC)         
         END IF     
 9240    FORMAT (9(1X,I3,I4)) 
      END IF        
C         
C  EEEEEEEEEE       
C         
  990 CONTINUE      
C         
C        OBSERVED HEIGHTS CARD
C          CARD TYPE 9        
C         
      IF(IOBS.GT.0) THEN      
       KRD=9        
       DO 900 J=1,IOBS        
       INDX=(5*J - 4)         
       ICONT=J-1    
       WRITE(IOUT,3020)(OBSAGE(I),IOBSHT(1,I),IOBSHT(2,I),I=INDX,INDX+4)        
     $ ,KRD,ICONT   
3020   FORMAT(5(I4,2(I5)),T79,2I1)      
900    CONTINUE     
      ENDIF         
C         
C        END WITH CONTROL FILE SETUP; CHECK FOR ANOTHER ROUND         
C         
      WRITE(*,6100)       
6100  FORMAT(/' DO YOU WISH TO SET UP ANOTHER RUN?',3X)    
      READ(*,5100,END=10020) CNTNUE  
10020 CONTINUE      
5100  FORMAT(BZ,A1) 
      IF(CNTNUE.EQ.'Y'.or.cntnue.eq.'y') THEN  
       MULT=1       
       IF(IUT.EQ.1) THEN      
        IUT=0       
       ELSE         
        IUT=1       
       ENDIF        
       GO TO 10     
      ENDIF         
      WRITE(IOUT,3900)        
3900  FORMAT('END') 
      REWIND IOUT   
      STOP
      END 
