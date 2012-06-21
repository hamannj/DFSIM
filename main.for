      PROGRAM DFSIM
C                                                                       DFS  10
C      D F S I M  IS A MANAGED STAND GROWTH AND YIELD SIMULATOR FOR     DFS  15
C      COASTAL DOUGLAS-FIR.                                             DFS  20
C                                                                       DFS  25
C     IUIN IS INPUT FLE - TAPE5
C     IUOUT IS OUTPUT - TAPE6
C
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16DFS  30
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), DFS  35
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     DFS  40
     $ TARESD(16), TA(16,4), VA(16,4)                                   DFS  45
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), DFS  50
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     DFS  55
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  DFS  60
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               DFS  65
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,DFS  70
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,DFS  75
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,DFS  80
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, DFS  85
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   DFS  90
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          DFS  95
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, DFS 100
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 DFS 105
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMDFS 110
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDDFS 115
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            DFS 120
      common year,month,day,hour,minute,second,i100th
      integer*2 year,month,day,hour,minute,second,i100th
      COMMON /ECONO/ A,B,C,COSTAB,ECO,EUNITS,FC,FCI,FHRC,FHRCI,
     $HAULC1,HC,HCI,IFER,
     $LCI,LCM,LDF,LDI,LOGC1,LOGCF(12,12),LOGCI(12,12),
     $LOGDF(12),LOGDI(12),LOGVF(12),LOGVI(12),LVF,LVI,
     $MAH,NETRV1,OAHC,OAHCI,OATC,OTHRC1,OVHC,OVHCI,
     $OVTC,PCTC,PCTCI,PNW1,PNW2,PNDVL1,PONDVL(8,2),PONTAB,
     $PVI,R,RCI,REGENC,SEV,TRUED,TYHV,VOLM,FAGE(15,2),CNIT(15,2)
     $ ,DNR,VDEAD,CVDEAD,VOLUME,CUMVOL,FALL,IMORT,CMORT
      COMMON/EMORT/MORTE(16,4),DMORT,GMORT,TAMORT,VMORTE
      COMMON/CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      REAL MORTE
      REAL LOGC1,LOGCF,LOGCI,LOGDF,LOGDI,LOGVF,LOGVI,MAH,NETRV1,LCI,LCM
      INTEGER COSTAB,ECO,EUNITS,PONTAB,TYHV

      integer*4 today(3), now(3)

C                                                                       DFS 125
C     ************ DEFINITION OF IMPORTANT DFSIM VARIABLES ***********  DFS 130
C                                                                       DFS 135
C     AGE    = BREAST HEIGHT STAND AGE                                  DFS 140
C     AGEF   = ARRAY OF STAND AGES AT TIMES OF NITROGEN FERTILIZER      DFS 145
C                   APPLICATION                                         DFS 150
C     AGET   = ARRAY OF STAND AGES AT TIMES OF CUTTING                  DFS 155
C     AHARR  = TOTAL STAND AGE AT HARVEST CUT                           DFS 160
C     AHARV  = BREAST HEIGHT STAND AGE AT HARVEST CUT                   DFS 165
C     ARG    = ARRAY OF PLANTATION DECAY VARIABLES                      DFS 170
C     ASYMP  = BEGINNING POINT FOR RELATIVE DENSITY CONTROL             DFS 175
C     BAB    = BASAL AREA PER ACRE BEFORE MOST RECENT THINNING          DFS 180
C     BAC    = BASAL AREA PER ACRE CUT MOST RECENT THINNING             DFS 185
C     BALL20 = MINIMUM BASAL AREA PER ACRE NEEDED IN TREES 1.6-INCHES + DFS 190
C                   DBH BEFORE FIRST COMMERCIAL THINNING CAN BE DONE    DFS 195
C     BAPA   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            DFS 200
C     CAI    = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE NET CURRENT      DFS 205
C                   ANNUAL INCREMENT IN TREES 1.6-INCHES + DBH          DFS 210
C     CTAS   = INDICATOR SWITCH FOR SPECIFIED COMMERCIAL THINNING AGES  DFS 215
C     CT1    = INDICATOR FOR COMMERCIAL THINNING IN DENSE STANDS        DFS 220
C     D      = ARRAY OF QUADRATIC MEAN DIAMETERS FOR LIVE STAND BEFORE  DFS 225
C                   CUT, AFTER CUT, AND MORTALITY OF TREES 1.6-INCHES + DFS 230
C                   DBH FOR EACH COMMERCIAL CUTTING                     DFS 235
C     DC     = MINIMUM QUADRATIC MEAN STAND DIAMETER NEEDED IN TREES    DFS 240
C                   1.6-INCHES + DBH BEFORE FIRST COMMERCIAL THINNING   DFS 245
C                   CAN BE DONE                                         DFS 250
C     DDMRT  = QUADRATIC MEAN DIAMETER OF MORTALITY TREES 1.6-INCHES +  DFS 255
C                   DBH                                                 DFS 260
C     DH     = MINIMUM STAND TOP HEIGHT INCREMENT BETWEEN COMMERCIAL    DFS 265
C                   THINNINGS                                           DFS 270
C     DLIM   = MINIMUM QUADRATIC MEAN DIAMETER OF CUT TREES FOR A       DFS 275
C                   COMMERCIAL THINNING                                 DFS 280
C     DMTMIN = MINIMUM QUADRATIC MEAN DIAMETER OF MORTALITY TREES       DFS 285
C                   1.6-INCHES + DBH                                    DFS 290
C     DNOW   = QUADRATIC MEAN STAND DIAMETER LIVE TREES 1.6-INCHES +    DFS 295
C                   DBH                                                 DFS 300
C     DR     = ARRAY OF DIAMETER CUT / DIAMETER BEFORE CUT RATIOS FOR   DFS 305
C                   EACH COMMERCIAL THINNING                            DFS 310
C     EXIST  = INDICATOR SWITCH FOR EXISTING STAND                      DFS 315
C     FCT    = TOTAL STAND AGE NEEDED FOR FIRST COMMERCIAL THINNING     DFS 320
C     FCTA   = BREAST HEIGHT STAND AGE NEEDED FOR FIRST COMMERCIAL      DFS 325
C                   THINNING                                            DFS 330
C     FERTAS = INDICATOR SWITCH FOR NITROGEN FERTILIZER APPLICATIONS    DFS 335
C                   AT SPECIFIED STAND AGES (NO. FERTILIZER CARDS)      DFS 340
C     FERTCT = INDICATOR SWITCH FOR NITROGEN FERTILIZER APPLICATIONS    DFS 345
C                   AT COMMERCIAL THINNINGS (NO.FERTILIZER CARDS)       DFS 350
C     FERTEF = NITROGEN FERTLILZER EFFECT                               DFS 355
C     FNLBS  = ARRAY OF EFFECTIVE DOSAGE OF NITROGEN FERTLIZER IN POUNDSDFS 360
C                   PER ACRE                                            DFS 365
C     GA     = ARRAY OF BASAL AREAS PER ACRE FOR LIVE STAND BEFORE CUT, DFS 370
C                   CUT, AFTER CUT, AND MORTALITY TREES 1.6-INCHES + DBHDFS 375
C                   FOR EACH COMMERCIAL CUTTING                         DFS 380
C     GCLIM  = MINIMUM BASAL AREA PER ACRE TO BE REMOVED IN TREES       DFS 385
C                   1.6-INCHES + DBH FOR A COMMERCIAL THINNING          DFS 390
C     GL     = MINIMUM BASAL AREA PER ACRE NEEDED IN TREES 1.6-INCHES + DFS 395
C                   DBH FOR A COMMERCIAL THINNING                       DFS 400
C     GLIM   = MINIMUM BASAL AREA PER ACRE NEEDED IN TREES 5.6-INCHES + DFS 405
C                   DBH FOR FIRST COMMERCIAL THINNING                   DFS 410
C     GRESD  = ARRAY OF BASAL AREA RESIDUALS FOR EACH COMMERCIAL        DFS 415
C                   THINNING                                            DFS 420
C     H40    = ARRAY OF HEIGHTS OF 40 LARGEST TREES PER ACRE 1.6-INCHES DFS 425
C                   + DBH (STAND TOP HEIGHTS) FOR EACH COMMERCIAL       DFS 430
C                   CUTTING                                             DFS 435
C     HAT    = STAND TOP HEIGHT AFTER MOST RECENT THINNING              DFS 440
C     HFCT   = STAND TOP HEIGHT AT FIRST COMMERCIAL THINNING            DFS 445
C     HPCT   = STAND TOP HEIGHT AFTER PRECOMMERCIAL THINNING            DFS 450
C     HS     = STAND TOP HEIGHT                                         DFS 455
C     HTHARV = STAND TOP HEIGHT AT HARVEST CUT                          DFS 460
C     HTL    = LOREY?S HEIGHT. HEIGHT OF TREE OF AVERAGE CUBIC-FOOT     DFS 465
C                   VOLUME (TOTAL STEM)                                 DFS 470
C     HTLA   = LOREY?S HEIGHT AFTER THINNING                            DFS 475
C     HTLFTH = DIFFERENCE BETWEEN CURRENT STAND TOP HEIGHT AND STAND    DFS 480
C                   TOP HEIGHT AT HARVEST CUT                           DFS 485
C     HTLFTT = DIFFERENCE BETWEEN CURRENT STAND TOP HEIGHT AND STAND    DFS 490
C                   TOP HEIGHT AT NEXT SPECIFIED COMMERIAL THINNING     DFS 495
C     HY40   = ARRAY OF SPECIFIED COMMERCIAL THINNING STAND TOP HEIGHTS DFS 500
C     ICT1   = INDICATOR SWITCH THAT AT LEAST 1 COMMERCIAL THINNING HAS DFS 505
C                   BEEN DONE                                           DFS 510
C     IDR    = INDICATOR SWITCH FOR SPECIFIED DIAMETER CUT / DIAMETER   DFS 515
C                   BEFORE CUT RATIOS                                   DFS 520
C     IGS    = INDICATOR SWITCH FOR SPECIFIED RESIDUAL BASAL AREAS      DFS 525
C     IMV    = INDICATOR SWITCH FOR 5.6- AND 7.6-INCH + DBH YIELD TABLESDFS 530
C     IOBS   = NUMBER OF SPECIFIED STAND TOP HEIGHT OBSERVATIONS        DFS 535
C                   (INITIALLY NO. OBSERVED HEIGHT CARDS)               DFS 540
C     IORG   = STAND ORIGIN                                             DFS 545
C     IPAGE  = NUMBER OF PAGES PRINTED FOR YIELD TABLE                  DFS 550
C     IQ     = INDEX FOR REPORT AGES ARRAY                              DFS 555
C     IRD    = INDICATOR SWITCH FOR NET BASAL AREA INCREMENT FUNCTION   DFS 560
C     ISWITA = INDICATOR SWITCH FOR DELAY OF FIRST COMMERCIAL THINNING  DFS 565
C     ISWITB = INDICATOR SWITCH FOR DELAY OF FIRST COMMERCIAL THINNING  DFS 570
C     ITHN   = INDICATOR SWITCH FOR THINNING DONE                       DFS 575
C     ITITLE = ARRAY FOR USER SPECIFIED TITLE TO BE PRINTED AT TOP OF   DFS 580
C                   EACH PAGE OF THE YIELD TABLE                        DFS 585
C     IUT    = INDICATOR SWITCH FOR THIN / NO THIN OPTION               DFS 590
C     IZ     = INDEX FOR PLANTATION AND THINNING DECAY ARRAYS           DFS 595
C     JRD    = INDICATOR SWITCH FOR NET CUBIC-FOOT VOLUME (TOTAL        DFS 600
C                   STEM) INCREMENT FUNCTION                            DFS 605
C     LINE   = NUMBER OF LINES PRINTED ON CURRENT PAGE OF YIELD TABLE   DFS 610
C     LIUT   = ARRAY OF COMMERCIAL THINNING STATUS FOR EACH COMMERCIAL  DFS 615
C                   THINNING SCHEDULED OR DONE                          DFS 620
C     LTREE  = INDICATOR SWITCH FOR RESIDUAL NUMBER OF TREES            DFS 625
C     NCT    = NUMBER OF COMMERCIAL THINNINGS SPECIFIED OR IMPLIED      DFS 630
C     NRA    = NUMBER OF REPORT AGES SPECIFIED                          DFS 635
C                   (INITIALLY NO. REPORT AGE CARDS)                    DFS 640
C     OBSAGE = ARRAY OF STAND AGES FOR SPECIFIED STAND TOP HEIGHT       DFS 645
C                   OBSERVATIONS                                        DFS 650
C     OBSHTS = ARRAY OF SPECIFIED STAND TOP HEIGHT OBSERVATIONS         DFS 655
C     PCT    = TOTAL STAND AGE AT PRECOMMERCIAL THINNING                DFS 660
C     PCTA   = BREAST HEIGHT STAND AGE AT PRECOMMERCIAL THINNING        DFS 665
C     PNIT   = ARRAY OF EFFECTIVE NITROGEN DOSAGES IN POUNDS PER        DFS 670
C                   ACRE FOR EACH FERTILIZER APPLICATION                DFS 675
C     RAN    = ARRAY OF SPECIFIED REPORT AGES                           DFS 680
C     SAGE   = EXISTING STAND TOTAL STAND AGE                           DFS 685
C     SBA    = EXISTING STAND BASAL AREA PER ACRE IN TREES 1.6-INCHES   DFS 690
C                   + DBH                                               DFS 695
C     SCTHT  = INDICATOR SWITCH FOR SPECIFIED STAND TOP HEIGHTS FOR     DFS 700
C                   EACH COMMERCIAL CUTTING                             DFS 705
C     SCUTBA = CUMULATIVE SUM OF BASAL AREA PER ACRE CUT IN TREES       DFS 710
C                   1.6-INCHES + DBH                                    DFS 715
C     SCUTTA = CUMULATIVE SUM OF NUMBER OF TREES PER ACRE CUT IN TREES  DFS 720
C                   1.6-INCHES + DBH                                    DFS 725
C     SCUTVA = CUMULATIVE SUM OF CUBIC-FOOT VOLUME (TOTAL STEM) PER     DFS 730
C                   ACRE CUT IN TREES 1.6-INCHES + DBH                  DFS 735
C     SDIA   = EXISTING STAND QUADRATIC MEAN STAND DIAMETER IN TREES    DFS 740
C                   1.6-INCHES + DBH                                    DFS 745
C     SGMORT = CUMULATIVE SUM OF BASAL AREA PER ACRE MORTALITY TREES    DFS 750
C                   1.6-INCHES + DBH                                    DFS 755
C     SI     = SITE INDEX (50-YEAR B H BASE AGE)                        DFS 760
C     SNMORT = CUMULATIVE SUM OF NUMBER OF TREES MORTALITY IN TREES     DFS 765
C                   1.6-INCHES + DBH                                    DFS 770
C     STA    = EXISTING STAND NUMBER OF TREES PER ACRE 1.6-INCHES +     DFS 775
C                   DBH                                                 DFS 780
C     SVMORT = CUMULATIVE SUM OF CUBIC-FOOT VOLUME (TOTAL STEM) PER ACREDFS 785
C                   IN MORTALITY TREES 1.6-INCHES + DBH                 DFS 790
C     TA     = ARRAY OF NUMBER OF TREES PER ACRE FOR LIVE STAND BEFORE  DFS 795
C                   CUT, CUT, AFTER CUT, AND MORTALITY IN TREES 1.6-    DFS 800
C                   INCHES + DBH FOR EACH COMMERCIAL CUTTING            DFS 805
C     TARESD = ARRAY OF RESIDUAL NO. TREES FOR EACH COMMERCIAL THINNING DFS 810
C     TAST   = NUMBER OF ESTABLISHED SEEDLINGS PER ACRE AFTER PLANTING  DFS 815
C                   OR NUMBER OF RESIDUAL TREES PER ACRE AFTER          DFS 820
C                   PRECOMMERCIAL THINNING                              DFS 825
C     TI     = MINIMUM NUMBER OF YEARS BETWEEN COMMERCIAL THINNINGS     DFS 830
C     TOAGE  = TOTAL STAND AGE                                          DFS 835
C     TTN    = ARRAY OF THINNING DECAY VARIABLES                        DFS 840
C     VA     = ARRAY OF CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE FOR     DFS 845
C                   LIVE STAND BEFORE CUT, CUT, AFTER CUT, AND          DFS 850
C                   MORTALITY IN TREES 1.6-INCHES + DBH FOR EACH        DFS 855
C                   COMMERCIAL CUTTING                                  DFS 860
C     VNOW   = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         DFS 865
C                   1.6-INCHES + DBH                                    DFS 870
C     XMAI16 = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE NET MEAN ANNUAL  DFS 875
C                   INCREMENT IN TREES 1.6-INCHES + DBH                 DFS 880
C     XMAI56 = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE NET MEAN ANNUAL  DFS 885
C                   INCREMENT IN TREES 5.6-INCHES + DBH                 DFS 890
C     XMAI76 = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE NET MEAN ANNUAL  DFS 895
C                   INCREMENT IN TREES 7.6-INCHES + DBH                 DFS 900
C     XNUM   = NUMBER OF TREES PER ACRE 1.6-INCHES + DBH                DFS 905
C     XNUMR  = NUMBER OF ESTABLISHED SEEDLINGS PER ACRE AFTER PLANTING  DFS 910
C                   OR NUMBER OF RESIDUAL TREES PER ACRE AFTER          DFS 915
C                   PRECOMMERCIAL THINNING                              DFS 920
C     YRLFTH = DIFFERENCE BETWEEN CURRENT STAND AGE AND STAND AGE AT    DFS 925
C                   HARVEST CUT                                         DFS 930
C     YRLFTT = DIFFERENCE BETWEEN CURRENT STAND AGE AND NEXT SPECIFIED  DFS 935
C                   COMMERCIAL THINNING AGE                             DFS 940
C     ZMAI16 = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE GROSS MEAN       DFS 941
C                   ANNUAL INCREMENT IN TREES 1.6-INCHES + DBH          DFS 921
C     ***************************************************************** DFS 945
        OPEN(3,FILE='TAPE3')
        OPEN(13,FILE='TAPE13')
         IUIN=3
      IUOUT=6
C  EEEEEEEEEE
C  SEE SUBROUTINE ECON FOR DEFINITION OF IMPORTANT ECON VARIABLES
C  EEEEEEEEEE
C                                                                       DFS 950
   10 CONTINUE                                                          DFS 955
c
c     Get current date and time
c
c$$$      call getdat (year,month,day)
c$$$      call gettim (hour,minute,second,i100th)

      call idate(today)   ! today(1)=day, (2)=month, (3)=year
      call itime(now)     ! now(1)=hour, (2)=minute, (3)=second

      year=today(3)
      month=today(2)
      day=today(1)
      hour=now(1)
      minute=now(2)
      second=now(3)
      i100th=0.0

      
      ISTOP = 0                                                         DFS 960
      VERS = '1.4'                                                      DFS 965
      AGEADD = 1.0                                                      DFS 970
      IHEAD = 0                                                         DFS 975
      DMTMIN = 0.0                                                      DFS 980
      GCLIM = 0.0                                                       DFS 985
      CAI = 0.0                                                         DFS 990
      XMAI56 = 0.0                                                      DFS 995
      XMAI76 = 0.0                                                      DFS1000
      IRD = 0                                                           DFS1005
      JRD = 0                                                           DFS1010
      CT1 = 0.0                                                         DFS1015
      ICT1 = 0                                                          DFS1020
      DMORT = 0.0                                                       DFS1025
      SCUTVA = 0.0                                                      DFS1030
      SCUTTA = 0.0                                                      DFS1035
      SCUTBA = 0.0                                                      DFS1040
      SCUT45 = 0.0                                                      DFS1045
      SCUT47 = 0.0                                                      DFS1050
      SDIA = 0.0                                                        DFS1055
      SAGE = 0.0                                                        DFS1060
      SBA = 0.0                                                         DFS1065
      STA = 0.0                                                         DFS1070
      IPAGE = 1                                                         DFS1075
      BAB = 0.0                                                         DFS1080
      BAC = 0.0                                                         DFS1085
      HAT = 0.0                                                         DFS1090
      HPCT = 0.0                                                        DFS1095
      HS = 0.0                                                          DFS1100
      AGE = 0.0                                                         DFS1105
      ITHN = 0                                                          DFS1110
      AHARV = 0.0                                                       DFS1115
      XNUMR = 0.0                                                       DFS1120
      SGMORT = 0.0                                                      DFS1125
      SNMORT = 0.0                                                      DFS1130
      SVMORT = 0.0                                                      DFS1135
      PCTA = 0.0                                                        DFS1140
      FCTA = 0.0                                                        DFS1145
      ISWITA = 0                                                        DFS1150
      ISWITB = 0                                                        DFS1155
      IQ = 0                                                            DFS1160
      IZ = 0                                                            DFS1165
      DO 20 II=1,76                                                     DFS1170
      RAN(II) = 0.0                                                     DFS1175
   20 CONTINUE                                                          DFS1180
      DO 40 II=1,16                                                     DFS1185
      ARG(II) = 0.0                                                     DFS1190
      TTN(II) = 0.0                                                     DFS1195
      LIUT(II) = 0                                                      DFS1200
      GRESD(II) = 0.0                                                   DFS1205
      DR(II) = 0.0                                                      DFS1210
      TARESD(II) = 0.0                                                  DFS1215
      H40(II) = 0.0                                                     DFS1220
      HY40(II) = 0.0                                                    DFS1225
      AGET(II) = 0.0                                                    DFS1230
      AGEF(II) = 0.0                                                    DFS1235
      FNLBS(II) = 0.0                                                   DFS1240
      PNITF(II) = 0.0                                                   DFS1245
      PNIT(II) = 0.0                                                    DFS1250
      D56(II) = 0.0                                                     DFS1255
      D76(II) = 0.0                                                     DFS1260
      DC56(II) = 0.0                                                    DFS1265
      DC76(II) = 0.0                                                    DFS1270
      G56(II) = 0.0                                                     DFS1275
      G76(II) = 0.0                                                     DFS1280
      GC56(II) = 0.0                                                    DFS1285
      GC76(II) = 0.0                                                    DFS1290
      VA56(II) = 0.0                                                    DFS1295
      VA456(II) = 0.0                                                   DFS1300
      VA76(II) = 0.0                                                    DFS1305
      VA476(II) = 0.0                                                   DFS1310
      VAA56(II) = 0.0                                                   DFS1315
      VAA456(II) = 0.0                                                  DFS1320
      VAA76(II) = 0.0                                                   DFS1325
      VAA476(II) = 0.0                                                  DFS1330
      VAC56(II) = 0.0                                                   DFS1335
      VAC456(II) = 0.0                                                  DFS1340
      VAC76(II) = 0.0                                                   DFS1345
      VA476(II) = 0.0                                                   DFS1350
      SVC456(II) = 0.0                                                  DFS1355
      SVC476(II) = 0.0                                                  DFS1360
      DO 30 JJ=1,4                                                      DFS1365
      VA(II,JJ) = 0.0                                                   DFS1370
      GA(II,JJ) = 0.0                                                   DFS1375
      D(II,JJ) = 0.0                                                    DFS1380
      TA(II,JJ) = 0.0                                                   DFS1385
   30 CONTINUE                                                          DFS1390
   40 CONTINUE                                                          DFS1395
C  EEEEEEEEEE
C  INITIALIZE ECON VARIABLES
      TYHV=0
      IFER=0
      IMORT=0
      LOGC1=0.0
      LOGC2=0.0
      MAH=0.0
      CUMVOL=0.0
      PNW1=0.0
      PNW2=0.0
      SEV=0.0
      TRUED=0.0
       DMORT=0.0
       GMORT=0.0
       TAMORT=0.0
       VMORTE=0.0
       FALL=0.0
       DNR=0.0
       VDEAD=0.0
       CVDEAD=0.0
       VOLUME=0.0
       CUMVOL=0.0
       CMORT=0.0
      DO 42 I=1,16
      DO 42 J=1,4
   42 MORTE(I,J)=0.0
       PVI=0.0
       RCI=0.0
       PCTCI=0.0
       FCI=0.0
       LCI=0.0
       FHRCI=0.0
       OAHCI=0.0
       OVHCI=0.0
       HCI=0.0
      DO 36 I=1,8
      DO 36 J=1,2
   36 PONDVL(I,J)=0.0
      DO 39 I=1,12
      DO 38 J=1,12
   38 LOGCI(I,J)=0.0
      LOGCF(I,J)=0.0
   39 LOGVI(I)=0.0
      LOGVF(I)=0.0
      LOGDI(I)=0.0
      LOGDF(I)=0.0
       DO 44 I=1,16
        DO 44 J=1,2
         FAGE(I,J)=0.0
   44    CNIT(I,J)=0.0
C  EEEEEEEEEE
C
      CALL READIN                                                       DFS1400
C  EEEEEEEEEE
C  INITIALIZE CUMULATIVE PRESENT NET WORTH
      IF (ISTOP.EQ.4) GO TO 90
      IF (EXIST.EQ.1) GO TO 45
      PNW2=-REGENC*(1+RCI)**50
      PNW1=-REGENC
   45 CONTINUE
C  EEEEEEEEEE
      IF (ISTOP.EQ.2) GO TO 90                                          DFS1405
      CALL HEADER                                                       DFS1410
      IF (ISTOP.EQ.1) GO TO 90                                          DFS1415
      IF (ISTOP.EQ.3) GO TO 10                                          DFS1420
      IF (SDIA.LE.5.55) GO TO 50                                        DFS1425
C                                                                       DFS1430
C     COMPUTE IMPORTANT HEIGHTS FOR EXISTING STAND WITH QUADRATIC       DFS1435
C     MEAN STAND DIAMETER GREATER THAN 5.55-INCHES DBH                  DFS1440
C                                                                       DFS1445
      CALL HTCAL                                                        DFS1450
      GO TO 60                                                          DFS1455
C                                                                       DFS1460
   50 CONTINUE                                                          DFS1465
C                                                                       DFS1470
C     GROW STAND TO 5.55-INCHES DBH                                     DFS1475
C                                                                       DFS1480
      CALL JUVGRO                                                       DFS1485
      IF (AGE.GE.AHARV.AND.AHARV.GT.0.0) IUT = 1                        DFS1490
      IF (HS.GE.HTHARV.AND.HTHARV.GT.0.0) IUT = 1                       DFS1495
C                                                                       DFS1500
C     COMPUTE BEGINNING POINT FOR RELATIVE DENSITY CONTROL              DFS1505
C                                                                       DFS1510
   60 CONTINUE                                                          DFS1515
      ASYMP = BAPA/SQRT(DNOW)*0.90                                      DFS1520
      IF (ASYMP.LT.rdasymp) ASYMP = rdasymp                             DFS1525
C                                                                       DFS1530
C     GROW STAND FROM 5.5 INCHES TO FIRST COMMERCIAL THINNING.          DFS1535
C                                                                       DFS1540
      IF (IUT.GE.1) GO TO 70                                            DFS1545
      CALL FRSTCT                                                       DFS1550
      IF (AGE.GE.AHARV.AND.AHARV.GT.0.0) GO TO 80                       DFS1555
      IF (HS.GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 80                      DFS1560
C                                                                       DFS1565
C      GROW STAN BETWEEN COMMERCIAL THINNINGS                           DFS1570
C                                                                       DFS1575
      CALL COMTHN                                                       DFS1580
      GO TO 80                                                          DFS1585
C                                                                       DFS1590
   70 CONTINUE                                                          DFS1595
C                                                                       DFS1600
C      GROW STAND TO HARVEST CUT WHEN NO THINNING OPTION USED           DFS1605
C                                                                       DFS1610
      CALL NOTHIN                                                       DFS1615
C                                                                       DFS1620
C     WRITE HARVEST CUT                                                 DFS1625
C                                                                       DFS1630
   80 CONTINUE                                                          DFS1635
      IF (IZ.LE.0) IZ = 0                                               DFS1640
      CALL HARVST                                                       DFS1645
c      print 10000, imv,eco,eunits
c10000 format (3i5/)
      IF (IMV.LE.0) GO TO 10                                            DFS1650
C                                                                       DFS1655
C     PRODUCE 5.6- AND 7.6-INCH + DBH YIELD TABLES                      DFS1660
C                                                                       DFS1665
      CALL VOLCON                                                       DFS1670
      GO TO 10                                                          DFS1675
C                                                                       DFS1680
   90 STOP                                                              DFS1685
      END                                                               DFS1690-
