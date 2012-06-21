      SUBROUTINE HEADER                                                 HDR   5
C                                                                       HDR  10
C     COMPUTE CONDITIONS IMPLIED BY CONTROL DECK AND PRINT HEADINGS     HDR  15
C     FOR 1.6-INCH YIELD TABLE                                          HDR  20
C                                                                       HDR  25
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16HDR  30
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), HDR  35
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     HDR  40
     $ TARESD(16), TA(16,4), VA(16,4)                                   HDR  45
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), HDR  50
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     HDR  55
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  HDR  60
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               HDR  65
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,HDR  70
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,HDR  75
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,HDR  80
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, HDR  85
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   HDR  90
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          HDR  95
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, HDR 100
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 HDR 105
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMHDR 110
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDHDR 115
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            HDR 120
      common year,month,day,hour,minute,second,i100th
      integer*2 year,month,day,hour,minute,second,i100th
      data ix /0/
      if (ix.eq.1) WRITE (6,10)                                         HDR 125
   10 FORMAT (1H1)                                                      HDR 130
      ix = 1
C                                                                       HDR 135
C     HEADING WRITE STATEMENT FOR STANDS 1.6 INCHES PLUS.               HDR 140
C     STANDS 1.6 INCHES PLUS                                            HDR 145
C                                                                       HDR 150
      WRITE (6,20) month,day,year,hour,minute,second,
     $ VERS,IPAGE,CTITLE,rdasymp/0.94,SI                                HDR 155
   20 FORMAT (68x,i2.2,1h-,i2.2,1h-,i4.4,/69x,i2.2,1h:,i2.2,1h:,i2.2/
     $ 29X,20HD F S I M   VERSION ,a8,12X,6H PAGE ,I2,
     $/1X,A/,30x,                                                       HDR 156
     $20H MANAGED YIELD TABLE/32X,16H FOR DOUGLAS-FIR/32X,16H 1.6 INCHESHDR 165
     $ PLUS/25x,31h ASYMPTOTIC RELATIVE DENSITY = ,F5.1//25X,14H SITE INHDR 166
     $DEX = ,F5.0,14H (50 YEARS BH)/)                                   HDR 170
      LINE = 8                                                          HDR 175
      IPAGE = IPAGE+1                                                   HDR 180
      IF (IHEAD.NE.0) GO TO 660                                         HDR 185
      IHEAD = 1                                                         HDR 190
      X = 0.0                                                           HDR 195
C                                                                       HDR 200
C     WRITE CONTROL CARD INITIALIZATION CONDITIONS                      HDR 205
C                                                                       HDR 210
      IF (IORG.GT.0) GO TO 40                                           HDR 215
      WRITE (6,30)                                                      HDR 220
   30 FORMAT (16X,25HSTAND ORIGIN --- NATURAL.)                         HDR 225
      LINE = LINE+1                                                     HDR 230
      GO TO 60                                                          HDR 235
C                                                                       HDR 240
   40 CONTINUE                                                          HDR 245
      X = TAST                                                          HDR 255
      IF (PCT.GT.0.0.AND.SAGE.GT.0.0.AND.PCT.GT.SAGE) X = STA           HDR 260
      WRITE (6,50) X                                                    HDR 265
   50 FORMAT (16X,28HSTAND ORIGIN --- PLANTED TO ,F5.0,16H TREES PER ACRHDR 270
     $E.)                                                               HDR 275
      LINE = LINE+1                                                     HDR 280
   60 CONTINUE                                                          HDR 285
      IF (PCTA.EQ.0.0) GO TO 80                                         HDR 290
      IF (SAGE.GT.0.0.AND.SAGE.GE.PCT.OR.SAGE.LE.0.0) ITHN = 1          HDR 300
      X = TAST                                                          HDR 305
      IF (SAGE.GT.0.0.AND.SAGE.EQ.PCT) X = STA                          HDR 310
      WRITE (6,70) PCT,X                                                HDR 315
   70 FORMAT (16X,44HSTAND WILL BE PRECOMMERCIALLY THINNED AT AGE,F5.0, HDR 320
     $4H TO ,F5.0,7H TREES /16X,9HPER ACRE.)                            HDR 325
      LINE = LINE+2                                                     HDR 330
   80 CONTINUE                                                          HDR 335
      IF (IUT.GT.0) GO TO 250                                           HDR 340
      IF (FCT.LE.0.0) GO TO 100                                         HDR 345
      WRITE (6,90) FCT                                                  HDR 350
   90 FORMAT (16X,42HFIRST COMMERCIAL THINNING IS WANTED AT AGE,F5.0)   HDR 355
      LINE = LINE+1                                                     HDR 360
  100 CONTINUE                                                          HDR 365
      IF (TI.LE.0.0) GO TO 120
      WRITE (6,110) TI                                                  HDR 375
  110 FORMAT (16X,24HTHE THINNING INTERVAL IS,F5.0,7H YEARS.)           HDR 380
      LINE = LINE+1                                                     HDR 385
  120 CONTINUE                                                          HDR 390
      IF (DH.LE.0.0) GO TO 140
      WRITE (6,130) DH                                                  HDR 425
  130 FORMAT (16X,24HTHE THINNING INTERVAL IS,F5.0,23H FEET OF HEIGHT GRHDR 430
     $OWTH.)                                                            HDR 435
      LINE = LINE+1                                                     HDR 440
  140 CONTINUE                                                          HDR 445
      IF (NCT.LE.0) GO TO 160                                           HDR 450
      WRITE (6,150) NCT                                                 HDR 455
  150 FORMAT (16X,47HTHE NUMBER OF COMMERCIAL THINNINGS SPECIFIED IS,I3,HDR 460
     $1H.)                                                              HDR 465
      LINE = LINE+1                                                     HDR 470
  160 CONTINUE                                                          HDR 475
      IF (NCT.LE.0) NCT = 15                                            HDR 480
      IF (CTAS.NE.1) GO TO 180                                          HDR 485
      WRITE (6,170) (AGET(II),II=1,NCT)                                 HDR 490
  170 FORMAT (16X,57HCOMMERCIAL THINNINGS ARE SCHEDULED AT THE FOLLOWINGHDR 495
     $ AGES:/16X,10F6.0/16X,10F6.0)                                     HDR 500
      LINE = LINE+3                                                     HDR 505
  180 CONTINUE                                                          HDR 510
      IF (SCTHT.NE.1) GO TO 200                                         HDR 515
      WRITE (6,190) (HY40(II),II=1,NCT)                                 HDR 520
  190 FORMAT (16X,56HCOMMERCIAL THINNINGS WILL BE MADE WHEN THE STAND ATHDR 525
     $TAINS/16X,22HTHE FOLLOWING HEIGHTS:/16X,10F6.0/16X,10F6.0)        HDR 530
      LINE = LINE+5                                                     HDR 535
  200 CONTINUE                                                          HDR 540
      IF (FIY.LE.0.0) GO TO 220                                         HDR 545
      WRITE (6,210) FIY                                                 HDR 550
  210 FORMAT (16X,55HTHE FINAL INTERVAL BETWEEN THE LAST COMMERCIAL THINHDR 555
     $NING/16X,22HAND THE HARVEST CUT IS,F5.0,7H YEARS.)                HDR 560
      LINE = LINE+2                                                     HDR 565
  220 CONTINUE                                                          HDR 570
      IF (FIH.LE.0.0) GO TO 240                                         HDR 575
      WRITE (6,230) FIH                                                 HDR 580
  230 FORMAT (16X,55HTHE FINAL INTERVAL BETWEEN THE LAST COMMERCIAL THINHDR 585
     $NING/16X,22HAND THE HARVEST CUT IS,F6.1,23H FEET OF HEIGHT GROWTH.HDR 590
     $)                                                                 HDR 595
      LINE = LINE+2                                                     HDR 600
  240 CONTINUE                                                          HDR 605
  250 CONTINUE                                                          HDR 610
      IF (AHARR.GT.0.0.OR.HTHARV.GT.0.0) GO TO 280                      HDR 615
      IF (FCT.LE.0.0.AND.HY40(1).LE.0.0) GO TO 260                      HDR 620
      IF (TI.GT.0.0.AND.FIY.GT.0.0) AHARR = NCT*TI+FCT+FIY              HDR 625
      IF (TI.GT.0.0.AND.FIY.LE.0.0) AHARR = (NCT+1.0)*TI+FCT            HDR 630
      IF (AHARR.GT.0.0) GO TO 270                                       HDR 635
      HFCT = 0.0                                                        HDR 640
      IF (FCTA.GT.0.0) HFCT = HEIGHT(SI,FCTA,IOBS,OBSAGE,OBSHTS)        HDR 645
      IF (HY40(1).GT.HFCT) HFCT = HY40(1)                               HDR 650
      IF (DH.GT.0.0.AND.FIH.GT.0.0) HTHARV = NCT*DH+HFCT+FIH            HDR 655
      IF (DH.GT.0.0.AND.FIH.LE.0.0) HTHARV = (NCT+1.0)*DH+HFCT          HDR 660
      IF (HTHARV.GT.0.0) GO TO 280                                      HDR 665
  260 CONTINUE                                                          HDR 670
      AHARR = 100.0                                                     HDR 675
  270 CONTINUE                                                          HDR 680
      AHARV = BHAGE(AHARR,SI)                                           HDR 685
  280 CONTINUE                                                          HDR 690
      IF (AHARR.LE.0.0) GO TO 310                                       HDR 695
      WRITE (6,290) AHARR                                               HDR 700
  290 FORMAT (16X,39HTHE SCHEDULED AGE AT THE HARVEST CUT IS,F5.0)      HDR 705
      LINE = LINE+1                                                     HDR 710
      IF (AHARR.LE.100.0) GO TO 310                                     HDR 715
      WRITE (6,300)                                                     HDR 720
  300 FORMAT (/1X,39(2H**)/79H WARNING -- STAND STATISTICS BEYOND 100 YEHDR 725
     $ARS ARE GROSS EXTRAPOLATIONS OF MODEL/1X,39(2H**)/)               HDR 730
      LINE = LINE+4                                                     HDR 735
  310 CONTINUE                                                          HDR 740
      IF (HTHARV.LE.0.0) GO TO 340                                      HDR 745
      WRITE (6,320) HTHARV                                              HDR 750
  320 FORMAT (16X,42HTHE SCHEDULED HEIGHT AT THE HARVEST CUT IS,F6.0,   HDR 755
     $6H FEET.)                                                         HDR 760
      LINE = LINE+1                                                     HDR 765
      BHA = BHAGE(100.0,SI)                                             HDR 770
      HARVH = HEIGHT(SI,BHA,IOBS,OBSAGE,OBSHTS)                         HDR 775
      IF (HTHARV.LE.HARVH) GO TO 340                                    HDR 780
      WRITE (6,330) HARVH,SI                                            HDR 785
  330 FORMAT (/1X,39(2H**)/35H WARNING -- STAND STATISTICS BEYOND,F6.1, HDR 790
     $20H FEET FOR SITE INDEX,F5.0,10H ARE GROSS/25H EXTRAPOLATIONS OF MHDR 795
     $ODEL./1X,39(2H**)/)                                               HDR 800
      LINE = LINE+5                                                     HDR 805
  340 CONTINUE                                                          HDR 810
      IF (LTREE.EQ.1.AND.IGS.EQ.1) GO TO 370                            HDR 815
      IF (IDR.LE.0.OR.IUT.GE.1) GO TO 370                               HDR 820
      DO 350 I=1,NCT                                                    HDR 825
      IF (I.EQ.1) GO TO 350                                             HDR 830
      IF (DR(I).LE.0.0) DR(I) = DR(I-1)                                 HDR 835
  350 CONTINUE                                                          HDR 840
      WRITE (6,360) (DR(II),II=1,NCT)                                   HDR 845
  360 FORMAT (16X,59HCOMMERCIAL THINNING WILL BE DONE ACCORDING TO THE FHDR 850
     $OLLOWING/16X,52HUSER SUPPLIED DIAMETER CUT / DIAMETER BEFORE RATIOHDR 855
     $S:/16X,10F6.2/16X,10F6.2)                                         HDR 860
      LINE = LINE+4+(NCT-1)/10                                          HDR 865
  370 CONTINUE                                                          HDR 870
      IF (IGS.LE.0.OR.IUT.GE.1) GO TO 400                               HDR 875
      DO 380 I=1,NCT                                                    HDR 880
      IF (I.EQ.1) GO TO 380                                             HDR 885
      IF (GRESD(I).LE.0.0) GRESD(I) = GRESD(I-1)                        HDR 890
  380 CONTINUE                                                          HDR 895
      WRITE (6,390) (GRESD(II),II=1,NCT)                                HDR 900
  390 FORMAT (16X,59HCOMMERCIAL THINNING WILL BE DONE ACCORDING TO THE FHDR 905
     $OLLOWING/16X,36HUSER SUPPLIED RESIDUAL BASAL AREAS: /16X,10F6.0/  HDR 910
     $16X,10F6.0)                                                       HDR 915
      LINE = LINE+4+(NCT-1)/10                                          HDR 920
  400 CONTINUE                                                          HDR 925
      IF (LTREE.LE.0.OR.IUT.GE.1) GO TO 420                             HDR 930
      WRITE (6,410) (TARESD(I),I=1,NCT)                                 HDR 935
  410 FORMAT (16X,59HCOMMERCIAL THINNING WILL BE DONE ACCORDING TO THE FHDR 940
     $OLLOWING/16X,39HUSER SUPPLIED RESIDUAL NUMBER OF TREES:/16X,10F6.0HDR 945
     $/16X,10F6.0)                                                      HDR 950
      LINE = LINE+4+(NCT-1)/10                                          HDR 955
  420 CONTINUE                                                          HDR 960
      IF (FERTCT.LE.0) GO TO 470                                        HDR 965
      IF (IUT.GT.0) GO TO 440                                           HDR 970
      WRITE (6,430)                                                     HDR 975
  430 FORMAT (16X,52HTHE STAND WILL BE FERTILIZED AT COMMERCIAL THINNINGHDR 980
     $S/16X,47HWITH THE FOLLOWING POUNDS OF NITROGEN PER ACRE://20X,    HDR 985
     $32HTHINNING      POUNDS OF NITROGEN)                              HDR 990
      LINE = LINE+4                                                     HDR 995
  440 CONTINUE                                                          HDR1000
      M = 0                                                             HDR1001
      DO 460 II=1,NCT                                                   HDR1005
      IF (AGET(II).GT.0.0) AGEF(II) = AGET(II)                          HDR1010
      WRITE (6,450) II,PNIT(II)                                         HDR1015
  450 FORMAT (23X,I2,15X,F4.0)                                          HDR1020
      IF(PNIT(II).GT.0.0) M = M + 1                                     HDR1021
      LINE = LINE+1                                                     HDR1025
  460 CONTINUE                                                          HDR1030
      IF(M.GT.3) WRITE(6,445)                                           HDR1031
  445 FORMAT(1X,39(2H**)/79H WARNING -- FREQUENT REPEATED FERTILIZATIONSHDR1032
     1 ARE GROSS EXTRAPOLATIONS OF MODEL./1X,39(2H**))                  HDR1033
      LINE = LINE + 3                                                   HDR1034
  470 CONTINUE                                                          HDR1035
      IF (FERTAS.LE.0) GO TO 510                                        HDR1040
      WRITE (6,480)                                                     HDR1045
  480 FORMAT (16X,40HTHE STAND WILL BE FERTILIZED AS FOLLOWS://20X,     HDR1050
     $33HSTAND AGE      POUNDS OF NITROGEN)                             HDR1055
      LINE = LINE+3                                                     HDR1060
      M = 0                                                             HDR1061
      DO 500 II=1,15                                                    HDR1065
      IF (AGEF(II).LE.0.0.OR.PNIT(II).LE.0.0) GO TO 500                 HDR1070
      WRITE (6,490) AGEF(II),PNIT(II)                                   HDR1075
  490 FORMAT (23X,F4.0,14X,F4.0)                                        HDR1080
      M = M + 1                                                         HDR1081
      LINE = LINE+1                                                     HDR1085
  500 CONTINUE                                                          HDR1090
      IF(M.GT.3) WRITE(6,445)                                           HDR1091
      LINE = LINE + 3                                                   HDR1092
  510 CONTINUE                                                          HDR1095
      IF (NRA.LE.0.0) GO TO 530                                         HDR1100
      WRITE (6,520)                                                     HDR1105
  520 FORMAT (16X,63HTHE OUTPUT TABLE WILL HAVE REPORT AGES OTHER THAN CHDR1110
     $UTTING AGES.)                                                     HDR1115
      LINE = LINE+1                                                     HDR1120
  530 CONTINUE                                                          HDR1125
      IF (GCLIM.LE.0.0) GCLIM = 20.0                                    HDR1130
      IF (DLIM.LE.0.0) DLIM = 8.00                                      HDR1135
C                                                                       HDR1140
C     COMPUTE MINIMUM QUADRATIC MEAN STAND DIAMETER FOR FIRST           HDR1145
C     COMMERCIAL THINNING                                               HDR1150
C                                                                       HDR1155
      DC = DLIM/1.05                                                    HDR1160
      IF (FCT.LE.0.0.AND.IDR.LE.0.AND.AGET(1).LE.0.0.AND.HY40(1).LE.0.0)HDR1165
     $ DC = 9.0                                                         HDR1170
      IF (IORG.EQ.1.OR.PCTA.GT.0.0) DC = DLIM                           HDR1175
      IF (DR(1).GE.0.8.AND.DR(1).LE.1.15.OR.IDR.EQ.2) DC = DLIM/DR(1)   HDR1180
      IF (GLIM.LE.0.0) GLIM = 100.0                                     HDR1185
      IF (IUT.GT.0) GO TO 570                                           HDR1190
      WRITE (6,540) DLIM                                                HDR1195
  540 FORMAT (16X,61HTHE AVERAGE DIAMETER OF ALL CUT TREES AT COMMERCIALHDR1200
     $ THINNINGS/16X,16HMUST BE AT LEAST,F6.2,8H INCHES.)               HDR1205
      IF (IGS.LE.0) WRITE (6,550) GCLIM                                 HDR1210
  550 FORMAT (16X,55HTHE BASAL AREA CUT AT EACH COMMERCIAL THINNING MUSTHDR1215
     $ BE /16X,9HAT LEAST ,F3.0,22H SQUARE FEET PER ACRE.)              HDR1220
      WRITE (6,560) GLIM                                                HDR1225
  560 FORMAT (16X,60HTHE BASAL AREA PER ACRE OF ALL TREES 5.6 INCHES PLUHDR1230
     $S MUST BE/16X,8HAT LEAST,F7.1,40H SQUARE FEET BEFORE THE FIRST COMHDR1235
     $MERCIAL/16X,19HTHINNING CAN OCCUR.)                               HDR1240
      LINE = LINE+8                                                     HDR1245
      IF (IGS.GE.1) LINE = LINE-2                                       HDR1250
  570 CONTINUE                                                          HDR1255
      IF (SAGE.LE.0.0) GO TO 590                                        HDR1260
      WRITE (6,580) SAGE,STA,SBA,SDIA                                   HDR1265
  580 FORMAT (16X,40HEXISTING STAND STATISTICS SPECIFIED ARE:/20X,      HDR1270
     $10HTOTAL AGE=,F5.0/20X,15HTREES PER ACRE=,F7.0/20X,20HBASAL AREA PHDR1275
     $ER ACRE=,F7.1/20X,24HQUADRATIC MEAN DIAMETER=,F6.2)               HDR1280
      LINE = LINE+5                                                     HDR1285
  590 CONTINUE                                                          HDR1290
      IF (IUT.LE.0) GO TO 610                                           HDR1295
      WRITE (6,600)                                                     HDR1300
  600 FORMAT (16X,34HNO COMMERCIAL THINNING TO BE DONE.)                HDR1305
      LINE = LINE+1                                                     HDR1310
  610 CONTINUE                                                          HDR1315
      IF (ISTOP.EQ.1) GO TO 690                                         HDR1320
      IF (IOBS.EQ.1.OR.IOBS.GT.30) GO TO 690                            HDR1325
      IF (IOBS.LE.0) GO TO 630                                          HDR1330
      WRITE (6,620) (OBSAGE(I),OBSHTS(1,I),OBSHTS(2,I),I=2,IOBS)        HDR1335
  620 FORMAT (/16X,39HUSER SPECIFIED OBSERVED HEIGHTS FOLLOW://2X,4(4X, HDR1340
     $14HOBS   HT    HT)/4X,4(2X,16HAGE BEFORE AFTER)//8(4X,12F6.1/))   HDR1345
      LINE = LINE+5+IOBS/4                                              HDR1350
  630 CONTINUE                                                          HDR1355
      WRITE (6,640)                                                     HDR1360
  640 FORMAT (//)                                                       HDR1365
      LINE = LINE+2                                                     HDR1370
      IF (IOBS.LE.0) GO TO 660                                          HDR1375
      DO 650 I=2,IOBS                                                   HDR1380
      OBSAGE(I) = BHAGE(OBSAGE(I),SI)                                   HDR1385
      IF (OBSHTS(2,I).LE.0.0) OBSHTS(2,I) = OBSHTS(1,I)                 HDR1390
  650 CONTINUE                                                          HDR1395
      OBSAGE(1) = 0.0                                                   HDR1396
      OBSHTS(1,1) = 4.5                                                 HDR1397
      OBSHTS(2,1) = 4.5                                                 HDR1398
  660 CONTINUE                                                          HDR1400
      WRITE (6,670)                                                     HDR1405
C2345678901234567890123456789012345678901234567890123456789012345678901234567890
  670 FORMAT(74H TOT  BH                BASAL  TREES   CVTS   CAI  *MAI HDR1410
     1 CVTS* **MAI CV4**/74H AGE AGE   HT40    DBH  AREA/A  PER     PER HDR1415
     2  NET  GROSS  NET  ****NET****/73H YRS YRS   FEET   INCH  SQ FT   HDR1420
     3ACRE   ACRE  CVTS  1.6+  1.6+  5.6+  7.6+/)                       HDR1425
C2345678901234567890123456789012345678901234567890123456789012345678901234567890
      LINE = LINE+4                                                     HDR1430
      IF (X.GE.300.0.OR.TAST.LE.0.0.OR.IPAGE.GT.2) RETURN               HDR1435
      WRITE (6,680)                                                     HDR1440
  680 FORMAT (1X,39(2H**)/76H WARNING -- STANDS PLANTED OR PRECOMMERCIALHDR1445
     $LY THINNED TO LESS THAN 300 STEMS/51H PER ACRE ARE QUESTIONABLE EXHDR1450
     $TRAPOLATIONS OF MODEL./1X,39(2H**))                               HDR1455
      LINE = LINE+4                                                     HDR1460
      RETURN                                                            HDR1465
  690 CONTINUE                                                          HDR1470
      IF (IOBS.EQ.2) WRITE (6,700)                                      HDR1475
  700 FORMAT (//74H A MINIMUM OF TWO OBSERVATION PERIODS REQUIRED FOR USHDR1480
     $ER SPECIFIED HEIGHTS./19H CORRECT AND RERUN.)                     HDR1485
      IF (IOBS.GT.31) WRITE (6,710)                                     HDR1490
  710 FORMAT (//75H A MAXIMUN OF 30 OBSERVATION PERIODS IS ALLOWED FOR UHDR1495
     $SER SPECIFIED HEIGHTS./19H CORRECT AND RERUN.)                    HDR1500
      IF (IOBS.EQ.1.OR.IOBS.GT.30) RETURN                               HDR1505
      ISTOP = 3                                                         HDR1510
      WRITE (6,720)                                                     HDR1515
  720 FORMAT (//73H STANDS PLANTED OR PRECOMMERCIALLY THINNED TO LESS THHDR1520
     $AN 75 STEMS PER ACRE/47H CANNOT BE SIMULATED. YIELD TABLE NOT PRODHDR1525
     $UCED.//)                                                          HDR1530
      RETURN                                                            HDR1535
      END                                                               HDR1540-
      SUBROUTINE XFERT(AGENOW)                                          XFR   5
C                                                                       XFR  10
C     NITROGEN FERTILIZER EFFECT ROUTINE                                XFR  15
C                                                                       XFR  20
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16XFR  25
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), XFR  30
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     XFR  35
     $ TARESD(16), TA(16,4), VA(16,4)                                   XFR  40
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), XFR  45
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     XFR  50
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  XFR  55
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               XFR  60
      DIMENSION AGEFF(16)                                               XFR  65
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,XFR  70
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,XFR  75
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,XFR  80
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, XFR  85
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   XFR  90
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          XFR  95
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, XFR 100
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 XFR 105
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMXFR 110
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDXFR 115
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      COMMON /ECONO/ A,B,C,COSTAB,ECO,EUNITS,FC,FCI,FHRC,FHRCI,
     $HAULC1,HC,HCI,IFER,
     $LCI,LCM,LDF,LDI,LOGC1,LOGCF(12,12),LOGCI(12,12),
     $LOGDF(12),LOGDI(12),LOGVF(12),LOGVI(12),LVF,LVI,
     $MAH,NETRV1,OAHC,OAHCI,OATC,OTHRC1,OVHC,OVHCI,
     $OVTC,PCTC,PCTCI,PNW1,PNW2,PNDVL1,PONDVL(8,2),PONTAB,
     $PVI,R,RCI,REGENC,SEV,TRUED,TYHV,VOLM,FAGE(15,2),CNIT(15,2)
     $ ,DNR,VDEAD,CVDEAD,VOLUME,CUMVOL,FALL,IMORT,CMORT
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            XFR 120
C                                                                       XFR 125
C     ************ DEFINITION OF IMPORTANT XFERT VARIABLES ************ XFR 130
C                                                                       XFR 135
C  EEEEEEEEEE
C  SEE SUBROUTINE ECON FOR DEFINITION OF IMPORTANT ECON VARIABLES
C  EEEEEEEEEE
C     AGECK  = TOTAL STAND AGE AT MIDPOINT OF GROWTH PERIOD             XFR 140
C     AGEDIF = ELAPSED TIME IN YEARS FOR MOST RECENT NITROGEN           XFR 145
C                   FERTILIZER APPLICATION                              XFR 150
C     AGEF   = ARRAY OF STAND AGES FOR FERTILIZER APPLICATIONS IN       XFR 155
C                   ASCENDING ORDER                                     XFR 160
C     AGEFF  = ARRAY OF STAND AGES FOR FERTILIZER APPLICATIONS IN       XFR 165
C                   DESCENDING ORDER                                    XFR 170
C     AGENOW = BREAST HEIGHT STAND AGE AT MIDPOINT OF GROWTH PERIOD     XFR 175
C     FERTEF = EFFECT OF NITROGEN FERTILIZER FOR                        XFR 180
C                   MOST RECENT APPLICATION                             XFR 185
C     FERTXX = VARIABLE FOR FERTILIZER EFFECT OF                        XFR 190
C                   MOST RECENT APPLICATION                             XFR 195
C     PNIT   = ARRAY OF EFFECTIVE NITROGEN DOSAGES IN POUNDS PER ACRE   XFR 200
C                   FOR EACH FERTILIZER APPLICATION IN ASCENDING ORDER  XFR 205
C     PNITF  = ARRAY OF EFFECTIVE NITROGEN DOSAGES IN POUNDS PER ACRE   XFR 210
C                   FOR EACH FERTILIZER APPLICATION IN DESCENDING ORDER XFR 215
C     SI     = SITE INDEX (50-YEAR B H BASE AGE)                        XFR 220
C     ***************************************************************** XFR 225
C                                                                       XFR 230
      IF (FERTCT.LE.0.AND.FERTAS.LE.0) GO TO 40                         XFR 235
      DO 10 II=1,15                                                     XFR 240
      AGEFF(16-II) = AGEF(II)                                           XFR 245
      PNITF(16-II) = PNIT(II)                                           XFR 250
   10 CONTINUE                                                          XFR 255
      FERTXX = 0.0                                                      XFR 260
      AGECK = TOAGE(AGENOW,SI)                                          XFR 265
      DO 20 II=1,15                                                     XFR 270
      IF (AGEFF(II).LE.0.0) GO TO 20                                    XFR 275
      IF (AGEFF(II).GT.AGECK) GO TO 20                                  XFR 280
      AGEDIF = AGECK-AGEFF(II)                                          XFR 285
C
C  EEEEEEEEEE
      IF (AGEDIF.GT.1.0) GO TO 35
C     ACCUMULATE AGES FOR FERT COST
      DO 15 J=1,15
         IF (FAGE(J,1).GT.0.0) GO TO 15
             FAGE(J,1)=AGEFF(II)
          FAGE(J,2)=PNITF(II)
             GO TO 17
   15 CONTINUE
   17 CONTINUE
C  EEEEEEEEEE
C
   35 CONTINUE
      FERT = (PNITF(II)*(AGEDIF*EXP(-AGEDIF/(4.7188-.01667*SI)))**3)    XFR 290
      IF (FERT.GT.FERTXX) FERTXX = FERT                                 XFR 295
C                                                                       XFR 300
   20 CONTINUE                                                          XFR 305
   30 CONTINUE                                                          XFR 310
      if (fertxx.le.1.0e-10) fertxx = 0.0
      IF (FERTXX.EQ.0.0) GO TO 40                                       XFR 315
      FERTEF = (ALOG(FERTXX+1.))**2/SI                                  XFR 320
      GO TO 50                                                          XFR 325
C                                                                       XFR 330
   40 CONTINUE                                                          XFR 335
      FERTEF = 0.0                                                      XFR 340
   50 CONTINUE                                                          XFR 345
      RETURN                                                            XFR 350
      END                                                               XFR 355-

      SUBROUTINE JUVGRO                                                 JUV   5
C                                                                       JUV  10
C     JUVENILE STAND GROWTH ROUTINE. GROW STAND TO 5.55-INCHES DBH.     JUV  15
C                                                                       JUV  20
C     IUIN IS INPUT FILE - TAPE5
C     IUOUT IS OUTPUT - TAPE6
C
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16JUV  25
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), JUV  30
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     JUV  35
     $ TARESD(16), TA(16,4), VA(16,4)                                   JUV  40
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), JUV  45
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     JUV  50
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  JUV  55
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               JUV  60
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,JUV  65
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,JUV  70
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,JUV  75
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, JUV  80
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   JUV  85
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          JUV  90
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, JUV  95
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 JUV 100
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMJUV 105
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDJUV 110
      COMMON /ECONO/ A,B,C,COSTAB,ECO,EUNITS,FC,FCI,FHRC,FHRCI,
     1 HAULC1,HC,HCI,IFER,
     2 LCI,LCM,LDF,LDI,LOGC1,LOGCF(12,12),LOGCI(12,12),
     3 LOGDF(12),LOGDI(12),LOGVF(12),LOGVI(12),LVF,LVI,
     4 MAH,NETRV1,OAHC,OAHCI,OATC,OTHRC1,OVHC,OVHCI,
     5 OVTC,PCTC,PCTCI,PNW1,PNW2,PNDVL1,PONDVL(8,2),PONTAB,
     6 PVI,R,RCI,REGENC,SEV,TRUED,TYHV,VOLM,FAGE(15,2),CNIT(15,2)
     $ ,DNR,VDEAD,CVDEAD,VOLUME,CUMVOL,FALL,IMORT,CMORT
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      REAL LOGC,LOGCI,LOGDI,LOGVI,LOGCF,LOGDF,LOGVF,LCI,LCM,LOGC1,
     1 LOGC2,MAH,NETRV1,NETRV2
      INTEGER COSTAB,PONTAB,ECO,EUNITS,TYHV
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            JUV 115
C                                                                       JUV 120
C     ************ DEFINITION OF IMPORTANT JUVGRO VARIABLES *********** JUV 125
C                                                                       JUV 130
C     AGE    = BREAST HEIGHT STAND AGE                                  JUV 135
C     AGEADD = LENGTH OF GROWTH PERIOD                                  JUV 140
C     AGECK  = TOTAL STAND AGE                                          JUV 145
C     AGEFF  = BREAST HEIGHT STAND AGE AT TIME OF FIRST NITROGEN        JUV 150
C                   FERTILIZER APPLICATION                              JUV 155
C     AGEMID = BREAST HEIGHT STAND AGE AT MIDPOINT OF GROWTH PERIOD     JUV 160
C     AF     = BREAST HEIGHT STAND AGE AT TIME OF FIRST                 JUV 165
C                   NITROGEN FERTILIZER APPLICATION                     JUV 170
C     AHARV  = BREAST HEIGHT STAND AGE AT HARVEST CUT                   JUV 175
C     BAPA   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            JUV 180
C     CAI    = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE NET CURRENT      JUV 185
C                   ANNUAL INCREMENT IN TREES 1.6-INCHES + DBH          JUV 190
C     D1     = QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH  JUV 195
C                   'NORMAL' STAND AT BEGINNING OF SIMULATION           JUV 200
C     DEODG  = QUADRATIC MEAN STAND DIAMETER OF 'NORMAL' STAND AT       JUV 205
C                   BEGINNING OF SIMULATION DIVIDED BY QUADRATIC STAND  JUV 210
C                   DIAMETER OF EXISTING STAND AT BEGINNING OF SIMULATIOJUV 215
C     DDH    = STAND TOP HEIGHT INCREMENT SINCE BEGINNING OF SIMULATION JUV 220
C     DNOW   = QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH  JUV 225
C     FERT   = NITROGEN FERTILIZER EFFECTIVE DOSAGE IN POUNDS PER ACRE  JUV 230
C                   AT TIME OF FIRST APPLICATION                        JUV 235
C     H1     = STAND TOP HEIGHT                                         JUV 240
C     H2     = STAND TOP HEIGHT 1 YEAR HENCE                            JUV 245
C     HAFF   = STAND TOP HEIGHT AT TIME OF FIRST NITROGEN FERTILIZER    JUV 250
C                   APPLICATION                                         JUV 255
C     HAT    = STAND TOP HEIGHT AFTER PRECOMMERCIAL THINNING            JUV 260
C     HPCT   = STAND TOP HEIGHT AFTER PRECOMMERCIAL THINNING            JUV 265
C     HR     = STAND TOP HEIGHT AT BEGINNING OF SIMULATION              JUV 270
C     HS     = STAND TOP HEIGHT PASSED TO DFSIM                         JUV 275
C     HTADD  = STAND TOP HEIGHT INCREMENT                               JUV 280
C     HTHARV = STAND TOP HEIGHT AT HARVEST CUT                          JUV 285
C     HTL    = LOREY?S HEIGHT                                           JUV 290
C     IORG   = STAND ORIGIN                                             JUV 295
C     IQ     = INDEX FOR REPORT AGES ARRAY                              JUV 300
C     ITHN   = INDICATOR SWITCH FOR THINNING DONE                       JUV 305
C     PCT    = TOTAL STAND AGE AT PRECOMMERCIAL THINNING                JUV 310
C     PCTA   = BREAST HEIGHT STAND AGE AT PRECOMMERCIAL THINNING        JUV 315
C     RD     = RELATIVE DENSITY                                         JUV 320
C     RDECAY = ADJUSTMENT FACTOR FOR 'NON-NORMAL' EXISTING STAND        JUV 325
C                   QUADRATIC MEAN STAND DIAMETER. ADJUSTS DIAMETERS    JUV 330
C                   TOWARD 'NORMAL' OVER A 40-FOOT HEIGHT GROWTH PERIOD JUV 335
C     SBA    = EXISTING STAND BASAL AREA PER ACRE IN TREES 1.6-INCHES + JUV 340
C                   DBH                                                 JUV 345
C     SDIA   = EXISTING STAND QUADRATIC MEAN STAND DIAMETER OF TREES    JUV 350
C                   1.6-INCHES + DBH                                    JUV 355
C     STA    = EXISTING STAND NUMBER OF TREES PER ACRE 1.6-INCHES + DBH JUV 360
C     VGR    = CUBIC-FOOT VOLUME (TOTAL STEM) / BASAL AREA RATIO        JUV 365
C     VNOW   = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES 1.6-    JUV 370
C                   INCHES + DBH                                        JUV 375
C     VSAVE  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES 1.6-    JUV 380
C                   INCHES + DBH 1 YEAR AGO                             JUV 385
C     XMAI16 = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE NET MEAN ANNUAL  JUV 390
C                   INCREMENT IN TREE 1.6-INCHES + DBH                  JUV 395
C     XN1    = NUMBER OF TREES PER ACRE AT STAND TOP HEIGHT H1          JUV 400
C     XN2    = NUMBER OF TREES PER ACRE AT STAND TOP HEIGHT H2          JUV 405
C     XNUM   = NUMBER OF TREES PER ACRE PASSED TO DFSIM                 JUV 410
C     XNUM1  = 'NORMAL' NUMBER OF TREES PER ACRE AT STAND TOP HEIGHT H1 JUV 415
C     XNUM2  = 'NORMAL' NUMBER OF TREES PER ACRE AT STAND TOP HEIGHT H2 JUV 420
C     XNUMR  = NUMBER OF ESTABLISHED SEEDLINGS PER ACRE AFTER PLANTING OJUV 425
C                   NUMBER OF RESIDUAL TREES PER ACRE AFTER PRECOMMERCIAJUV 430
C                   THINNING                                            JUV 435
C     ***************************************************************** JUV 440
C
C  EEEEEEEEEE
C  SEE SUBROUTINE ECON FOR DEFINITIONS OF IMPORTANT ECON VARIABLES
C  EEEEEEEEEE
      IUIN=3
      IUOUT=6
C                                                                       JUV 445
      H1 = 0.0                                                          JUV 450
      XN1 = STA                                                         JUV 455
      DNOW = SDIA                                                       JUV 460
      BAPA = SBA                                                        JUV 465
      VSAVE = 0.0                                                       JUV 470
      HF = 0.0                                                          JUV 475
      FERT = 0.0                                                        JUV 480
      AGEFF = BHAGE(AGEF(1),SI)                                         JUV 485
      IF (AGEFF.LE.0.0) AGEFF = 0.0                                     JUV 490
      AGE = BHAGE(SAGE,SI)                                              JUV 495
      IF (AGE.LE.0.0) AGE = 0.0                                         JUV 500
      IAGE = AGE+.5                                                     JUV 505
      AGE = IAGE                                                        JUV 510
      XN2 = STA                                                         JUV 515
      XNUM1 = STA                                                       JUV 520
      XNUM2 = STA                                                       JUV 525
      H2 = 0.0                                                          JUV 530
      HAT = 0.0                                                         JUV 535
      HAFF = 0.0                                                        JUV 540
      IF (AGE.LE.0.0) GO TO 240                                         JUV 545
      IF (AGEF(1).LE.0.0) GO TO 20                                      JUV 550
C                                                                       JUV 555
C     GROW FERTILIZED STAND FROM EARLIEST AGE TO CURRENT AGE            JUV 560
C                                                                       JUV 565
      EAGE = SAGE                                                       JUV 570
      IF (PCT.GT.0.0.AND.PCT.LT.EAGE) EAGE = PCT                        JUV 575
      IF (AGEF(1).GT.0.0.AND.AGEF(1).LT.EAGE) EAGE = AGEF(1)            JUV 580
      IF (EAGE.EQ.SAGE) GO TO 20                                        JUV 585
      IF (AGEFF.GE.AGE.AND.PCTA.LE.0.0) GO TO 230                       JUV 590
      AGEMID = BHAGE(EAGE,SI)                                           JUV 595
      H1 = HEIGHT(SI,AGEMID,IOBS,OBSAGE,OBSHTS)                         JUV 600
   10 CONTINUE                                                          JUV 605
      AGEMID = AGEMID+AGEADD/2.0                                        JUV 610
      CALL XFERT (AGEMID)                                               JUV 615
      AGEMID = AGEMID-AGEADD/2.0                                        JUV 620
      IF (PCTA.EQ.AGEMID) HAT = H1                                      JUV 625
      HTADD = HTGROW(SI,AGEMID,AGEADD,FERTEF,IOBS,OBSAGE,OBSHTS)        JUV 630
      H1 = H1+HTADD                                                     JUV 635
      AGEMID = AGEMID+AGEADD                                            JUV 640
      IF (AGEMID.LT.AGE) GO TO 10                                       JUV 645
      IF (PCTA.LE.0.0) GO TO 230                                        JUV 650
      IF (HAT.LE.0.0) GO TO 30                                          JUV 655
      ITHN = 1                                                          JUV 660
      TTN(1) = 1.0                                                      JUV 665
C
C  EEEEEEEEEE
C     CALCULATE P-THIN COST
      IF (PCT.LT.SAGE) GO TO 30
      YAGE=PCT-SAGE
      IF (YAGE.GE.50) YAGE=50
      PNW2=PNW2-PCTC*(1+PCTCI)**50/(1+R)**PCT
      PNW1=PNW1-PCTC*(1+PCTCI)**YAGE/(1+R)**(PCT-SAGE)
C  EEEEEEEEEE
      GO TO 30                                                          JUV 670
C                                                                       JUV 675
C     EXISTING UNFERTILIZED STAND                                       JUV 680
C                                                                       JUV 685
   20 IF (PCTA.LE.0.0) GO TO 230                                        JUV 690
C                                                                       JUV 695
C     EXISTING UNFERTILIZED PRECOMMERCIALLY THINNED STAND               JUV 700
C                                                                       JUV 705
      IF (PCTA.GT.AGE) GO TO 30                                         JUV 710
      TTN(1) = 1.0                                                      JUV 715
      ITHN = 1                                                          JUV 720
      HAT = HEIGHT(SI,PCTA,IOBS,OBSAGE,OBSHTS)                          JUV 725
C
C  EEEEEEEEEE
C     CALCULATE P-THIN COST
      IF (PCT.LT.SAGE) GO TO 30
      YAGE=PCT-SAGE
      IF (YAGE.GE.50) YAGE=50
      PNW2=PNW2-PCTC*(1+PCTCI)**50/(1+R)**PCT
      PNW1=PNW1-PCTC*(1+PCTCI)**YAGE/(1+R)**(PCT-SAGE)
C  EEEEEEEEEE
C                                                                       JUV 730
C     GROW STAND TO 5.55-INCHES + DBH                                   JUV 735
C                                                                       JUV 740
   30 IF (H1.LE.0.0) H1 = HEIGHT(SI,AGE,IOBS,OBSAGE,OBSHTS)             JUV 745
      IF (AGEFF.GT.AGE.OR.AGEFF.LE.0.0) GO TO 40                        JUV 750
      HF = HEIGHT(SI,AGEFF,IOBS,OBSAGE,OBSHTS)                          JUV 755
      FERT = PNIT(1)                                                    JUV 760
   40 CONTINUE                                                          JUV 765
      D1 = DIAMJ(H1,XN1,TTN(1),HAT,FERT,HF)                             JUV 770
      IF (DNOW.LE.0.0) DNOW = D1                                        JUV 775
      IF (BAPA.LE.0.0) BAPA = 0.005454154*DNOW*DNOW*XN1                 JUV 780
      DEODG = DNOW/D1                                                   JUV 785
      HR = H1                                                           JUV 790
      IF (PCTA.LE.AGE.AND.PCTA.GT.0.0) ITHN = 1                         JUV 795
      AGEMID = AGE                                                      JUV 800
      H2 = H1                                                           JUV 805
      IF (DEODG.GT.0.77.AND.DEODG.LT.1.30) GO TO 60                     JUV 810
      WRITE (IUOUT,50)                                                  JUV 815
   50 FORMAT (1X,39(2H**)/75H WARNING -- EXISTING STAND DIAMETER AT BEGIJUV 820
     $NNING OF SIMULATION IS THREE (3)/47H OR MORE STANDARD ERRORS FROM JUV 825
     $REGIONAL AVERAGE./1X,39(2H**))                                    JUV 830
      LINE = LINE+4                                                     JUV 835
   60 CONTINUE                                                          JUV 840
      IF (IORG.EQ.1.OR.PCT.GT.0.0) GO TO 80                             JUV 845
      XTREE = TRENUM(H1)                                                JUV 850
      XT = XTREE*0.5                                                    JUV 855
      YT = XTREE*1.5                                                    JUV 860
      IF (XN1.GE.XT.AND.XN1.LE.YT) GO TO 80                             JUV 865
      WRITE (IUOUT,70)                                                  JUV 870
   70 FORMAT (1X,39(2H**)/58H WARNING -- EXISTING NATURAL STAND INITIAL JUV 875
     $NUMBER OF TREES/76H EXCEEDS + OR - 50 PERCENT OF 'NORMAL'. QUESTIOJUV 880
     $NABLE EXTRAPOLATION OF MODEL./1X,39(2H**))                        JUV 885
      LINE = LINE+4                                                     JUV 890
   80 CONTINUE                                                          JUV 895
      AGEMID = AGEMID+AGEADD/2.0                                        JUV 900
      CALL XFERT (AGEMID)                                               JUV 905
      AGEMID = AGEMID-AGEADD/2.0                                        JUV 910
      HTADD = HTGROW(SI,AGE,AGEADD,FERTEF,IOBS,OBSAGE,OBSHTS)           JUV 915
      H2 = H2+HTADD                                                     JUV 920
      AGEMID = AGEMID+AGEADD                                            JUV 925
      IF (AGEMID.LT.(AGE+1.0)) GO TO 60                                 JUV 930
      XNUM1 = TRENUM(H1)                                                JUV 935
      IF (RAN(1).LE.0.0) GO TO 100                                      JUV 940
      IF (IQ.GT.0) GO TO 100                                            JUV 945
      AGECK = TOAGE(AGE,SI)                                             JUV 950
      DO 90 IQ=1,76                                                     JUV 955
      IF (RAN(IQ).GE.AGECK) GO TO 100                                   JUV 960
   90 CONTINUE                                                          JUV 965
C                                                                       JUV 970
C     BEGIN JUVENILE GROWTH LOOP                                        JUV 975
C                                                                       JUV 980
  100 CONTINUE                                                          JUV 985
      rd = bapa / sqrt(dnow)                                            temp
      IAGE = AGE+.5                                                     JUV 990
      ARG(1) = SORG(IORG,H1)                                            JUV 995
      TTN(1) = THN(ITHN,H1,HAT)                                         JUV1000
      VGR = VGRAT(DNOW,H1,AGE,TTN(1),ARG(1),BAPA)                       JUV1005
      VNOW = BAPA*VGR                                                   JUV1010
      CAI = VNOW-VSAVE                                                  JUV1015
      IF (VSAVE.LE.0.005) CAI = 0.0                                     JUV1020
c      HTL = HTLOR(DNOW,VNOW,BAPA,XN1,H1)                                JUV1025
      AGECK = TOAGE(AGE,SI)                                             JUV1030
      XMAI16 = VNOW/AGECK                                               JUV1035
      VSAVE = VNOW                                                      JUV1040
      AGE = IAGE                                                        JUV1045
      IF (IQ.LE.0.AND.NRA.EQ.0) GO TO 130                               JUV1050
      IF (NRA.GE.77) GO TO 110                                          JUV1055
      IF (AGECK.EQ.RAN(IQ)) GO TO 110                                   JUV1060
      GO TO 130                                                         JUV1065
C                                                                       JUV1070
  110 CONTINUE                                                          JUV1075
      IQ = IQ+1                                                         JUV1080
      IF (LINE.GE.53) CALL HEADER                                       JUV1085
      JAGECK = AGECK + 0.5                                              JUV1086
      JAGE = AGE + 0.5                                                  JUV1087
      WRITE (IUOUT,120) JAGECK,JAGE,H1,DNOW,BAPA,XN1,VNOW,CAI,XMAI16    JUV1090
  120 FORMAT (1X,I3,1X,I3,F7.1,   F7.2,F7.1,F7.0,F7.0,F6.0,6X,F6.0)     JUV1095
      LINE = LINE+1                                                     JUV1100
  130 CONTINUE                                                          JUV1105
      IF (AGE.GE.AHARV.AND.AHARV.GT.0.0) GO TO 290                      JUV1110
      IF (H1.GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 290                     JUV1115
      IF (DNOW.GT.5.55) GO TO 290                                       JUV1120
      IF (IORG.GE.1.AND.STA.EQ.XN1.AND.H2.GE.38.8) XNUM1 = TRENUM(H1)   JUV1125
      AGE = AGE+1.0                                                     JUV1130
      XNUM2 = TRENUM(H2)                                                JUV1135
      IF (IORG.GE.1.AND.H2.LT.38.8.OR.ITHN.EQ.1.AND.H2.LT.38.8)         JUV1140
     $ GO TO 140                                                        JUV1145
      IF (XN1.LT.XNUM2) GO TO 140                                       JUV1150
C                                                                       JUV1155
C     ADJUST FOR 'NON-NORMAL' NUMBER OF TREES IN EXISTING STAND         JUV1160
C                                                                       JUV1165
      XN2 = ADJNUM(XNUM2,XNUM1,XN1,H1,H2)                               JUV1170
  140 CONTINUE                                                          JUV1175
      IF (IORG.LE.0.AND.ITHN.EQ.0) GO TO 150                            JUV1180
      IF (XN2.GT.XNUM2.AND.H2.GT.38.8) GO TO 150                        JUV1185
      RD = BAPA/SQRT(DNOW)                                              JUV1190
C                                                                       JUV1195
C     PLANTATION AND PRECOMMERCIALLY THINNED STAND MORTALITY BEGINS     JUV1200
C     WHEN RELATIVE DENSITY REACHES 20.0                                JUV1205
C                                                                       JUV1210
      IF (RD.LT.20.0) GO TO 150                                         JUV1215
      IF (RD.GE.45.0) RD = 45.0                                         JUV1220
      XN2 = XN2-0.005*XN2*(1.0-(1.0-(RD-20.0)/25.0)**2.5)               JUV1225
  150 CONTINUE                                                          JUV1230
      AGECK = TOAGE(AGE,SI)                                             JUV1235
      IF (AGEFF.NE.AGE) GO TO 160                                       JUV1240
      HF = H2                                                           JUV1245
      FERT = PNIT(1)                                                    JUV1250
  160 CONTINUE                                                          JUV1255
      DNOW = DIAMJ(H2,XN2,TTN(1),HAT,FERT,HF)                           JUV1260
C                                                                       JUV1265
C     'NON-NORMAL' DIAMETER IN EXISTING STANDS APPROACHES 'NORMAL'      JUV1270
C                                                                       JUV1275
      DDH = H2-HR                                                       JUV1280
      IF (DDH.GT.40.0) DDH = 40.0                                       JUV1285
      RDECAY = DEODG-(DEODG-1.0)/40.0*DDH                               JUV1290
      DNOW = DNOW*RDECAY                                                JUV1295
      BAPA = .005454154*DNOW*DNOW*XN2                                   JUV1300
      H1 = H2                                                           JUV1305
      XN1 = XN2                                                         JUV1310
      XNUM1 = XNUM2                                                     JUV1315
  170 CONTINUE                                                          JUV1320
      AGEMID = AGEMID+AGEADD/2.0                                        JUV1325
      CALL XFERT (AGEMID)                                               JUV1330
      AGEMID = AGEMID-AGEADD/2.0                                        JUV1335
      HTADD = HTGROW(SI,AGE,AGEADD,FERTEF,IOBS,OBSAGE,OBSHTS)           JUV1340
      H2 = H2+HTADD                                                     JUV1345
      AGEMID = AGEMID+AGEADD                                            JUV1350
      IF (AGEMID.LT.(AGE+1.0)) GO TO 170                                JUV1355
      IF (PCTA.EQ.AGE) GO TO 180                                        JUV1360
      GO TO 100                                                         JUV1365
C                                                                       JUV1370
C     EXISTING STAND GROWN TO PCTA. DO PRECOMMERCIAL THINNING           JUV1375
C                                                                       JUV1380
  180 CONTINUE                                                          JUV1385
      IF (XNUMR.GE.XN2) GO TO 210                                       JUV1390
      XN1 = XNUMR                                                       JUV1395
      XN2 = XNUMR                                                       JUV1400
      XNUM1 = XNUMR                                                     JUV1405
      XNUM2 = XNUMR                                                     JUV1410
      TTN(1) = 1.0                                                      JUV1415
      ITHN = 1                                                          JUV1420
C
C  EEEEEEEEEE
C     CALCULATE P-THIN COST
      IF (PCT.LT.SAGE) GO TO 185
      YAGE=PCT-SAGE
      IF (YAGE.GE.50) YAGE=50
      PNW2=PNW2-PCTC*(1+PCTCI)**50/(1+R)**AGECK
      PNW1=PNW1-PCTC*(1+PCTCI)**YAGE/(1+R)**(AGECK-SAGE)
  185 CONTINUE
C  EEEEEEEEEE
C
      WRITE (IUOUT,190) AGECK,XN1                                       JUV1425
  190 FORMAT (//36H PRECOMMERCIAL THINNING DONE AT AGE ,F3.0,29H RESIDUAJUV1430
     $L NUMBER OF TREES IS ,F5.0//)                                     JUV1435
      LINE = LINE+5                                                     JUV1440
      HAT = H1                                                          JUV1445
      IF (AGEFF.NE.AGE) GO TO 200                                       JUV1450
      HF = H1                                                           JUV1455
      FERT = PNIT(1)                                                    JUV1460
  200 CONTINUE                                                          JUV1465
      DNOW = DIAMJ(H1,XN1,TTN(1),HAT,FERT,HF)                           JUV1470
      DDH = H1-HR                                                       JUV1475
      IF (DDH.GT.40.0) DDH = 40.0                                       JUV1480
      RDECAY = DEODG-(DEODG-1.0)/40.0*DDH                               JUV1485
      DNOW = DNOW*RDECAY                                                JUV1490
      BAPA = 0.005454154*DNOW*DNOW*XN2                                  JUV1495
      GO TO 100                                                         JUV1500
C                                                                       JUV1505
  210 CONTINUE                                                          JUV1510
      WRITE (IUOUT,220) AGECK                                           JUV1515
  220 FORMAT (1X,79(1H*)/6X,62HWARNING -- RESIDUAL TREES AFTER PRECOMMERJUV1520
     $CIAL THINNING AT AGE ,F3.0/6X,59HEXCEEDS EXISTING TREES. NO PRECOMJUV1525
     $MERCIAL THINNING WAS DONE./1X,79(1H*))                            JUV1530
      LINE = LINE+4                                                     JUV1535
      PCTA = 0.0                                                        JUV1540
      PCT = 0.0                                                         JUV1545
      XNUMR = 0.0                                                       JUV1550
      ITHN = 0                                                          JUV1555
      TTN(1) = 0.0                                                      JUV1560
      GO TO 100                                                         JUV1565
C                                                                       JUV1570
  230 CONTINUE                                                          JUV1575
      PCTA = 0.0                                                        JUV1580
      PCT = 0.0                                                         JUV1585
      XNUMR = 0.0                                                       JUV1590
      ITHN = 0                                                          JUV1595
      TTN(1) = 0.0                                                      JUV1600
      GO TO 30                                                          JUV1605
C                                                                       JUV1610
  240 CONTINUE                                                          JUV1615
C                                                                       JUV1620
C     REGIONAL 'AVERAGE' STAND INITIALIZATION                           JUV1625
C                                                                       JUV1630
      H1 = 40.0                                                         JUV1635
      IF (IORG.EQ.1) H1 = 25.0                                          JUV1640
      AGE = AGEJUV(SI,H1,IOBS,OBSAGE,OBSHTS)                            JUV1645
      IAGE = AGE+.5                                                     JUV1650
      AGE = IAGE                                                        JUV1655
      IF (PCTA.GT.0.0.AND.PCTA.LT.AGE) AGE = PCTA                       JUV1660
      EAGE = TOAGE(AGE,SI)                                              JUV1665
      IF (AGEF(1).GT.0.0.AND.AGEF(1).LT.EAGE) EAGE = AGEF(1)            JUV1670
      AGEMID = BHAGE(EAGE,SI)                                           JUV1675
      H1 = HEIGHT(SI,AGEMID,IOBS,OBSAGE,OBSHTS)                         JUV1680
      IF (AGEF(1).EQ.EAGE) HF = H1                                      JUV1685
      IF (PCT.EQ.EAGE) GO TO 260                                        JUV1690
  250 CONTINUE                                                          JUV1695
      IF (AGEMID.GE.AGE) GO TO 260                                      JUV1700
      AGEMID = AGEMID+AGEADD/2.0                                        JUV1705
      CALL XFERT (AGEMID)                                               JUV1710
      AGEMID = AGEMID-AGEADD/2.0                                        JUV1715
      HTADD = HTGROW(SI,AGEMID,AGEADD,FERTEF,IOBS,OBSAGE,OBSHTS)        JUV1720
      H1 = H1+HTADD                                                     JUV1725
      AGEMID = AGEMID+AGEADD                                            JUV1730
      GO TO 250                                                         JUV1735
C                                                                       JUV1740
  260 CONTINUE                                                          JUV1745
      IF (XNUMR.LE.0.0) XN1 = TRENUM(H1)                                JUV1750
      IF (XNUMR.GT.0.0) XN1 = XNUMR                                     JUV1755
      IF (PCTA.LE.0.0) GO TO 270                                        JUV1760
      ITHN = 1                                                          JUV1765
      TTN(1) = 1.0                                                      JUV1770
      HAT = H1                                                          JUV1775
C
C  EEEEEEEEEE
C     CALCULATE P-THIN COST
      IF (PCT.LT.SAGE) GO TO 270
      YAGE=PCT-SAGE
      IF (YAGE.GE.50) YAGE=50
      PNW2=PNW2-PCTC*(1+PCTCI)**50/(1+R)**PCT
      PNW1=PNW1-PCTC*(1+PCTCI)**YAGE/(1+R)**(PCT-SAGE)
C  EEEEEEEEEE
  270 CONTINUE                                                          JUV1780
      IF (AGEFF.GT.AGE.OR.AGEFF.LE.0.0) GO TO 280                       JUV1785
      FERT = PNIT(1)                                                    JUV1790
  280 CONTINUE                                                          JUV1795
      DNOW = DIAMJ(H1,XN1,TTN(1),HAT,FERT,HF)                           JUV1800
      BAPA = .005454154*DNOW*DNOW*XN1                                   JUV1805
      AGECK = TOAGE(AGE,SI)                                             JUV1810
      XNUM1 = XN1                                                       JUV1815
      XNUM2 = XN1                                                       JUV1820
      XN2 = XN1                                                         JUV1825
      GO TO 30                                                          JUV1830
C                                                                       JUV1835
  290 CONTINUE                                                          JUV1840
      HPCT = HAT                                                        JUV1845
      HS = H1                                                           JUV1850
      XNUM = XN1                                                        JUV1855
      RETURN                                                            JUV1860
      END                                                               JUV1865-
      SUBROUTINE GROWTH                                                 GRO   5
C                                                                       GRO  10
C     MAIN STAND GROWTH ROUTINE. GROW STAND FROM 5.55-INCHES DBH.       GRO  15
C     THIS SUBROUTINE IS EXECUTED ONCE FOR EACH YEAR OF GROWTH AFTER    GRO  20
C     STAND REACHES 5.55-INCHES DBH UNTIL HARVEST CUT.                  GRO  25
C                                                                       GRO  30
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16GRO  35
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), GRO  40
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     GRO  45
     $ TARESD(16), TA(16,4), VA(16,4)                                   GRO  50
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), GRO  55
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     GRO  60
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  GRO  65
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               GRO  70
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,GRO  75
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,GRO  80
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,GRO  85
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, GRO  90
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   GRO  95
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          GRO 100
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, GRO 105
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 GRO 110
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMGRO 115
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDGRO 120
      COMMON/EMORT/MORTE(16,4),DMORT,GMORT,TAMORT,VMORTE
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      REAL MORTE
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            GRO 125
C                                                                       GRO 130
C     ************ DEFINITION OF IMPORTANT GROWTH VARIABLES *********** GRO 135
C                                                                       GRO 140
C     A12    = BREAST HEIGHT STAND AGE AT END OF GROWTH PERIOD          GRO 145
C     A14    = BREAST HEIGHT STAND AGE AT MIDPOINT OF GROWTH PERIOD     GRO 150
C     AGE    = BREAST HEIGHT STAND AGE                                  GRO 155
C     AGEADD = LENGTH OF GROWTH PERIOD                                  GRO 160
C     AGECK  = TOTAL STAND AGE                                          GRO 165
C     AGENXT = BREAST HEIGHT STAND AGE AT END OF 2 GROWTH PERIODS,      GRO 170
C                   EACH PERIOD OF LENGTH AGEADD                        GRO 175
C     BAB    = BASAL AREA PER ACRE BEFORE MOST RECENT THINNING          GRO 180
C     BAC    = BASAL AREA PER ACRE CUT MOST RECENT THINNING             GRO 185
C     BAGADD = BASAL AREA PER ACRE GROSS (NET + MORTALITY) INCREMENT    GRO 190
C     BALIM  = MINIMUM BASAL AREA PER ACRE MORTALITY                    GRO 195
C     BANADD = BASAL AREA PER ACRE NET INCREMENT                        GRO 200
C     BAPA   = BASAL AREA PER ACRE                                      GRO 205
C     CAI    = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE NET CURRENT      GRO 210
C                   ANNUAL INCREMENT                                    GRO 215
C     D12    = ESTIMATE OF QUADRATIC MEAN STAND DIAMETER AT END OF      GRO 220
C                   GROWTH PERIOD                                       GRO 225
C     D14    = ESTIMATE OF QUADRATIC MEAN STAND DIAMETER AT MIDPOINT    GRO 230
C                   OF GROWTH PERIOD                                    GRO 235
C     DADD   = QUADRATIC MEAN STAND DIAMETER NET INCREMENT              GRO 240
c     db4    = Quadratic mean stand diameter at end previous growth 
c                   period
c     dinc1  = Quadratic mean stand diameter net increment last growth
c                   period
c     dinc2  = Quadratic mean stand diameter net increment current
c                   growth period
C     DMORT  = QUADRATIC MEAN DIAMETER MORTALITY                        GRO 245
C     DMR    = RATIO OF MORTALITY QUADRATIC MEAN DIAMETER TO LIVE STAND GRO 250
C                   QUADRATIC MEAN STAND DIAMETER AT END OF GROWTH      GRO 255
C                   PERIOD                                              GRO 260
C     DMTLL  = LOWER LIMIT QUADRATIC MEAN DIAMETER OF MORTALITY FOR LOW GRO 265
C                    DENSITY STANDS                                     GRO 270
C     DMTMIN = MINIMUM QUADRATIC MEAN DIAMETER OF MORTALITY             GRO 275
C     DNEXT  = ESTIMATE OF QUADRATIC MEAN STAND DIAMETER AT END OF      GRO 280
C                   GROWTH PERIOD                                       GRO 285
C     DNOW   = QUADRATIC MEAN STAND DIAMETER                            GRO 290
C     G12    = ESTIMATE OF BASAL AREA PER ACRE AT END OF GROWTH PERIOD  GRO 295
C     G14    = ESTIMATE OF BASAL AREA PER ACRE AT MIDPOINT OF GROWTH    GRO 300
C                   PERIOD                                              GRO 305
c     gb4    = Basal area per acre at end previous growth period
C     GEST   = ESTIMATE OF BASAL AREA PER ACRE AT END OF GROWTH PERIOD  GRO 310
C     GESTT  = GROSS (NET + MORTALITY) BASAL AREA PER ACRE AT END OF    GRO 315
C                   GROWTH PERIOD                                       GRO 320
c     ginc1  = Basal area per acre net increment last growth period.
c     ginc2  = Basal area per acre net increment current growth period.
C     GMORT  = ESTIMATE OF MORTALITY BASAL AREA PER ACRE                GRO 325
C     H12    = STAND TOP HEIGHT AT END OF GROWTH PERIOD                 GRO 330
C     H14    = STAND TOP HEIGHT AT MIDPOINT OF GROWTH PERIOD            GRO 335
C     HAT    = STAND TOP HEIGHT AFTER MOST RECENT THINNING              GRO 340
C     HEST   = STAND TOP HEIGHT AT END OF GROWTH PERIOD                 GRO 345
C     HS     = STAND TOP HEIGHT                                         GRO 350
C     HTADD  = STAND TOP HEIGHT INCREMENT                               GRO 355
C     HTL    = LOREY'S HEIGHT                                           GRO 360
C     HTONE  = STAND TOP HEIGHT INCREMENT FOR 1/AGEADD-GROWTH PERIODS   GRO 365
C     IQ     = INDEX FOR REPORT AGES ARRAY                              GRO 370
C     IZ     = INDEX FOR PLANTATION AND THINNING DECAY ARRAYS           GRO 375
c     rd1st  = Relative density at BH age 84
c     rdb4   = Relative density at end previous growth period
C     SAGE   = EXISTING STAND TOTAL STAND AGE                           GRO 380
C     SGMORT = CUMULATIVE SUM OF BASAL AREA PER ACRE MORTALITY          GRO 385
C     SI     = SITE INDEX (50-YEAR B H BASE AGE)                        GRO 390
C     SNMORT = CUMULATIVE SUM OF NUMBER OF TREES PER ACRE MORTALITY     GRO 395
C     SVMORT = CUMULATIVE SUM OF CUBIC-FOOT VOLUME (TOTAL STEM) PER     GRO 400
C                   ACRE MORTALITY                                      GRO 405
C     TAMORT = ESTIMATE OF NUMBER OF TREES PER ACRE MORTALITY           GRO 410
C     TANEXT = ESTIMATE OF NUMBER LIVE TREES PER ACRE AT END OF GROWTH  GRO 415
C                   PERIOD                                              GRO 420
C     VEST   = ESTIMATE OF CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE      GRO 425
C                   AT END OF GROWTH PERIOD                             GRO 430
C     VESTT  = GROSS (NET + MORTALITY) CUBIC-FOOT VOLUME (TOTAL STEM)   GRO 435
C                   PER ACRE AT END OF GROWTH PERIOD                    GRO 440
C     VGNOW  = CUBIC-FOOT VOLUME (TOTAL STEM) / BASAL AREA RATIO FOR    GRO 445
C                   LIVE TREES                                          GRO 450
C     VMORTE =  CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE                 GRO 455
C                   MORTALITY TREES                                     GRO 460
C     VNOW   = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE                  GRO 465
C     VOLGR  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE GROSS (NET +     GRO 470
C                   MORTALITY) INCREMENT                                GRO 475
C     VOLNT  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE NET INCREMENT    GRO 480
C     VSAVE  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE AT BEGINNING OF  GRO 485
C                   GROWTH PERIOD                                       GRO 490
C     XMAI16 = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE NET MEAN         GRO 495
C                   ANNUAL INCREMENT IN TREES 1.6-INCHES + DBH          GRO 500
C     XMAI56 = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE NET MEAN ANNUAL  GRO 505
C                   INCREMENT IN TREES 5.6-INCHES + DBH                 GRO 510
C     XMAI76 = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE NET MEAN ANNUAL  GRO 515
C                   INCREMENT IN TREES 7.6-INCHES + DBH                 GRO 520
C     XNUM   = NUMBER OF LIVE TREES PER ACRE                            GRO 525
C     ***************************************************************** GRO 530
C                                                                       GRO 535
C  EEEEEEEEEE
C  SEE SUBROUTINE ECON FOR DEFINITIONS OF IMPORTANT ECONOMICS VARIABLES
C  EEEEEEEEEE
C
      if (age.gt.83.0) dinc1 = dadd
      if (age.gt.83.0) ginc1 = banadd
      if (age.eq.83.0) rd1st = bapa/sqrt(dnow)
      if (age.eq.83.0) rdslope = rd1st - rdb4 
      rdb4 = bapa/sqrt(dnow)
      IF (BAB.LE.0.0) BAB = BAPA                                        GRO 540
      IF (IZ.LE.1) BALIM = 0.0                                          GRO 545
      I = IZ                                                            GRO 550
      IF (I.LE.0) I = 1                                                 GRO 555
      IZ = IZ+1                                                         GRO 560
      AGENXT = AGE+1.0                                                  GRO 565
      VSAVE = VNOW                                                      GRO 570
      AGECK = TOAGE(AGE,SI)                                             GRO 575
      IF (SAGE.NE.AGECK.OR.IQ.LE.0.AND.NRA.LE.76.AND.IUT.EQ.0.OR.IZ.GT.2GRO 580
     $.OR.AGE.EQ.AGET(1).AND.IUT.EQ.0) GO TO 30                         GRO 585
C                                                                       GRO 590
C     PRINT INITIAL STAND STATISTICS FOR EXISTING STAND LARGER THAN     GRO 595
C     5.55-INCHES DBH.                                                  GRO 600
C                                                                       GRO 605
      IF (IZ.EQ.2.AND.IUT.EQ.0) IQ = IQ+1                               GRO 610
      IF (NRA.GE.77) GO TO 10                                           GRO 615
      IF (RAN(IQ).NE.AGECK) GO TO 30                                    GRO 620
      IQ = IQ+1                                                         GRO 625
   10 CONTINUE                                                          GRO 630
c      HTL = HTLOR(DNOW,VNOW,BAPA,XNUM,HS)                               GRO 635
      XMAI16 = (VNOW+SCUTVA)/AGECK                                      GRO 640
      XMAI56 = (VA456(IZ)+SVC456(I))/AGECK                              GRO 645
      XMAI76 = (VA476(IZ)+SVC476(I))/AGECK                              GRO 650
      ZMAI16 = (VNOW + SCUTVA + SVMORT) / AGECK                         GRO 651
      JAGECK = AGECK + 0.5                                              GRO 652
      JAGE = AGE + 0.5                                                  GRO 653
      WRITE (6,20) JAGECK,JAGE,HS,DNOW,BAPA,XNUM,VNOW,CAI,ZMAI16,       GRO 655
     $XMAI16,XMAI56,XMAI76                                              GRO 660
   20 FORMAT (1X,I3,1X,I3,F7.1,     F7.2,F7.1,F7.0,F7.0,5F6.0)          GRO 665
      LINE = LINE+1                                                     GRO 670
C                                                                       GRO 675
C     GROW STAND FOR ONE GROWTH PERIOD                                  GRO 680
C                                                                       GRO 685
   30 CONTINUE                                                          GRO 690
      AGE = AGE+AGEADD/2.0                                              GRO 695
      CALL XFERT (AGE)                                                  GRO 700
      AGE = AGE-AGEADD/2.0                                              GRO 705
      HTADD = HTGROW(SI,AGE,AGEADD,FERTEF,IOBS,OBSAGE,OBSHTS)           GRO 710
      HEST = HS+HTADD                                                   GRO 715
      HTONE = HTADD/AGEADD                                              GRO 720
      HATS = HAT                                                        GRO 725
      IF (HS.EQ.HAT) HATS = HS-HTADD                                    GRO 730
C                                                                       GRO 735
C     BEGIN ESTIMATION OF STAND STATISTICS AT MIDPOINT OF GROWTH PERIOD GRO 740
C                                                                       GRO 745
      DADD = DINCR(ARG(IZ),HTONE,AGE,HS,BAPA,DNOW,FERTEF,HATS,ICT1,TTN  GRO 750
     $(IZ),BAB,BAC)                                                     GRO 755
      DADD = DADD*AGEADD                                                GRO 760
C                                                                       GRO 765
C     ESTIMATE DIAMETER AT END OF PERIOD                                GRO 770
C                                                                       GRO 775
      D12 = DNOW+DADD                                                   GRO 780
      BAGADD = BAINCR(HTONE,AGE,HS,BAPA,DNOW,TTN(IZ),HATS,FERTEF,BAB,BACGRO 785
     $)                                                                 GRO 790
      BAGADD = BAGADD*AGEADD                                            GRO 795
      BANADD = BANET(HS,BAPA,DNOW,TTN(IZ),BAGADD,AGE,ASYMP,IRD,CT1)     GRO 800
C                                                                       GRO 810
C     ESTIMATE BASAL AREA AT END OF PERIOD                              GRO 815
C                                                                       GRO 820
      G12 = BAPA+BANADD                                                 GRO 825
      A12 = AGE+AGEADD                                                  GRO 830
      DADD = DINCR(ARG(IZ),HTONE,A12,HEST,G12,D12,FERTEF,HAT,ICT1,TTN(IZGRO 835
     $),BAB,BAC)                                                        GRO 840
      DADD = DADD*AGEADD                                                GRO 845
C                                                                       GRO 850
C     ESTIMATE DIAMETER AT MIDPOINT OF PERIOD                           GRO 855
C                                                                       GRO 860
      D14 = ((D12-DNOW)+DADD)/2.0*AGEADD/2.0+DNOW                       GRO 865
      BAGADD = BAINCR(HTONE,A12,HEST,G12,D12,TTN(IZ),HAT,FERTEF,BAB,BAC)GRO 870
      BAGADD = BAGADD*AGEADD                                            GRO 875
      BANADD = BANET(HEST,G12,D12,TTN(IZ),BAGADD,A12,ASYMP,IRD,CT1)     GRO 880
C                                                                       GRO 890
C     ESTIMATE BASAL AREA AT MIDPOINT OF PERIOD                         GRO 895
C                                                                       GRO 900
      G14 = ((G12-BAPA)+BANADD)/2.0*AGEADD/2.0+BAPA                     GRO 905
C                                                                       GRO 910
C     AGE AND HEIGHT AT MIDPOINT OF PERIOD                              GRO 915
C                                                                       GRO 920
      A14 = AGE+AGEADD*0.5                                              GRO 925
      H14 = HS+HTADD*0.5                                                GRO 930
C                                                                       GRO 935
C      ESTIMATE GROSS AND NET BASAL AREA INCREMENTS USING PERIOD        GRO 940
C      MIDPOINT ESTIMATED STAND STATISTICS                              GRO 945
C                                                                       GRO 950
      BAGADD = BAINCR(HTONE,A14,H14,G14,D14,TTN(IZ),HAT,FERTEF,BAB,BAC) GRO 955
      BAGADD = BAGADD*AGEADD                                            GRO 960
      BANADD = BANET(H14,G14,D14,TTN(IZ),BAGADD,A14,ASYMP,IRD,CT1)      GRO 965
      GESTT = BAPA+BAGADD                                               GRO 975
      GEST = BAPA+BANADD                                                GRO 980
C                                                                       GRO 985
C     ESTIMATE GROSS AND NET VOLUME INCREMENTS USING PERIOD MIDPOINT    GRO 990
C     ESTIMATED STAND STATISTICS                                        GRO 995
C                                                                       GRO1000
      VOLGR = VINCR(HTONE,H14,A14,G14,D14,HAT,FERTEF,TTN(IZ),BAB,BAC)   GRO1005
      VOLGR = VOLGR*AGEADD                                              GRO1010
      VOLNT = VNET(H14,G14,D14,A14,TTN(IZ),VOLGR,ASYMP,JRD,CT1)         GRO1015
      VESTT = VNOW+VOLGR                                                GRO1020
      VEST = VNOW+VOLNT                                                 GRO1025
C                                                                       GRO1030
C     ESTIMATE NET DIAMETER INCREMENT USING PERIOD MIDPOINT ESTIMATED   GRO1035
C     STAND STATISTICS                                                  GRO1040
C                                                                       GRO1045
      DADD = DINCR(ARG(IZ),HTONE,A14,H14,G14,D14,FERTEF,HAT,ICT1,TTN(IZ)GRO1050
     $,BAB,BAC)                                                         GRO1055
      DADD = DADD*AGEADD                                                GRO1060
      DNEXT = DNOW+DADD                                                 GRO1065
      AGE = AGE+AGEADD                                                  GRO1070
      VGR = VESTT/GESTT                                                 GRO1075
      VGG = VGRAT(DNEXT,HEST,AGE,TTN(IZ),ARG(IZ),GESTT)                 GRO1080
      VESTT = VESTT*VADJ(VGR,VGG)                                       GRO1085
      GESTT = GESTT*GADJ(VGR,VGG)                                       GRO1090
      VGRN = VEST/GEST                                                  GRO1095
      VGN = VGRAT(DNEXT,HEST,AGE,TTN(IZ),ARG(IZ),GEST)                  GRO1100
      VEST = VEST*VADJ(VGRN,VGN)                                        GRO1105
      GEST = GEST*GADJ(VGRN,VGN)                                        GRO1110
      GMORT = GESTT-GEST                                                GRO1115
      IF (GMORT.LT.0.025) GMORT = 0.025                                 GRO1120
      VMORTE = VESTT-VEST                                               GRO1125
      IF (GMORT.GE.BALIM) GO TO 40                                      GRO1130
      GMORT = BALIM                                                     GRO1135
      GEST = GESTT-GMORT                                                GRO1140
      VEST = GEST*VGN                                                   GRO1145
      VMORTE = VESTT-VEST                                               GRO1150
   40 CONTINUE                                                          GRO1155
      IF (HS.EQ.HAT) BALIM = GMORT                                      GRO1160
      TANEXT = GEST/(0.005454154*DNEXT*DNEXT)                           GRO1165
      IF (TANEXT.LE.(XNUM-0.01)) GO TO 45                               GRO1170
      TANEXT = XNUM-0.01                                                GRO1175
   45 continue   
      ITER = 0                                                          GRO1180
   50 CONTINUE                                                          GRO1185
C                                                                       GRO1190
C      MORTALITY BALANCING LOOP                                         GRO1195
C                                                                       GRO1200
      ITER = ITER+1                                                     GRO1205
      TAMORT = XNUM-TANEXT                                              GRO1210
      DMORT = (GMORT/(0.005454154*TAMORT))**0.5                         GRO1215
      IF (AGE.LT.20.0.AND.DMORT.LT.(0.5*DNEXT)) DMORT = 0.5*DNEXT       GRO1220
      IF (AGE.LT.10.0.AND.DMORT.GT.(0.6*DNEXT)) DMORT = 0.6*DNEXT       GRO1225
c
c      Adjust diameter of mortality for stand 83 to 200 + years
c
      dmru = (0.85+0.0004*(Age - 83.0))*DNEXT 
      dmrl = (0.75+0.0004*(Age - 83.0))*DNEXT
      IF (AGE.GE.83.0.AND.DMORT.LT.dmrl) DMORT = dmrl                   GRO1230
      IF (AGE.GE.83.0.AND.DMORT.GT.dmru) DMORT = dmru  
c                        
C     DMR <= 1.0
c
      if(dmort.gt.dnext) dmort = dnext
      IF (AGE.LT.10.0.OR.AGE.GE.83.0) GO TO 60                          GRO1240
      DMR = DMORT/DNEXT                                                 GRO1245
      IF (DMR.LT.(0.42+0.004*AGE).AND.AGE.GE.20.0) DMORT = (0.42+0.004* GRO1250
     $AGE)*DNEXT                                                        GRO1255
      IF (DMR.GT.(0.56575+0.003425*AGE)) DMORT = (0.56575+0.003425*AGE)*GRO1260
     $DNEXT                                                             GRO1265
      RD = GEST/SQRT(DNEXT)                                             GRO1270
      DMTLL = 0.5*DNEXT+(DNEXT-DMORT)*(40.0-RD)/40.0                    GRO1275
      IF (RD.LT.40.0.AND.DMTLL.GT.DMORT) DMORT = DMTLL                  GRO1280
   60 CONTINUE                                                          GRO1285
      IF (DMTMIN.GT.0.0.AND.DMORT.GE.DMTMIN.OR.DMTMIN.LE.0.0) GO TO 70  GRO1290
      DMORT = DMTMIN                                                    GRO1295
   70 CONTINUE                                                          GRO1300
      TAMORT = GMORT/(0.005454154*DMORT*DMORT)                          GRO1305
      TANEXT = XNUM-TAMORT                                              GRO1310
      IF (TANEXT.LE.(XNUM-0.01)) GO TO 75                               GRO1315
      TANEXT = XNUM-0.01                                                GRO1320
      if(iter.lt.10) go to 50
   75 iter = 0                                    
c      GO TO 80                                                          GRO1330
C                                                                       GRO1335
   80 CONTINUE
      iter = iter + 1                                                   
      IF (TANEXT.LE.0.0) GO TO 120                                      GRO1345
C                                                                       GRO1350
C      STAND STATISTICS AT END OF GROWTH PERIOD                         GRO1355
C                                                                       GRO1360
      db4 = dnow
      gb4 = bapa
      xnumb4 = xnum
      DNEXT = (GEST/(0.005454154*TANEXT))**0.5                          GRO1365
      dinc2 = dnext - dnow
      ginc2 = gest - bapa
      DNOW = DNEXT                                                      GRO1370
      XNUM = TANEXT                                                     GRO1375
      BAPA = GEST                                                       GRO1380
      rd = bapa/ sqrt(dnow)
c
c      Control approach to asymptotic relative density for stands over 83 years bh
c 
      if (age.le.83) go to 85
      rda = rdasymp
      if((rd1st.gt.rdasymp/0.94.and.rdslope.gt.0.0.and.age.ge.84.0).or.
     1 (rd1st.lt.rdasymp/0.94.and.rdslope.lt.0.0.and.age.ge.84.0))
     2 rda = (rd1st - rdslope)*0.94
      if(ithn.eq.1) rda = rdasymp
      dnow = db4 + dinc1
      bapa = gb4 + ginc1
      rd = bapa/ sqrt(dnow)
      bapa = sqrt(dnow)*(rdb4-((rdb4-rda/0.94)/100.0))
      xnum = bapa / (dnow * dnow * 0.005454154)
      if(xnum.lt.xnumb4) go to 82
      xnum = xnumb4 - 0.01
      ginc2 = bapa - gb4
      if(iter.ge.10) go to 82
      go to 80
   82 tamort = tamort - xnum + tanext 
      bamort = bamort - bapa + gest
   85 continue
      HS = HEST                                                         GRO1385
      VNOW = VEST                                                       GRO1390
      SVMORT = SVMORT+VMORTE                                            GRO1395
      SGMORT = SGMORT+GMORT                                             GRO1400
      SNMORT = SNMORT+TAMORT                                            GRO1405
      AGENX = AGENXT-0.00005                                            GRO1410
      IF (AGE.LT.AGENX) GO TO 30                                        GRO1415
C                                                                       GRO1420
C      STAND STATISTICS AT END OF YEAR                                  GRO1425
C                                                                       GRO1430
      AGE = AGENXT                                                      GRO1435
      CAI = VEST-VSAVE                                                  GRO1440
      CALL MERCHV                                                       GRO1445
      DMTMIN = DMORT                                                    GRO1450
      AGECK = TOAGE(AGE,SI)                                             GRO1455
      dinc1 = dinc2
      ginc1 = ginc2
      if (nra.ge.77) go to 95
      if (iq.le.0.and.nra.eq.0) go to 110                            
      if(iq.gt.0.and.ran(iq).le.0.0) go to 110                                   
      IF (AGECK.ge.RAN(IQ)) GO TO 95
      go to 110 
   95 continue
      IQ = IQ+1                                                         GRO1490
c      HTL = HTLOR(DNOW,VNOW,BAPA,XNUM,HS)                               GRO1495
      XMAI16 = (VNOW+SCUTVA)/AGECK                                      GRO1500
      XMAI56 = (VA456(IZ)+SVC456(I))/AGECK                              GRO1505
      XMAI76 = (VA476(IZ)+SVC476(I))/AGECK                              GRO1510
      ZMAI16 = (VNOW + SCUTVA + SVMORT) / AGECK                         GRO1511
      JAGECK = AGECK + 0.5                                              GRO1512
      JAGE = AGE + 0.5                                                  GRO1513
c      go to 110
C                                                                       GRO1515
C     PRINT STAND STATISTICS FOR YEAR                                   GRO1520
C                                                                       GRO1525
      IF (LINE.GE.51) CALL HEADER                                       GRO1530
      WRITE (6,100) DMORT,GMORT,TAMORT,VMORTE,JAGECK,JAGE,HS,DNOW,      GRO1535
     $BAPA,XNUM,VNOW,CAI,ZMAI16,XMAI16,XMAI56,XMAI76                    GRO1540
  100 FORMAT (17H YEARLY MORTALITY,F5.2,F7.1,F7.0,F7.0/1X,I3,1X,I3,1X   GRO1545
     $ ,F6.1,   F7.2,F7.1,F7.0,F7.0,5F6.0)                              GRO1550
      LINE = LINE+2                                                     GRO1555
  110 CONTINUE                                                          GRO1560
      IZ = IZ-1                                                         GRO1565
      RETURN                                                            GRO1570
  120 CONTINUE                                                          GRO1575
      AGE = AGE-AGEADD                                                  GRO1580
      AHARV = AGE                                                       GRO1585
      IZ = IZ-1                                                         GRO1590
      RETURN                                                            GRO1595
      END                                                               GRO1600-

      SUBROUTINE THIN                                                   THI   5
C                                                                       THI  10
C     COMMERCIAL THINNING ROUTINE                                       THI  15
C                                                                       THI  20
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16THI  25
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), THI  30
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     THI  35
     $ TARESD(16), TA(16,4), VA(16,4)                                   THI  40
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), THI  45
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     THI  50
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  THI  55
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               THI  60
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,THI  65
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,THI  70
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,I, THI  75
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, THI  80
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   THI  85
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          THI  90
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, THI  95
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 THI 100
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMTHI 105
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDTHI 110
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            THI 115
C                                                                       THI 120
C     ************ DEFINITION OF INPORTANT THIN VARIABLES ************  THI 125
C                                                                       THI 130
C     AGE    = BREAST HEIGHT AGE                                        THI 135
C     ARG    = ARRAY OF PLANTATION DECAY VARIABLES                      THI 140
C     BAB    = BASAL AREA PER ACRE BEFORE THINNING                      THI 145
C     BAC    = BASAL AREA PER ACRE CUT DURING THINNING                  THI 150
C     BA23   = TWO-THIRDS OF BASAL AREA PER ACRE BEFORE THINNING        THI 155
C     BALTL  = LOWER BASAL AREA THINNING LIMIT                          THI 160
C     BAPA   = BASAL AREA PER ACRE                                      THI 165
C     BAPT   = BASAL AREA PER ACRE AFTER THINNING                       THI 170
C     CT1    = INDICATOR FOR COMMERCIAL THINNING IN DENSE STANDS        THI 175
C     D      = ARRAY OF QUADRATIC MEAN STAND DIAMETERS FOR LIVE STAND   THI 180
C                   BEFORE CUT, CUT, AFTER CUT, AND MORTALITY OF TREES  THI 185
C                   1.6-INCHES + DBH FOR EACH CUTTING                   THI 190
C     DCUT   = QUADRATIC MEAN DIAMETER OF TREES REMOVED                 THI 195
C     DLIM   = MINIMUM QUADRATIC MEAN DIAMETER OF CUT TREES             THI 200
C     DMTMIN = MINIMUM QUADRATIC DIAMETER OF MORTALITY TREES            THI 205
C     DNOW   = QUADRATIC MEAN STAND DIAMETER                            THI 210
C     DR     = ARRAY OF DIAMETER CUT / DIAMETER BEFORE CUT RATIOS FOR   THI 215
C                   EACH COMMERCIAL THINNING                            THI 220
C     DRAT   = DIAMETER CUT / DIAMETER BEFORE CUT RATIO COMPUTED IN     THI 225
C                   FUNCTION DCODB                                      THI 230
C     DRATIO = DIAMETER CUT / DIAMETER BEFORE CUT RATIO USED FOR CURRENTTHI 235
C                   COMMERCIAL THINNING                                 THI 240
C     DRLIM  = MAXIMUM DIAMETER CUT / DIAMETER BEFORE CUT RATIO LIMIT   THI 245
C     GA     = ARRAY OF BASAL AREA PER ACRE FOR LIVE STAND BEFORE       THI 250
C                   CUT, CUT, AFTER CUT, AND MORTALITY FOR EACH CUTTING THI 255
C     GRESD  = ARRAY OF BASAL AREA RESIDUALS FOR EACH COMMERCIAL        THI 260
C                   THINNING                                            THI 265
C     H40    = ARRAY OF STAND TOP HEIGHTS FOR EACH COMMERCIAL THINNING  THI 270
C     HAFT   = STAND TOP HEIGHT AFTER CURRENT COMMERCIAL THINNING       THI 275
C     HAT    = STAND TOP HEIGHT AFTER CURRENT COMMERCIAL THINNING PASSEDTHI 280
C                   TO DFSIM                                            THI 285
C     HS     = STAND TOP HEIGHT                                         THI 290
C     HTCH   = RATIO OF STAND TOP HEIGHT AFTER COMMERCIAL THINNING TO   THI 295
C                   STAND TOP HEIGHT BEFORE COMMERCIAL THINNING         THI 300
C     I      = COMMERCIAL THINNING NUMBER                               THI 305
C     ICT1   = INDICATOR SWITCH THAT AT LEAST ONE COMMERCIAL THINNING   THI 310
C                   HAS BEEN DONE                                       THI 315
C     IDR    = INDICATOR SWITCH FOR SPECIFIED DIAMETER CUT / DIAMETER   THI 320
C                   BEFORE CUT RATIOS                                   THI 325
C     IOBS   = NUMBER OF SPECIFIED STAND TOP HEIGHT OBSERVATIONS        THI 330
C     ITHN   = INDICATOR SWITCH FOR THINNING DONE                       THI 335
C     LTREE  = INDICATOR SWITCH FOR SPECIFIED RESIDUAL NUMBER TREES     THI 340
C     OBSAGE = ARRAY OF STAND AGES FOR SPECIFIED STAND TOP HEIGHT       THI 345
C                   OBSERVATIONS                                        THI 350
C     OBSHTS = ARRAY OF SPECIFIED STAND TOP HEIGHTS                     THI 355
C     OBTAGE = ARRAY OF STAND AGES FOR SPECIFIED TREE FREQUENCY         THI 360
C                   OBSERVATIONS                                        THI 365
C     OBTREE = ARRAY OF SPECIFIED TREE FREQUENCY OBSERVATIONS           THI 370
C     SI     = SITE INDEX (50-YEAR B H BASE AGE)                        THI 375
C     TA     = ARRAY OF NUMBER OF TREES PER ACRE FOR LIVE STAND BEFORE  THI 380
C                   CUT, CUT, AFTER CUT, AND MORTALITY FOR EACH CUTTING THI 385
C     TACUT  = NUMBER OF TREES PER ACRE CUT CURRENT COMMERCIAL THINNING THI 390
C     TARESD = ARRAY OF RESIDUAL NUMBER TREES FOR EACH COMMERCIAL       THI 395
C                   THINNING                                            THI 400
C     TTN    = ARRAY OF THINNING DECAY VARIABLES                        THI 405
C     VGRCUT = CUBIC-FOOT VOLUME (TOTAL STEM) / BASAL AREA RATIO OF CUT THI 410
C                   TREES CURRENT COMMERCIAL THINNING                   THI 415
C     XNUM   = NUMBER OF TREES PER ACRE                                 THI 420
C     ***************************************************************** THI 425
C                                                                       THI 430
      BAPT = 0.0                                                        THI 435
      TAGE = TOAGE(AGE,SI)                                              THI 436
      BALTL = GLTL(DNOW,TAGE)                                           THI 440
      BA23 = 2./3.*BAPA                                                 THI 445
      BAB = BAPA                                                        THI 450
      IF (I.EQ.1) GO TO 10                                              THI 455
      BAPT = GA(I-1,3)                                                  THI 460
   10 CONTINUE                                                          THI 465
C                                                                       THI 470
C     DETERMINE BASAL AREA PER ACRE AFTER COMMERCIAL THINNING           THI 475
C                                                                       THI 480
      IF (BA23.GT.BAPT) BAPT = BA23                                     THI 485
      IF (BAPT.LT.BALTL) BAPT = BALTL                                   THI 490
      IF (GRESD(I).GT.BAPA) GRESD(I) = BAPA                             THI 495
      IF (GRESD(I).GT.0.0) BAPT = GRESD(I)                              THI 500
      BAC = BAPA-BAPT                                                   THI 505
C                                                                       THI 510
C     DETERMINE DCUT / DBEFORE RATIO FOR COMMERCIAL THINNING            THI 515
C                                                                       THI 520
      DRAT = DCODB(DNOW,I,ARG(I),TTN(I))                                THI 525
      IF (BAPT.GE.BAPA) GO TO 100                                       THI 530
      IF (DR(I).GT.0.0) GO TO 20                                        THI 535
      IF (DR(I).EQ.0.0) DR(I) = DRAT                                    THI 540
      GO TO 70                                                          THI 545
C                                                                       THI 550
   20 CONTINUE                                                          THI 555
      DRLIM = 1.15                                                      THI 560
      IF (DR(I).GT.1.15.AND.IDR.LE.1) GO TO 30                          THI 565
      DRLIM = 1.0                                                       THI 570
      IF (DR(I).GT.1.0.AND.DNOW.GE.10.0.AND.IDR.LE.1) GO TO 30          THI 575
      IF (DR(I).GT.1.0.AND.HS.GT.100.0.AND.IDR.LE.1) GO TO 30           THI 580
      IF (DR(I).LT.0.8.AND.IDR.LE.1) GO TO 50                           THI 585
      GO TO 70                                                          THI 590
C                                                                       THI 595
   30 CONTINUE                                                          THI 600
      IF (LINE.GE.51) CALL HEADER                                       THI 605
      WRITE (6,40) DR(I),DRLIM                                          THI 610
   40 FORMAT (10X,26HTHE USER SUPPLIED VALUE OF,F6.2,32H FOR (DCUT/DBEFOTHI 615
     $RE) EXCEEDED THE/10X,19HACCEPTABLE LIMIT OF,F4.2,1H.)             THI 620
      LINE = LINE+3                                                     THI 625
      DR(I) = DRLIM                                                     THI 630
      GO TO 70                                                          THI 635
C                                                                       THI 640
   50 CONTINUE                                                          THI 645
      IF (LINE.GE.51) CALL HEADER                                       THI 650
      WRITE (6,60) DR(I)                                                THI 655
   60 FORMAT (10X,26HTHE USER SUPPLIED VALUE OF,F6.2,37H FOR (DCUT/DBEFOTHI 660
     $RE) WAS LESS THAN THE/10X,25HACCEPTABLE LIMIT OF 0.80.)           THI 665
      LINE = LINE+3                                                     THI 670
      DR(I) = .8                                                        THI 675
   70 CONTINUE                                                          THI 680
      IF (DR(I).GT.1.15.AND.IDR.LE.1) DR(I) = 1.15                      THI 685
      IF (DR(I).LT.0.80.AND.IDR.LE.1) DR(I) = .8                        THI 690
C                                                                       THI 695
C     DIAMETER OF CUT TREES                                             THI 700
C                                                                       THI 705
      DCUT = DR(I)*DNOW                                                 THI 710
      IF (DCUT.LT.DLIM) DCUT = DLIM                                     THI 715
      DRATIO = DCUT/DNOW                                                THI 720
      IF (DRATIO.GT.1.15.AND.IDR.LE.1) GO TO 100                        THI 725
C                                                                       THI 730
C     NUMBER OF TREES REMOVED                                           THI 735
C                                                                       THI 740
      TACUT = BAC/(.005454154*DCUT*DCUT)                                THI 745
      IF (LTREE.EQ.1) GO TO 80                                          THI 750
      GO TO 90                                                          THI 755
C                                                                       THI 760
   80 CONTINUE                                                          THI 765
      TACUT = XNUM-TARESD(I)                                            THI 770
      IF (TACUT.LE.0.0) GO TO 100                                       THI 775
      IF (IDR.GT.0.AND.IGS.LE.0) BAC = DR(I)*DR(I)*BAB*TACUT/XNUM       THI 780
      IF (BAC.LE.0.0) GO TO 100                                         THI 785
      DCUT = (BAC/TACUT/0.005454154)**0.5                               THI 790
      DRATIO = DCUT/DNOW                                                THI 795
      IF(DRATIO.GE.0.80.AND.DRATIO.LE.1.15) GO TO 90                    THI 796
      WRITE(6,85) DRATIO                                                THI 797
   85 FORMAT(1X,39(2H**)/78H WARNING -- USER SPECIFIED RESIDUAL NUMBER OTHI 798
     1F TREES AND/OR RESIDUAL BASAL AREA/47H RESULTS IN QUESTIONABLE ( DTHI 799A

     2CUT / DBEFORE ) OF ,F5.2/1X,39(2H**))                             THI 799B

      LINE = LINE + 4                                                   THI 799C

   90 CONTINUE                                                          THI 800
      IF (TACUT.LT.0.0000005) GO TO 100                                 THI 805
      HTCH = 1.0                                                        THI 810
C                                                                       THI 815
C     REDUCE STAND TOP HEIGHT FOR THINNING FROM ABOVE                   THI 820
C                                                                       THI 825
      IF (DRATIO.GT.1.0.OR.IOBS.GT.0) HTCH = CTHTC(XNUM,TACUT,BAPA,DCUT,THI 830
     $DNOW,BAC,IOBS,OBSAGE,OBSHTS,AGE)                                  THI 835
      HAFT = HTCH*H40(I)                                                THI 840
      HAT = HAFT                                                        THI 845
      GA(I,2) = BAC                                                     THI 850
      TA(I,2) = TACUT                                                   THI 855
      D(I,2) = DCUT                                                     THI 860
      TA(I,3) = TA(I,1)-TA(I,2)                                         THI 865
      GA(I,3) = GA(I,1)-GA(I,2)                                         THI 870
      D(I,3) = ((GA(I,3)/TA(I,3))/0.005454154)**0.5                     THI 875
      VGRCUT = VGRAT(D(I,3),HAT,AGE,1.0,ARG(I),GA(I,3))                 THI 880
      VA(I,3) = VGRCUT*GA(I,3)                                          THI 885
      IF (VA(I,3).GT.VA(I,1)) VA(I,3) = VA(I,1)/GA(I,1)*GA(I,3)         THI 890
      VA(I,2) = VA(I,1)-VA(I,3)                                         THI 895
      CALL MERCHT                                                       THI 900
      HS = HAT                                                          THI 905
C     SET COMMERCIAL THINNING SWITCHES                                  THI 915
C                                                                       THI 920
      CT1 = 0.0                                                         THI 930
      RD = BAPA/SQRT(DNOW)                                              THI 935
      IF (RD.GE.60.0) CT1 = 1.0                                         THI 940
      ICT1 = 1                                                          THI 945
      ITHN = 1                                                          THI 950
      IRD = 0                                                           THI 955
      JRD = 0                                                           THI 960
      GO TO 110                                                         THI 965
  100 CONTINUE                                                          THI 975
C                                                                       THI 990
      VA(I,2) = 0.0                                                     THI1000
      VAC56(I) = 0.0                                                    THI1005
      VAC456(I) = 0.0                                                   THI1010
      VAC76(I) = 0.0                                                    THI1015
      VAC476(I) = 0.0                                                   THI1020
      TA(I,2) = 0.0                                                     THI1025
      D(I,2) = 0.0                                                      THI1030
      BAC = 0.0                                                         THI1035
      GA(I,3) = GA(I,1)                                                 THI1040
      VA(I,3) = VA(I,1)                                                 THI1045
      VAA56(I) = VA56(I)                                                THI1050
      VAA456(I) = VA456(I)                                              THI1055
      VAA76(I) = VA76(I)                                                THI1060
      VAA476(I) = VA476(I)                                              THI1065
      TA(I,3) = TA(I,1)                                                 THI1070
      D(I,3) = D(I,1)                                                   THI1075
  110 CONTINUE                                                          THI1080
      RETURN                                                            THI1085
      END                                                               THI1090-
      SUBROUTINE FRSTCT                                                 FCT   5
C                                                                       FCT  10
C      GROW STAND TO FIRST COMMERCIAL THINNING                          FCT  15
C                                                                       FCT  20
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16FCT  25
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), FCT  30
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     FCT  35
     $ TARESD(16), TA(16,4), VA(16,4)                                   FCT  40
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), FCT  45
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     FCT  50
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  FCT  55
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               FCT  60
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,FCT  65
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,FCT  70
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,FCT  75
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, FCT  80
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   FCT  85
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          FCT  90
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, FCT  95
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 FCT 100
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMFCT 105
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDFCT 110
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            FCT 115
      ISWITA = 0                                                        FCT 120
      ISWITB = 0                                                        FCT 125
      IF (IGS.GT.0) GO TO 30                                            FCT 130
C                                                                       FCT 135
C      GROW STAND TO GCLIM + LOWER THINNING LIMIT                       FCT 140
C                                                                       FCT 145
   10 CONTINUE                                                          FCT 150
      TAGE = TOAGE(AGE,SI)                                              FCT 151
      BALL20 = GLTL(DNOW,TAGE)+GCLIM                                    FCT 155
      IF (BAPA.GT.BALL20) GO TO 20                                      FCT 160
      IF (AGE.GE.AHARV.AND.AHARV.GT.0.0) GO TO 280                      FCT 165
      IF (HS.GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 280                     FCT 170
      ARG(1) = SORG(IORG,HS)                                            FCT 175
      TTN(1) = THN(ITHN,HS,HAT)                                         FCT 180
      CALL GROWTH                                                       FCT 185
      ISWITA = 1                                                        FCT 190
      GO TO 10                                                          FCT 195
C                                                                       FCT 200
   20 CONTINUE                                                          FCT 205
      TOTAGE = TOAGE(AGE,SI)                                            FCT 210
      IF(AGET(1).GT.0.0.AND.AGET(1).GE.TOTAGE) ISWITA = 0               FCT 211
      IF(HY40(1).GT.0.0.AND.HY40(1).GE.HS) ISWITA = 0                   FCT 212
   30 CONTINUE                                                          FCT 215
C                                                                       FCT 220
C      GROW STAND TO USER SPECIFIED BA LIMITS AND D CUT SPECS           FCT 225
C                                                                       FCT 230
      BA56 = BAPA56(ARG(1),DNOW,BAPA,HS)                                FCT 235
      IF (DNOW.GE.DC.AND.BA56.GE.GLIM) GO TO 40                         FCT 240
      IF (AGE.GE.AHARV.AND.AHARV.GT.0.0) GO TO 280                      FCT 245
      IF (HS.GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 280                     FCT 250
      ARG(1) = SORG(IORG,HS)                                            FCT 255
      TTN(1) = THN(ITHN,HS,HAT)                                         FCT 260
      CALL GROWTH                                                       FCT 265
      ISWITB = 1                                                        FCT 270
      GO TO 30                                                          FCT 275
C                                                                       FCT 280
   40 CONTINUE                                                          FCT 285
      TOTAGE = TOAGE(AGE,SI)                                            FCT285A
      IF(AGET(1).GT.0.0.AND.AGET(1).GE.TOTAGE) ISWITB = 0               FCT 286
      IF(HY40(1).GT.0.0.AND.HY40(1).GE.HS) ISWITB = 0                   FCT 287
C
C     GROW STAND TO RELATIVE DENSITY 47.5 IF COMMERCIAL THINNING HEIGHTS
C     OR AGES ARE NOT SPECIFIED
C
      IF(AGET(1).GT.0.0.OR.HY40(1).GT.0.0) GO TO 45
      IF(AGE.GE.AHARV.AND.AHARV.GT.0.0) GO TO 280
      IF(HS.GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 280
      RD = BAPA / SQRT(DNOW)
      IF(RD.GE.47.5) GO TO 45
      ARG(1) = SORG(IORG,HS)
      TTN(1) = THN(ITHN,HS,HAT)
      CALL GROWTH
      GO TO 40
   45 CONTINUE
      TOTAGE = TOAGE(AGE,SI)                                            FCT 290
      IF (HY40(1).GT.0.0) GO TO 90                                      FCT 295
      IF (AGET(1).GT.0.0.AND.TOTAGE.LT.AGET(1)) GO TO 60                FCT 300
      IA = 0                                                            FCT 305
      AGET(1) = TOTAGE                                                  FCT 310
      DO 50 IB=1,16                                                     FCT 315
      IF (AGET(IB).LT.TOTAGE.AND.AGET(IB).NE.0.0) GO TO 50              FCT 320
      IA = IA+1                                                         FCT 325
      AGET(IA) = AGET(IB)                                               FCT 330
   50 CONTINUE                                                          FCT 335
   60 CONTINUE                                                          FCT 340
      IF (AGET(1).GT.0.0.AND.FCT.EQ.0.0) FCTA = BHAGE(AGET(1),SI)       FCT 345
      IF (AGET(1).GT.0.0.AND.FCT.EQ.0.0) FCT = AGET(1)                  FCT 350
      IF (FCTA.EQ.0.0) GO TO 90                                         FCT 355
      IF (AGE.LT.FCTA) GO TO 80                                         FCT 360
      KAGE = AGE+.1                                                     FCT 365
      KFCTA = FCTA+.1                                                   FCT 370
      IF (KAGE.EQ.KFCTA) GO TO 100                                      FCT 375
      TOTAGE = TOAGE(AGE,SI)                                            FCT 380
      IF (TOTAGE.GT.FCT.AND.ISWITA.EQ.1) GO TO 100                      FCT 385
      WRITE (6,70) FCT,TOTAGE                                           FCT 390
   70 FORMAT (1X,79(1H-)/6X,65HTHE STAND DOES NOT MEET THE DIAMETER AND FCT 395
     $BASAL AREA LIMITS AT AGE,F5.0/6X,49HTHE FIRST COMMERCIAL THINNING FCT 400
     $WILL BE MADE AT AGE,F5.0/1X,79(1H-))                              FCT 405
      LINE = LINE+6                                                     FCT 410
      GO TO 100                                                         FCT 415
C                                                                       FCT 420
   80 CONTINUE                                                          FCT 425
C                                                                       FCT 430
C      GROW STAND TO FIRST SPECIFIED AGE                                FCT 435
C                                                                       FCT 440
      IF (AGE.GE.AHARV.AND.AHARV.GT.0.0) GO TO 280                      FCT 445
      IF (HS.GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 280                     FCT 450
      ARG(1) = SORG(IORG,HS)                                            FCT 455
      TTN(1) = THN(ITHN,HS,HAT)                                         FCT 460
      CALL GROWTH                                                       FCT 465
      GO TO 60                                                          FCT 470
C                                                                       FCT 475
   90 CONTINUE                                                          FCT 480
C                                                                       FCT 485
C      GROW STAND TO FIRST SPECIFIED HEIGHT                             FCT 490
C                                                                       FCT 495
      IF (HY40(1).EQ.0.0) GO TO 100                                     FCT 500
      IF (HS.GE.HY40(1)) GO TO 100                                      FCT 505
      IF (AGE.GE.AHARV.AND.AHARV.GT.0.0) GO TO 280                      FCT 510
      IF (HS.GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 280                     FCT 515
      ARG(1) = SORG(IORG,HS)                                            FCT 520
      TTN(1) = THN(ITHN,HS,HAT)                                         FCT 525
      CALL GROWTH                                                       FCT 530
      GO TO 90                                                          FCT 535
C                                                                       FCT 540
  100 CONTINUE                                                          FCT 545
      TOTAGE = TOAGE(AGE,SI)                                            FCT 550
      IF (IUT.GE.1) GO TO 240                                           FCT 555
      IF (HY40(1).GT.0.0.AND.HS.GT.HY40(1)) ISWITA = 1                  FCT 560
      IF (HY40(1).GT.0.0.AND.HY40(1).LT.HS.AND.ISWITA.EQ.1.AND.ISWITB.NEFCT 565
     $.1) WRITE (6,110) HY40(1),HS                                      FCT 570
  110 FORMAT (1X,79(1H-)/6X,55H THE STAND DOES NOT MEET THE MINIMUM THINFCT 575
     $NING LIMIT AT ,F4.0,6H FEET./6X,47H THE FIRST COMMERCIAL THINNING FCT 580
     $WILL BE DONE AT ,F4.0,6H FEET./1X,79(1H-))                        FCT 585
      IF (FCTA.NE.0.0.AND.FCTA.LT.AGE.AND.ISWITA.EQ.1.AND.ISWITB.NE.1)  FCT 590
     $ WRITE (6,120) FCT,TOTAGE                                         FCT 595
  120 FORMAT (1X,79(1H-)/6X,55H THE STAND DOES NOT MEET THE MINIMUM THINFCT 600
     $NING LIMIT AT ,F4.0,7H YEARS./6X,47H THE FIRST COMMERCIAL THINNINGFCT 605
     $ WILL BE DONE AT ,F4.0,7H YEARS./1X,79(1H-))                      FCT 610
      IF (TOTAGE.LT.AGET(1).OR.AGET(1).LE.0.0) GO TO 180                FCT 615
      AGET(1) = TOTAGE                                                  FCT 620
      KK = 2                                                            FCT 625
      DO 130 JJ=2,16                                                    FCT 630
      IF (AGET(1).LT.AGET(JJ)) GO TO 140                                FCT 635
      KK = JJ+1                                                         FCT 640
  130 CONTINUE                                                          FCT 645
  140 CONTINUE                                                          FCT 650
      IF (KK.EQ.2) GO TO 180                                            FCT 655
      IF (KK.GE.17) GO TO 160                                           FCT 660
      MM = 2                                                            FCT 665
      DO 150 II=KK,16                                                   FCT 670
      AGET(MM) = AGET(II)                                               FCT 675
      MM = MM+1                                                         FCT 680
  150 CONTINUE                                                          FCT 685
      GO TO 180                                                         FCT 690
C                                                                       FCT 695
  160 CONTINUE                                                          FCT 700
      DO 170 II=2,16                                                    FCT 705
      AGET(II) = 0.0                                                    FCT 710
  170 CONTINUE                                                          FCT 715
  180 CONTINUE                                                          FCT 720
      IF (HS.LT.HY40(1).OR.HY40(1).LE.0.0) GO TO 240                    FCT 725
      HY40(1) = HS                                                      FCT 730
      KK = 2                                                            FCT 735
      DO 190 JJ=2,16                                                    FCT 740
      IF (HY40(1).LT.HY40(JJ)) GO TO 200                                FCT 745
      KK = JJ+1                                                         FCT 750
  190 CONTINUE                                                          FCT 755
  200 CONTINUE                                                          FCT 760
      IF (KK.EQ.2) GO TO 240                                            FCT 765
      IF (KK.GE.17) GO TO 220                                           FCT 770
      MM = 2                                                            FCT 775
      DO 210 II=KK,16                                                   FCT 780
      HY40(MM) = HY40(II)                                               FCT 785
      MM = MM+1                                                         FCT 790
  210 CONTINUE                                                          FCT 795
      GO TO 240                                                         FCT 800
C                                                                       FCT 805
  220 CONTINUE                                                          FCT 810
      DO 230 II=2,16                                                    FCT 815
      HY40(II) = 0.0                                                    FCT 820
  230 CONTINUE                                                          FCT 825
  240 CONTINUE                                                          FCT 830
      DO 250 II=1,16                                                    FCT 835
      IF (AGET(II).GT.0.0) AGET(II) = BHAGE(AGET(II),SI)                FCT 840
  250 CONTINUE                                                          FCT 845
      DO 260 II=2,16                                                    FCT 850
      IF (AHARR.EQ.0.0.AND.AGET(II).NE.0.0) AHARV = AGET(II)            FCT 855
  260 CONTINUE                                                          FCT 860
      IF (AHARR.EQ.0.0.AND.HTHARV.EQ.0.0) GO TO 270                     FCT 865
      GO TO 280                                                         FCT 870
C                                                                       FCT 875
  270 CONTINUE                                                          FCT 880
      IF (FIY.EQ.0.0.AND.FIH.EQ.0.0.AND.TI.GT.0.0) FIY = TI             FCT 885
      IF (FIY.EQ.0.0.AND.FIH.EQ.0.0.AND.DH.GT.0.0) FIH = DH             FCT 890
  280 CONTINUE                                                          FCT 895
      RETURN                                                            FCT 900
      END                                                               FCT 905-

      SUBROUTINE NOTHIN                                                 NOT   5
C                                                                       NOT  10
C      GROW STAND TO HARVEST CUT WITHOUT COMMERCIAL THINNING            NOT  15
C                                                                       NOT  20
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16NOT  25
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), NOT  30
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     NOT  35
     $ TARESD(16), TA(16,4), VA(16,4)                                   NOT  40
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), NOT  45
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     NOT  50
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  NOT  55
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               NOT  60
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,NOT  65
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,NOT  70
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,NOT  75
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, NOT  80
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   NOT  85
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          NOT  90
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, NOT  95
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 NOT 100
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMNOT 105
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDNOT 110
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      COMMON/EMORT/MORTE(16,4),DMORT,GMORT,TAMORT,VMORTE
      REAL MORTE
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            NOT 115
      LIUT(1) = IUT                                                     NOT 120
      SCUTBA = 0.0                                                      NOT 125
      SCUTTA = 0.0                                                      NOT 130
      SCUTVA = 0.0                                                      NOT 135
      SCUT45 = 0.0                                                      NOT 140
      SCUT47 = 0.0                                                      NOT 145
      IZ = 1                                                            NOT 150
      AGET(1) = AGE                                                     NOT 155
      D(1,1) = DNOW                                                     NOT 160
      TA(1,1) = XNUM                                                    NOT 165
      VA(1,1) = VNOW                                                    NOT 170
      GA(1,1) = BAPA                                                    NOT 175
      H40(1) = HS                                                       NOT 180
      CALL MERCHV                                                       NOT 185
      IF (HY40(1).EQ.0.0) HY40(1) = HS                                  NOT 190
      TOTAGE = TOAGE(AGET(1),SI)                                        NOT 195
c      HTL = HTLOR(DNOW,VNOW,BAPA,XNUM,HS)                               NOT 200
      SVC456(1) = SCUT45                                                NOT 205
      SVC476(1) = SCUT47                                                NOT 210
      IF (RAN(1).EQ.0.0) GO TO 20                                       NOT 215
      IF (IQ.GT.0) GO TO 20                                             NOT 220
      AGECK = TOAGE(AGE,SI)                                             NOT 225
      DO 10 IQ=1,76                                                     NOT 230
      IF (RAN(IQ).GE.AGECK) GO TO 20                                    NOT 235
   10 CONTINUE                                                          NOT 240
   20 CONTINUE                                                          NOT 245
      IF (AGE.GE.AHARV.AND.AHARV.GT.0.0) GO TO 30                       NOT 250
      IF (HS.GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 30                      NOT 255
      ARG(2) = SORG(IORG,HS)                                            NOT 260
      TTN(2) = THN(ITHN,HS,HAT)                                         NOT 265
      CALL GROWTH                                                       NOT 270
      GO TO 20                                                          NOT 275
C                                                                       NOT 280
   30 CONTINUE                                                          NOT 285
C  EEEEEEEEEE
      MORTE(16,1)=DMORT
      MORTE(16,2)=GMORT
      MORTE(16,3)=TAMORT
      MORTE(16,4)=VMORTE
C  EEEEEEEEEE
C
      RETURN                                                            NOT 290
      END                                                               NOT 295-

      SUBROUTINE COMTHN                                                 COM   5
C                                                                       COM  10
C      GROW STAND BETWEEN COMMERCIAL THINNINGS                          COM  15
C                                                                       COM  20
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16COM  25
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), COM  30
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     COM  35
     $ TARESD(16), TA(16,4), VA(16,4)                                   COM  40
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), COM  45
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     COM  50
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  COM  55
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               COM  60
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,COM  65
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,COM  70
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,COM  75
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, COM  80
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   COM  85
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          COM  90
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, COM  95
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 COM 100
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMCOM 105
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDCOM 110
      COMMON/EMORT/MORTE(16,4),DMORT,GMORT,TAMORT,VMORTE
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      REAL MORTE
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            COM 115
      SCUTBA = 0.0                                                      COM 120
      SCUTTA = 0.0                                                      COM 125
      SCUTVA = 0.0                                                      COM 130
      SCUT45 = 0.0                                                      COM 135
      SCUT47 = 0.0                                                      COM 140
C                                                                       COM 145
C     LOOP FOR COMMERCIAL THINNING                                      COM 150
C                                                                       COM 155
      DO 250 I=1,16                                                     COM 160
      IZ = I                                                            COM 165
      AGET(I) = AGE                                                     COM 170
      D(I,1) = DNOW                                                     COM 175
      TA(I,1) = XNUM                                                    COM 180
      VA(I,1) = VNOW                                                    COM 185
      GA(I,1) = BAPA                                                    COM 190
      H40(I) = HS                                                       COM 195
      IF (IZ.NE.1) GO TO 10                                             COM 200
      CALL MERCHV                                                       COM 205
   10 CONTINUE                                                          COM 210
      IF (HY40(I).EQ.0.0) HY40(I) = HS                                  COM 215
      IF (FERTCT.LT.1) GO TO 20                                         COM 220
      AGEF(I) = TOAGE(AGE,SI)                                           COM 225
   20 CONTINUE                                                          COM 230
      TOTAGE = TOAGE(AGET(I),SI)                                        COM 235
c      HTL = HTLOR(DNOW,VNOW,BAPA,XNUM,HS)                               COM 240
      IF (AGET(I).EQ.AGE.AND.CTAS.EQ.1) GO TO 30                        COM 245
      IF (HY40(I).EQ.HS.AND.SCTHT.EQ.1) GO TO 30                        COM 250
      YRLFTH = 0.0                                                      COM 255
      YRLFTT = 0.0                                                      COM 260
      HTLFTH = 0.0                                                      COM 265
      HTLFTT = 0.0                                                      COM 270
      IF (AHARV.GT.0.0) YRLFTH = AHARV-AGE                              COM 275
      IF (HTHARV.GT.0.0) HTLFTH = HTHARV-HS                             COM 280
      IF (AHARV.GT.0.0.AND.YRLFTH.LE.0.0.AND.AGET(I+1).LE.0.0) LIUT(I) =COM 285
     $ 2                                                                COM 290
      IF (HTHARV.GT.0.0.AND.HTLFTH.LE.0.0.AND.HY40(I+1).LE.0.0) LIUT(I) COM 295
     $= 4                                                               COM 300
      IF (AGET(I+1).GT.AGE) YRLFTT = AGET(I+1)-AGE                      COM 305
      IF (HY40(I+1).GT.HS) HTLFTT = HY40(I+1)-HS                        COM 310
      IF (YRLFTH.LT.FIY.AND.FIY.GT.0.0.AND.YRLFTH.GE.1.0.OR.YRLFTH.LT.5.COM 315
     $0.AND.YRLFTH.GE.1.0.AND.FIH.LE.0.0) LIUT(I) = 2                   COM 320
      IF (YRLFTT.LT.TI.AND.TI.GT.0.0.AND.YRLFTT.GE.1.0.OR.YRLFTT.LT.5.0.COM 325
     $AND.YRLFTT.GE.1.0.AND.TI.LE.0.0) LIUT(I) = 3                      COM 330
      IF (HTLFTH.LT.FIH.AND.FIH.GT.0.0.AND.HTLFTH.GE.1.0.OR.HTLFTH.LT.10COM 335
     $.0.AND.HTLFTH.GE.1.0.AND.FIH.LE.0.0) LIUT(I) = 4                  COM 340
      IF (HTLFTT.LT.DH.AND.DH.GT.0.0.AND.HTLFTT.GE.1.0.OR.HTLFTT.LT.10.0COM 345
     $.AND.HTLFTT.GE.1.0.AND.DH.LE.0.0) LIUT(I) = 5                     COM 350
      IF (LIUT(I).GT.0) GO TO 40                                        COM 355
C                                                                       COM 360
C     DO COMMERCIAL THINNING                                            COM 365
C                                                                       COM 370
   30 CALL THIN                                                         COM 375
C                                                                       COM 380
C     SET AFTER THINNING VALUES                                         COM 385
C                                                                       COM 390
C  EEEEEEEEEE
      DO 35 JJ=1,16
         IF (MORTE(JJ,4).GT.0.0) GO TO 35
      MORTE(JJ,1)=DMORT
      MORTE(JJ,2)=GMORT
      MORTE(JJ,3)=TAMORT
      MORTE(JJ,4)=VMORTE
      GO TO 36
   35 CONTINUE
   36 CONTINUE
C  EEEEEEEEEE
      XNUM = TA(I,3)                                                    COM 395
      BAPA = GA(I,3)                                                    COM 400
      VNOW = VA(I,3)                                                    COM 405
      DNOW = D(I,3)                                                     COM 410
      XMAI16 = (VA(I,1)+SCUTVA)/TOTAGE                                  COM 415
      XMAI56 = (VA456(I)+SCUT45)/TOTAGE                                 COM 420
      XMAI76 = (VA476(I)+SCUT47)/TOTAGE                                 COM 425
      ZMAI16 = (VA(I,1) + SCUTVA + SVMORT) / TOTAGE                     COM 426
      JAGECK = TOTAGE + 0.5                                             COM 427
      JAGE = AGE + 0.5                                                  COM 428
C                                                                       COM 430
C     SUM CUT VARIABLES                                                 COM 435
C                                                                       COM 440
      SCUTBA = SCUTBA+GA(I,2)                                           COM 445
      SCUTTA = SCUTTA+TA(I,2)                                           COM 450
      SCUTVA = SCUTVA+VA(I,2)                                           COM 455
      SCUT45 = SCUT45+VAC456(I)                                         COM 460
      SCUT47 = SCUT47+VAC476(I)                                         COM 465
   40 CONTINUE                                                          COM 470
      SVC456(I) = SCUT45                                                COM 475
      SVC476(I) = SCUT47                                                COM 480
C                                                                       COM 485
C     WRITE OUT MANAGED YIELD TABLE FOR CURRENT THINNING                COM 490
C                                                                       COM 495
C           BEFORE CUT                                                  COM 500
C                                                                       COM 505
      IF (LIUT(I).GT.0) GO TO 130                                       COM 510
      IF (LINE.GE.50) CALL HEADER                                       COM 515
      WRITE (6,50) JAGECK,JAGE                                          COM 520
   50 FORMAT (1X,I3,1X,I3/)                                             COM 525
      LINE = LINE+2                                                     COM 530
      WRITE (6,60) H40(I),D(I,1),GA(I,1),TA(I,1),VA(I,1),CAI,ZMAI16,    COM 535
     $XMAI16,XMAI56,XMAI76                                              COM 540
   60 FORMAT (1X,6HBEFORE,2X,F6.1,   F7.2,F7.1,F7.0,F7.0,5F6.0)         COM 545
      LINE = LINE+1                                                     COM 550
C                                                                       COM 555
C           CUT                                                         COM 560
C                                                                       COM 565
      WRITE (6,70) D(I,2),GA(I,2),TA(I,2),VA(I,2)                       COM 570
   70 FORMAT (1X,8HCUT     ,7X,F6.2,F7.1,F7.0,F7.0)                     COM 575
      LINE = LINE+1                                                     COM 580
C                                                                       COM 585
C           AFTER CUT                                                   COM 590
C                                                                       COM 595
c      HTLA = HTLOR(D(I,3),VA(I,3),GA(I,3),TA(I,3),HS)                   COM 600
      WRITE (6,80) HS,D(I,3),GA(I,3),TA(I,3),VA(I,3)                    COM 605
   80 FORMAT (1X,8HRESIDUAL,F6.1,F7.2,F7.1,F7.0,F7.0/)                  COM 610
      LINE = LINE+2                                                     COM 615
C                                                                       COM 620
C           SUM OF THE CUTS                                             COM 625
C                                                                       COM 630
      IF (LINE.GE.52) CALL HEADER                                       COM 635
      WRITE (6,90) SCUTBA,SCUTTA,SCUTVA                                 COM 640
   90 FORMAT (1X,8HSUM CUTS,14x,F6.1,F7.0,F7.0)                         COM 645
      LINE = LINE+1                                                     COM 650
C                                                                       COM 655
C     SUM OF THE MORTALITY                                              COM 660
C                                                                       COM 665
      IF (SNMORT.LT.0.0005) GO TO 100                                   COM 670
      SDMORT = ((SGMORT/SNMORT)/0.005454154)**0.5                       COM 675
      GO TO 110                                                         COM 680
C                                                                       COM 685
  100 SDMORT = 0.0                                                      COM 690
  110 CONTINUE                                                          COM 695
      D(I,4) = SDMORT                                                   COM 700
      GA(I,4) = SGMORT                                                  COM 705
      TA(I,4) = SNMORT                                                  COM 710
      VA(I,4) = SVMORT                                                  COM 715
      IF (LINE.GE.52) CALL HEADER                                       COM 720
      WRITE (6,120) SGMORT,SNMORT,SVMORT                                COM 725
  120 FORMAT (1X,13HSUM MORTALITY, 9x,F6.1,F7.0,F7.0/)                  COM 730
      LINE = LINE+2                                                     COM 735
      GO TO 180                                                         COM 740
C                                                                       COM 745
  130 CONTINUE                                                          COM 750
      IF (LIUT(I).EQ.2) WRITE (6,140) TOTAGE                            COM 755
  140 FORMAT (1X,79(1H-)/6X,40HTHE LAST COMMERCIAL THINNING NOT DONE AT,COM 760
     $F5.0,27H YEARS, BECAUSE THE HARVEST/6X,65HCUT IS SCHEDULED IN LESSCOM 765
     $ THAN 5 YEARS OR LESS THAN FINAL THINNING/6X,9HINTERVAL./1X,79(1H-COM 770
     $))                                                                COM 775
      IF (LIUT(I).EQ.3) WRITE (6,150) TOTAGE                            COM 780
  150 FORMAT (1X,79(1H-)/6X,41HTHE FIRST COMMERCIAL THINNING NOT DONE ATCOM 785
     $,F5.0,24H YEARS, BECAUSE THE NEXT/6X,69HTHINNING IS SCHEDULED IN LCOM 790
     $ESS THAN 5 YEARS OR LESS THAN THE SPECIFIED/6X,27HTHINNING INTERVACOM 795
     $L IN YEARS./1X,79(1H-))                                           COM 800
      IF (LIUT(I).EQ.4) WRITE (6,160) HS                                COM 805
  160 FORMAT (1X,79(1H-)/6X,40HTHE LAST COMMERCIAL THINNING NOT DONE AT,COM 810
     $F5.0,26H FEET, BECAUSE THE HARVEST/6X,73HCUT IS SCHEDULED IN LESS COM 815
     $THAN 10 FEET OF HEIGHT GROWTH OR LESS THAN FINAL/6X,25HTHINNING HECOM 820
     $IGHT INTERVAL /1X,79(1H-))                                        COM 825
      IF (LIUT(I).EQ.5) WRITE (6,170) HS                                COM 830
  170 FORMAT (1X,79(1H-)/6X,41HTHE FIRST COMMERCIAL THINNING NOT DONE ATCOM 835
     $,F5.0,23H FEET, BECAUSE THE NEXT/6X,72HTHINNING IS SCHEDULED IN LECOM 840
     $SS THAN 10 FEET OF HEIGHT GROWTH OR LESS THAN/6X,41HTHE SPECIFIED COM 845
     $THINNING INTERVAL IN YEARS./1X,79(1H-))                           COM 850
      LINE = LINE+5                                                     COM 855
  180 CONTINUE                                                          COM 860
      if(liut(i).eq.0) RDAFT1 = GA(I,3) / SQRT(D(I,3))                  COM 861
C                                                                       COM 865
C     CALCULATE NEXT CUTTING AGE OR NEXT CUTTING HEIGHT                 COM 870
C                                                                       COM 875
      IF (HY40(I+1).EQ.0.0.AND.DH.GT.0.0) HY40(I+1) = HS+DH             COM 880
      IF (AGET(I+1).EQ.0.0.AND.TI.GT.0.0) AGET(I+1) = AGET(I)+TI        COM 885
      AAT = AGE
      if(ta(i,3).le.50) go to 190 
      IF (I.EQ.NCT) GO TO 190                                           COM 890
      IF (AGET(I+1).GE.AHARV.AND.AHARV.GT.0.0) GO TO 190                COM 895
      IF (HY40(I+1).GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 190              COM 900
      GO TO 200                                                         COM 905
C                                                                       COM 910
  190 CONTINUE                                                          COM 915
      IF (FIY.GT.0.0.AND.AHARV.LE.0.0) AHARV = AGET(I)+FIY              COM 920
      IF (FIH.GT.0.0.AND.HTHARV.LE.0.0) HTHARV = HY40(I)+FIH            COM 925
      AGET(I+1) = AHARV                                                 COM 930
      HY40(I+1) = HTHARV                                                COM 935
  200 CONTINUE                                                          COM 940
      IF (RAN(1).EQ.0.0) GO TO 220                                      COM 945
      IF (IQ.GT.0) GO TO 220                                            COM 950
      AGECK = TOAGE(AGE,SI)                                             COM 955
      DO 210 IQ=1,76                                                    COM 960
      IF (RAN(IQ).GE.AGECK) GO TO 220                                   COM 965
  210 CONTINUE                                                          COM 970
  220 CONTINUE                                                          COM 975
      TOTAGE = TOAGE(AGE,SI)
      GL = GLTL(DNOW,TOTAGE)+GCLIM                                      COM 980
      IF (BAPA.LT.GL.AND.IGS.LE.0) GO TO 230                            COM 985
      IF (AGET(I+1).EQ.0.0.AND.HY40(I+1).EQ.0.0) GO TO 230              COM 990
      IF (AGE.GE.AGET(I+1).AND.AGET(I+1).GT.0.0) GO TO 240              COM 995
      IF (HS.GE.HY40(I+1).AND.HY40(I+1).GT.0.0) GO TO 240               COM1000
  230 CONTINUE                                                          COM1005
      IF (AGE.GE.AHARV.AND.AHARV.GT.0.0) GO TO 260                      COM1010
      IF (HS.GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 260                     COM1015
      IF(AGET(I+1).GT.0.0.OR.HY40(I+1).GT.0.0) GO TO 235
      IF(BAPA.LT.GL.AND.IGS.LE.0) GO TO 235
      IF(LIUT(I).GE.1) GO TO 235
      RD = BAPA / SQRT(DNOW)
      DELRD = RD - RDAFT1
      IF(RD.GE.50.0.AND.DELRD.GT.7.0) GO TO 240
  235 CONTINUE
      ARG(I+1) = SORG(IORG,HS)                                          COM1020
      TTN(I+1) = THN(ITHN,HS,HAT)                                       COM1025
      CALL GROWTH                                                       COM1030
      GO TO 220                                                         COM1035
C                                                                       COM1040
  240 CONTINUE                                                          COM1045
      IF (AGE.GE.AHARV.AND.AHARV.GT.0.0) GO TO 260                      COM1050
      IF (HS.GE.HTHARV.AND.HTHARV.GT.0.0) GO TO 260                     COM1055
      IF (I.EQ.NCT) GO TO 260                                           COM1060
  250 CONTINUE                                                          COM1065
  260 CONTINUE                                                          COM1070
      RETURN                                                            COM1075
      END                                                               COM1080-

      SUBROUTINE HARVST                                                 HAR   5
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16HAR  10
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), HAR  15
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     HAR  20
     $ TARESD(16), TA(16,4), VA(16,4)                                   HAR  25
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), HAR  30
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     HAR  35
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  HAR  40
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               HAR  45
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,HAR  50
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,HAR  55
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,HAR  60
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, HAR  65
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   HAR  70
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          HAR  75
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, HAR  80
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 HAR  85
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMHAR  90
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDHAR  95
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            HAR 100
      TOTAGE = TOAGE(AGE,SI)                                            HAR 105
      AHARR = TOTAGE                                                    HAR 110
      IF (LINE.GE.50) CALL HEADER                                       HAR 115
      JAGECK = TOTAGE + 0.5                                             HAR 116
      JAGE = AGE + 0.5                                                  HAR 117
      WRITE (6,10) JAGECK,JAGE                                          HAR 120
   10 FORMAT (1X,I3,1X,I3/)                                             HAR 125
      LINE = LINE+2                                                     HAR 130
      I = IZ                                                            HAR 135
      AGET(I+1) = AGE                                                   HAR 140
      D(I+1,1) = DNOW                                                   HAR 145
      TA(I+1,1) = XNUM                                                  HAR 150
      VA(I+1,1) = VNOW                                                  HAR 155
      GA(I+1,1) = BAPA                                                  HAR 160
      H40(I+1) = HS                                                     HAR 165
      HY40(I+1) = HS                                                    HAR 170
      IZ = IZ+1                                                         HAR 175
      CALL MERCHV                                                       HAR 180
      XMAI16 = (VA(I+1,1)+SCUTVA)/TOTAGE                                HAR 185
      XMAI56 = (VA456(I+1)+SCUT45)/TOTAGE                               HAR 190
      XMAI76 = (VA476(I+1)+SCUT47)/TOTAGE                               HAR 195
      ZMAI16 = (VA(I+1,1) + SCUTVA + SVMORT) / TOTAGE                   HAR 196
c      HTL = HTLOR(DNOW,VNOW,BAPA,XNUM,HS)                               HAR 200
      IF (LINE.GE.51) CALL HEADER                                       HAR 205
      WRITE (6,20) HS,DNOW,BAPA,XNUM,VNOW,CAI,ZMAI16,XMAI16,XMAI56,     HAR 210
     1 XMAI76                                                           HAR 211
   20 FORMAT (1X,7HHARVEST,1X,F6.1,   F7.2,F7.1,F7.0,F7.0,5F6.0/)       HAR 215
      SCUTBA = SCUTBA+BAPA                                              HAR 220
      SCUTTA = SCUTTA+XNUM                                              HAR 225
      SCUTVA = SCUTVA+VNOW                                              HAR 230
      SVC456(I+1) = SCUT45+VA456(I+1)                                   HAR 235
      SVC476(I+1) = SCUT47+VA476(I+1)                                   HAR 240
      WRITE (6,30) SCUTBA,SCUTTA,SCUTVA                                 HAR 245
   30 FORMAT (1X,8HSUM CUTS,14X,F6.1,F7.0,F7.0)                         HAR 250
      IF (SNMORT.LE.0.0) GO TO 40                                       HAR 255
      SDMORT = ((SGMORT/SNMORT)/.005454154)**.5                         HAR 260
      GO TO 50                                                          HAR 265
C                                                                       HAR 270
   40 CONTINUE                                                          HAR 275
      SDMORT = 0.0                                                      HAR 280
   50 CONTINUE                                                          HAR 285
      D(I+1,4) = SDMORT                                                 HAR 290
      GA(I+1,4) = SGMORT                                                HAR 295
      TA(I+1,4) = SNMORT                                                HAR 300
      VA(I+1,4) = SVMORT                                                HAR 305
      WRITE (6,60) SGMORT,SNMORT,SVMORT                                 HAR 310
   60 FORMAT (1X,13HSUM MORTALITY,9X,F6.1,F7.0,F7.0/)                   HAR 315
      LINE = 0                                                          HAR 320
      RETURN                                                            HAR 325
      END                                                               HAR 330-

      SUBROUTINE HTCAL                                                  HTC   5
C                                                                       HTC  10
C     COMPUTE IMPORTANT HEIGHTS FOR EXISTING STAND WITH INITIAL         HTC  15
C     QUADRATIC MEAN STAND DIAMETER LARGER THAN 5.55 INCHES             HTC  20
C                                                                       HTC  25
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16HTC  30
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), HTC  35
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     HTC  40
     $ TARESD(16), TA(16,4), VA(16,4)                                   HTC  45
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), HTC  50
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     HTC  55
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  HTC  60
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               HTC  65
      DIMENSION IAGET(16)                                               HTC  70
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,HTC  75
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,HTC  80
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,HTC  85
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, HTC  90
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   HTC  95
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          HTC 100
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, HTC 105
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 HTC 110
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMHTC 115
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDHTC 120
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            HTC 125
C                                                                       HTC 130
C     ************ DEFINITION OF IMPORTANT HTCAL VARIABLES ************ HTC 135
C                                                                       HTC 140
C     AGE    = BREAST HEIGHT STAND AGE                                  HTC 145
C     AGEADD = LENGTH OF GROWTH PERIOD                                  HTC 150
C     AGECK  = TOTAL STAND AGE                                          HTC 155
C     AGEF   = ARRAY OF STAND AGES AT TIMES OF NITROGEN FERTILIZER      HTC 160
C                   APPLICATIONS                                        HTC 165
C     AGEMID = BREAST HEIGHT STAND AGE AT MIDPOINT OF GROWTH PERIOD     HTC 170
C     AGET   = ARRAY OF STAND AGES AT TIMES OF CUTTING                  HTC 175
C     EAGE   = EARLIEST BREAST HEIGHT STAND AGE IMPLIED BY CONTROL DECK HTC 180
C                   FOR EXISTING STAND OVER 5.55-INCHES DBH             HTC 185
C     FERTEF = NITROGEN FERTILIZER EFFECT                               HTC 190
C     HAT    = STAND TOP HEIGHT AFTER MOST RECENT THINNING              HTC 195
C     HPCT   = STAND TOP HEIGHT AFTER PRECOMMERCIAL THINNING            HTC 200
C     HS     = STAND TOP HEIGHT                                         HTC 205
C     HTADD  = STAND TOP HEIGHT INCREMENT                               HTC 210
C     IOBS   = NUMBER OF SPECIFIED STAND TOP HEIGHT OBSERVATIONS        HTC 215
C     ITHN   = INDICATOR SWITCH FOR THINNING DONE                       HTC 220
C     OBSAGE = ARRAY OF STAND AGES FOR SPECIFIED STAND TOP HEIGHT       HTC 225
C                   OBSERVATIONS                                        HTC 230
C     OBSHTS = ARRAY OF SPECIFIED STAND TOP HEIGHT OBSERVATIONS         HTC 235
C     PCT    = TOTAL STAND AGE AT PRECOMMERCIAL THINNING                HTC 240
C     SAGE   = EXISTING STAND TOTAL STAND AGE                           HTC 245
C     SI     = SITE INDEX (50-YEAR B H BASE AGE)                        HTC 250
C     ***************************************************************** HTC 255
C                                                                       HTC 260
C                                                                       HTC 265
C     CHECK FOR EARLIEST AGE (TOTAL AGE BASIS)                          HTC 270
C                                                                       HTC 275
C     EXISTING STAND INITIAL AGE                                        HTC 280
C                                                                       HTC 285
      EAGE = SAGE                                                       HTC 290
      DO 10 I=1,16                                                      HTC 295
C                                                                       HTC 300
C     SPECIFIED COMMERCIAL THINNING AGES                                HTC 305
C                                                                       HTC 310
      IF (AGET(I).GT.0.0.AND.AGET(I).LT.EAGE) EAGE = AGET(I)            HTC 315
C                                                                       HTC 320
C     SPECIFIED FERTILIZER APPLICATION AGES                             HTC 325
C                                                                       HTC 330
      IF (AGEF(I).GT.0.0.AND.AGEF(I).LT.EAGE) EAGE = AGEF(I)            HTC 335
   10 CONTINUE                                                          HTC 340
C                                                                       HTC 345
C     PRECOMMERCIAL THINNING AGE                                        HTC 350
C                                                                       HTC 355
      IF (PCT.GT.0.0.AND.PCT.LT.EAGE) EAGE = PCT                        HTC 360
C                                                                       HTC 365
C     CALCULATE AND STORE IMPORTANT HEIGHTS                             HTC 370
C                                                                       HTC 375
      AGE = BHAGE(EAGE,SI)                                              HTC 380
      HS = HEIGHT(SI,AGE,IOBS,OBSAGE,OBSHTS)                            HTC 385
      ISAGE = SAGE*10.+.5                                               HTC 390
      DO 20 IW=1,16                                                     HTC 395
      IAGET(IW) = AGET(IW)*10.+.5                                       HTC 400
   20 CONTINUE                                                          HTC 405
   30 CONTINUE                                                          HTC 410
      IEAGE = EAGE*10.+.5                                               HTC 415
      IPCT = PCT*10.+.5                                                 HTC 420
      IF (IEAGE.EQ.IPCT) HPCT = HS                                      HTC 425
      IF (IEAGE.EQ.IPCT) ITHN = 1                                       HTC 430
      DO 40 IW=1,16                                                     HTC 435
      IF (IAGET(IW).EQ.IEAGE) HAT = HS                                  HTC 440
      IF (IAGET(IW).EQ.IEAGE) ITHN = 1                                  HTC 445
   40 CONTINUE                                                          HTC 450
      IF (ISAGE.EQ.IEAGE) GO TO 50                                      HTC 455
      AGE = BHAGE(EAGE,SI)                                              HTC 460
      AGEMID = AGE+AGEADD/2.0                                           HTC 465
      CALL XFERT (AGEMID)                                               HTC 470
      HTADD = HTGROW(SI,AGE,AGEADD,FERTEF,IOBS,OBSAGE,OBSHTS)           HTC 475
      HS = HS+HTADD                                                     HTC 480
      EAGE = EAGE+AGEADD                                                HTC 485
      GO TO 30                                                          HTC 490
C                                                                       HTC 495
   50 CONTINUE                                                          HTC 500
C                                                                       HTC 505
C     CHECK FOR COMMERCIAL THINNING AND RESIDUAL BASAL AREAS BEFORE     HTC 510
C     EXISTING STAND AGE AND REPOSITION ARRAYS FOR BEGINNING OF         HTC 515
C     SIMULATION IF NEEDED                                              HTC 520
C                                                                       HTC 525
      IF (AGET(1).EQ.0.0) GO TO 110                                     HTC 530
      IF (AGET(1).GT.0.0.AND.AGET(1).LT.SAGE) ICT1 = 1                  HTC 535
      RD = SBA/SQRT(SDIA)                                               HTC 540
      IF (ICT1.EQ.1.AND.RD.GE.60.0) CT1 = 1.0                           HTC 545
   60 CONTINUE                                                          HTC 550
      IF (AGET(1).GE.SAGE) GO TO 80                                     HTC 555
      DO 70 I=1,15                                                      HTC 560
      AGET(I) = AGET(I+1)                                               HTC 565
      GRESD(I) = GRESD(I+1)                                             HTC 570
      IF (AGET(16).EQ.0.0) GO TO 70                                     HTC 575
      AGET(16) = 0.0                                                    HTC 580
      GRESD(16) = 0.0                                                   HTC 585
   70 CONTINUE                                                          HTC 590
      GO TO 60                                                          HTC 595
C                                                                       HTC 600
   80 CONTINUE                                                          HTC 605
      DO 90 I=1,16                                                      HTC 610
      IF (AGET(I).EQ.0.0) GO TO 100                                     HTC 615
   90 CONTINUE                                                          HTC 620
  100 CONTINUE                                                          HTC 625
      NCT = I-1                                                         HTC 630
      FCT = AGET(1)                                                     HTC 635
      FCTA = BHAGE(FCT,SI)                                              HTC 640
  110 CONTINUE                                                          HTC 645
      ARG(1) = SORG(IORG,HS)                                            HTC 650
      TTN(1) = THN(ITHN,HS,HAT)                                         HTC 655
      XNUM = STA                                                        HTC 660
      BAPA = SBA                                                        HTC 665
      DNOW = SDIA                                                       HTC 670
      AGE = BHAGE(SAGE,SI)                                              HTC 675
      VGR = VGRAT(DNOW,HS,AGE,TTN(1),ARG(1),BAPA)                       HTC 680
      VNOW = VGR*BAPA                                                   HTC 685
      IF (RAN(1).EQ.0.0) GO TO 130                                      HTC 690
      DO 120 IQ=1,76                                                    HTC 695
      IF (RAN(IQ).GE.SAGE) GO TO 130                                    HTC 700
  120 CONTINUE                                                          HTC 705
  130 CONTINUE                                                          HTC 710
      HOD = HS/DNOW                                                     HTC 715
      IF (HOD.GT.5.72.AND.HOD.LT.12.02) GO TO 150                       HTC 720
      WRITE (6,140) HOD                                                 HTC 725
  140 FORMAT (1X,39(2H**)/57H WARNING -- EXISTING STAND HEIGHT OVER DIAMHTC 730
     $ETER RATIO OF ,F5.2,13H AT BEGINNING/66H OF SIMULATION IS 3 OR MORHTC 735
     $E STANDARD ERRORS FROM REGIONAL AVERAGE./1X,39(2H**))             HTC 740
      LINE = LINE+4                                                     HTC 745
  150 CONTINUE                                                          HTC 750
      RETURN                                                            HTC 755
      END                                                               HTC 760-

      SUBROUTINE MERCHV                                                 MEV   5
C                                                                       MEV  10
C     COMPUTE QUADRATIC MEAN STAND DIAMETER, BASAL AREA PER ACRE, AND   MEV  15
C     CUBIC-FOOT VOLUME (TOTAL STEM AND 4-INCH TOP) PER ACRE FOR LIVE   MEV  20
C     PORTION OF STAND LARGER THAN 5.6-INCHES AND 7.6-INCHES DBH        MEV  25
C                                                                       MEV  30
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16MEV  35
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), MEV  40
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     MEV  45
     $ TARESD(16), TA(16,4), VA(16,4)                                   MEV  50
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), MEV  55
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     MEV  60
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  MEV  65
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               MEV  70
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,MEV  75
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,MEV  80
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,MEV  85
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, MEV  90
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   MEV  95
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          MEV 100
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, MEV 105
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 MEV 110
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMMEV 115
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDMEV 120
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            MEV 125
      IF(DNOW.LT.14.0) R = 0.0                                          MEV 126
      TTHN = TTN(IZ)                                                    MEV 130
      IF (IZ.EQ.1.OR.LIUT(IZ).GE.2) TTHN = 0.0                          MEV 135
      D56(IZ) = DIAM56(ARG(IZ),TTHN,DNOW,BAPA,HS)                       MEV 140
      G56(IZ) = BAPA56(ARG(IZ),DNOW,BAPA,HS)                            MEV 145
      IF (D56(IZ).GT.DNOW.AND.G56(IZ).LT.BAPA) GO TO 10                 MEV 150
      D56(IZ) = DNOW                                                    MEV 155
      G56(IZ) = BAPA                                                    MEV 160
      VA56(IZ) = VNOW                                                   MEV 165
      GO TO 20                                                          MEV 170
C                                                                       MEV 175
   10 CONTINUE                                                          MEV 180
      VA56(IZ) = CVTS56(ARG(IZ),DNOW,HS,VNOW)                           MEV 185
   20 CONTINUE                                                          MEV 190
      VA456(IZ) = CV456(ARG(IZ),TTHN,DNOW,BAPA,HS,VNOW,VA56(IZ))        MEV 195
      D76(IZ) = DIAM76(ARG(IZ),TTHN,DNOW,BAPA,HS,D56(IZ))               MEV 200
      G76(IZ) = BA76(ARG(IZ),TTHN,DNOW,BAPA,HS,G56(IZ))                 MEV 205
      IF (D76(IZ).GT.D56(IZ).AND.G76(IZ).LT.G56(IZ)) GO TO 30           MEV 210
      D76(IZ) = D56(IZ)                                                 MEV 215
      G76(IZ) = G56(IZ)                                                 MEV 220
      VA76(IZ) = VA56(IZ)                                               MEV 225
      VA476(IZ) = VA456(IZ)                                             MEV 230
      RATMAI = 1.0                                                      MEV 231
      GO TO 40                                                          MEV 235
C                                                                       MEV 240
   30 CONTINUE                                                          MEV 245
      VA76(IZ) = CVTS76(ARG(IZ),DNOW,BAPA,HS,VNOW,VA56(IZ))             MEV 250
      VA476(IZ) = CV476(ARG(IZ),TTHN,DNOW,BAPA,HS,VNOW,VA76(IZ),VA456(IZMEV 255
     $))                                                                MEV 260
      IF(DNOW.LT.14.0) GO TO 40                                         MEV 261
      IF(R.LE.0.0) D14 = DNOW                                           MEV 262
      IF(R.LE.0.0) R = VA476(IZ) / VA456(IZ)                            MEV 263
      RATMAI = R+(1.0-R)/(18.0-D14)*(DNOW-D14)                          MEV 264
      VA476(IZ) = VA456(IZ) * RATMAI                                    MEV 265
   40 CONTINUE                                                          MEV 266
      RETURN                                                            MEV 270
      END                                                               MEV 275-

      SUBROUTINE MERCHT                                                 MET   5
C                                                                       MET  10
C     COMPUTE QUADRATIC MEAN DIAMETER, BASAL AREA PER ACRE, AND         MET  15
C     CUBIC-FOOT VOLUME (TOTAL STEM AND 4-INCH TOP) PER ACRE OF CUT     MET  20
C     TREES FOR PORTION OF STAND LARGER THAN 5.6-INCHES AND 7.6-INCHES  MET  25
C     DBH                                                               MET  30
C                                                                       MET  35
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16MET  40
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), MET  45
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     MET  50
     $ TARESD(16), TA(16,4), VA(16,4)                                   MET  55
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), MET  60
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     MET  65
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  MET  70
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               MET  75
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,MET  80
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,MET  85
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,MET  90
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, MET  95
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   MET 100
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          MET 105
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, MET 110
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 MET 115
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMMET 120
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDMET 125
      COMMON /CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            MET 130
      IF (D56(IZ).GT.D(IZ,1)) GO TO 10                                  MET 135
      DC56(IZ) = D(IZ,2)                                                MET 140
      GC56(IZ) = GA(IZ,2)                                               MET 145
      VAC56(IZ) = VA(IZ,2)                                              MET 150
      GO TO 20                                                          MET 155
C                                                                       MET 160
   10 CONTINUE                                                          MET 165
      DC56(IZ) = DCUT56(ARG(IZ),D(IZ,2),HS)                             MET 170
      GC56(IZ) = BAC56(D(IZ,2),GA(IZ,2),HS)                             MET 175
      VAC56(IZ) = CVC56(D(IZ,2),HS,D(IZ,1),GA(IZ,1),VA(IZ,2))           MET 180
   20 CONTINUE                                                          MET 185
      VAC456(IZ) = CVC456(D(IZ,2),HS,VA(IZ,2),VAC56(IZ))                MET 190
      IF (DC56(IZ).LE.0.0.OR.GC56(IZ).LE.0.0.OR.VAC56(IZ).LE.0.0) GO TO MET 195
     $30                                                                MET 200
      GO TO 40                                                          MET 205
C                                                                       MET 210
   30 CONTINUE                                                          MET 215
      VAC56(IZ) = 0.0                                                   MET 220
      VAC456(IZ) = 0.0                                                  MET 225
      DC56(IZ) = 0.0                                                    MET 230
      GC56(IZ) = 0.0                                                    MET 235
      VAC76(IZ) = 0.0                                                   MET 240
      VAC476(IZ) = 0.0                                                  MET 245
      DC76(IZ) = 0.0                                                    MET 250
      GC76(IZ) = 0.0                                                    MET 255
   40 CONTINUE                                                          MET 260
      IF (VAC56(IZ).LE.0.0) GO TO 80                                    MET 265
      IF (D76(IZ).GT.D56(IZ)) GO TO 60                                  MET 270
   50 CONTINUE                                                          MET 275
      DC76(IZ) = DC56(IZ)                                               MET 280
      GC76(IZ) = GC56(IZ)                                               MET 285
      VAC76(IZ) = VAC56(IZ)                                             MET 290
      VAC476(IZ) = VAC456(IZ)                                           MET 295
      GO TO 90                                                          MET 300
C                                                                       MET 305
   60 CONTINUE                                                          MET 310
      DC76(IZ) = DCUT76(D(IZ,2),HS,DC56(IZ))                            MET 315
      GC76(IZ) = BAC76(D(IZ,2),GA(IZ,2),HS,GC56(IZ))                    MET 320
      VAC76(IZ) = CVC76(D(IZ,2),HS,VA(IZ,2),VAC56(IZ))                  MET 325
      VAC476(IZ) = CVC476(D(IZ,2),HS,VA(IZ,2),VAC76(IZ),VAC456(IZ))     MET 330
      IF (DC76(IZ).LE.0.0.OR.GC76(IZ).LE.0.0.OR.VAC76(IZ).LE.0.0) GO TO MET 335
     $70                                                                MET 340
      GO TO 80                                                          MET 345
C                                                                       MET 350
   70 CONTINUE                                                          MET 355
      DC76(IZ) = 0.0                                                    MET 360
      GC76(IZ) = 0.0                                                    MET 365
      VAC76(IZ) = 0.0                                                   MET 370
      VAC476(IZ) = 0.0                                                  MET 375
   80 CONTINUE                                                          MET 380
      IF (DC76(IZ).EQ.DC56(IZ).OR.GC76(IZ).EQ.GC56(IZ)) GO TO 50        MET 385
   90 CONTINUE                                                          MET 390
      VAA56(IZ) = VA56(IZ)-VAC56(IZ)                                    MET 395
      VAA456(IZ) = VA456(IZ)-VAC456(IZ)                                 MET 400
      VAA76(IZ) = VA76(IZ)-VAC76(IZ)                                    MET 405
      VAA476(IZ) = VA476(IZ)-VAC476(IZ)                                 MET 410
      IF(DNOW.LT.14.0) GO TO 100                                        MET 411
      VAA476(IZ) = VAA456(IZ) * RATMAI                                  MET 412
      VAC476(IZ) = VA476(IZ) - VAA476(IZ)                               MET 413
  100 CONTINUE                                                          MET 414
      RETURN                                                            MET 415
      END                                                               MET 420-

      SUBROUTINE VOLCON                                                 VOL   5
C                                                                       VOL  10
C     5.6-INCH PLUS AND 7.6-INCH PLUS YIELD TABLE ROUTINE               VOL  15
C                                                                       VOL  20
C     IUIN IS INPUT FILE - TAPE5
C     IUOUT IS OUTPUT - TAPE6
C
      DIMENSION AGEF(16), AGET(16), ARG(16), DR(16), FNLBS(16), GRESD(16VOL  25
     $), H40(16), HY40(16), ITITLE(20), LIUT(16), OBSAGE(31), PNIT(16), VOL  30
     $PNITF(16), RAN(76), TTN(16), D(16,4), GA(16,4), OBSHTS(2,31),     VOL  35
     $ TARESD(16), TA(16,4), VA(16,4)                                   VOL  40
      DIMENSION D56(16), D76(16), DC56(16), DC76(16), G56(16), G76(16), VOL  45
     $GC56(16), GC76(16), VA56(16), VA456(16), VA76(16), VA476(16),     VOL  50
     $ VAA56(16), VAA456(16), VAA76(16), VAA476(16), VAC56(16), VAC456  VOL  55
     $(16), VAC76(16), VAC476(16), SVC456(16), SVC476(16)               VOL  60
      DIMENSION XN56(16), XNC56(16), VAM56(16), VAM76(16), DA56(16),    VOL  65
     $ DA76(16)                                                         VOL  70
      COMMON AGE,AGEF,AGET,AHARR,AHARV,ARG,ASYMP,BAB,BAC,BAPA,CAI,CT1,D,VOL  75
     $DDMRT,DLIM,DMTMIN,DNOW,DR,FCTA,FERTEF,FNLBS,GA,GRESD,H40,HAT,HPCT,VOL  80
     $HS,HTHARV,HY40,ICT1,IDR,IOBS,IORG,IPAGE,IQ,IRD,ITHN,ITITLE,IUT,IZ,VOL  85
     $JRD,LINE,LIUT,LTREE,NRA,OBSAGE,OBSHTS,TARESD,PCT,PCTA,PNIT,PNITF, VOL  90
     $RAN,SAGE,SBA,SCUTVA,SDIA,SGMORT,SI,SNMORT,STA,SVMORT,TA,TTN,VA,   VOL  95
     $VGNOW,VNOW,XMAI16,XMAI56,XMAI76,XNUM,XNUMR,ISTOP,RATMAI,          VOL 100
     $rdasymp,isv
      COMMON D56,D76,DC56,DC76,G56,G76,GC56,GC76,VA56,VA456,VA76,VA476, VOL 105
     $VAA56,VAA456,VAA76,VAA476,VAC56,VAC456,VAC76,VAC476,SVC456,SVC476 VOL 110
      COMMON CTAS,DC,DH,EXIST,FCT,FERTAS,FERTCT,FIH,IHEAD,FIY,GCLIM,GLIMVOL 115
     $,HFCT,IGS,IMV,NCT,SCTHT,SCUT45,SCUT47,SCUTBA,SCUTTA,TAST,TI,AGEADDVOL 120
      common year,month,day,hour,minute,second,i100th
      integer*2 year,month,day,hour,minute,second,i100th
      COMMON/CHAR/ CTITLE,vers
      CHARACTER CTITLE*80,vers*8
      COMMON /ECONO/ A,B,C,COSTAB,ECO,EUNITS,FC,FCI,FHRC,FHRCI,
     $HAULC1,HC,HCI,IFER,
     $LCI,LCM,LDF,LDI,LOGC1,LOGCF(12,12),LOGCI(12,12),
     $LOGDF(12),LOGDI(12),LOGVF(12),LOGVI(12),LVF,LVI,
     $MAH,NETRV1,OAHC,OAHCI,OATC,OTHRC1,OVHC,OVHCI,
     $OVTC,PCTC,PCTCI,PNW1,PNW2,PNDVL1,PONDVL(8,2),PONTAB,
     $PVI,R,RCI,REGENC,SEV,TRUED,TYHV,VOLM,FAGE(15,2),CNIT(15,2)
     $ ,DNR,VDEAD,CVDEAD,VOLUME,CUMVOL,FALL,IMORT,CMORT
       COMMON/EMORT/MORTE(16,4),DMORT,GMORT,TAMORT,VMORTE
      REAL MORTE
      REAL LOGC1,LOGCF,LOGCI,LOGDF,LOGDI,LOGVF,LOGVI,MAH,NETRV1,LCI,LCM
      INTEGER COSTAB,ECO,EUNITS,PONTAB,TYHV
      INTEGER CTAS,EXIST,FERTAS,FERTCT,SCTHT                            VOL 125
C  EEEEEEEEEE
C                                                                       VOL 130
C     ************ DEFINITION OF INPORTANT VOLCON VARIABLES *********** VOL 135
C                                                                       VOL 140
C     AGET   = ARRAY OF BREAST HEIGHT STAND AGES AT COMMERCIAL CUTTING  VOL 145
C     AGEXXX = TOTAL STAND AGE                                          VOL 150
C     AHARR  = TOTAL STAND AGE AT HARVEST CUT                           VOL 155
C     ARG    = ARRAY OF PLANTATION DECAY VARIABLES                      VOL 160
C     D      = ARRAY OF QUADRATIC MEAN DIAMETERS FOR LIVE STAND BEFORE  VOL 165
C                   CUT, CUT, AFTER CUT, AND MORTALITY OF TREES 1.6-    VOL 170
C                   INCHES + DBH FOR EACH COMMERCIAL CUTTING            VOL 175
C     D56    = ARRAY OF QUADRATIC MEAN STAND DIAMETERS OF TREES 5.6-    VOL 180
C                   INCHES + DBH FOR EACH COMMERCIAL CUTTING            VOL 185
C     D76    = QUADRATIC MEAN STAND DIAMETER OF TREES 7.6-INCHES + DBH  VOL 190
C     DA56   = ARRAY OF QUADRATIC MEAN STAND DIAMETERS OF TREES 5.6-INCHVOL 195
C                   + DBH AFTER COMMERCIAL THINNING FOR EACH COMMERCIAL VOL 200
C                   THINNING                                            VOL 205
C     DA76   = QUADRATIC MEAN STAND DIAMETER OF TREES 7.6-INCHES + DBH  VOL 210
C                   AFTER COMMERCIAL THINNING                           VOL 215
C     DC56   = ARRAY OF QUADRATIC MEAN DIAMETERS OF CUT TREES 5.6-INCHESVOL 220
C                   + DBH FOR EACH COMMERCIAL THINNING                  VOL 225
C     DC76   = QUADRATIC MEAN DIAMETER OF CUT TREES 7.6-INCHES + DBH    VOL 230
C     G56    = ARRAY OF BASAL AREAS PER ACRE OF TREES 5.6-INCHES + DBH  VOL 235
C                   BEFORE EACH COMMERCIAL THINNING                     VOL 240
C     G76    = BASAL AREA PER ACRE OF TREES 7.6-INCHES + DBH BEFORE     VOL 245
C                   COMMERCIAL THINNING                                 VOL 250
C     GA     = ARRAY OF BASAL AREAS PER ACRE FOR LIVE STAND BEFORE      VOL 255
C                   CUT, CUT, AFTER CUT, AND MORTALITY OF TREES         VOL 260
C                   1.6-INCHES + DBH FOR EACH COMMERCIAL CUTTING        VOL 265
C     GA56   = BASAL AREAS PER ACRE OF TREES 5.6-INCHES + DBH AFTER     VOL 270
C                   EACH COMMERCIAL THINNING                            VOL 275
C     GAA76  = BASAL AREA PER ACRE OF TREES 7.6-INCHES + DBH AFTER      VOL 280
C                   COMMERCIAL THINNING                                 VOL 285
C     GC56   = ARRAY OF BASAL AREAS PER ACRE CUT IN TREES 5.6-INCHES    VOL 290
C                   + DBH FOR EACH COMMERCIAL CUTTING                   VOL 295
C     GC76   = BASAL AREA PER ACRE CUT IN TREES 7.6-INCHES + DBH        VOL 300
C     H40    = ARRAY OF STAND TOP HEIGHTS FOR EACH COMMERCIAL CUTTING   VOL 305
C     I56    = INDICATOR SWITCH FOR PRINTING 5.6-INCH + DBH YIELD TABLE VOL 310
C     I76    = INDICATOR SWITCH FOR PRODUCING 7.6-INCH + DBH YIELD TABLEVOL 315
C     IE     = COMMERCIAL THINNING NUMBER                               VOL 320
C     IMV    = INDICATOR SWITCH FOR PRODUCING 5.6- AND 7.6-INCH YIELD   VOL 325
C                   TABLES                                              VOL 330
C     IPAGE  = NUMBER OF PAGES PRINTED FOR YIELD TABLE                  VOL 335
C     ITITLE = ARRAY FOR USER SUPPLIED TITLE TO BE PRINTED AT TOP OF    VOL 340
C                   EACH PAGE OF THE YIELD TABLE                        VOL 345
C     LINE   = NUMBER OF LINES PRINTED ON CURRENT PAGE OF YIELD TABLE   VOL 350
C     LIUT   = ARRAY OF COMMERCIAL THINNING STATUS FOR EACH COMMERCIAL  VOL 355
C                   THINNING SCHEDULED OR DONE                          VOL 360
C     SCI676 = CUMULATIVE SUM OF INTERNATIONAL 1/4-INCH BOARD-FOOT      VOL 365
C                   VOLUME (6-INCH TOP) PER ACRE CUT IN TREES 7.6-      VOL 370
C                   INCHES + DBH                                        VOL 375
C     SCS676 = CUMULATIVE SUM OF SCRIBNER BOARD-FOOT VOLUME (6-INCH     VOL 380
C                   TOP) PER ACRE CUT IN TREES 7.6-INCHES + DBH         VOL 385
C     SGC56  = CUMULATIVE SUM OF BASAL AREA PER ACRE CUT IN TREES 5.6-  VOL 390
C                   INCHES + DBH                                        VOL 395
C     SGC76  = CUMULATIVE SUM OF BASAL AREA PER ACRE CUT IN TREES 7.6-INVOL 400
C                   INCHES + DBH                                        VOL 405
C     SI     = SITE INDEX (50-YEAR B H BASE AGE)                        VOL 410
C     SVAC56 = CUMULATIVE SUM OF CUBIC-FOOT VOLUME (TOTAL STEM) PER ACREVOL 415
C                   CUT IN TREES 5.6-INCHES + DBH                       VOL 420
C     SVAC76 = CUMULATIVE SUM OF CUBIC-FOOT VOLUME (TOTAL STEM) PER ACREVOL 425
C                   CUT IN TREES 7.6-INCHES + DBH                       VOL 430
C     SVC456 = CUMULATIVE SUM OF CUBIC-FOOT VOLUME (4-INCH TOP) PER ACREVOL 435
C                   CUT IN TREES 5.6-INCHES + DBH                       VOL 440
C     SVC476 = CUMULATIVE SUM OF CUBIC-FOOT VOLUME (4-INCH TOP) PER ACREVOL 445
C                   CUT IN TREES 7.6-INCHES + DBH                       VOL 450
C     SVC676 = CUMULATIVE SUM OF CUBIC-FOOT VOLUME (6-INCH TOP) PER ACREVOL 455
C                   CUT IN TREES 7.6-INCHES + DBH                       VOL 460
C     SXNC56 = CUMULATIVE SUM OF NUMBER OF TREES PER ACRE CUT IN TREES  VOL 465
C                   5.6-INCHES + DBH                                    VOL 470
C     SXNC76 = CUMULATIVE SUM OF NUMBER OF TREES PER ACRE CUT IN TREES  VOL 475
C                   7.6-INCHES + DBH                                    VOL 480
C     TTN    = ARRAY OF THINNING DECAY VARIABLES                        VOL 485
C     VA56   = ARRAY OF CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREESVOL 490
C                   5.6-INCHES + DBH BEFORE EACH COMMERCIAL CUTTING     VOL 495
C     VA76   = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES 7.6-INCHVOL 500
C                   + DBH BEFORE COMMERCIAL THINNING                    VOL 505
C     VA456  = ARRAY OF CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE IN TREESVOL 510
C                   5.6-INCHES + DBH BEFORE EACH COMMERCIAL CUTTING     VOL 515
C     VA476  = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE IN TREES 7.6-    VOL 520
C                   INCHES + DBH BEFORE COMMERCIAL CUTTING              VOL 525
C     VA676  = CUBIC-FOOT VOLUME (6-INCH TOP) PER ACRE IN TREES 7.6-    VOL 530
C                   INCHES + DBH BEFORE COMMERCIAL CUTTING              VOL 535
C     VAA56  = CUBIC-FOOT VOLUMS (TOTAL STEM) PER ACRE IN TREES         VOL 540
C                   5.6-INCHES + DBH AFTER COMMERCIAL THINNING          VOL 545
C     VAA76  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES 7.6-    VOL 550
C                   INCHES + DBH AFTER COMMERCIAL THINNING              VOL 555
C     VAA456 = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE IN TREES 5.6-    VOL 560
C                   INCHES + DBH AFTER COMMERCIAL THINNING              VOL 565
C     VAA476 = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE IN TREES 7.6-    VOL 570
C                   INCHES + DBH AFTER COMMERCIAL THINNING              VOL 575
C     VAA676 = CUBIC-FOOT VOLUME (6-INCH TOP) PER ACRE IN TREES 7.6 -   VOL 580
C                   INCHES + DBH AFTER COMMERCIAL THINNING              VOL 585
C     VAC56  = ARRAY OF CUBIC-FOOT VOLUMES (TOTAL STEM) PER ACRE CUT    VOL 590
C                   IN TREES 5.6-INCHES + DBH FOR EACH COMMERCIAL       VOL 595
C                   CUTTING                                             VOL 600
C     VAC76  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE CUT IN TREES     VOL 605
C                   7.6-INCHES + DBH                                    VOL 610
C     VAC456 = ARRAY OF CUBIC-FOOT VOLUMES (4-INCH TOP) PER ACRE CUT    VOL 615
C                   IN TREES 5.6-INCHES + DBH FOR EACH COMMERCIAL       VOL 620
C                   CUTTING                                             VOL 625
C     VAC476 = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE CUT IN TREES     VOL 630
C                   7.6-INCHES + DBH                                    VOL 635
C     VAC676 = CUBIC-FOOT VOLUME (6-INCH TOP) PER ACRE CUT IN TREES     VOL 640
C                   7.6-INCHES + DBH                                    VOL 645
C     VAI676 = INTERNATIONAL 1/4-INCH BOARD-FOOT VOLUME (6-INCH TOP)    VOL 650
C                   PER ACRE IN TREES 7.6-INCHES + DBH AFTER COMMERCIAL VOL 655
C                   THINNING                                            VOL 660
C     VAM56  = ARRAY OF CUBIC-FOOT VOLUMES (TOTAL STEM) PER ACRE IN     VOL 665
C                   MORTALITY TREES 5.6-INCHES + DBH                    VOL 670
C     VAM76  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN MORTALITY     VOL 675
C                   TREES 7.6-INCHES + DBH                              VOL 680
C     VAS676 = SCRIBNER BOARD-FOOT VOLUME (6-INCH TOP) PER ACRE IN      VOL 685
C                   TREES 7.6-INCHES + DBH AFTER COMMERCIAL THINNING    VOL 690
C     VCI676 = INTERNATIONAL 1/4-INCH BOARD-FOOT VOLUME (6-INCH TOP)    VOL 695
C                   PER ACRE CUT IN TREES 7.6-INCHES + DBH              VOL 700
C     VCS676 = SCRIBNER BOARD-FOOT VOLUME (6-INCH TOP) PER ACRE CUT     VOL 705
C                   IN TREES 7.6-INCHES + DBH                           VOL 710
C     VS676  = SCRIBNER BOARD-FOOT VOLUME (6-INCH TOP) PER ACRE IN      VOL 715
C                   TREES 7.6-INCHES + DBH BEFORE COMMERCIAL CUTTING    VOL 720
C     XN56   = ARRAY OF NUMBER OF TREES PER ACRE 5.6-INCHES + DBH       VOL 725
C                   BEFORE EACH COMMERCIAL CUTTING                      VOL 730
C     XN76   = NUMBER OF TREES PER ACRE 7.6-INCHES + DBH BEFORE         VOL 735
C                   COMMERCIAL CUTTING                                  VOL 740
C     XNA56  = NUMBER OF TREES PER ACRE 5.6-INCHES + DBH AFTER          VOL 745
C                   COMMERCIAL THINNING                                 VOL 750
C     XNA76  = NUMBER OF TREES PER ACRE 7.6-INCHES + DBH AFTER          VOL 755
C                   COMMERCIAL CUTTING                                  VOL 760
C     XNC56  = ARRAY OF NUMBER OF TREES PER ACRE CUT 5.6-INCHES + DBH   VOL 765
C                   FOR EACH COMMERCIAL CUTTING                         VOL 770
C     XNC76  = NUMBER OF TREES PER ACRE CUT 7.6-INCHES + DBH            VOL 775
C     YI676  = INTERNATIONAL 1/4-INCH BOARD-FOOT VOLUME (6-INCH TOP)    VOL 780
C                   PER ACRE IN TREES 7.6-INCHES + DBH BEFORE           VOL 785
C                   COMMERCIAL CUTTING                                  VOL 790
C     ***************************************************************** VOL 795
C                                                                       VOL 800
C  EEEEEEEEEE
C  SEE SUBROUTINE ECON FOR DEFINITION OF IMPORTANT ECON VARIABLES
      IUIN=3
      IUOUT=6
C
C  EEEEEEEEEE
      DO 10 IE=1,16                                                     VOL 805
      VAM56(IE) = 0.0                                                   VOL 810
      XN56(IE) = 0.0                                                    VOL 815
      XNC56(IE) = 0.0                                                   VOL 820
      VAM76(IE) = 0.0                                                   VOL 825
      DA56(IE) = 0.0                                                    VOL 830
      DA76(IE) = 0.0                                                    VOL 835
   10 CONTINUE                                                          VOL 840
      SGC56 = 0.0                                                       VOL 845
      SXNC56 = 0.0                                                      VOL 850
      SVAC56 = 0.0                                                      VOL 855
      SGC76 = 0.0                                                       VOL 860
      SXNC76 = 0.0                                                      VOL 865
      SVAC76 = 0.0                                                      VOL 870
      SVC676 = 0.0                                                      VOL 875
      SCI676 = 0.0                                                      VOL 880
      SCS676 = 0.0                                                      VOL 885
      AGEXXX=0.0
C                                                                       VOL 890
C     DETERMINE WHICH YIELD TABLES TO PRODUCE                           VOL 895
C                                                                       VOL 900
      I56 = 1                                                           VOL 905
      I76 = 1                                                           VOL 910
      IF (IMV.EQ.2) I76 = 0                                             VOL 915
      IF (IMV.EQ.3) I56 = 0                                             VOL 920
      IF (I56.EQ.0) GO TO 30                                            VOL 925
C
C  EEEEEEEEEE
C     WRITE CHOSEN ECONOMICS VALUES
      IF (ECO.LE.0.OR.EUNITS.NE.1) GO TO 18
      WRITE (IUOUT,15) month,day,year,hour,minute,second,
     $ VERS,IPAGE,CTITLE,rdasymp/0.94,SI
   15 FORMAT (68x,i2.2,1h-,i2.2,1h-,i4.4,/69x,i2.2,1h:,i2.2,1h:,i2.2/
     $29X,20HD F S I M   VERSION ,a8,12X,6H PAGE ,I2/1X,A/,30X,         HDR 156
     $20H MANAGED YIELD TABLE/32X,16H FOR DOUGLAS-FIR/32X,16H 1.6 INCHESHDR 165
     $ PLUS/25x,31h ASYMPTOTIC RELATIVE DENSITY = ,F5.1//25X,14H SITE INHDR 166
     $DEX = ,F5.0,14H (50 YEARS BH)//)
      IPAGE=IPAGE+2
      TYHV=0
      CALL EOUT
   18 CONTINUE
C  EEEEEEEEEE
C
     
      WRITE (iuout,20) month,day,year,hour,minute,second,
     $ VERS,IPAGE,CTITLE,rdasymp/0.94,SI                                HDR 155
   20 FORMAT (1h1,67x,i2.2,1h-,i2.2,1h-,i4.4,/69x,i2.2,1h:,i2.2,1h:,i2.2 
     $/29X,20HD F S I M   VERSION ,a8,12X,6H PAGE ,I2,/1X,              HDR 156
     $A/,30X,                                                           HDR 157
     $20H MANAGED YIELD TABLE/32X,16H FOR DOUGLAS-FIR/32X,16H 5.6 INCHESHDR 165
     $ PLUS/25x,31h ASYMPTOTIC RELATIVE DENSITY = ,F5.1//25X,14H SITE INHDR 166
     $DEX = ,F5.0,14H (50 YEARS BH)//                                   HDR 170
     $24X,40HBASAL AREA  TREES    TOTAL    CUBIC FEET/18X,45HDBH    PER 
     $ACRE    PER CUBIC FEET   PER ACRE /
     $17X,47HINCHES  SQ. FT.    ACRE   PER ACRE   4-INCH TOP)           VOL 956
      LINE = 15                                                         VOL 960
      IPAGE = IPAGE+1                                                   VOL 965
   30 CONTINUE                                                          VOL 970
C                                                                       VOL 975
C     5.6-INCH PLUS YIELD TABLE                                         VOL 980
C                                                                       VOL 985
      DO 210 IE=1,16                                                    VOL 990
      IF (LIUT(IE).GE.1) GO TO 210                                      VOL 995
      AGEXXX = TOAGE(AGET(IE),SI)                                       VOL1000
C                                                                       VOL1005
C     BEFORE COMMERCIAL THINNING STAND STATICTICS 5.6-INCHES + DBH      VOL1010
C                                                                       VOL1015
      IF (IE.EQ.1) TTN(IE) = 0.0                                        VOL1020
      XN56(IE) = G56(IE)/(.005454154*D56(IE)**2)                        VOL1025
      IF (IE.EQ.16) GO TO 220                                           VOL1030
      IF (AGET(IE+1).LE.0.05) GO TO 220                                 VOL1035
      IF(IE.EQ.1) GO TO 35                                              VOL1035A

      IF(LIUT(IE-1).EQ.1) GO TO 220                                     VOL1036
   35 IF (I56.EQ.0) GO TO 50                                            VOL1040
      WRITE (IUOUT,40) AGEXXX,D56(IE),G56(IE),XN56(IE),VA56(IE),        VOL1045
     $ VA456(IE)
   40 FORMAT (1X,  9HTOTAL AGE,F5.0//1X,10HBEFORE    ,5X,F6.1,F9.1,F9.0,VOL1050
     $F10.0,F12.0)                                                      VOL1055
   50 CONTINUE                                                          VOL1060
C                                                                       VOL1065
C     CUT PORTION OF STAND 5.6-INCHES + DBH                             VOL1070
C                                                                       VOL1075
      IF (D(IE,2).EQ.0.0) GO TO 60                                      VOL1080
      IF (DC56(IE).LE.0.0) GO TO 60                                     VOL1085
      XNC56(IE) = GC56(IE)/(.005454154*DC56(IE)**2)                     VOL1090
      IF (DC56(IE).LE.0.0.OR.GC56(IE).LE.0.0.OR.VAC56(IE).LE.0.0) GO TO VOL1095
     $60                                                                VOL1100
      GO TO 70                                                          VOL1105
C                                                                       VOL1110
   60 CONTINUE                                                          VOL1115
      XNC56(IE) = 0.0                                                   VOL1120
   70 CONTINUE                                                          VOL1125
      IF (I56.EQ.0) GO TO 90                                            VOL1130
      WRITE (IUOUT,80) DC56(IE),GC56(IE),XNC56(IE),VAC56(IE),VAC456(IE) VOL1135
   80 FORMAT (1X,8HCUT     ,7X,F6.1,F9.1,F9.0,F10.0,F12.0)              VOL1140
C                                                                       VOL1145
C     AFTER CUT STAND STATISTICS 5.6-INCHES + DBH                       VOL1150
C                                                                       VOL1155
   90 CONTINUE                                                          VOL1160
      XNA56 = XN56(IE)-XNC56(IE)                                        VOL1165
      GA56 = G56(IE)-GC56(IE)                                           VOL1170
      DA56(IE) = ((GA56/XNA56)/.005454154)**.5                          VOL1175
      IF (I56.EQ.0) GO TO 110                                           VOL1180
      WRITE (IUOUT,100) DA56(IE),GA56,XNA56,VAA56(IE),VAA456(IE)        VOL1185
  100 FORMAT (1X,8HRESIDUAL,7X,F6.1,F9.1,F9.0,F10.0,F12.0/)             VOL1190
C                                                                       VOL1195
C     SUM CUT STATISTICS FOR 5.6-INCHES + DBH                           VOL1200
C                                                                       VOL1205
  110 CONTINUE                                                          VOL1210
      SGC56 = SGC56+GC56(IE)                                            VOL1215
      SXNC56 = SXNC56+XNC56(IE)                                         VOL1220
      IF (SXNC56.EQ.0.0) GO TO 120                                      VOL1225
      GO TO 130                                                         VOL1230
C                                                                       VOL1235
  120 CONTINUE                                                          VOL1240
      TTN(IE) = 0.0                                                     VOL1245
  130 CONTINUE                                                          VOL1250
      SVAC56 = SVAC56+VAC56(IE)                                         VOL1255
      IF (I56.EQ.0) GO TO 150                                           VOL1260
      WRITE (IUOUT,140) SGC56,SXNC56,SVAC56,SVC456(IE)                  VOL1265
  140 FORMAT (9H SUM CUTS,13X,F9.1,F9.0,F10.0,F12.0)                    VOL1270
      LINE = LINE+10                                                    VOL1275
C
C  EEEEEEEEEE
      IF (ECO.GT.0) LINE=LINE+5
C  EEEEEEEEEE
C
      IF (LINE.LT.53) GO TO 150                                         VOL1280
      WRITE (IUOUT,20) month,day,year,hour,minute,second,
     $ VERS,IPAGE,CTITLE,rdasymp/0.94,SI                                VOL1285
      IPAGE = IPAGE+1                                                   VOL1290
      LINE = 15                                                         VOL1295
  150 CONTINUE                                                          VOL1300
C                                                                       VOL1305
C     MORTALITY STATISTICS 5.6-INCHES + DBH                             VOL1310
C                                                                       VOL1315
      IF (D(IE,4).LT.0.00005) GO TO 170                                 VOL1320
      IF ((D56(IE)-D(IE,1)).GT..001.OR.IE.LE.1) GO TO 160
      IF ((DA56(IE-1)-D(IE-1,3)).GT..001) GO TO 160
      VAM56(IE) = VAM56(IE-1)+VA(IE,4)-VA(IE-1,4)                       VOL1335
      GO TO 180                                                         VOL1340
C                                                                       VOL1345
  160 CONTINUE                                                          VOL1350
      VAM56(IE) = CVM56(D(IE,4),VA(IE,4))                               VOL1355
      GO TO 180                                                         VOL1360
C                                                                       VOL1365
  170 CONTINUE                                                          VOL1370
      VAM56(IE) = 0.0                                                   VOL1375
  180 CONTINUE                                                          VOL1380
      IF (I56.EQ.0) GO TO 200                                           VOL1385
      WRITE (IUOUT,190) VAM56(IE)                                       VOL1390
  190 FORMAT (1X,13HSUM MORTALITY,26X,F10.0/)                           VOL1395
C
C  EEEEEEEEEE
C     CALCULATE AND WRITE VALUES FOR 5.6-IN. INTERM HARVEST
      IF (ECO.LE.0.OR.EUNITS.NE.1) GO TO 75
      DCUT=DC56(IE)
      VOL=VAC456(IE)
      TYHV=1
      CALL ECON(AGEXXX,SAGE,IORG,PCT,VOL,DCUT,DLIM)
C     PRINT *,'VOLCON',VDEAD,VOL,DLIM,IMORT,CMORT,DMORT,TAMORT,VMORTE,MORTE
      CALL EOUT
   75 CONTINUE
C  EEEEEEEEEE
C
  200 CONTINUE                                                          VOL1400
      IF (AGEXXX.GE.AHARR) GO TO 220                                    VOL1405
  210 CONTINUE                                                          VOL1410
  220 CONTINUE                                                          VOL1415
C                                                                       VOL1420
C     HARVEST CUT 5.6-INCHES + DBH                                      VOL1425
C                                                                       VOL1430
      IF (I56.EQ.0) GO TO 240                                           VOL1435
      WRITE (IUOUT,230) AGEXXX,D56(IE),G56(IE),XN56(IE),VA56(IE),       VOL1440
     $ VA456(IE)
  230 FORMAT (1X,  9HTOTAL AGE,F5.0//1X,11HHARVEST    ,4X,F6.1,F9.1,F9.0VOL1445
     $,F10.0,F12.0/)                                                    VOL1450
C                                                                       VOL1455
C     ADD HARVEST CUT TO SUM OF CUTS 5.6-INCHES + DBH                   VOL1460
C                                                                       VOL1465
  240 CONTINUE                                                          VOL1470
      SGC56 = SGC56+G56(IE)                                             VOL1475
      SXNC56 = SXNC56+XN56(IE)                                          VOL1480
      SVAC56 = SVAC56+VA56(IE)                                          VOL1485
      IF (I56.EQ.0) GO TO 250                                           VOL1490
      WRITE (IUOUT,140) SGC56,SXNC56,SVAC56,SVC456(IE)                  VOL1495
C                                                                       VOL1500
C     LAST MORTALITY LINE 5.6-INCHES + DBH                              VOL1505
C                                                                       VOL1510
  250 CONTINUE                                                          VOL1515
      IF (D(IE,4).LT.0.00005) GO TO 270                                 VOL1520
      IF ((D56(IE)-D(IE,1)).GT..001.OR.IE.LE.2) GO TO 260
      ID = IE-1                                                         VOL1530
      IF (LIUT(ID).GT.1) ID = ID-1                                      VOL1535
      IF (ID.LE.0) GO TO 260                                            VOL1540
      IF ((DA56(ID)-D(ID,3)).GT..001) GO TO 260
      VAM56(IE) = VAM56(ID)+VA(IE,4)-VA(ID,4)                           VOL1550
      GO TO 280                                                         VOL1555
C                                                                       VOL1560
  260 CONTINUE                                                          VOL1565
      VAM56(IE) = CVM56(D(IE,4),VA(IE,4))                               VOL1570
      GO TO 280                                                         VOL1575
C                                                                       VOL1580
  270 CONTINUE                                                          VOL1585
      VAM56(IE) = 0.0                                                   VOL1590
  280 CONTINUE                                                          VOL1595
      IF (I56.EQ.0) GO TO 290                                           VOL1600
      WRITE (IUOUT,190) VAM56(IE)                                       VOL1605
C
C  EEEEEEEEEE
C     CALCULATE AND WRITE VALUES FOR 5.6-IN. FINAL HARVEST
      IF (ECO.LE.0.OR.EUNITS.NE.1) GO TO 225
      DCUT=D56(IE)
      VOL=VA456(IE)
      TYHV=2
      CALL ECON(AGEXXX,SAGE,IORG,PCT,VOL,DCUT,DLIM)
      CALL EOUT
  225 CONTINUE
C  EEEEEEEEEE
C
  290 CONTINUE                                                          VOL1610
      TTN(1) = 0.0                                                      VOL1615
      IF (I76.EQ.0) GO TO 500                                           VOL1620
C
C  EEEEEEEEEE
C     WRITE CHOSEN ECONOMICS VALUES
      IF (ECO.LE.0.OR.EUNITS.ne.1) GO TO 285
      WRITE (IUOUT,15) month,day,year,hour,minute,second,
     $ VERS,IPAGE,CTITLE,rdasymp/0.94,SI                                HDR 155
      IPAGE=IPAGE+2
      TYHV=0
      CALL EOUT
  285 CONTINUE
C  EEEEEEEEEE
C
      if (isv.le.0) WRITE (IUOUT,300) month,day,year,hour,minute,second,
     $ VERS,IPAGE,CTITLE,rdasymp/0.94,SI                                VOL1625
  300 FORMAT (1H1,67x,i2.2,1h-,i2.2,1h-,i4.4,/69x,i2.2,1h:,i2.2,1h:,i2.2 
     $/28X,20HD F S I M   VERSION ,a8,12X,6H PAGE ,I2/,1X,A,/
     $31X,19HMANAGED YIELD TABLE/33X,15HFOR DOUGLAS-FIR/33X,15H7.6 INCHEVOL1635
     $S PLUS/25x,31h ASYMPTOTIC RELATIVE DENSITY = ,f5.1/
     $/26X,13HSITE INDEX = ,F4.0,14H (50 YEARS BH)//24X,5HBASAL/        VOL1640
     $25X,12HAREA   TREES/ 17X,19HDBH   PER ACRE  PER,32X,7H  32-FT/16X,
     $58HINCHES SQ. FT.  ACRE   CVTS    CV4     CV6     IV6     SV6)    VOL1650
      if (isv.ge.1) WRITE (IUOUT,301) month,day,year,hour,minute,second,
     $VERS,IPAGE,CTITLE,rdasymp/0.94,SI
  301 FORMAT (1H1,67x,i2.2,1h-,i2.2,1h-,i4.4,/69x,i2.2,1h:,i2.2,1h:,i2.2 
     $/28X,20HD F S I M   VERSION ,a8,12X,6H PAGE ,I2/,1X,A,/
     $31X,19HMANAGED YIELD TABLE/33X,15HFOR DOUGLAS-FIR/33X,15H7.6 INCHEVOL1635
     $S PLUS/25x,31h ASYMPTOTIC RELATIVE DENSITY = ,f5.1/
     $/26X,13HSITE INDEX = ,F4.0,14H (50 YEARS BH)//24X,5HBASAL/        VOL1640
     $25X,12HAREA   TREES/ 17X,19HDBH   PER ACRE  PER,32X,7H  16-FT/16X,
     $58HINCHES SQ. FT.  ACRE   CVTS    CV4     CV6     IV6     SV6)    VOL1650
      IPAGE = IPAGE+1                                                   VOL1655
      LINE = 14                                                         VOL1660
C                                                                       VOL1665
C     7.6-INCHES + DBH YIELD TABLE                                      VOL1670
C                                                                       VOL1675
      DO 440 IE=1,16                                                    VOL1680
      IF (LIUT(IE).GE.1) GO TO 440                                      VOL1685
      AGEXXX = TOAGE(AGET(IE),SI)                                       VOL1690
C                                                                       VOL1695
C     BEFORE COMMERCIAL THINNING STAND STATISTICS 7.6-INCHES + DBH      VOL1700
C                                                                       VOL1705
      XN76 = G76(IE)/(.005454154*D76(IE)*D76(IE))                       VOL1710
      VA676 = CV676(ARG(IE),TTN(IE),D(IE,1),GA(IE,1),H40(IE),VA(IE,1),  VOL1715
     $VA76(IE),VA476(IE))                                               VOL1720
      YI676 = VI676(ARG(IE),TTN(IE),D(IE,1),GA(IE,1),H40(IE),VA(IE,1),  VOL1725
     $VA676)                                                            VOL1730
      VS676 = SV676(ARG(IE),TTN(IE),D(IE,1),GA(IE,1),H40(IE),VA(IE,1),  VOL1735
     $YI676,va476(ie),xn76,d76(ie),isv)                                 VOL1740
      IF (IE.EQ.16) GO TO 450                                           VOL1745
      IF (AGET(IE+1).LE.0.05) GO TO 450                                 VOL1750
      IF(IE.EQ.1) GO TO 305                                             VOL1750A

      IF(LIUT(IE-1).EQ.1) GO TO 450                                     VOL1751
  305 WRITE (IUOUT,310) AGEXXX,D76(IE),G76(IE),XN76,VA76(IE),VA476(IE), VOL1755
     $ VA676,YI676,VS676                                                VOL1760
  310 FORMAT (1X,9HTOTAL AGE,F5.0//1X,10HBEFORE    ,3X,F6.1,F8.1,F8.0,  VOL1765
     $2F7.0,3F8.0)                                                      VOL1770
C                                                                       VOL1775
C     CUT PORTION OF STAND 7.6-INCHES + DBH                             VOL1780
C                                                                       VOL1785
      TTN(IE) = 1.0                                                     VOL1790
      IF (D(IE,2).EQ.0.0) GO TO 320                                     VOL1795
      IF (DC76(IE).LE.0.0.OR.DC56(IE).LE.0.0) GO TO 320                 VOL1800
      XNC76 = GC76(IE)/(.005454154*DC76(IE)*DC76(IE))                   VOL1805
      VAC676 = CVC676(D(IE,2),H40(IE),VA(IE,2),VAC76(IE),VAC476(IE))    VOL1810
      VCI676 = VIC676(D(IE,2),H40(IE),VA(IE,2),VAC676)                  VOL1815
      VCS676 = CVS676(D(IE,2),H40(IE),VA(IE,2),VCI676,vac476(ie),       VOL1820
     $ xnc76,dc76(ie),isv)
      IF (DC76(IE).LE.0.0.OR.GC76(IE).LE.0.0.OR.VAC76(IE).LE.0.0.OR.    VOL1825
     $VAC476(IE).LE.0.0) GO TO 320                                      VOL1830
      GO TO 330                                                         VOL1835
C                                                                       VOL1840
  320 CONTINUE                                                          VOL1845
      XNC76 = 0.0                                                       VOL1850
      VAC676 = 0.0                                                      VOL1855
      VCI676 = 0.0                                                      VOL1860
      VCS676 = 0.0                                                      VOL1865
  330 CONTINUE                                                          VOL1870
      WRITE (IUOUT,340) DC76(IE),GC76(IE),XNC76,VAC76(IE),VAC476(IE),   VOL1875
     $ VAC676,VCI676,VCS676                                             VOL1880
  340 FORMAT (1X,8HCUT     ,5X,F6.1,F8.1,F8.0,2F7.0,3F8.0)              VOL1885
C                                                                       VOL1890
C     AFTER CUT STAND STATISTICS 7.6-INCHES + DBH                       VOL1895
C                                                                       VOL1900
      GAA76 = G76(IE)-GC76(IE)                                          VOL1905
      XNA76 = XN76-XNC76                                                VOL1910
      DA76(IE) = ((GAA76/XNA76)/.005454154)**.5                         VOL1915
      VAA676 = VA676-VAC676                                             VOL1920
      VAI676 = YI676-VCI676                                             VOL1925
      VAS676 = VS676-VCS676                                             VOL1930
      WRITE (IUOUT,350) DA76(IE),GAA76,XNA76,VAA76(IE),VAA476(IE),      VOL1935
     $ VAA676,VAI676,VAS676                                             VOL1940
  350 FORMAT (1X,8HRESIDUAL,5X,F6.1,F8.1,F8.0,2F7.0,3F8.0/)             VOL1945
C                                                                       VOL1950
C     SUM CUT STATISTICS 7.6-INCHES + DBH                               VOL1955
C                                                                       VOL1960
      SGC76 = SGC76+GC76(IE)                                            VOL1965
      SXNC76 = SXNC76+XNC76                                             VOL1970
      IF (SXNC76.EQ.0.0) GO TO 360                                      VOL1975
      GO TO 370                                                         VOL1980
C                                                                       VOL1985
  360 CONTINUE                                                          VOL1990
      TTN(IE) = 0.0                                                     VOL1995
  370 CONTINUE                                                          VOL2000
      SVAC76 = SVAC76+VAC76(IE)                                         VOL2005
      SVC676 = SVC676+VAC676                                            VOL2010
      SCI676 = SCI676+VCI676                                            VOL2015
      SCS676 = SCS676+VCS676                                            VOL2020
      WRITE (IUOUT,380) SGC76,SXNC76,SVAC76,SVC476(IE),SVC676,SCI676,   VOL2025
     $ SCS676
  380 FORMAT (9H SUM CUTS,11X,F8.1,F8.0,2F7.0,3F8.0)                    VOL2030
      LINE = LINE+10                                                    VOL2035
C
C  EEEEEEEEEE
      IF (ECO.GT.0) LINE=LINE+5
C  EEEEEEEEEE
C
      IF (LINE.LT.53) GO TO 390                                         VOL2040
      if (ISV.le.0) WRITE (IUOUT,300) month,day,year,hour,minute,second,
     $ VERS,IPAGE,CTITLE,rdasymp/0.94,si                                VOL2045
      if (ISV.ge.1) WRITE (IUOUT,301) month,day,year,hour,minute,second,
     $ VERS,IPAGE,CTITLE,rdasymp/0.94,si                                VOL2045
      IPAGE = IPAGE+1                                                   VOL2050
      LINE = 15                                                         VOL2055
  390 CONTINUE                                                          VOL2060
C                                                                       VOL2065
C     MORTALITY STATISTIC 7.6-INCHES + DBH                              VOL2070
C                                                                       VOL2075
      IF (D(IE,4).LT.0.00005) GO TO 410                                 VOL2080
      IF ((D76(IE)-D56(IE)).GT..001.OR.IE.LE.1) GO TO 400
      IF ((DA76(IE-1)-DA56(IE-1)).GT..001) GO TO 400
      VAM76(IE) = VAM76(IE-1)+VAM56(IE)-VAM56(IE-1)                     VOL2095
      GO TO 420                                                         VOL2100
C                                                                       VOL2105
  400 CONTINUE                                                          VOL2110
      VAM76(IE) = CVM76(D(IE,4),VA(IE,4),VAM56(IE))                     VOL2115
      GO TO 420                                                         VOL2120
C                                                                       VOL2125
  410 CONTINUE                                                          VOL2130
      VAM76(IE) = 0.0                                                   VOL2135
  420 CONTINUE                                                          VOL2140
      WRITE (IUOUT,430) VAM76(IE)                                       VOL2145
  430 FORMAT (1X,13HSUM MORTALITY,22X,F7.0/ )                           VOL2150
C
C  EEEEEEEEEE
C     CALCULATE AND WRITE VALUES FOR 7.6-IN. INTERM HARVEST
      IF (ECO.LE.0.OR.EUNITS.EQ.1) GO TO 435
      IF (EUNITS.EQ.2) VOL=VAC676
      IF (EUNITS.EQ.3)  VOL=VCI676
      IF (EUNITS.EQ.4) VOL=VCS676
      DCUT=DC76(IE)
      TYHV=1
      CALL ECON(AGEXXX,SAGE,IORG,PCT,VOL,DCUT,DLIM)
      CALL EOUT
  435 CONTINUE
C  EEEEEEEEEE
C
      IF (AGEXXX.GE.AHARR) GO TO 450                                    VOL2155
  440 CONTINUE                                                          VOL2160
  450 CONTINUE                                                          VOL2165
C                                                                       VOL2170
C     HARVEST CUT 7.6-INCHES + DBH                                      VOL2175
C                                                                       VOL2180
      WRITE (IUOUT,460) AGEXXX,D76(IE),G76(IE),XN76,VA76(IE),VA476(IE), VOL2185
     $ VA676,YI676,VS676                                                VOL2190
  460 FORMAT (1X,9HTOTAL AGE,F5.0//1X,11HHARVEST    ,2X,F6.1,F8.1,F8.0, VOL2195
     $2F7.0,3F8.0/)                                                     VOL2200
C                                                                       VOL2205
C     SUM FINAL CUT STATISTICS 7.6-INCHES + DBH                         VOL2210
C                                                                       VOL2215
      SGC76 = SGC76+G76(IE)                                             VOL2220
      SXNC76 = SXNC76+XN76                                              VOL2225
      SVAC76 = SVAC76+VA76(IE)                                          VOL2230
      SVC676 = SVC676+VA676                                             VOL2235
      SCI676 = SCI676+YI676                                             VOL2240
      SCS676 = SCS676+VS676                                             VOL2245
      WRITE (IUOUT,380) SGC76,SXNC76,SVAC76,SVC476(IE),SVC676,SCI676,   VOL2250
     $ SCS676
C                                                                       VOL2255
C     LAST MORTALITY LINE 7.6-INCHES + DBH                              VOL2260
C                                                                       VOL2265
      IF (D(IE,4).LT.0.00005) GO TO 480                                 VOL2270
      IF ((D76(IE)-D56(IE)).GT..001.OR.IE.LE.2) GO TO 470
      ID = IE-1                                                         VOL2280
      IF (LIUT(ID).GT.1) ID = ID-1                                      VOL2285
      IF (ID.LE.0) GO TO 470                                            VOL2290
      IF ((DA76(ID)-DA56(ID)).GT..001) GO TO 470
      VAM76(IE) = VAM76(ID)+VAM56(IE)-VAM56(ID)                         VOL2300
      GO TO 490                                                         VOL2305
C                                                                       VOL2310
  470 CONTINUE                                                          VOL2315
      VAM76(IE) = CVM76(D(IE,4),VA(IE,4),VAM56(IE))                     VOL2320
      GO TO 490                                                         VOL2325
C                                                                       VOL2330
  480 CONTINUE                                                          VOL2335
      VAM76(IE) = 0.0                                                   VOL2340
  490 CONTINUE                                                          VOL2345
      WRITE (IUOUT,430) VAM76(IE)                                       VOL2350
C
C  EEEEEEEEEE
C     CALCULATE AND WRITE VALUES FOR 7.6-IN. FINAL HARVEST
      IF (ECO.LE.0.OR.EUNITS.EQ.1) GO TO 495
      IF (EUNITS.EQ.2) VOL=VA676
      IF (EUNITS.EQ.3)  VOL=YI676
      IF (EUNITS.EQ.4) VOL=VS676
      DCUT=D76(IE)
      TYHV=2
      CALL ECON(AGEXXX,SAGE,IORG,PCT,VOL,DCUT,DLIM)
      CALL EOUT
  495 CONTINUE
C  EEEEEEEEEE
C
  500 CONTINUE                                                          VOL2355
      RETURN                                                            VOL2360
      END                                                               VOL2365
