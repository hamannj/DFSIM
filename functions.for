      FUNCTION SORG(IORG,HS)                                            SOR   5
C                                                                       SOR  10
C     DECAY FUNCTION FOR STAND ORIGIN.                                  SOR  15
C     ALLOWS PLANTING EFFECTS TO DECAY LINEARLY FOR STAND HEIGHTS       SOR  20
C     BETWEEN 60 AND 100 FEET. AT 100 FEET PLANTING HAS NO EFFECT.      SOR  25
C                                                                       SOR  30
C     ************ DEFINITION OF IMPORTANT SORG VARIABLES ************  SOR  35
C                                                                       SOR  40
C     IORG   = STAND ORIGIN                                             SOR  45
C     HS     = STAND TOP HEIGHT                                         SOR  50
C     ***************************************************************** SOR  55
C                                                                       SOR  60
      SORG = IORG                                                       SOR  65
      IF (IORG.EQ.0) RETURN                                             SOR  70
      IF (HS.GT.60.0) SORG = (100.0-HS)*0.025                           SOR  75
      IF (SORG.LE.0.0) SORG = 0.0                                       SOR  80
      RETURN                                                            SOR  85
      END                                                               SOR  90-

      FUNCTION THN(ITHN,HS,HAT)                                         THN   5
C                                                                       THN  10
C     DECAY FUNCTION FOR THINNING EFFECTS.                              THN  15
C     ALLOWS THINNING EFFECTS TO DECAY LINEARLY FOR HEIGHT INCREMENTS   THN  20
C     SINCE LAST THINNING BETWEEN 40 AND 80 FEET. AT 80 FEET CHANGE     THN  25
C     IN HEIGHT, THINNING HAS NO EFFECT.                                THN  30
C                                                                       THN  35
C     ************ DEFINITION OF IMPORTANT THN VARIABLES ************   THN  40
C                                                                       THN  45
C     DHT    = STAND TOP HEIGHT INCREMENT SINCE LAST THINNING           THN  50
C     HAT    = STAND TOP HEIGHT AFTER MOST RECENT THINNING              THN  55
C     HS     = STAND TOP HEIGHT                                         THN  60
C     ITHN   = INDICATOR SWITCH FOR THINNING DONE                       THN  65
C     ***************************************************************** THN  70
C                                                                       THN  75
      THN = ITHN                                                        THN  80
      IF (ITHN.EQ.0) RETURN                                             THN  85
      DHT = HS-HAT                                                      THN  90
      IF (DHT.GT.40.0) THN = 1.0-(DHT-40.0)*0.025                       THN  95
      IF (THN.LE.0.0) THN = 0.0                                         THN 100
      RETURN                                                            THN 105
      END                                                               THN 110-

      FUNCTION HEIGHT(SITE,AGE,IOBS,OBSAGE,OBSHTS)                      HGT   5
      DIMENSION OBSAGE(31), OBSHTS(2,31)                                HGT  10
C                                                                       HGT  15
C     NATURAL STAND HEIGHT EQUATION                                     HGT  20
C                                                                       HGT  25
C     3/20/78 MEMO                                                      HGT  30
C                                                                       HGT  35
C     ************ DEFINITION OF IMPORTANT HEIGHT VARIABLES *********** HGT  40
C                                                                       HGT  45
C     AGE    = BREAST HEIGHT STAND AGE                                  HGT  50
C     IOBS   = NUMBER OF SPECIFIED STAND TOP HEIGHT OBSERVATIONS        HGT  55
C     OBSAGE = ARRAY OF BREAST HEIGHT STAND AGES FOR SPECIFIED STAND    HGT  60
C                   TOP HEIGHT OBSERVATIONS                             HGT  65
C     OBSHTS = ARRAY OF SPECIFIED STAND TOP HEIGHTS                     HGT  70
C     SITE   = SITE INDEX (50-YEAR B H BASE AGE)                        HGT  75
C     ***************************************************************** HGT  80
C                                                                       HGT  85
      IF (IOBS.GT.0) GO TO 10                                           HGT  90
      PIN = (-.477762-.00894427*SITE+.0000793548*SITE**2-.000000171666* HGT  95
     $SITE**3)                                                          HGT 100
      AIN = ALOG(4.5/SITE)/((13.25-SITE/20.)**PIN-(63.25-SITE/20.)**PIN)HGT 105
      HEIGHT = SITE*EXP(AIN*((AGE+13.25-SITE/20.)**PIN-(63.25-SITE/20.)*HGT 110
     $*PIN))                                                            HGT 115
      GO TO 40                                                          HGT 120
C                                                                       HGT 125
C     SPECIFIED STAND TOP HEIGHTS                                       HGT 130
C                                                                       HGT 135
   10 CONTINUE                                                          HGT 140
      J = IOBS-1                                                        HGT 145
      DO 20 I=1,J                                                       HGT 150
      IF (OBSAGE(I).LE.AGE.AND.OBSAGE(I+1).GE.AGE) GO TO 30             HGT 155
   20 CONTINUE                                                          HGT 160
   30 CONTINUE                                                          HGT 165
      HEIGHT = OBSHTS(2,I)-(OBSHTS(1,I+1)-OBSHTS(2,I))*(OBSAGE(I)-AGE)/ HGT 170
     $(OBSAGE(I+1)-OBSAGE(I))                                           HGT 175
   40 CONTINUE                                                          HGT 180
      RETURN                                                            HGT 185
      END                                                               HGT 190-

      FUNCTION HTGROW(SITE,AGE,DELTAA,FERTEF,IOBS,OBSAGE,OBSHTS)        HTG   5
      DIMENSION OBSAGE(31), OBSHTS(2,31)                                HTG  10
C                                                                       HTG  15
C     STAND TOP HEIGHT INCREMENT EQUATION                               HTG  20
C                                                                       HTG  25
C     2/22/79 DERIVED FROM HEIGHT EQUATION                              HTG  30
C                                                                       HTG  35
C     ************ DEFINITION OF INPORTANT HTGROW VARIABLES *********** HTG  40
C                                                                       HTG  45
C     AGE    = BREAST HEIGHT STAND AGE                                  HTG  50
C     DELTAA = LENGTH OF GROWTH PERIOD                                  HTG  55
C     FERTEF = NITROGEN FERTILIZER EFFECT                               HTG  60
C     IOBS   = NUMBER OF SPECIFIED STAND TOP HEIGHT OBSERVATIONS        HTG  65
C     OBSAGE = ARRAY OF BREAST HEIGHT STAND AGES FOR SPECIFIED STAND    HTG  70
C                   TOP HEIGHTS                                         HTG  75
C     OBSHTS = ARRAY OF SPECIFIED STAND TOP HEIGHTS                     HTG  80
C     SITE   = SITE INDEX (50-YEAR B H BASE AGE)                        HTG  85
C     ***************************************************************** HTG  90
C                                                                       HTG  95
      IF (IOBS.GT.0) GO TO 10                                           HTG 100
      PIN = (-.477762-.00894427*SITE+.0000793548*SITE**2-.000000171666* HTG 105
     $SITE**3)                                                          HTG 110
      B = 13.25-SITE/20.0                                               HTG 115
      C = 63.25-SITE/20.0                                               HTG 120
      AIN = ALOG(4.5/SITE)/(B**PIN-C**PIN)                              HTG 125
      D = SITE*EXP(-AIN*C**PIN)                                         HTG 130
      E = EXP(AIN*(AGE+DELTAA+B)**PIN)-EXP(AIN*(AGE+B)**PIN)            HTG 135
      HTGROW = D*E                                                      HTG 140
      IF (FERTEF.GT.0.0) HTGROW = HTGROW*EXP(.54989*FERTEF)             HTG 145
      GO TO 40                                                          HTG 150
C                                                                       HTG 155
C     SPECIFIED STAND TOP HEIGHTS                                       HTG 160
C                                                                       HTG 165
   10 CONTINUE                                                          HTG 170
      I = 2                                                             HTG 175
      IF (OBSAGE(1).GE.AGE) GO TO 30                                    HTG 180
      I = IOBS                                                          HTG 185
      IF (OBSAGE(IOBS).LE.AGE) GO TO 30                                 HTG 190
      DO 20 I=2,IOBS                                                    HTG 195
      IF (OBSAGE(I-1).LE.AGE.AND.OBSAGE(I).GT.AGE) GO TO 30             HTG 200
   20 CONTINUE                                                          HTG 205
   30 CONTINUE                                                          HTG 210
      HTGROW = (OBSHTS(1,I)-OBSHTS(2,I-1))/(OBSAGE(I)-OBSAGE(I-1))*     HTG 215
     $DELTAA                                                            HTG 220
   40 CONTINUE                                                          HTG 225
      IF (HTGROW.LE.0.0) HTGROW = 0.00001                               HTG 230
      RETURN                                                            HTG 235
      END                                                               HTG 240-
c      FUNCTION HTLOR(DIAM,VOL,BA,XNUM,HT)                               HTL   5
C                                                                       HTL  10
C     CALCULATE LOREY'S HEIGHT                                          HTL  15
C                                                                       HTL  20
C     5/12/78 MEMO--- REVISED LOREY'S HEIGHT                            HTL  25
C                                                                       HTL  30
C     ************ DEFINITION OF IMPORTANT HTLOR VARIABLES ************ HTL  35
C                                                                       HTL  40
C     BA     = BASAL AREA PER ACRE                                      HTL  45
C     DIAM   = QUADRATIC MEAN STAND DIAMETER                            HTL  50
C     VOL    = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE                  HTL  55
C     ***************************************************************** HTL  60
C                                                                       HTL  65
c      HTLOR = 0.0                                                       HTL  70
c      AC = .480961 - 0.00409083 * DIAM                                  HTL  75
c      BC = -(0.107809 * DIAM + (VOL / BA))                              HTL  80
c      CC = 42.46542 - 10.99643 * DIAM                                   HTL  85
c      DC = BC * BC - 4.0 * AC * CC                                      HTL  90
c      IF (DC.LT.0.0) GO TO 10                                           HTL  95
c      HTLOR = (-BC + (DC)**0.5) / (2.0 * AC)                            HTL 100
c   10 CONTINUE                                                          HTL 105
c      IF (HTLOR.LE.0.0.OR.DC.LT.0.0) HTLOR = 0.0                        HTL 110
cc      IF (XNUM.LE.40.0.OR.HTLOR.GT.HT) HTLOR = HT                       HTL 115
c      if (htlor.gt.ht) htlor = ht
c      RETURN                                                            HTL 120
c      END                                                               HTL 125-

      FUNCTION BHAGE(TAGE,SI)                                           BHA   5
C                                                                       BHA  10
C     CALCULATE BREAST HEIGHT AGE                                       BHA  15
C                                                                       BHA  20
C     11/16/77 MEMO-- YIELD FORMAT OUTPUT                               BHA  25
C                                                                       BHA  30
C     ************ DEFINITION OF IMPORTANT BHAGE VARIABLES ************ BHA  35
C                                                                       BHA  40
C     SI     = SITE INDEX (50-YEAR B H BASE AGE)                        BHA  45
C     TAGE   = TOTAL STAND AGE                                          BHA  50
C     ***************************************************************** BHA  55
C                                                                       BHA  60
      IF (SI.GE.136) ADJ = 6.                                           BHA  65
      IF (SI.GE.116.0.AND.SI.LE.135.0) ADJ = 7.                         BHA  70
      IF (SI.GE.96.00.AND.SI.LE.115.0) ADJ = 8.                         BHA  75
      IF (SI.GE.76.00.AND.SI.LE.95.0) ADJ = 9.                          BHA  80
      IF (SI.LE.75.0) ADJ = 10.                                         BHA  85
      BHAGE = TAGE-ADJ                                                  BHA  90
      RETURN                                                            BHA  95
      END                                                               BHA 100-

      FUNCTION TOAGE(TAGE,SI)                                           TOA   5
C                                                                       TOA  10
C     CALCULATE TOTAL AGE                                               TOA  15
C                                                                       TOA  20
C     11/16/77 MEMO--- YIELD PROGRAM OUTPUT                             TOA  25
C                                                                       TOA  30
C     ************ DEFINITION OF IMPORTANT TOAGE VARIABLES ************ TOA  35
C                                                                       TOA  40
C     SI     = SITE INDEX (50-YEAR B H BASE AGE)                        TOA  45
C     TAGE   = BREAST HEIGHT STAND AGE                                  TOA  50
C     ***************************************************************** TOA  55
C                                                                       TOA  60
      IF (SI.GE.136) ADJ = 6.                                           TOA  65
      IF (SI.GE.116.0.AND.SI.LE.135.0) ADJ = 7.                         TOA  70
      IF (SI.GE.96.00.AND.SI.LE.115.0) ADJ = 8.                         TOA  75
      IF (SI.GE.76.00.AND.SI.LE.95.0) ADJ = 9.                          TOA  80
      IF (SI.LE.75.0) ADJ = 10.                                         TOA  85
      TOAGE = TAGE+ADJ                                                  TOA  90
      RETURN                                                            TOA  95
      END                                                               TOA 100-

      FUNCTION AGEJUV(SITE,HTJUV,IOBS,OBSAGE,OBSHTS)                    AGE   5
C                                                                       AGE  10
C     NATURAL STAND HEIGHT EQUATION SOLVED FOR AGE                      AGE  15
C                                                                       AGE  20
C     DERIVED FROM HEIGHT EQUATION 3/20/78                              AGE  25
C                                                                       AGE  30
      DIMENSION OBSAGE(31), OBSHTS(2,31)                                AGE  35
C                                                                       AGE  40
C     ************ DEFINITION OF IMPORTANT AGEJUV VARIABLES *********** AGE  45
C                                                                       AGE  50
C     HTJUV  = STAND TOP HEIGHT                                         AGE  55
C     IOBS   = NUMBER OF SPECIFIED STAND TOP HEIGHT OBSERVATIONS        AGE  60
C     OBSAGE = ARRAY OF BREAST HEIGHT STAND AGES FOR SPECIFIED STAND    AGE  65
C                   TOP HEIGHT OBSERVATIONS                             AGE  70
C     OBSHTS = ARRAY OF SPECIFIED STAND TOP HEIGHTS                     AGE  75
C     SITE   = SITE INDEX (50-YEAR B H BASE AGE)                        AGE  80
C     ***************************************************************** AGE  85
C                                                                       AGE  90
      IF (IOBS.GT.0) GO TO 10                                           AGE  95
      PIN = (-.477762-.00894427*SITE+.0000793548*SITE**2-.000000171666* AGE 100
     $SITE**3)                                                          AGE 105
      AIN = ALOG(4.5/SITE)/((13.25-SITE/20.)**PIN-(63.25-SITE/20.)**PIN)AGE 110
      AGEJUV = ((-ALOG(HTJUV/SITE)-AIN*(63.25-SITE/20.)**PIN)**(1./PIN))AGE 115
     $/(-AIN)**(1./PIN)-13.25+SITE/20.                                  AGE 120
      GO TO 40                                                          AGE 125
C                                                                       AGE 130
C     SPECIFIED STAND TOP HEIGHTS                                       AGE 135
C                                                                       AGE 140
   10 CONTINUE                                                          AGE 145
      J = IOBS-1                                                        AGE 150
      DO 20 I=1,J                                                       AGE 155
      IF (OBSHTS(2,I).LE.HTJUV.AND.OBSHTS(1,I+1).GE.HTJUV) GO TO 30     AGE 160
   20 CONTINUE                                                          AGE 165
   30 CONTINUE                                                          AGE 170
      AGEJUV = OBSAGE(I)+(HTJUV-OBSHTS(2,I))*(OBSAGE(I+1)-OBSAGE(I))/   AGE 175
     $(OBSHTS(1,I+1)-OBSHTS(2,I))                                       AGE 180
   40 CONTINUE                                                          AGE 185
      RETURN                                                            AGE 190
      END                                                               AGE 195-

      FUNCTION DIAMJ(HS,XNUM,TTHN,HAT,PN,HF)                            DIA   5
C                                                                       DIA  10
C     CALCULATE QUADRATIC MEAN STAND DIAMETER FOR JUVENILE STANDS       DIA  15
C                                                                       DIA  20
C     6/2/80 SUB 1 STEP 9                                               DIA  25
C                                                                       DIA  30
C     ************ DEFINITION OF IMPORTANT DIAMJ VARIABLES ************ DIA  35
C                                                                       DIA  40
C     DHT    = STAND TOP HEIGHT INCREMENT SINCE PRECOMMERCIAL THINNING  DIA  45
C     DHF    = STAND TOP HEIGHT INCREMENT SINCE FERTILIZATION           DIA  50
C     HAT    = STAND TOP HEIGHT AFTER PRECOMMERCIAL THINNING            DIA  55
C     HF     = STAND TOP HEIGHT AT MOST RECENT NITROGEN FERTILIZATION   DIA  60
C     HS     = STAND TOP HEIGHT                                         DIA  65
C     PN     = POUNDS OF NITROGEN FERTILIZER APPLIED PER ACRE MOST      DIA  70
C                   RECENT FERTILIZATION                                DIA  75
C     SI     = SITE INDEX (50-YEAR B H BASE AGE)                        DIA  80
C     TTHN   = THINNING DECAY VARIABLE                                  DIA  85
C     XNUM   = NUMBER OF TREES PER ACRE 1.6-INCHES + DBH                DIA  90
C     ***************************************************************** DIA  95
C                                                                       DIA 100
      DHT = HS-HAT                                                      DIA 105
      IF (DHT.LE.0.0) DHT = 1.0                                         DIA 110
      DHF = HS-HF                                                       DIA 115
      HAF = HF+1.0                                                      DIA 120
      HSLOG = ALOG(HS)                                                  DIA 125
      XNLOG = ALOG(XNUM)                                                DIA 130
      FERT = (ALOG(PN*(DHF*EXP(-DHF/10.0))**1.5+1.0))/HAF               DIA 135
      DIAMJ = EXP(-7.4044+0.19352*FERT+4.0036*HSLOG-4.1815*HSLOG/XNUM-0.DIA 140
     $30037*HSLOG*HSLOG-0.11268*XNLOG*HSLOG+0.44634*XNLOG/HSLOG-0.      DIA 145
     $0096588*TTHN*XNLOG*HSLOG+0.31679*TTHN+0.0099793*(ALOG(DHT))**2)   DIA 150
      RETURN                                                            DIA 155
      END                                                               DIA 160-

      FUNCTION TRENUM(HTS)                                              TRE   5
C                                                                       TRE  10
C     NUMBER OF TREES PER ACRE FOR JUVENILE STANDS                      TRE  15
C                                                                       TRE  20
C      12/22/77 SUB 01 STEP 02                                          TRE  25
C                                                                       TRE  30
C     ************ DEFINITION OF IMPORTANT TRENUM VARIABLES *********** TRE  35
C                                                                       TRE  40
C     HTS    = STAND TOP HEIGHT                                         TRE  45
C     ***************************************************************** TRE  50
C                                                                       TRE  55
      TRENUM = EXP(45.8267-5.29786*ALOG(HTS)-70.8701/ALOG(HTS))         TRE  60
      RETURN                                                            TRE  65
      END                                                               TRE  70-

      FUNCTION ADJNUM(XNUM,XNUM1,STA,HS1,HS)                            ADJ   5
C                                                                       ADJ  10
C      NON-NORMAL NUMBER OF TREES FOR EXISTING JUVENILE STAND           ADJ  15
C                                                                       ADJ  20
C     MEMO 2/22/79                                                      ADJ  25
C                                                                       ADJ  30
C     ************ DEFINITION OF IMPORTANT ADJNUM VARIABLES *********** ADJ  35
C                                                                       ADJ  40
C     ADJNUM = ESTIMATED ACTUAL NUMBER OF TREES PER ACRE ONE YEAR HENCE ADJ  45
C     HS     = STAND TOP HEIGHT ONE YEAR HENCE                          ADJ  50
C     HS1    = PRESENT STAND TOP HEIGHT                                 ADJ  55
C     STA    = PRESENT ACTUAL NUMBER OF TREES PER ACRE                  ADJ  60
C     XNUM   = 'NORMAL' NUMBER OF TREES PER ACRE AT STAND TOP HEIGHT    ADJ  65
C                   HS ONE YEAR HENCE                                   ADJ  70
C     XNUM1  = PRESENT 'NORMAL' NUMBER OF TREES PER ACRE AT STAND TOP   ADJ  75
C                   HEIGHT HS1                                          ADJ  80
C     ***************************************************************** ADJ  85
C                                                                       ADJ  90
      ADJNUM = XNUM*(STA/XNUM1)**((0.1*HS1+1.0)/(0.1*HS+1.0))           ADJ  95
      RETURN                                                            ADJ 100
      END                                                               ADJ 105-

      FUNCTION VINCR(HTADD,HS,AGE,BONE,DNOW,HAT,FERTEF,TTHN,BAB,BAC)    VIN   5
C                                                                       VIN  10
C      ESTIMATE GROSS (NET + MORTALITY) VOLUME INCREMENT                VIN  15
C                                                                       VIN  20
C      5/28/80 SUB 3 STEP 8                                             VIN  25
C                                                                       VIN  30
C     ************ DEFINITION OF IMPORTANT VINCR VARIABLES ************ VIN  35
C                                                                       VIN  40
C     AGE    = BREAST HEIGHT STAND AGE                                  VIN  45
C     BAB    = BASAL AREA PER ACRE BEFORE MOST RECENT THINNING          VIN  50
C     BAC    = BASAL AREA PER ACRE CUT MOST RECENT THINNING             VIN  55
C     BONE   = BASAL AREA PER ACRE                                      VIN  60
C     DNOW   = QUADRATIC MEAN STAND DIAMETER                            VIN  65
C     FERTEF = NITROGEN FERTILIZER EFFECT                               VIN  70
C     HAT    = STAND TOP AFTER MOST RECENT THINNING                     VIN  75
C     HS     = STAND TOP HEIGHT                                         VIN  80
C     RD     = RELATIVE DENSITY                                         VIN  85
C     ZZ     = THINNING EFFECT                                          VIN  90
C     ***************************************************************** VIN  95
C                                                                       VIN 100
      HATS = HAT                                                        VIN 105
      IF (HAT.LE.0.0) HATS = HS                                         VIN 110
      Z = HS-HATS                                                       VIN 115
      A = 0.75134                                                       VIN 120
      IF (TTHN.GT.0.0.AND.BAC.LE.0.0) GO TO 10                          VIN 125
C                                                                       VIN 130
C     ADJUST THINNING EFFECT COEFFICIENT FOR LIGHT THINNING LEVEL       VIN 135
C                                                                       VIN 140
      Q = BAC/BAB                                                       VIN 145
      IF (Q.GE.0.15) GO TO 10                                           VIN 150
      Q = Q/0.15                                                        VIN 155
      A = A*Q                                                           VIN 160
   10 CONTINUE                                                          VIN 165
      RD = BONE/SQRT(DNOW)                                              VIN 170
      B = HS/DNOW/HTADD                                                 VIN 175
      Z = Z*B                                                           VIN 180
      IF (Z.GT.600.0) Z = 0.0                                           VIN 185
      ZZ = (Z*EXP(-Z))**(1.0/RD)                                        VIN 190
      IF (RD.LE.51.0) GO TO 20                                          VIN 195
      P = (RD-41.0)/10.0                                                VIN 200
      ZZ = ZZ/P                                                         VIN 205
   20 CONTINUE                                                          VIN 210
      DVLOG = -0.29119-0.72916*(HTADD*RD*HS)**0.25/SQRT(AGE)+A*ZZ/SQRT  VIN 215
     $(AGE)+1.0840*ALOG(HS)+0.44557*FERTEF+3.2253*(HTADD/AGE)**0.25*RD**VIN 220
     $0.20-1.9337*ALOG(HS/DNOW)/ALOG(RD)-0.0036761*AGE                  VIN 225
C                                                                       VIN 230
C     ADJUSTMENT FOR YOUNG, LOW DENSITY STANDS                          VIN 235
C                                                                       VIN 240
      RAG = 1.0579-0.002632*AGE                                         VIN 245
      IF (RAG.GT.1.0) RAG = 1.0                                         VIN 250
      R = TTHN*(100.0+(40.0-RD)/2.0)/100.0*RAG                          VIN 255
      IF (R.LT.1.0) R = 1.0                                             VIN 260
      IF (R.GT.1.10) R = 1.10                                           VIN 265
      VINCR = 1.01*R*EXP(DVLOG)                                         VIN 270
      RETURN                                                            VIN 275
      END                                                               VIN 280-

      FUNCTION VNET(HT,BONE,DNOW,AGE,TTHN,VADD,A,JRD,CT1)               VNE   5
C                                                                       VNE  10
C      CONVERT GROSS VOLUME INCREMENT TO NET VOLUME INCREMENT           VNE  15
C                                                                       VNE  20
C     7/13/79 SUB 03 STEP 05 FOR RD.LT.40.                              VNE  25
C      6/19/79 SUB 6 STEP 3 FOR RD.GT.55                                VNE  30
C      LESSER OF TWO ABOVE FOR RD.GE.40.AND.RD.LE.55                    VNE  35
C                                                                       VNE  40
C     ************DEFINITION OF IMPORTANT VNET VARIABLES ************   VNE  45
C                                                                       VNE  50
C     A      = BEGINNING POINT FOR RELATIVE DENSITY CONTROL             VNE  55
C     AGE    = BREAST HEIGHT STAND AGE                                  VNE  60
C     BONE   = BASAL AREA PER ACRE                                      VNE  65
C     CT1    = INDICATOR FOR COMMERCIAL THINNING IN DENSE STANDS        VNE  70
C     DNOW   = QUADRATIC MEAN STAND DIAMETER                            VNE  75
C     HT     = STAND TOP HEIGHT                                         VNE  80
C     IORG   = STAND ORIGIN                                             VNE  85
C     JRD    = SWITCH INDICATING IF R DETERMINED BY R1 OR R2            VNE  90
C     K      = SWITCH INDICATING IF R2 USED FOR FIRST TIME              VNE  95
C     PCT    = TOTAL STAND AGE AT PRECOMMERCIAL THINNING                VNE 100
C     RD     = RELATIVE DENSITY                                         VNE 105
C     TTHN   = THINNING DECAY VARIABLE                                  VNE 110
C     VADD   = GROSS (NET + MORTALITY) CUBIC-FOOT VOLUME (TOTAL STEM)   VNE 115
C                  PER ACRE INCREMENT                                   VNE 120
C     ***************************************************************** VNE 125
C                                                                       VNE 130
      RD = BONE/SQRT(DNOW)                                              VNE 135
      HOD = HT/DNOW                                                     VNE 140
      K = 2                                                             VNE 145
      R1 = 0.0                                                          VNE 150
      R2 = 0.0                                                          VNE 155
      IF (RD.GT.55.0.OR.JRD.GE.9999) GO TO 10                           VNE 160
C                                                                       VNE 165
C     RELATIVE DENSITY LESS THAN 40.0                                   VNE 170
C                                                                       VNE 175
      R1 = 0.94313+0.0026204*RD-0.000011158*AGE*RD*HOD+0.000043199*TTHN*VNE 180
     $AGE*RD-0.00010773*TTHN*RD*HOD                                     VNE 185
      R = R1                                                            VNE 190
      K = 0                                                             VNE 195
      IF (RD.LT.40.0) GO TO 20                                          VNE 200
   10 CONTINUE                                                          VNE 205
      K = K+1                                                           VNE 210
C                                                                       VNE 215
C     RELATIVE DENSITY GREATER THAN 55.0                                VNE 220
C                                                                       VNE 225
      R2 = 0.99787-0.69256E-05*AGE*RD*HOD+0.001052*TTHN*RD              VNE 230
      R = R2                                                            VNE 235
      IF (R2.LT.R1) JRD = JRD+1                                         VNE 240
   20 CONTINUE                                                          VNE 245
      IF (RD.GE.40.0.AND.RD.LE.55.0.AND.R1.LT.R2.AND.JRD.LT.9999) R = R1VNE 250
      IF (JRD.EQ.1.AND.K.EQ.1) R = (R1+R2)/2.0                          VNE 255
      IF (R2.LT.R1.AND.R2.GT.0.0) JRD = 9999                            VNE 260
      IF (CT1.EQ.1.0) GO TO 30                                          VNE 265
C                                                                       VNE 270
C     ADJUST MORTALITY FOR PREVIOUSLY THINNED STAND WITH INITIAL        VNE 275
C     LOW DENSITY                                                       VNE 280
C                                                                       VNE 285
      RADJ = R+(1.0-R)*(1.0-(RD/50.0)**5.0)                             VNE 290
      IF (RD.GE.50.0) RADJ = R                                          VNE 295
      R = RADJ                                                          VNE 300
   30 CONTINUE                                                          VNE 305
      IF (R.GE.0.995) R = 0.995                                         VNE 310
      IF (R.LE.0.0) R = 0.0                                             VNE 315
      F = 1.0                                                           VNE 320
C                                                                       VNE 325
C     ADJUST MORTALITY FOR RELATIVE DENSITY CONTROL                     VNE 330
C                                                                       VNE 335
      IF (RD.GT.A) F = 1.0+0.04*(RD-A)**1.5                             VNE 340
      R = 1.0/F*R                                                       VNE 345
      VNET = R*VADD                                                     VNE 350
      RETURN                                                            VNE 355
      END                                                               VNE 360-

      FUNCTION BAINCR(HTADD,AGE,HS,BONE,DNOW,TTHN,HAT,FERTEF,BAB,BAC)   BAI   5
C                                                                       BAI  10
C      ESTIMATE GROSS (NET + MORTALITY) BASAL AREA INCREMENT            BAI  15
C                                                                       BAI  20
C      5/30/80 SUB 03 STEP 07                                           BAI  25
C                                                                       BAI  30
C     **********DEFINITION OF IMPORTANT BAINCR VARIABLES ***********    BAI  35
C                                                                       BAI  40
C     AGE    = BREAST HEIGHT STAND AGE                                  BAI  45
C     BAB    = BASAL AREA PER ACRE BEFORE MOST RECENT THINNING          BAI  50
C     BAC    = BASAL AREA PER ACRE CUT MOST RECENT THINNING             BAI  55
C     BONE   = BASAL AREA PER ACRE                                      BAI  60
C     DNOW   = QUADRATIC MEAN STAND DIAMETER                            BAI  65
C     FERTEF = NITROGEN FERTILIZER EFFECT                               BAI  70
C     HAT    = STAND TOP AFTER MOST RECENT THINNING                     BAI  75
C     HS     = STAND TOP HEIGHT                                         BAI  80
C     HTADD  = STAND TOP HEIGHT INCREMENT                               BAI  85
C     RD     = RELATIVE DENSITY                                         BAI  90
C     TTHN   = THINNING DECAY VARIABLE                                  BAI  95
C     ZZ     = THINNING EFFECT                                          BAI 100
C     ***************************************************************** BAI 105
C                                                                       BAI 110
      HATS = HAT                                                        BAI 115
      IF (HAT.LE.0.0) HATS = HS                                         BAI 120
      Z = HS-HATS                                                       BAI 125
      A = 1.4689                                                        BAI 130
      IF (TTHN.GT.0.0.AND.BAC.LE.0.0) GO TO 10                          BAI 135
C                                                                       BAI 140
C     ADJUST THINNING EFFECT COEFFICIENT FOR LIGHT THINNING LEVEL       BAI 145
C                                                                       BAI 150
      Q = BAC/BAB                                                       BAI 155
      IF (Q.GE.0.15) GO TO 10                                           BAI 160
      Q = Q/0.15                                                        BAI 165
      A = A*Q                                                           BAI 170
   10 CONTINUE                                                          BAI 175
      RD = BONE/SQRT(DNOW)                                              BAI 180
      B = HS/DNOW/HTADD                                                 BAI 185
      Z = Z*B                                                           BAI 190
      IF (Z.GT. 60.0) Z = 0.0                                           BAI 195
      ZZ = (Z*EXP(-Z))**(1.0/RD)                                        BAI 200
      IF (RD.LE.51.0) GO TO 20                                          BAI 205
      P = (RD-41.0)/10.0                                                BAI 210
      ZZ = ZZ/P                                                         BAI 215
   20 CONTINUE                                                          BAI 220
      DGLOG = 0.79737+2.2704*(HTADD*RD)**0.25/SQRT(AGE)+1.2612/SQRT(AGE)BAI 225
     $+A*ZZ/SQRT(AGE)+0.72673*FERTEF-1.4164*ALOG(HS)*ALOG(HS/DNOW)/SQRT BAI 230
     $(AGE)/ALOG(RD)-18.729/RD**2                                       BAI 235
C                                                                       BAI 240
C     ADJUSTMENT FOR YOUNG, LOW DENSITY STANDS                          BAI 245
C                                                                       BAI 250
      RAG = 1.0579-0.002632*AGE                                         BAI 255
      IF (RAG.GT.1.0) RAG = 1.0                                         BAI 260
      R = TTHN*(100.0+(40.0-RD)/2.0)/100.0*RAG                          BAI 265
      IF (R.LT.1.0) R = 1.0                                             BAI 270
      IF (R.GT.1.10) R = 1.10                                           BAI 275
      BAINCR = 1.02*R*EXP(DGLOG)                                        BAI 280
      RETURN                                                            BAI 285
      END                                                               BAI 290-

      FUNCTION BANET(HT,BONE,DNOW,TTHN,BAGROS,AGE,A,IRD,CT1)            BAN   5
C                                                                       BAN  10
C     CONVERT GROSS BASAL AREA INCREMENT TO NET BASAL AREA INCREMENT    BAN  15
C                                                                       BAN  20
C                                                                       BAN  25
C      6/7/79 SUB 1 STEP 4 FOR RD.LT.40                                 BAN  30
C      6/19/79 SUB 3 STEP 4 FOR RD.GT.55                                BAN  35
C      LESSER OF TWO ABOVE FOR RD.GE.40.AND.RD.LT.55                    BAN  40
C                                                                       BAN  45
C     ************ DEFINITION OF IMPORTANT BANET VARIABLES ************ BAN  50
C                                                                       BAN  55
C     A      = BEGINNING POINT FOR RELATIVE DENSITY CONTROL             BAN  60
C     AGE    = BREAST HEIGHT STAND AGE                                  BAN  65
C     BAGROS = GROSS (NET + MORTALITY) BASAL AREA PER ACRE INCREMENT    BAN  70
C     BONE   = BASAL AREA PER ACRE                                      BAN  75
C     CT1    = INDICATOR FOR COMMERCIAL THINNING IN DENSE STANDS        BAN  80
C     DNOW   = QUADRATIC MEAN STAND DIAMETER                            BAN  85
C     HT     = STAND TOP HEIGHT                                         BAN  90
C     IORG   = STAND ORIGIN                                             BAN  95
C     IRD    = SWITCH VARIABLE INDICATING IF R DETERMINED BY R1 OR R2   BAN 100
C     K      = SWITCH VARIABLE INDICATING IF R2 USED FOR FIRST TIME     BAN 105
C     PCT    = TOTAL STAND AGE AT PRECOMMERCIAL THINNING                BAN 110
C     RD     = RELATIVE DENSITY                                         BAN 115
C     TTHN   = THINNING DECAY VARIABLE                                  BAN 120
C     ***************************************************************** BAN 125
C                                                                       BAN 130
      RD = BONE/SQRT(DNOW)                                              BAN 135
      HOD = HT/DNOW                                                     BAN 140
      K = 2                                                             BAN 145
      R1 = 0.0                                                          BAN 150
      R2 = 0.0                                                          BAN 155
      IF (RD.GT.55.0.OR.IRD.GE.9999) GO TO 10                           BAN 160
C                                                                       BAN 165
C     RELATIVE DENSITY LESS THAN 40.0                                   BAN 170
C                                                                       BAN 175
      R1 = 0.93609+0.0021756*RD-0.15588E-04*AGE*RD*HOD+0.46458E-04*TTHN*BAN 180
     $AGE*RD                                                            BAN 185
      R = R1                                                            BAN 190
      K = 0                                                             BAN 195
      IF (RD.LT.40.0) GO TO 20                                          BAN 200
   10 CONTINUE                                                          BAN 205
      K = K+1                                                           BAN 210
C                                                                       BAN 215
C     RELATIVE DENSITY GREATER THAN 55.0                                BAN 220
C                                                                       BAN 225
      R2 = 0.87126-0.15861E-04*AGE*RD*HOD+0.0018314*TTHN*RD+0.0049359*  BAN 230
     $AGE                                                               BAN 235
      R = R2                                                            BAN 240
      IF (R2.LT.R1) IRD = IRD+1                                         BAN 245
   20 CONTINUE                                                          BAN 250
      IF (RD.GE.40.0.AND.RD.LE.55.0.AND.R1.LT.R2.AND.IRD.LT.9999) R = R1BAN 255
      IF (IRD.EQ.1.AND.K.EQ.1) R = (R1+R2)/2.0                          BAN 260
      IF (R2.LT.R1.AND.R2.GT.0.0) IRD = 9999                            BAN 265
      IF (CT1.EQ.1.0) GO TO 30                                          BAN 270
C                                                                       BAN 275
C     ADJUST MORTALITY FOR PREVIOUSLY THINNED STAND WITH INITIAL        BAN 280
C     LOW DENSITY                                                       BAN 285
C                                                                       BAN 290
      RADJ = R+(1.0-R)*(1.0-(RD/50.0)**5.0)                             BAN 295
      IF (RD.GE.50.0) RADJ = R                                          BAN 300
      R = RADJ                                                          BAN 305
   30 CONTINUE                                                          BAN 310
      IF (R.GE.0.995) R = 0.995                                         BAN 315
      IF (R.LE.0.0) R = 0.0                                             BAN 320
      F = 1.0                                                           BAN 325
C                                                                       BAN 330
C     ADJUST MORTALITY FOR RELATIVE DENSITY CONTROL                     BAN 335
C                                                                       BAN 340
      IF (RD.GT.A) F = 1.0+0.04*(RD-A)**1.5                             BAN 345
      R = 1.0/F*R                                                       BAN 350
      BANET = R*BAGROS                                                  BAN 355
      RETURN                                                            BAN 360
      END                                                               BAN 365-

      FUNCTION DINCR(AORG,HTADD,AGE,HS,BAPA,DNOW,FERTEF,HAT,ICT1,TTHN,  DIN   5
     $BAB,BAC)                                                          DIN  10
C                                                                       DIN  15
C      COMPUTE NET DIAMETER INCREMENT                                   DIN  20
C                                                                       DIN  25
C     6/4/80 SUB 04 STEP 10                                             DIN  30
C                                                                       DIN  35
C     ************ DEFINITION OF IMPORTANT DINCR VARIABLES ************ DIN  40
C                                                                       DIN  45
C     AGE    = BREAST HEIGHT STAND AGE                                  DIN  50
C     AORG   = STAND ORIGIN DECAY VARIABLE                              DIN  55
C     BAB    = BASAL AREA PER ACRE BEFORE MOST RECENT THINNING          DIN  60
C     BAC    = BASAL AREA PER ACRE CUT MOST RECENT THINNING             DIN  65
C     BAPA   = BASAL AREA PER ACRE                                      DIN  70
C     DNOW   = QUADRATIC MEAN STAND DIAMETER                            DIN  75
C     FERTEF = NITROGEN FERTILIZER EFFECT                               DIN  80
C     HAT    = STAND TOP HEIGHT AFTER MOST RECENT THINNING              DIN  85
C     HS     = STAND TOP HEIGHT                                         DIN  90
C     HTADD  = STAND TOP HEIGHT INCREMENT                               DIN  95
C     ICT1   = INDICATOR SWITCH FOR AT LEAST ONE COMMERCIAL THINNING DONDIN 100
C     PORPCT = STAND ORIGIN OR PRECOMMERCIAL THINNING VARIABLE THE LARGEDIN 105
C                   OF THE TWO VALUES FOR STANDS NOT COMMERCIALLY THINNEDIN 110
C     RD     = RELATIVE DENSITY                                         DIN 115
C     TTHN   = THINNING DECAY VARIABLE                                  DIN 120
C     ZZ     = THINNING EFFECT                                          DIN 125
C     ***************************************************************** DIN 130
C                                                                       DIN 135
      HATS = HAT                                                        DIN 140
      IF (HAT.LE.0.0) HATS = HS                                         DIN 145
      Z = HS-HATS                                                       DIN 150
      A = 0.99647                                                       DIN 155
      IF (TTHN.GT.0.0.AND.BAC.LE.0.0) GO TO 10                          DIN 160
C                                                                       DIN 165
C     ADJUST THINNING EFFECT COEFFICIENT FOR LIGHT THINNING LEVEL       DIN 170
C                                                                       DIN 175
      Q = BAC/BAB                                                       DIN 180
      IF (Q.GE.0.15) GO TO 10                                           DIN 185
      Q = Q/0.15                                                        DIN 190
      A = A*Q                                                           DIN 195
   10 CONTINUE                                                          DIN 200
      RD = BAPA/SQRT(DNOW)                                              DIN 205
      B = HS/DNOW/HTADD                                                 DIN 210
      Z = Z*B                                                           DIN 215
      IF (Z.GT.600.0) Z = 0.0                                           DIN 220
      ZZ = (Z*EXP(-Z))**(1.0/RD)                                        DIN 225
      IF (RD.LE.51.0) GO TO 20                                          DIN 230
      P = (RD-41.0)/10.0                                                DIN 235
      ZZ = ZZ/P                                                         DIN 240
   20 CONTINUE                                                          DIN 245
      PORPCT = 0.0                                                      DIN 250
      IF (AORG.GT.0.0.AND.ICT1.EQ.0) PORPCT = AORG                      DIN 255
      IF (TTHN.GT.0.0.AND.ICT1.EQ.0) PORPCT = TTHN                      DIN 260
      DINLOG = -3.6383-3.9828/AGE+1.701*ALOG(HTADD)/SQRT(AGE)+0.21584*  DIN 265
     $ALOG(HS/DNOW)-0.0074227*PORPCT*RD+2.8978*ALOG(DNOW)/RD+A*ZZ/SQRT  DIN 270
     $(AGE)+3.6183*ALOG(DNOW)/SQRT(AGE)+0.25949*PORPCT+0.86855*FERTEF   DIN 275
C                                                                       DIN 280
C     ADJUSTMENT FOR YOUNG, LOW DENSITY STANDS                          DIN 285
C                                                                       DIN 290
      RAG = 1.0579-0.002632*AGE                                         DIN 295
      IF (RAG.GT.1.0) RAG = 1.0                                         DIN 300
      R = TTHN*(100.0+(40.0-RD)/2.0)/100.0*RAG                          DIN 305
      IF (R.LT.1.0) R = 1.0                                             DIN 310
      IF (R.GT.1.10) R = 1.10                                           DIN 315
      DINCR = 1.03*R*EXP(DINLOG)                                        DIN 325
      RETURN                                                            DIN 330
      END                                                               DIN 335-

      FUNCTION VGRAT(DIAM,HS,AGE,TTHN,AORG,BA)                          VGR   5
C                                                                       VGR  10
C     CALCULATE VOLUME / BASAL AREA RATIO FOR LIVE STAND                VGR  15
C                                                                       VGR  20
C     7/12/79 SUB 04 STEP 12                                            VGR  25
C                                                                       VGR  30
C     ************ DEFINITION OF IMPORTANT VGRAT VARIABLES ************ VGR  35
C                                                                       VGR  40
C     AGE    = BREAST HEIGHT STAND AGE                                  VGR  45
C     AORG   = STAND ORIGIN DECAY VARIABLE                              VGR  50
C     BA     = BASAL AREA PER ACRE                                      VGR  55
C     DIAM   = QUADRATIC MEAN STAND DIAMETER                            VGR  60
C     HS     = STAND TOP HEIGHT                                         VGR  65
C     PORT   = STAND ORIGIN OR THINNING DECAY VARIABLE; WHICHEVER IS    VGR  70
C                   LARGEST IN VALUE                                    VGR  75
C     TTHN   = THINNING DECAY VARIABLE                                  VGR  80
C     ***************************************************************** VGR  85
C                                                                       VGR  90
      RD = BA/SQRT(DIAM)                                                VGR  95
      DILOG = ALOG(DIAM)                                                VGR 100
      HSLOG = ALOG(HS)                                                  VGR 105
      PORT = 0.0                                                        VGR 110
      IF (AORG.GT.0.0.AND.AORG.GE.TTHN) PORT = AORG                     VGR 115
      IF (TTHN.GT.0.0.AND.TTHN.GT.AORG) PORT = TTHN                     VGR 120
      VGRAT = EXP(0.42332+0.37319*HSLOG+0.00034474*RD+0.022759*PORT     VGR 125
     $ -0.24306*DILOG**2+0.26034*HSLOG*DILOG-0.00028026*AGE*HSLOG)      VGR 130
      RETURN                                                            VGR 135
      END                                                               VGR 140-

      FUNCTION VADJ(VGR,VG)                                             VAD   5
C                                                                       VAD  10
C      SCRATCH MEMO 7/14/80                                             VAD  15
C                                                                       VAD  20
      VGDIFF = (VG-VGR)/2.0                                             VAD  25
      VADJ = (VGR+VGDIFF)/(2.0*VGR)+VG/(2.0*(VGR+VGDIFF))               VAD  30
      RETURN                                                            VAD  35
      END                                                               VAD  40-

      FUNCTION GADJ(VGR,VG)                                             GAD   5
C                                                                       GAD  10
C      SCRATCH MEMO 7/14/80                                             GAD  15
C                                                                       GAD  20
      VGDIFF = (VG-VGR)/2.0                                             GAD  25
      GADJ = (VGR+VGDIFF)/(2.0*VG)+VGR/(2.0*(VGR+VGDIFF))               GAD  30
      RETURN                                                            GAD  35
      END                                                               GAD  40-

      FUNCTION DCODB(DNOW,J,AORG,TTHN)                                  DCO   5
C                                                                       DCO  10
C     CALCULATE (DIAMETER CUT/DIAMETER BEFORE) RATIO                    DCO  15
C                                                                       DCO  20
C      7/08/80 SCRATCH MEMO                                             DCO  25
C                                                                       DCO  30
C     ************ DEFINITION OF IMPORTANT DCODB VARIABLES ************ DCO  35
C                                                                       DCO  40
C     AORG   = STAND ORIGIN DECAY VARIABLE                              DCO  45
C     DNOW   = QUADRATIC MEAN STAND DIAMETER                            DCO  50
C     J      = COMMERCIAL THINNING NUMBER                               DCO  55
C     TTHN   = THINNING DECAY VARIABLE                                  DCO  60
C     ***************************************************************** DCO  65
C                                                                       DCO  70
      R = 1.05                                                          DCO  75
      IF (DNOW.LT.6.0) GO TO 10                                         DCO  80
      R = (8.0+0.1595*(DNOW-6.0)**1.5)/DNOW                             DCO  85
   10 CONTINUE                                                          DCO  90
      IF (R.GT.1.05) R = 1.05                                           DCO  95
      IF (TTHN.GT.0.0.AND.R.GT.1.0.OR.AORG.GT.0.0.AND.R.GT.1.0) R = 1.0 DCO 100
      IF (J.EQ.1) GO TO 20                                              DCO 105
      IF (DCODB.LE.0.85.AND.R.GT.0.85) R = 0.85                         DCO 110
   20 CONTINUE                                                          DCO 115
      DCODB = R                                                         DCO 120
      RETURN                                                            DCO 125
      END                                                               DCO 130-

      FUNCTION CTHTC(XNUM,TACUT,BONE,DCUT,DNOW,BACUT,IOBS,OBSAGE,OBSHTS,CTH   5
     $AGE)                                                              CTH  10
C                                                                       CTH  15
C     COMMERCIAL THINNING HEIGHT CHANGE                                 CTH  20
C                                                                       CTH  25
C     8/24/77 SUB 1 STEP 10                                             CTH  30
C                                                                       CTH  35
      DIMENSION OBSAGE(31), OBSHTS(2,31)                                CTH  40
C                                                                       CTH  45
C     ************ DEFINITION OF IMPORTANT CTHTC VARIABLES ************ CTH  50
C                                                                       CTH  55
C     AGE    = BREAST HEIGHT STAND AGE                                  CTH  60
C     BACUT  = BASAL AREA PER ACRE CUT                                  CTH  65
C     BONE   = BASAL AREA PER ACRE BEFORE CUT                           CTH  70
C     DCUT   = QUADRATIC MEAN DIAMETER OF CUT TREES                     CTH  75
C     DNOW   = QUADRATIC MEAN STAND DIAMETER BEFORE CUT                 CTH  80
C     IOBS   = NUMBER OF SPECIFIED STAND TOP HEIGHT OBSERVATIONS        CTH  85
C     OBSAGE = ARRAY OF BREAST HEIGHT STAND AGES FOR SPECIFIED STAND    CTH  90
C                   TOP HEIGHT OBSERVATIONS                             CTH  95
C     OBSHTS = ARRAY OF SPECIFIED STAND TOP HEIGHT OBSERVATIONS         CTH 100
C     RD     = RELATIVE DENSITY                                         CTH 105
C     TACUT  = NUMBER OF TREES PER ACRE CUT                             CTH 110
C     XNUM   = NUMBER OF TREES PER ACRE BEFORE CUT                      CTH 115
C     ***************************************************************** CTH 120
C                                                                       CTH 125
      CTHTC = 1.0                                                       CTH 130
      IF (IOBS.GT.0) GO TO 20                                           CTH 135
   10 CONTINUE                                                          CTH 140
      RATIO = DCUT/DNOW                                                 CTH 145
      IF (RATIO.LE.1.0) GO TO 50                                        CTH 150
      RD = BONE/DNOW**.5                                                CTH 155
      CTHTC = 1.0+.0000346999*TACUT-.212242*(TACUT/XNUM)**2-.00050593*  CTH 160
     $(TACUT/XNUM)*BONE+.100096*(TACUT/XNUM)*(DCUT/DNOW)+.468967*(TACUT/CTH 165
     $XNUM)*(BACUT/BONE)-.214834*(BACUT/BONE)*(DCUT/DNOW)-.0001071*     CTH 170
     $(BACUT/BONE)*XNUM+.0025045*(BACUT/BONE)*RD-.390331*((BACUT/BONE)* CTH 175
     $DCUT/DNOW)**3                                                     CTH 180
      GO TO 50                                                          CTH 185
C                                                                       CTH 190
   20 CONTINUE                                                          CTH 195
C                                                                       CTH 200
C     SPECIFIED STAND TOP HEIGHTS                                       CTH 205
C                                                                       CTH 210
      DO 30 I=1,IOBS                                                    CTH 215
      IF (OBSAGE(I).EQ.AGE) GO TO 40                                    CTH 220
   30 CONTINUE                                                          CTH 225
      GO TO 10                                                          CTH 230
C                                                                       CTH 235
   40 CONTINUE                                                          CTH 240
      CTHTC = OBSHTS(2,I)/OBSHTS(1,I)                                   CTH 245
   50 CONTINUE                                                          CTH 250
      RETURN                                                            CTH 255
      END                                                               CTH 260-

      FUNCTION GLTL(DNOW,TAGE)                                          GLT  5
C                                                                       GLT  10
C     CALCULATE LOWER THINNING LIMIT BASAL AREA PER ACRE                GLT  15
C                                                                       GLT  20
C     11/14/80 SCRATCH MEMO                                             GLT  25
C                                                                       GLT  30
C                                                                       GLT  35
C     ************ DEFINITION OF IMPORTANT GLTL VARIABLES ************  GLT  40
C                                                                       GLT  45
C     DNOW   = QUADRATIC MEAN STAND DIAMETER BEFORE CUT                 GLT  50
C     TAGE   = TOTAL AGE                                                GLT  55
C     ***************************************************************** GLT  60
C                                                                       GLT  65
      RDD = 20.91 + 6.775 * ALOG(DNOW)
      RDA = 1.134 + 9.526 * ALOG(TAGE)
      IF(RDA.GT.RDD) RDD = RDA
      IF(RDD.LT.35.0) RDD = 35.0
      GLTL = RDD * SQRT(DNOW)
      RETURN                                                            GLT  80
      END                                                               GLT  85-

      FUNCTION DIAM56(AORG,TTHN,D16,BA16,HS)                            D56   5
C                                                                       D56  10
C     CONVERT QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH   D56  15
C     TO QUADRATIC MEAN STAND DIAMETER OF TREES 5.6-INCHES + DBH        D56  20
C                                                                       D56  25
C     1/19/78 B SUB 1 STEP 6                                            D56  30
C                                                                       D56  35
C     ************ DEFINITION OF IMPORTANT DIAM56 VARIABLES *********** D56  40
C                                                                       D56  45
C     AORG   = STAND ORIGIN DECAY VARIABLE                              D56  50
C     BA16   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            D56  55
C     D16    = QUADRATIC MEAN DIAMETER OF TREES 1.6-INCHES + DBH        D56  60
C     HS     = STAND TOP HEIGHT                                         D56  65
C     TTHN   = THINNING DECAY VARIABLE                                  D56  70
C     ***************************************************************** D56  75
C                                                                       D56  80
      DIAM56 = D16*(.984357-.583464*AORG/D16+1.89968*HS/D16**3-12.3567* D56  85
     $HS/D16**5-30.8412*TTHN/D16**3+662.059*TTHN/D16**5-.0115069*(BA16/ D56  90
     $D16**.5)/D16)                                                     D56  95
      IF (DIAM56.LT.D16.OR.D16.GT.16.0) DIAM56 = D16                    D56 100
      RETURN                                                            D56 105
      END                                                               D56 110-

      FUNCTION BAPA56(AORG,DNOW,BAPA,HS)                                B56   5
C                                                                       B56  10
C     CONVERT BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH TO          B56  15
C     BASAL AREA PER ACRE IN TREES 5.6-INCHES + DBH                     B56  20
C                                                                       B56  25
C     1/19/78 B SUB 2 STEP 7                                            B56  30
C                                                                       B56  35
C     ************ DEFINITION OF IMPORTANT BAPA56 VARIABLES *********** B56  40
C                                                                       B56  45
C     AORG   = STAND ORIGIN DECAY VARIABLE                              B56  50
C     BAPA   = BASAL AREA PER ACRE OF TREES 1.6-INCHES + DBH            B56  55
C     DNOW   = QUADRATIC MEAN DIAMETER OF TREES 1.6-INCHES + DBH        B56  60
C     HS     = STAND TOP HEIGHT                                         B56  65
C     ***************************************************************** B56  70
C                                                                       B56  75
      BAPA56 = BAPA*(.893313+1.47633/DNOW-3055.41/DNOW**5+.00983207*HS/ B56  80
     $DNOW-2.03654*HS/DNOW**3+63.6836*HS/DNOW**5+37.5119*AORG/DNOW**3-  B56  85
     $1412.33*AORG/DNOW**5)                                             B56  90
      IF (BAPA56.GT.BAPA.OR.DNOW.GT.16.0) BAPA56 = BAPA                 B56  95
      RETURN                                                            B56 100
      END                                                               B56 105-

      FUNCTION CVTS56(AORG,D16,HS,CVTS16)                               C56   5
C                                                                       C56  10
C    CONVERT CUBIC-FOOT VOLUME OF TREES 1.6-INCHES + DBH TO CUBIC-      C56  15
C        FOOT VOLUME OF TREES 5.6-INCHES + DBH                          C56  20
C                                                                       C56  25
C      5/9/78 SUB 1 STEP 10                                             C56  30
C                                                                       C56  35
C     ************ DEFINITION OF IMPORTANT CVTS56 VARIABLES *********** C56  40
C                                                                       C56  45
C     AORG   = STAND ORIGIN DECAY VARIABLE                        \ 2   C56  50
C     CVTS16 = CUBIC-F\\\ VOLUME (TOTAL STEM) PER ACRE IN TREES         C56  55
C                   1.6-INCHES + DBH                                    C56  60
C     D16    = QUADRATIC MEAN DIAMETER OF TREES 1.6-INCHES + DBH        C56  65
C     HS     = STAND TOP HEIGHT                    \0                   C56  70
C    \*P*********************************************\*P***********\** C56  75
C                                                                       C56  80
      CVTS56 = CVTS16*(.975949+.255308*AORG/D16+.106148*HS/D16**2-2288. C56  85
     $48/D16**5-21.9646*HS/D16**4+121.832*HS/D16**5+2685.35/(HS*D16**3)-C56  90
     $81974.7/(HS*D16**5)-435.843*AORG/D16**5)                          C56  95
      IF (CVTS56.GT.CVTS16.OR.D16.GT.16.0) CVTS56 = CVTS16              C56 100
      RETURN                                                            C56 105
      END                                                               C56 110-

      FUNCTION CV456(AORG,TTHN,D16,BA16,HS,CVTS16,VC56)                 V45   5
C                                                                       V45  10
C     CONVERT CUBIC-FOOT VOLUME IN TREES 1.6-INCHES PLUS TO CUBIC-      V45  15
C        FOOT VOLUME (4-INCH TOP) IN TREES 5.6-INCHES + DBH             V45  20
C                                                                       V45  25
C     5/9/78 SUB 2 STEP 9                                               V45  30
C                                                                       V45  35
C     *********** DEFINITION OF IMPORTANT CV456 VARIABLES ************  V45  40
C                                                                       V45  45
C     AORG   = STAND ORIGIN DECAY VARIABLE                              V45  50
C     BA16   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            V45  55
C     D16    = QUADRATIC MEAN DIAMETER OF TREES 1.6-INCHES + DBH        V45  60
C     CVTS16 = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         V45  65
C                   1.6-INCHES + DBH                                    V45  70
C     HS     = STAND TOP HEIGHT                                         V45  75
C     TTHN   = THINNING DECAY VARIABLE                                  V45  80
C     VC56   = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         V45  85
C                   5.6-INCHES + DBH                                    V45  90
C     ***************************************************************** V45  95
C                                                                       V45 100
      CV456 = CVTS16*(.961397-768.661/D16**4+.138938*AORG/D16+.0330124* V45 105
     $HS/D16**2+42.8285/D16**3-13.9873*HS/D16**4+99.8628*HS/D16**5-515.9V45 110
     $*AORG/D16**5-1.09081*(BA16/D16**.5)/D16**4-34.8689*TTHN/D16**4)   V45 115
      R = 0.96*VC56                                                     V45 120
      IF (CV456.GT.R) CV456 = R                                         V45 125
      RETURN                                                            V45 130
      END                                                               V45 135-

      FUNCTION DCUT56(AORG,DC16,HS)                                     DC5   5
C                                                                       DC5  10
C     CONVERT QUADRATIC MEAN DIAMETER CUT TREES 1.6-INCHES + DBH TO     DC5  15
C     QUADRATIC MEAN DIAMETER CUT TREES 5.6-INCHES + DBH                DC5  20
C                                                                       DC5  25
C     2/8/78 SUB 1 STEP 3                                               DC5  30
C                                                                       DC5  35
C     ************ DEFINITION OF IMPORTANT DCUT56 VARIABLES *********** DC5  40
C                                                                       DC5  45
C     AORG   = STAND ORIGIN DECAY VARIABLE                              DC5  50
C     DC16   = QUADRATIC MEAN DIAMETER OF CUT TREES 1.6-INCHES + DBH    DC5  55
C     HS     = STAND TOP HEIGHT                                         DC5  60
C     ***************************************************************** DC5  65
C                                                                       DC5  70
      DCUT56 = DC16*(.99778-.150016*AORG/DC16+24.4189*HS/DC16**5)       DC5  75
      IF (DCUT56.LT.DC16.OR.DC16.GT.15.0) DCUT56 = DC16                 DC5  80
      IF (DCUT56.LE.5.6) DCUT56 = 0.0                                   DC5  85
      RETURN                                                            DC5  90
      END                                                               DC5  95-

      FUNCTION BAC56(DC16,BAC16,HS)                                     BC5   5
C                                                                       BC5  10
C     CONVERT BASAL AREA PER ACRE CUT IN TREES 1.6-INCHES + DBH TO      BC5  15
C     BASAL AREA PER ACRE CUT IN TREES 5.6-INCHES + DBH                 BC5  20
C                                                                       BC5  25
C     7/5/78 SUB 2 STEP 6                                               BC5  30
C                                                                       BC5  35
C     ************ DEFINITION OF IMPORTANT BAC56 VARIABLES ************ BC5  40
C                                                                       BC5  45
C     BAC16  = BASAL AREA PER ACRE CUT IN TREES 1.6-INCHES + DBH        BC5  50
C     DC16   = QUADRATIC MEAN DIAMETER CUT TREES 1.6-INCHES + DBH       BC5  55
C     HS     = STAND TOP HEIGHT                                         BC5  60
C     ***************************************************************** BC5  65
C                                                                       BC5  70
      BAC56 = BAC16*(.996566-40594.1/DC16**6+961584.0/DC16**8-1286970.0/BC5  75
     $(HS*DC16**6)+38406.5/(HS*DC16**4))                                BC5  80
      IF (BAC56.GT.BAC16.OR.DC16.GT.15.0) BAC56 = BAC16                 BC5  85
      IF (BAC56.LE.0.0) BAC56 = 0.0                                     BC5  90
      RETURN                                                            BC5  95
      END                                                               BC5 100-

      FUNCTION CVC56(DC16,HS,D16,BA16,CVC16)                            CV5   5
C                                                                       CV5  10
C     CONVERT CUBIC-FOOT VOLUME CUT (TOTAL STEM) 1.6-INCHES + DBH       CV5  15
C      TO CUBIC FOOT-VOLUME CUT (TOTAL STEM) 5.6-INCHES + DBH           CV5  20
C                                                                       CV5  25
C     4/25/78 SUB 2 STEP 6                                              CV5  30
C                                                                       CV5  35
C     ************ DEFINITION OF IMPORTANT CVC56 VARIABLES ************ CV5  40
C                                                                       CV5  45
C     BA16   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            CV5  50
C     CVC16  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE CUT IN TREES     CV5  55
C                   1.6-INCHES + DBH                                    CV5  60
C     D16    = QUADRATIC MEAN DIAMETER OF TREES 1.6-INCHES + DBH        CV5  65
C     DC16   = QUADRATIC MEAN DIAMETER OF CUT TREES 1.6-INCHES + DBH    CV5  70
C     HS     = STAND TOP HEIGHT                                         CV5  75
C     ***************************************************************** CV5  80
C                                                                       CV5  85
      CVC56 = CVC16*(.99674-26160.8/DC16**6+797.08*HS/DC16**8+2020.83/  CV5  90
     $(HS*DC16**3)-808.02*(BA16/D16**.5)/DC16**7)                       CV5  95
      IF (CVC56.GT.CVC16.OR.DC16.GT.15.0) CVC56 = CVC16                 CV5 100
      IF (CVC56.LE.0.0) CVC56 = 0.0                                     CV5 105
      RETURN                                                            CV5 110
      END                                                               CV5 115-

      FUNCTION CVC456(DC16,HS,CVC16,VAC56)                              CV4   5
C                                                                       CV4  10
C     CONVERT CUBIC-FOOT VOLUME CUT (TOTAL STEM) 1.6-INCHES + DBH       CV4  15
C     TO CUBIC-FOOT VOLUME CUT (4-INCH TOP) 5.6-INCHES + DBH            CV4  20
C                                                                       CV4  25
C     4/20/78 SUB 3 STEP 4                                              CV4  30
C                                                                       CV4  35
C     ************ DEFINITION OF IMPORTANT CVC456 VARIABLES *********** CV4  40
C                                                                       CV4  45
C     CVC16  = CUBIC-FOOT VOLUME (TOTAL STEM) CUT IN TREES 1.6-INCHES   CV4  50
C                   + DBH                                               CV4  55
C     DC16   = QUADRATIC MEAN DIAMETER CUT TREES 1.6-INCHES + DBH       CV4  60
C     HS     = STAND TOP HEIGHT                                         CV4  65
C     VAC56  = CUBIC-FOOT VOLUME (TOTAL STEM) CUT IN TREES 5.6-INCHES   CV4  70
C                   + DBH                                               CV4  75
C     ***************************************************************** CV4  80
C                                                                       CV4  85
      CVC456 = CVC16*(.967377-641.703/DC16**4+878.217/DC16**5+.634701*HSCV4  90
     $/DC16**4)                                                         CV4  95
      R = 0.96*VAC56                                                    CV4 100
      IF (CVC456.GT.R) CVC456 = R                                       CV4 105
      IF (CVC456.LE.0.0) CVC456 = 0.0                                   CV4 110
      RETURN                                                            CV4 115
      END                                                               CV4 120-

      FUNCTION CVM56(DM16,CVM16)                                        VM5   5
C                                                                       VM5  10
C     ESTIMATE CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE MORTALITY        VM5  15
C     IN TREES 5.6-INCHES + DBH                                         VM5  20
C                                                                       VM5  25
C      2/26/79 SUB 1 STEP 4                                             VM5  30
C                                                                       VM5  35
C     ************ DEFINITION OF IMPORTANT CVM56 VARIABLES ************ VM5  40
C                                                                       VM5  45
C     CVM16  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE MORTALITY IN     VM5  50
C                   TREES 1.6-INCHES + DBH                              VM5  55
C     DM16   = QUADRATIC MEAN DIAMETER OF MORTALITY TREES 1.6-INCHES    VM5  60
C                   + DBH                                               VM5  65
C     ***************************************************************** VM5  70
C                                                                       VM5  75
      FF = 5.065-271.93/DM16**5+160.5/DM16**4-9.5417/SQRT(DM16)-0.1079* VM5  80
     $DM16                                                              VM5  85
      IF (FF.GT.0.98) FF = 0.98                                         VM5  90
      CVM56 = CVM16*FF                                                  VM5  95
      IF (CVM56.LT.0.0) CVM56 = 0.0                                     VM5 100
      RETURN                                                            VM5 105
      END                                                               VM5 110-

      FUNCTION DIAM76(AORG,TTHN,D16,BA16,HS,D56)                        D76   5
C                                                                       D76  10
C     CONVERT QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH   D76  15
C     TO QUADRATIC MEAN STAND DIAMETER OF TREES 7.6-INCHES + DBH        D76  20
C                                                                       D76  25
C     1/25/78 SUB 1 STEP 15                                             D76  30
C                                                                       D76  35
C     ************ DEFINITION OF IMPORTANT DIAM76 VARIABLES *********** D76  40
C                                                                       D76  45
C     AORG   = STAND ORIGIN DECAY VARIABLE                              D76  50
C     BA16   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            D76  55
C     D16    = QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH  D76  60
C     HS     = STAND TOP HEIGHT                                         D76  65
C     TTHN   = THINNING DECAY VARIABLE                                  D76  70
C     ***************************************************************** D76  75
C                                                                       D76  80
      DIAM76 = D16*(1.07445-.609012*AORG/D16+3.86485*HS/D16**3-124.037* D76  85
     $HS/D16**5-2.48009/D16+8786.25/D16**5-20.0203*TTHN/D16**3+8368.65/ D76  90
     $(HS*D16**3)-448096.0/(HS*D16**5)-.00821434*(BA16/D16**.5)/D16)    D76  95
      IF (DIAM76.LT.D16.OR.D16.GT.18.0) DIAM76 = D16                    D76 100
      IF (DIAM76.LT.D56) DIAM76 = D56                                   D76 105
      RETURN                                                            D76 110
      END                                                               D76 115-

      FUNCTION BA76(AORG,TTHN,D16,BA16,HS,G56)                          B76   5
C                                                                       B76  10
C     CONVERT BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH TO          B76  15
C     BASAL AREA PER ACRE IN TREES 7.6-INCHES + DBH                     B76  20
C                                                                       B76  25
C     1/20/78 SUB 10 STEP 15                                            B76  30
C                                                                       B76  35
C     ************ DEFINITION OF IMPORTANT BA76 VARIABLES ************  B76  40
C                                                                       B76  45
C     AORG   = STAND ORIGIN DECAY VARIABLE                              B76  50
C     BA16   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            B76  55
C     D16    = QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH  B76  60
C     G56    = BASAL AREA PER ACRE IN TREES 5.6-INCHES + DBH            B76  65
C     HS     = STAND TOP HEIGHT                                         B76  70
C     TTHN   = THINNING DECAY VARIABLE                                  B76  75
C     ***************************************************************** B76  80
C                                                                       B76  85
      BA76 = BA16*(1.36153-.0237196*HS/D16+1.11498*AORG/D16-79.8001*AORGB76  90
     $/D16**3-53.6643/HS+23409.5/HS**3+4116.87/D16**5+639.163/(HS*D16)- B76  95
     $41771.5/(HS*D16**3)+556335.0/(HS*D16**5)-687.478*TTHN/D16**5-.    B76 100
     $428632*(BA16/D16**.5)/D16**3)                                     B76 105
      IF (BA76.GT.BA16.OR.D16.GT.18.0) BA76 = BA16                      B76 110
      IF (BA76.GT.G56) BA76 = G56                                       B76 115
      RETURN                                                            B76 120
      END                                                               B76 125-

      FUNCTION CVTS76(AORG,D16,BA16,HS,CVTS16,VA56)                     C76   5
C                                                                       C76  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) IN TREES 1.6-INCHES        C76  15
C     + DBH TO CUBIC-FOOT VOLUME (TOTAL STEM) IN TREES 7.6-INCHES       C76  20
C     + DBH                                                             C76  25
C                                                                       C76  30
C     1/24/78 SUB 9 STEP 9                                              C76  35
C                                                                       C76  40
C     ************ DEFINITION OF IMPORTANT CVTS76 VARIABLES *********** C76  45
C                                                                       C76  50
C     AORG   = STAND ORIGIN DECAY VARIABLE                              C76  55
C     BA16   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            C76  60
C     CVTS16 = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         C76  65
C                   1.6-INCHES + DBH                                    C76  70
C     D16    = QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH  C76  75
C     HS     = STAND TOP HEIGHT                                         C76  80
C     VA56   = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         C76  85
C                   5.6-INCHES + DBH                                    C76  90
C     ***************************************************************** C76  95
C                                                                       C76 100
      CVTS76 = CVTS16*(.972557+.37322*AORG/D16+133.694/(HS*D16)-21229.9/C76 105
     $(HS*D16**3)+404110.0/(HS*D16**5)-1912.74*AORG/D16**5-.249844*(BA16C76 110
     $/D16**.5)/D16**3)                                                 C76 115
      IF (CVTS76.GT.CVTS16) CVTS76 = CVTS16                             C76 120
      IF (CVTS76.GT.VA56) CVTS76 = VA56                                 C76 125
      RETURN                                                            C76 130
      END                                                               C76 135-

      FUNCTION CV476(AORG,TTHN,D16,BA16,HS,CVTS16,VA76,VA456)           V47   5
C                                                                       V47  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) IN TREES 1.6-INCHES        V47  15
C     + DBH TO CUBIC-FOOT VOLUME (4-INCH TOP) IN TREES 7.6-INCHES       V47  20
C     + DBH                                                             V47  25
C                                                                       V47  30
C     5/8/78 SUB 4 STEP 12                                              V47  35
C                                                                       V47  40
C     ************ DEFINITION OF IMPORTANT CV476 VARIABLES ************ V47  45
C                                                                       V47  50
C     AORG   = STAND ORIGIN DECAY VARIABLE                              V47  55
C     BA16   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            V47  60
C     CVTS16 = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         V47  65
C                   1.6-INCHES + DBH                                    V47  70
C     D16    = QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH  V47  75
C     HS     = STAND TOP HEIGHT                                         V47  80
C     TTHN   = THINNING DECAY VARIABLE                                  V47  85
C     VA76   = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         V47  90
C                   7.6-INCHES + DBH                                    V47  95
C     VA456  = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE IN TREES         V47 100
C                   5.6-INCHES + DBH                                    V47 105
C     ***************************************************************** V47 110
C                                                                       V47 115
      CV476 = CVTS16*(1.23073+87.0257/D16**3-36.3249/HS-.0204814*HS/D16+V47 120
     $501.03/(HS*D16)-38884.7/(HS*D16**3)+595741.0/(HS*D16**5)-1060.09* V47 125
     $AORG/D16**5-11.7193*TTHN/D16**3-.00805095*(BA16/D16**.5)/D16+     V47 130
     $22461400.0/HS**5)                                                 V47 135
      R1 = 0.96*VA76                                                    V47 140
      IF (CV476.GT.R1) CV476 = R1                                       V47 145
      IF (CV476.GT.VA456) CV476 = VA456                                 V47 150
      RETURN                                                            V47 155
      END                                                               V47 160-

      FUNCTION CV676(AORG,TTHN,D16,BA16,HS,CVTS16,VA76,VA476)           V67   5
C                                                                       V67  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) IN TREES 1.6-INCHES + DBH  V67  15
C     TO CUBIC-FOOT VOLUME (6-INCH TOP) IN TREES 7.6-INCHES + DBH       V67  20
C                                                                       V67  25
C     12/21/77  SUB 3 STEP 7                                            V67  30
C                                                                       V67  35
C     ************ DEFINITION OF IMPORTANT CV676 VARIABLES ************ V67  40
C                                                                       V67  45
C     AORG   = STAND ORIGIN DECAY VARIABLE                              V67  50
C     BA16   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            V67  55
C     CVTS16 = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         V67  60
C                   1.6-INCHES + DBH                                    V67  65
C     D16    = QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH  V67  70
C     HS     = STAND TOP HEIGHT                                         V67  75
C     TTHN   = THINNING DECAY VARIABLE                                  V67  80
C     VA76   = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         V67  85
C                   7.6-INCHES + DBH                                    V67  90
C     VA476  = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE IN TREES         V67  95
C                   7.6-INCHES + DBH                                    V67 100
C     ***************************************************************** V67 105
C                                                                       V67 110
      CV676 = CVTS16*(.984543+6.55643*HS/D16**5-19687.8/(HS*D16**3)+    V67 115
     $393312.0/(HS*D16**5)-28.83*AORG/D16**3-13.588*TTHN/D16**3-.       V67 120
     $00890657*(BA16/D16**.5)/D16+16376.3/HS**3)                        V67 125
      R1 = 0.96*VA76                                                    V67 130
      R2 = 0.99*VA476                                                   V67 135
      IF (CV676.GT.R1) CV676 = R1                                       V67 140
      IF (CV676.GT.R2) CV676 = R2                                       V67 145
      RETURN                                                            V67 150
      END                                                               V67 155-
      FUNCTION VI676(AORG,TTHN,D16,BA16,HS,CVTS16,VA676)                VI6   5
C                                                                       VI6  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) IN TREES 1.6-INCHES + DBH  VI6  15
C     TO INTERNATIONAL 1/4-INCH BOARD-FOOT VOLUME (6-INCH TOP) IN TREES VI6  20
C     7.6-INCHES + DBH                                                  VI6  25
C                                                                       VI6  30
C     5/8/78 SUB 7 STEP 14                                              VI6  35
C                                                                       VI6  40
C     ************ DEFINITION OF IMPORTANT VI676 VARIABLES ************ VI6  45
C                                                                       VI6  50
C     AORG   = STAND ORIGIN DECAY VARIABLE                              VI6  55
C     BA16   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            VI6  60
C     CVTS16 = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         VI6  65
C                   1.6-INCHES + DBH                                    VI6  70
C     D16    = QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH  VI6  75
C     HS     = STAND TOP HEIGHT                                         VI6  80
C     TTHN   = THINNING DECAY VARIABLE                                  VI6  85
C     VA676  = CUBIC-FOOT VOLUME (6-INCH TOP) PER ACRE IN TREES         VI6  90
C                   7.6-INCHES + DBH                                    VI6  95
C     ***************************************************************** VI6 100
C                                                                       VI6 105
      VI676 = CVTS16*(8.04286-1329.66/D16**3-192.389/HS+20839.0/D16**5+ VI6 110
     $106.122*HS/D16**5-139.485*AORG/D16**3-66.0059*TTHN/D16**3-.0556108VI6 115
     $*(BA16/D16**.5)/D16+98099100.0/HS**5)                             VI6 120
      R = 7.05*VA676                                                    VI6 125
      IF (VI676.GT.R) VI676 = R                                         VI6 130
      RETURN                                                            VI6 135
      END                                                               VI6 140-

      FUNCTION SV676(AORG,TTHN,D16,BA16,HS,CVTS16,VI676,cv476,xn76,d76, SV6   5
     $ isv)                                                             SV6   6
C                                                                       SV6  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) IN TREES 1.6-INCHES        SV6  15
C     + DBH TO SCRIBNER BOARD-FOOT VOLUME (6-INCH TOP) IN TREES         SV6  20
C     7.6-INCHES + DHB                                                  SV6  25
C                                                                       SV6  30
C     5/8/78 SUB 6 STEP 12                                              SV6  35
C                                                                       SV6  40
C     ************ DEFINITION OF IMPORTANT SV676 VARIABLES ************ SV6  45
C                                                                       SV6  50
C     AORG   = STAND ORIGIN DECAY VARIABLE                              SV6  55
C     BA16   = BASAL AREA PER ACRE IN TREES 1.6-INCHES + DBH            SV6  60
C     CVTS16 = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN TREES         SV6  65
C                   1.6-INCHES + DBH                                    SV6  70
c     cv476  = Cubic-foot volume to 4-inch top per acre in trees        sv6  71
c                   7.6-inches+ dbh                                     sv6  72
C     D16    = QUADRATIC MEAN STAND DIAMETER OF TREES 1.6-INCHES + DBH  SV6  75
c     d76    = Quadratic mean stand diameter of trees 7.6-inches+ dbh   sv6  76
C     HS     = STAND TOP HEIGHT                                         SV6  80
c     isv    = 32-foot log (default) / 16-foot log switch               sv6  81
C     TTHN   = THINNING DECAY VARIABLE                                  SV6  85
C     VI676  = INTERNATIONAL 1/4-INCH BOARD-FOOT VOLUME (6-INCH TOP)    SV6  90
C                   PER ACRE IN TREES 7.6-INCHES + DBH                  SV6  95
c     xn76   = Number of trees per acre 7.6-inches+ dbh                 sv6  96
C     ***************************************************************** SV6 100
C                                                                       SV6 105
      SV676 = CVTS16*(8.34551-15.541/D16-232.834/HS-5.55669*HS/D16**3+  SV6 110
     $166.021*HS/D16**5-103.673*AORG/D16**3-53.4672*TTHN/D16**3-.0471846SV6 115
     $*(BA16/D16**.5)/D16+118540000.0/HS**5)                            SV6 120
      R = 0.99*VI676                                                    SV6 125
      IF (SV676.GT.R) SV676 = R                                         SV6 130
      if (isv.ge.1) return                                              sv6 131
c                                                                       sv6 132
c      convert 16-foot log rule to 32-foot log rule Scribner Volume     sv6 133
c                                                                       sv6 134
      t = 0.91273 * cv476 / xn76 / (0.005454154 * d76 * d76 - 0.087266) sv6 135
      sv676 = sv676 * (1.001491 - 6.924097 / t + 0.00001351 * d76 * d76)sv6 136
      RETURN                                                            SV6 137
      END                                                               SV6 140-

      FUNCTION DCUT76(DC16,HS,DC56)                                     DC7   5
C                                                                       DC7  10
C     CONVERT QUADRATIC MEAN DIAMETER CUT TREES 1.6-INCHES + DBH        DC7  15
C     TO QUADRATIC MEAN DIAMETER CUT TREES 7.6-INCHES + DBH             DC7  20
C                                                                       DC7  25
C     3/2/78 B SUB 4 STEP 5                                             DC7  30
C                                                                       DC7  35
C     ************ DEFINITION OF IMPORTANT DCUT76 VARIABLES *********** DC7  40
C                                                                       DC7  45
C     DC16   = QUADRATIC MEAN DIAMETER OF CUT TREES 1.6-INCHES + DBH    DC7  50
C     HS     = STAND TOP HEIGHT                                         DC7  55
C     ***************************************************************** DC7  60
C                                                                       DC7  65
      DCUT76 = DC16*(1.03177+53.8217/DC16**2-.246223*HS/DC16**2-233.096/DC7  70
     $(HS*DC16))                                                        DC7  75
      IF (DCUT76.LT.DC16.OR.DC16.GT.18.0) DCUT76 = DC16                 DC7  80
      IF (DCUT76.LT.DC56) DCUT76 = DC56                                 DC7  85
      IF (DCUT76.LE.7.6) DCUT76 = 0.0                                   DC7  90
      RETURN                                                            DC7  95
      END                                                               DC7 100-

      FUNCTION BAC76(DC16,BAC16,HS,GC56)                                BC7   5
C                                                                       BC7  10
C     CONVERT BASAL AREA IN CUT TREES 1.6-INCHES + DBH                  BC7  15
C     TO BASAL AREA IN CUT TREES 7.6-INCHES + DBH                       BC7  20
C                                                                       BC7  25
C     3/6/78 B SUB 1 STEP 7                                             BC7  30
C                                                                       BC7  35
C     ************ DEFINITION OF IMPORTANT BAC76 VARIABLES ************ BC7  40
C                                                                       BC7  45
C     BAC16  = BASAL AREA PER ACRE IN CUT TREES 1.6-INCHES + DBH        BC7  50
C     DC16   = QUADRATIC MEAN DIAMETER OF CUT TREES 1.6-INCHES + DBH    BC7  55
C     GC56   = BASAL AREA PER ACRE IN CUT TREES 5.6-INCHES + DBH        BC7  60
C     HS     = STAND TOP HEIGHT                                         BC7  65
C     ***************************************************************** BC7  70
C                                                                       BC7  75
      BAC76 = BAC16*(1.00712-192.551/DC16**3+3888.64/DC16**5+70373.0/(HSBC7  80
     $*DC16**3)+2931700.0/(HS*DC16**5)-934558.0/(HS*DC16**4))           BC7  85
      IF (BAC76.GT.BAC16.OR.DC16.GT.18.0) BAC76 = BAC16                 BC7  90
      IF (BAC76.GT.GC56) BAC76 = GC56                                   BC7  95
      IF (BAC76.LE.0.0) BAC76 = 0.0                                     BC7 100
      RETURN                                                            BC7 105
      END                                                               BC7 110-

      FUNCTION CVC76(DC16,HS,CVC16,VAC56)                               CV7   5
C                                                                       CV7  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) IN CUT TREES 1.6-INCHES    CV7  15
C     + DBH TO CUBIC-FOOT VOLUME (TOTAL STEM) IN CUT TREES 7.6-INCHES   CV7  20
C     + DBH                                                             CV7  25
C                                                                       CV7  30
C     2/7/78 SUB 4 STEP 8                                               CV7  35
C                                                                       CV7  40
C     ************ DEFINITION OF IMPORTANT CVC76 VARIABLES ************ CV7  45
C                                                                       CV7  50
C     CVC16  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE CUT IN TREES     CV7  55
C                  1.6-INCHES + DBH                                     CV7  60
C     DC16   = QUADRATIC MEAN DIAMETER CUT TREES 1.6-INCHES + DBH       CV7  65
C     HS     = STAND STOP HEIGHT                                        CV7  70
C     VAC56  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE CUT IN TREES     CV7  75
C                   5.6-INCHES + DBH                                    CV7  80
C     ***************************************************************** CV7  85
C                                                                       CV7  90
      CVC76 = CVC16*(.993026-89.5832/DC16**3+1306570.0/DC16**8-9631.5*  CV7  95
     $(HS/DC16**8)+2671.7/(HS*DC16**2)-24941.7/(HS*DC16**3))            CV7 100
      IF (CVC76.GT.CVC16.OR.DC16.GT.18.0) CVC76 = CVC16                 CV7 105
      IF (CVC76.GT.VAC56) CVC76 = VAC56                                 CV7 110
      IF (CVC76.LE.0.0) CVC76 = 0.0                                     CV7 115
      RETURN                                                            CV7 120
      END                                                               CV7 125-

      FUNCTION CVC476(DC16,HS,CVC16,VAC76,VAC456)                       C47   5
C                                                                       C47  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) CUT IN TREES               C47  15
C     1.6-INCHES + DBH TO CUBIC-FOOT VOLUME (4-INCH TOP) CUT            C47  20
C     IN TREES 7.6-INCHES + DBH                                         C47  25
C                                                                       C47  30
C     5/5/78 SUB 1 STEP 10                                              C47  35
C                                                                       C47  40
C     ************ DEFINITION OF IMPORTANT CVC476 VARIABLES *********** C47  45
C                                                                       C47  50
C     CVC16  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE CUT IN TREES     C47  55
C                   1.6-INCHES + DBH                                    C47  60
C     DC16   = QUADRATIC MEAN DIAMETER OF CUT TREES 1.6-INCHES + DBH    C47  65
C     HS     = STAND TOP HEIGHT                                         C47  70
C     VAC76  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE CUT IN TREES     C47  75
C                   7.6-INCHES + DBH                                    C47  80
C     VAC456 = CUBIC-FOOT VOLUME (4-INCH TOP) PER ACRE CUT IN TREES     C47  85
C                   5.6-INCHES + DBH                                    C47  90
C     ***************************************************************** C47  95
C                                                                       C47 100
      CVC476 = CVC16*(.915105-83.8359/DC16**3+6240260.0/(HS*DC16**6)-   C47 105
     $1242810.0/(HS*DC16**5)+9.67241/HS)                                C47 110
      R1 = 0.96*VAC76                                                   C47 115
      IF (CVC476.GT.R1) CVC476 = R1                                     C47 120
      IF (CVC476.GT.VAC456) CVC476 = VAC456                             C47 125
      IF (CVC476.LE.0.0) CVC476 = 0.0                                   C47 130
      RETURN                                                            C47 135
      END                                                               C47 140-

      FUNCTION CVC676(DC16,HS,CVC16,VAC76,VAC476)                       C67   5
C                                                                       C67  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) CUT IN TREES               C67  15
C     1.6-INCHES + DBH TO CUBIC-FOOT VOLUME (6-INCH TOP) CUT IN         C67  20
C     TREES 7.6-INCHES + DBH                                            C67  25
C                                                                       C67  30
C     5/3/78C SUB 5 STEP 8                                              C67  35
C                                                                       C67  40
C     ************ DEFINITION OF IMPORTANT CVC676 VARIABLES *********** C67  45
C                                                                       C67  50
C     CVC16  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE CUT IN TREES     C67  55
C                   1.6-INCHES + DBH                                    C67  60
C     DC16   = QUADRATIC MEAN DIAMETER OF CUT TREES 1.6-INCHES + DBH    C67  65
C     HS     = STAND TOP HEIGHT                                         C67  70
C     VAC76  = CUBIC-FOOT VOLUME (TOTAL STEM) CUT IN TREES 7.6-INCHES   C67  75
C                   + DBH                                               C67  80
C     VAC476 = CUBIC-FOOT VOLUME (4-INCH TOP) CUT IN TREES 7.6-INCHES   C67  85
C                   + DBH                                               C67  90
C     ***************************************************************** C67  95
C                                                                       C67 100
      CVC676 = CVC16*(.708274+10.1904/DC16-92.0439/DC16**2+1215.26/DC16*C67 105
     $*4-.0943753*HS/DC16**2-125129.0/(HS*DC16**4)+549227.0/(HS*DC16**5)C67 110
     $)                                                                 C67 115
      R1 = 0.96*VAC76                                                   C67 120
      R2 = 0.99*VAC476                                                  C67 125
      IF (CVC676.GT.R1) CVC676 = R1                                     C67 130
      IF (CVC676.GT.R2) CVC676 = R2                                     C67 135
      IF (CVC676.LE.0.0) CVC676 = 0.0                                   C67 140
      RETURN                                                            C67 145
      END                                                               C67 150-

      FUNCTION VIC676(DC16,HS,CVC16,VAC676)                             IC6   5
C                                                                       IC6  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) CUT IN TREES 1.6-INCHES    IC6  15
C     + DBH TO INTERNATIONAL 1/4-INCH BOARD-FOOT VOLUME (6-INCH TOP)    IC6  20
C     CUT IN TREES 7.6-INCHES + DBH                                     IC6  25
C                                                                       IC6  30
C     5/3/78 C SUB 9 STEP 7                                             IC6  35
C                                                                       IC6  40
C     ************ DEFINITION OF IMPORTANT VCI676 VARIABLES *********** IC6  45
C                                                                       IC6  50
C     CVC16  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE CUT IN TREES     IC6  55
C                   1.6-INCHES + DBH                                    IC6  60
C     DC16   = QUADRATIC MEAN DIAMETER OF CUT TREES 1.6-INCHES + DBH    IC6  65
C     HS     = STAND TOP HEIGHT                                         IC6  70
C     VAC676 = CUBIC-FOOT VOLUME (6-INCH TOP) PER ACRE CUT IN TREES     IC6  75
C                   7.6-INCHES + DBH                                    IC6  80
C     ***************************************************************** IC6  85
C                                                                       IC6  90
      VIC676 = CVC16*(6.95049+62.942/DC16-671.623/DC16**2-.113959*HS/   IC6  95
     $DC16+8156.0/DC16**4-171.945/HS)                                   IC6 100
      R = 7.05*VAC676                                                   IC6 105
      IF (VIC676.GT.R) VIC676 = R                                       IC6 110
      IF (VIC676.LE.0.0) VIC676 = 0.0                                   IC6 115
      RETURN                                                            IC6 120
      END                                                               IC6 125-

      FUNCTION CVS676(DC16,HS,CVC16,VCI676,cv476,xn76,d76,isv)          CS6   5
C                                                                       CS6  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) CUT IN TREE 1.6-INCHES     CS6  15
C     + DBH TO SCRIBNER BOARD-FOOT VOLUME (6-INCH TOP) CUT IN TREES     CS6  20
C     7.6-INCHES + DBH                                                  CS6  25
C                                                                       CS6  30
C     5/3/78 C SUB STEP 4                                               CS6  35
C                                                                       CS6  40
C     ************ DEFINITION OF IMPORTANT CVS676 VARIABLES *********** CS6  45
C                                                                       CS6  50
C     CVC16  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE CUT IN TREES     CS6  55
C                   1.6-INCHES + DBH                                    CS6  60
c     cv476 = cubic-foot volume cut to a 4-inch top per acre in trees
c                   7.6-inches+ dbh
C     DC16   = QUADRATIC MEAN DIAMETER OF CUT TREES 1.6-INCHES + DBH    CS6  65
c     d76    = Quadratic mean diameter of cut trees 7.6-inches+ dbh
C     HS     = STAND TOP HEIGHT                                         CS6  70
c     isv    = 32-foot log rule (default) / 16-foot log rule switch
C     VCI676 = INTERNATIONAL 1/4-INCH BOARD-FOOT VOLUME (6-INCH TOP)    CS6  75
C                   PER ACRE CUT IN TREES 7.6-INCHES + DBH              CS6  80
c     xn76   = Number of trees cut per acre 7.6-inches= dbh
C     ***************************************************************** CS6  85
C                                                                       CS6  90
      CVS676 = CVC16*(9.75211-32.992/DC16-.115131*HS/DC16+365922.0/(HS* CS6  95
     $DC16**5)-187.155/HS)                                              CS6 100
      R = 0.99*VCI676                                                   CS6 105
      IF (CVS676.GT.R) CVS676 = R                                       CS6 110
      IF (CVS676.LE.0.0) CVS676 = 0.0                                   CS6 115
      if (isv.ge.1) return                                              
c                                                                       
c      convert 16-foot log rule to 32-foot log rule Scribner Volume     
c                                                                       
      t = 0.91273 * cv476 / xn76 / (0.005454154 * d76 * d76 - 0.087266) 
      cvs676 = cvs676 * (1.001491 - 6.924097 / t + 0.00001351 * d76 * 
     $d76)
      RETURN                                                            CS6 120
      END                                                               CS6 125-

      FUNCTION CVM76(DM16,CVM16,VAM56)                                  VM7   5
C                                                                       VM7  10
C     CONVERT CUBIC-FOOT VOLUME (TOTAL STEM) IN MORTALITY TREES         VM7  15
C     1.6-INCHES + DBH TO CUBIC-FOOT VOLUME (TOTAL STEM) IN             VM7  20
C     MORTALITY TREES 7.6-INCHES + DBH                                  VM7  25
C                                                                       VM7  30
C      2/27/79  SUB 1 STEP 4                                            VM7  35
C                                                                       VM7  40
C     ************ DEFINITION OF IMPORTANT CVM76 VARIABLES ************ VM7  45
C                                                                       VM7  50
C     CVM16  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN MORTALITY     VM7  55
C                   TREES 1.6-INCHES + DBH                              VM7  60
C     DM16   = QUADRATIC MEAN DIAMETER OF MORTALITY TREES 1.6-INCHES    VM7  65
C                   + DBH                                               VM7  70
C     VAM56  = CUBIC-FOOT VOLUME (TOTAL STEM) PER ACRE IN MORTALITY     VM7  75
C                   TREES 5.6-INCHES + DBH                              VM7  80
C     ***************************************************************** VM7  85
C                                                                       VM7  90
      FFF = 8.8335-500.67/DM16**8+28.01/DM16-30.638/SQRT(DM16)-0.11872  VM7  95
     $ * DM16                                                           VM7 100
      IF (FFF.GT.0.96) FFF = 0.96                                       VM7 105
      CVM76 = CVM16*FFF                                                 VM7 110
      IF (CVM76.LT.0.0) CVM76 = 0.0                                     VM7 115
      IF (CVM76.GT.(0.98*VAM56)) CVM76 = 0.98*VAM56                     VM7 120
      IF (CVM76.LE.0.0) CVM76 = 0.0                                     VM7 125
      RETURN                                                            VM7 130
      END                                                               VM7 135-
