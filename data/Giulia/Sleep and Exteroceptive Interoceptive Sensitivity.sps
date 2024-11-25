* Encoding: UTF-8.

**ATTENTION CHECK

RENAME VARIABLES (FIRST_10 = AttentionCheck1) (MPS_15 = AttentionCheck2) (SeSS_36 = AttentionCheck3) (ASP_SS_16 = AttentionCheck4) (ASP_SA_12 = AttentionCheck5) 
    (HSP_28 = AttentionCheck6).
    
RECODE AttentionCheck1 (4=0) (SYSMIS=SYSMIS) (else=1) INTO AC1_FAIL.
RECODE AttentionCheck2 (0=0) (SYSMIS=SYSMIS) (else=1) INTO AC2_FAIL.
RECODE AttentionCheck3 (4=0) (SYSMIS=SYSMIS) (else=1) INTO AC3_FAIL.
RECODE AttentionCheck4 (5=0) (SYSMIS=SYSMIS) (else=1) INTO AC4_FAIL.
RECODE AttentionCheck5 (1=0) (SYSMIS=SYSMIS) (else=1) INTO AC5_FAIL.
RECODE AttentionCheck6 (1=0) (SYSMIS=SYSMIS) (else=1) INTO AC6_FAIL.

VARIABLE LABELS
AC1_Fail 'Attention check 1 - pass or fail'
AC2_Fail 'Attention check 2 - pass or fail'
AC3_Fail 'Attention check 3 - pass or fail'
AC4_Fail 'Attention check 4 - pass or fail'
AC5_Fail 'Attention check 5 - pass or fail'
AC6_Fail 'Attention check 6 - pass or fail'.

VALUE LABELS 
AC1_Fail TO AC6_Fail 1'FAIL' 0'PASS'.

*Calculate total attention check fails

COMPUTE Total_Attention_Fail=SUM(AC1_FAIL TO AC6_FAIL).
VARIABLE LABELS Total_Attention_Fail 'Total score for failed attention checks - higher values indicate more failed attention checks'. 

**filer out participants who failed at least 1 attention check

USE ALL.
COMPUTE filter_$=(Total_Attention_Fail = 0).
VARIABLE LABELS filter_$ 'Total_Attention_Fail = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

**HISTOGRAMS
    
EXAMINE VARIABLES=FIRST_SUM APS_MEAN PS_Arousal_SUM PS_Arousal_Somatic_SUM PS_Arousal_Cognitive_SUM SeSS_SUM
HSP_MEAN IAS_ATT_MEAN IAS_ACC_MEAN ASP_SS_MEAN ASP_SA_MEAN ASP_MEAN ISI_SUM MPS_YN_COUNT
MPS_COUNT  
  /PLOT BOXPLOT HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

**AROUSAL/ STRESS VARIABLES

**(FIRST) FORD INSOMNIA RESPONSE TO STRESS
    
COMPUTE FIRST_SUM=SUM(FIRST_1 TO FIRST_9). 
VARIABLE LABELS  FIRST_SUM 'Insomnia to stress - summed score from 9 to 36 - higher scores = '+
    'worse insomnia in stress'.

RELIABILITY
  /VARIABLES=FIRST_1 TO FIRST_9
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*Cronbach's alpha = .801
    
**(APS) AROUSAL PREDISPOSITION SCORE

RECODE APS_1 (1=5) (2=4) (3=3) (4=2) (5=1) INTO rAPS_1.

COMPUTE APS_MEAN=MEAN(rAPS_1,APS_2,APS_3,APS_4,APS_5,APS_6,APS_7,APS_8,APS_9,APS_10,APS_11,APS_12).
VARIABLE LABELS APS_MEAN 'Arousal predisposition scale - average score - higher score = greater arousablity'. 

RELIABILITY
  /VARIABLES=rAPS_1,APS_2,APS_3,APS_4,APS_5,APS_6,APS_7,APS_8,APS_9,APS_10,APS_11,APS_12
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*Cronbach's alpha = .842
    
**(PS_AROUSAL) PRE-SLEEP AROUSAL

**PS_AROUSAL SOMATIC

COMPUTE PS_Arousal_Somatic_SUM=SUM(PS_Arousal_9,PS_Arousal_10,PS_Arousal_11,PS_Arousal_12,PS_Arousal_13,PS_Arousal_14,PS_Arousal_15,PS_Arousal_16).
VALUE LABELS PS_Arousal_Somatic_SUM 'Pre-sleep arousal somatic subscale - summed score- higher score = more arousability'.

RELIABILITY
  /VARIABLES=PS_Arousal_9 TO PS_Arousal_16
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .879

**PS_AROUSAL COGNITIVE

COMPUTE PS_Arousal_Cognitive_SUM=SUM(PS_Arousal_1,PS_Arousal_2,PS_Arousal_3,PS_Arousal_4,PS_Arousal_5,PS_Arousal_6,PS_Arousal_7,PS_Arousal_8).
VALUE LABELS PS_Arousal_Cognitive_SUM 'Pre-sleep arousal cognitive subscale - summed score- higher score = more arousability'.

RELIABILITY
  /VARIABLES=PS_Arousal_1 TO PS_Arousal_8
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .895
 
**PS_AROUSAL SUM

COMPUTE PS_Arousal_SUM=SUM(PS_Arousal_1 TO PS_Arousal_16).
VALUE LABELS PS_Arousal_SUM 'pre sleep arousal - summed score - higher score = more arousability'.


VALUE LABELS PS_Arousal_SUM 'Pre-sleep arousal - summed score- higher score = more arousability'.


RELIABILITY
  /VARIABLES=PS_Arousal_1 TO PS_Arousal_16
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .918

**SENSITIVITY VARIABLES
    
**(SeSS) SENSORY SENSITIVITY SCALE  

COMPUTE SeSS_SUM=SUM(SeSS_1 TO SeSS_35).
VARIABLE LABELS SeSS_SUM 'Sensory Sensitivity scale - summed score from 27 to 189 - higher score = more sensitivity'.

RELIABILITY
  /VARIABLES=SeSS_1 TO SeSS_35
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .926

**(HSP) HIGHLY SENSITIVE PERSON SCALE
    
COMPUTE HSP_MEAN=MEAN(HSP_1 TO HSP_27).
VARIABLE LABELS HSP_MEAN 'HSP scale, higher score = greater sensitivity'.

RELIABILITY
  /VARIABLES=HSP_1 TO HSP_27
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .904
    
**(IAS)INTEROCEPTIVE ATTENTION SCALE
    
**(IAS_ATT_MEAN) INTEROCEPTIVE ATTENTION
    
COMPUTE IAS_ATT_MEAN=MEAN.21(IAS_ATT_1 TO IAS_ATT_21).
VARIABLE LABELS IAS_ATT_MEAN 'Trait interoceptive ATTENTION - higher scores = greater attention to bodily experiences'.

RELIABILITY
  /VARIABLES=IAS_ATT_1 TO IAS_ATT_21
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .944

**(IAS_ACC_MEAN) INTEROCEPTIVE ACCURACY

COMPUTE IAS_ACC_MEAN=MEAN.21(IAS_ACC_1 TO IAS_ACC_21).
VARIABLE LABELS IAS_ACC_MEAN 'Trait interoceptive ACCURACY - higher scores = greater accuracy of bodily experiences'.
    
RELIABILITY
  /VARIABLES=IAS_ACC_1 TO IAS_ACC_21
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .917
    
**(ASP) ADULT SENSY PROFILE

**(ASP_SS_MEAN) ADULT SENSORY PROFILE - SENSORY SENSITIVITY

COMPUTE ASP_SS_MEAN=MEAN(ASP_SS_1,ASP_SS_2,ASP_SS_3,ASP_SS_4,ASP_SS_5,ASP_SS_6,ASP_SS_7,ASP_SS_8,ASP_SS_9,ASP_SS_10,ASP_SS_11,ASP_SS_12,ASP_SS_13,ASP_SS_14,ASP_SS_15).
VARIABLE LABELS ASP_SS_MEAN 'Adult sensory profile - mean score - higher score = greater sensitivity'.

RELIABILITY
  /VARIABLES=ASP_SS_1,ASP_SS_2,ASP_SS_3,ASP_SS_4,ASP_SS_5,ASP_SS_6,ASP_SS_7,ASP_SS_8,ASP_SS_9,ASP_SS_10,ASP_SS_11,ASP_SS_12,ASP_SS_13,ASP_SS_14,ASP_SS_15
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .857
    
***ASP_SS INDIVIDUAL SESORY PROFILES
    - taste/SMELL : ASP_SS_1
    -  movement processing: ASP_SS_2 and ASP_SS_3 and ASP_SS_4
    - visual processing: ASP_SS_5 and  ASP_SS_6 and ASP_SS_7
    - touch processing:  ASP_SS_8  ASP_SS_9  ASP_SS_10 ASP_SS_11
    - activity level: ASP_SS_12
    - auditory processing: ASP_SS_13  ASP_SS_14  ASP_SS_15***

**SS TASTE/ SMELL

COMPUTE SS_T_S = MEAN(ASP_SS_1).
VARIABLE LABELS  SS_T_S"Adult sensory profile - sensory sensitivity - taste/smell processing  sub-scale". 

**SS MOVEMENT PROCESSING

COMPUTE SS_MOVE = MEAN(ASP_SS_2 TO ASP_SS_4).
    VARIABLE LABELS SS_MOVE"Adult sensory profile - sensory sensitivity - movement processing sub-scale". 

RELIABILITY
  /VARIABLES=ASP_SS_2 TO ASP_SS_4 
  /SCALE('SS_MOVE') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .543

**SS VISUAL PROCESSING

COMPUTE SS_VISU = MEAN(ASP_SS_5 TO ASP_SS_7).
    VARIABLE LABELS SS_VISU"Adult sensory profile - sensory sensitivity - visual processing sub-scale". 

RELIABILITY
  /VARIABLES=ASP_SS_5 TO ASP_SS_7
  /SCALE('SS_VISU') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbch's alpha = .677

**SS TOUCH PROCESSING

COMPUTE SS_TOUCH = MEAN(ASP_SS_8 TO ASP_SS_11).
VARIABLE LABELS SS_TOUCH "Adult sensory profile - sensory sensitivity - touch processing sub-scale". 

RELIABILITY
  /VARIABLES=ASP_SS_8 TO ASP_SS_11
  /SCALE('SS_TOUCH') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .571

**ACTIVITY LEVEL

COMPUTE SS_ACTIV = MEAN(ASP_SS_12).
VARIABLE LABELS SS_ACTIV "Adult sensory profile - sensory sensitivity - activity level sub-scale".

**SS AUDITORY PROCESSING

COMPUTE SS_AUDI = MEAN( ASP_SS_13 TO ASP_SS_15).
    VARIABLE LABELS SS_AUDI "Adult sensory profile - sensory sensitivity - auditory processing sub-scale".

RELIABILITY
  /VARIABLES=ASP_SS_13 TO ASP_SS_15
  /SCALE('SS_AUDI') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .711

**(ASP_SA_MEAN) ADULT SENSORY PROFILE - SENSORY AVOIDANCE

COMPUTE ASP_SA_MEAN=MEAN(ASP_SA_1,ASP_SA_2,ASP_SA_3,ASP_SA_4,ASP_SA_5,ASP_SA_6,ASP_SA_7,ASP_SA_8,ASP_SA_9,ASP_SA_10,ASP_SA_11,ASP_SA_13,ASP_SA_14,ASP_SA_15,ASP_SA_16).
VARIABLE LABELS ASP_SA_MEAN 'Adult sensory profile - mean score - higher score = more avoidance'.

RELIABILITY
  /VARIABLES=ASP_SA_1,ASP_SA_2,ASP_SA_3,ASP_SA_4,ASP_SA_5,ASP_SA_6,ASP_SA_7,ASP_SA_8,ASP_SA_9,ASP_SA_10,ASP_SA_11,ASP_SA_13,ASP_SA_14,ASP_SA_15,ASP_SA_16
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .864
    
***ASP_SA INDIVIDUAL SESORY PROFILES
    - taste/SMELL : ASP_SA_1 ASP_SA_2
    -  movement processing: ASP_SA_3 
    - visual processing: and ASP_SA _4 ASP_SA_5 ASP_SA_6  
    - touch processing:  ASP_SA_7 ASP_SA_8  ASP_SA_9  
    - activity level: ASP_SA_10 ASP_SA_11 ASP_SA_12
    - auditory pprocessing: ASP_SA_13  ASP_SA_14  ASP_SA_15***

**SA TASTE/ SMELL

COMPUTE SA_T_S = MEAN(ASP_SA_1 TO ASP_SA_2).
    VARIABLE LABELS SA_T_S"Adult sensory profile - sensory avoidance - taste/smell processing sub-scale". 

RELIABILITY
  /VARIABLES=ASP_SA_1 TO ASP_SA_2
  /SCALE('SA_T_S') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha .320

**SA MOVEMENT PROCESSING

COMPUTE SA_MOVE = MEAN(ASP_SA_3).
VARIABLE LABELS SA_MOVE"Adult sensory profile - sensory avoidance - movement processing sub-scale". 

**SA VISUAL PROCESSING

COMPUTE SA_VISU = MEAN( ASP_SA_4 TO ASP_SA_6).
VARIABLE LABELS SA_VISU"Adult sensory profile - sensory avoidance - visual processing sub-scale". 

RELIABILITY
  /VARIABLES=ASP_SA_4 TO ASP_SA_6
  /SCALE('SA_VISU') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*Cronbach's alpha = .314

**SA TOUCH PROCESSING

COMPUTE SA_TOUCH = MEAN(ASP_SA_7 TO ASP_SA_9).
 VARIABLE LABELS SA_TOUCH"Adult sensory profile - sensory avoidance - touch processing sub-scale". 

RELIABILITY
  /VARIABLES=ASP_SA_7 TO ASP_SA_9
  /SCALE('SA_TOUCH') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .695

**SA ACTIVITY LEVEL

COMPUTE SA_ACTIV = MEAN(ASP_SA_10,ASP_SA_11,ASP_SA_13).
VARIABLE LABELS SA_ACTIV"Adult sensory profile - sensory avoidance - activity level sub-scale".

RELIABILITY
  /VARIABLES=ASP_SA_10 ASP_SA_11 ASP_SA_13
  /SCALE('SA_ACTIV') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .660

**SA AUDITOY PROCESSING

COMPUTE SA_AUDI = MEAN(ASP_SA_14 TO ASP_SA_16).
VARIABLE LABELS SA_AUDI"Adult sensory profile - sensory avoidance - auditory processing sub-scale".

RELIABILITY
  /VARIABLES=ASP_SA_14 TO ASP_SA_16
  /SCALE('SA_AUDI') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.
  
*cronbach's alpha = .700

**(ASP_MEAN) ADULT SENSORY PROFILE OVERALL MEAN
    
COMPUTE ASP_MEAN=MEAN(ASP_SA_MEAN,ASP_SS_MEAN).
VARIABLE LABELS ASP_MEAN'Adult sensory profile mean score of SA & SS combined - higher score = more avoidance and greater sensitivity'.

**SLEEP VARIABLES
    
**(ISI) INSOMNIA SEVERITY INDEX

RENAME VARIABLES (ISI1_1 = ISI_1) (ISI1_2= ISI_2) (ISI1_3 = ISI_3) (ISI2 = ISI_4) (ISI3 = ISI_5) (ISI4 = ISI_6) (ISI5 = ISI_7).

COMPUTE ISI_SUM = SUM(ISI_1 TO ISI_7).
VARIABLE LABELS ISI_SUM 'Insomia Severity Index - summed score from 7 to 35 - greater score indicates greater insomnia'.

RELIABILITY
  /VARIABLES=ISI_1 TO ISI_7
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .853

**(MPS) MUNICH PARASOMNIA 

RECODE MPS_1 MPS_2 MPS_3 MPS_4 MPS_5 MPS_6 MPS_7 MPS_8 MPS_9 MPS_10 MPS_11 MPS_12 MPS_13 MPS_14 MPS_16 MPS_17 MPS_18 MPS_19 MPS_20
      MPS_21 MPS_22 (0=0) (1=1) (2=1) (3=1) (4=1) (5=1) (6=1) INTO rMPS_1 rMPS_2 rMPS_3 rMPS_4 rMPS_5 rMPS_6 rMPS_7 rMPS_8 rMPS_9 rMPS_10 rMPS_11 rMPS_12 rMPS_13 rMPS_14 rMPS_16 rMPS_17 rMPS_18 rMPS_19 rMPS_20 rMPS_21 rMPS_22.

COMPUTE MPS_YN_COUNT = SUM(rMPS_1,rMPS_2,rMPS_3,rMPS_4,rMPS_5,rMPS_6,rMPS_7,rMPS_8,rMPS_9,rMPS_10,rMPS_11,rMPS_12,rMPS_13,rMPS_14,rMPS_16,rMPS_17,rMPS_18,rMPS_19,rMPS_20,rMPS_21,rMPS_22).
VARIABLE LABELS MPS_YN_COUNT 'Munich parasomnia scale - score out of 21 - higher scores indicate gretaer number of parasomnias experienced'.

COMPUTE MPS_COUNT = SUM(MPS_1,MPS_2,MPS_3,MPS_4,MPS_5,MPS_6,MPS_7,MPS_8,MPS_9,MPS_10,MPS_11,MPS_12,MPS_13,MPS_14,MPS_16,MPS_17,MPS_18,MPS_19,MPS_20,MPS_21,MPS_22).
VARIABLE LABELS MPS_COUNT 'Munich parasomnia scale - summed scored taking into account the frequency of experience - greater score indicates a higher number of parasomnias experienced more frequently'.

RELIABILITY
  /VARIABLES=MPS_1,MPS_2,MPS_3,MPS_4,MPS_5,MPS_6,MPS_7,MPS_8,MPS_9,MPS_10,MPS_11,MPS_12,MPS_13,MPS_14,MPS_16,MPS_17,MPS_18,MPS_19,MPS_20,MPS_21,MPS_22
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*cronbach's alpha = .841

