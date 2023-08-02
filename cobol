~
       IDENTIFICATION DIVISION.
       PROGRAM-ID.            BE3010X.
       AUTHOR.                INFOSYS.
       DATE-WRITTEN.          AUG 2022.
      ****
      *
      * PARM LINK (BE3010 R) BIND (STATIC-ONLY) PLAN (BE3010EZ)
      * SQLSYNTAX (NONE)
      *
      * MigrationWare Easytrieve to COBOL Translator V3.0.0 MAIN
      *
      ****
      ********************* COMPILE OPTION 109 *************************
      ************************ COBOL ***********************************
      ******************************************************************
      *                                                                *
      * TRANID(S): NONE                                                *
      *                                                                *
      * PROGRAM NAME: BE3010, BUILD FLAT FILE FROM WEB ENROLLMENT      *
      *               UNLOAD TO UPDATE THE BENEFIT_ENROLLMENT TABLE    *
      * PROGRAM NUMBER: BE3010                                         *
      *                                                                *
      * PROGRAM DESCRIPTION:  READS THE FILE UNLOADED FROM THE WEB     *
      *                       ENROLLMENT SESSIONS AND CREATES A FLAT   *
      *                       FILE FOR BE480 TO USE TO UPDATE THE      *
      *                       BENEFIT_ENROLLMENT TABLE.                *
      * SYSTEM RELATIONSHIP:                                           *
      *                                                                *
      * 1. FILE/DATABASE UTILIZATION:                                  *
      *                                                                *
      *    A. NAME  : BENEFIT_ENROLLMENT                               *
      *       DESC  : BENEFITS ENROLLMENT TABLE                        *
      *       TYPE  : INPUT                                            *
      *       LRECL : DB2                                              *
      *                                                                *
      *    B. NAME  : N01.BEN.BE3000D.BEPDATA                          *
      *       DESC  : UNLOAD FILE FROM WEB ENROLLMENT TABLES           *
      *       TYPE  : INPUT                                            *
      *       LRECL : 136                                              *
      *                                                                *
      *    C. NAME  : N01.BEN.BE3010D.BE3010.UPDATES.                  *
      *       DESC  : DATA FILE TO UPDATE BENEFIT_ENROLLMENT TABLE     *
      *       TYPE  : OUTPUT                                           *
      *       LRECL : 190                                              *
      *                                                                *
      * 2. REPORT CONSIDERATIONS  : NONE                               *
      *                                                                *
      * 3. SUBPROGRAM UTILIZATION : NONE                               *
      *                                                                *
      * 4. MACROS:                                                     *
      *                                                                *
      *    A. NAME  : NONE                                             *
      *       DESC  :                                                  *
      *
      * 5. FORMULAS/SPECIAL CONSIDERATIONS : NONE                      *
      *                                                                *
      * 6. CONTROL-CARD DESCRIPTIONS : NONE                            *
      *                                                                *
      * 7. PARMS : NONE                                                *
      *                                                                *
      ******************************************************************
      * -----------------------  PROGRAM HISTORY -----------------------
      * 10/10/99 KRTESTE   INITIAL INSTALLATION
      * 11/06/99 JLSMELT   MODIFIED CODE FOR CATEGORY SPACE INSTANCES
      * 03/22/00 KRTESTE   MODIFIED CODE FOR CHANGES IN PROCESSING
      * 05/23/00 JLSMELT   MODIFIED CODE FOR NEW WEB ENROLLMENT
      * 06/21/00 JLSMELT   MODIFIED CODE FOR DLI CUTBACK, REASON 'MA'
      *                    AND LOOKUP FOR DENIAL LETTER FOR DLI
      * 07/07/00 KRTESTE   MODIFIED CODE TO FIX COPY-TO-PAYROLL-FL
      * 07/10/00 JLSMELT   MODIFIED CODE FOR STD + FOR  REASON   'BI'
      *                    TO USE EVENT DATE FOR COVERAGE START.
      * 07/31/00 JLSMELT  -MODIFIED CODE FOR STD + FOR REASONS 'DI','MA'
      *                    TO USE FIRST OF MONTH COVERAGE START FOR
      *                    CATEGORY CHANGES FROM A TO B OR B TO A.
      *                   -ADDED WITH UR TO SELECTS THAT DID NOT ALREADY
      *                    HAVE IT.
      * 08/17/00 JLSMELT  -MODIFIED CODE FOR DLI REASON 'MA' WHEN SPOUSE
      *                    GOING FROM NO COVERAGE TO COVERAGE AND CHILD
      *                    ALREADY HAS COVERAGE.
      * 08/29/00 SSMITH3  -ADDED CODE TO ACCOMMODATE THE INTRODUCTION AN
      *                    ON GOING PROCESSING OF THE PLANS (STD/LTD)
      *          KRTESTE  -FIXED CODE NOT TO GIVE NE'S 1 YEAR WAITS
      * 09/08/00 JLSMELT  -MODIFIED FOR MA CHANGE FOR THE AE RECORD TO
      *                    GIVE GUARANTEED COVERAGE IN THE NEW YEAR
      *                    FOR 'ALI' AND 'DLI'
      * 09/21/00 KRTESTE  -SET THE SW-DEBUG-SWITCH TO 'Y' FOR DISPLAYS
      * 10/13/00 JLSMELT  -MODIFIED FOR DLI LIMITS IN STATES LS, MD, WA
      *                    BASED ON ALI SELECTIONS
      *                   -MODIFIED FOR FSC GETTING THE INCORRECT
      *                    RESPONSES AND OR DATES DEPENDING ON WHETHER
      *                    AE RECORD/S ENTERED BEFORE OR AFTER OR AT ALL
      *                    WITHIN THE SAME DAY.
      * 12/07/00 JLSMELT  -COMMENTED CODE THAT BYPASSES MA,DI FOR
      *                    MED,DEN,& ADD SINCE WEB DOES NOT PASS THESE
      *                    FOR THE CURRENT YEAR IN MONTH 12
      * 06/27/01 JLSMELT  -ADD FSC CODES 'GS' AND 'LE' FOR FIRST OF
      *                    MONTH PRORATED ITEMS.
      * 01/30/02 KRTESTE  -CHANGED CODE TO LEAVE PREVIOUS HMO PROVIDER
      *                    WHEN CANCELLING MEDICAL COVERAGE
      * 03/07/02 HJMUNI   -CHANGED CODE TO PUT ENROLLMENT-LATE-IND FOR
      *                    EACH PLAN.
      * 05/20/02 KRTESTE  -CHANGED CODE TO MAKE STD/LTD EFFECTIVE 1 YEAR
      *                    FROM SELECTION DATE DURING AE
      * 05/22/02 GAPATIE  -CHANGED CODE TO INCLUDE SPECIAL ENROLLMENT
      * 08/29/02 HJMUNI   -CHANGED CODE FOR DLI FOR FL,WA,MD
      * 07/30/02 JLSMELT  -*J HOST BROKENS FIXES START HERE FOR OE 2003
      *                   -*J CHANGED TO ALLOW ALI GUAR COV IF LD AND
      *                   -STILL NEWLY ELIGIBLE
      * 10/30/02 GAPATIE  -FIX COPY OF COVERAGE SELECTION FROM CURRENT
      *                    YEAR CHANGE TO NEXT YEAR FOR ANNUAL ENROLLMEN
      * 12/30/02 HJMUNI   -CHANGED TO HAVE FSC EFFECTIVE DT AS EVENT DAT
      * 01/30/03 HJMUNI   -CHANGED TO FSC'S LOGIC FOR ALI,DLI,STD AND ST
      * 02/13/03 HJMUNI   -FIXED ISUUE WITH DLI
      * 09/17/03 JLSMELT  -IF AE, AND PREV CHG IS FSC PICK UP OF COVERAG
      *                    FIRST TIME FOR ALI OR DLI, ALLOW SAME OR LESS
      *                    COVERAGE ON THE AE RECORD, MAX IS GUARANTEED
      *                    COVERAGE FROM THE FSC RECORD, CAN BE LESS.
      * 09/18/03 JLSMELT  -IF LD, AND HAS PREV PROCESSABLE FSC SAME DAY
      *                    CHANGE LD TO THE PREV FSC CODE AND TYPE TO TH
      * 09/30/03 HJMUNI   -CORRECTED NEWLY ELIGIBLE LOGIC FOR 180 DAYS
      * 02/18/03 HJMUNI   -CHANGED FOR MD SPECIAL HMO
      * 08/15/04 SARAZA   -CHANGED TO CHECK THE LETTER CODE FOR ALI & DL
      * 09/07/04 SARAZA   -CHANGED TO PUT CHANGES IN BE3010
      * 09/14/04 SARAZA   -CHANGED TO PUT DATE CHECK IN CURSOR4A
      * 01/04/05 HJMUNI   -REMOVE SPECIAL LOGIC FOR DLI CUT BACK FOR
      *                    FL, WA , MD
      * 02/07/05 GAPATIE  -ALLOW LD PLANS TO BE PROCESSED FOR SE SESSION
      *                    ALSO ENSURE CATEGORY FOR PLAN ADD IS CHECKED
      * 08/04/05 E1RAO    -CHG PROGRAM TO PROCESS BROKER ID FILED.
      * 09/29/05 HJMUNI   -ADD BROKER ID TO CHECK FOR SAME COVRAGE
      * 04/17/06 RAFREY   -PEAK TIME ELIG - 365 DAYS WAIT PERIOD
      *                    CURSOR8 AND 3 OTHER LINE CHANGES
      * 05/23/06 RAFREY   -PEAK TIME ELIG -
      * 06/14/07 DMODALI  - OUT-POCKET-MAX-CD AND UP-FRONT-ALLOW-CD ARE
      *                     TO BENEFIT ENROLLMENT TABLE
      * 05/16/08 RDLEON   - ADDED STD+ VALIDATION.
      * 05/21/08 RDLEON   - HANDLE STP NOTFOUND IN CURSOR10 TO PREVENT A
      * 09/10/08 B0KULES  - ADDED CHECKS FOR STD/LTD TO SET 1 YR WAIT FO
      *          RDLEON     NE ASSOCS.  ADDED CHECKS FOR ALI/DLI TO SET
      *                     COVERAGE TO 1. FIXED WAIT DAYS FOR STD IF AS
      *                     HAS CATEGORY CHANGE.
      * 05/20/09 D0MUNIY  - CODE CHANGE TO RETAIN THE EVENT DATE AS
      *                     COVERAGE START DATE DURING FSC SESSION
      * 10/07/10 ANANDAK  - CODE CHANGE FOR CHANGE IN EFFECTIVE DATES AE
      *                     FOR ALI, DLI, STD, LTD FOR AE2011
      * 10/17/10 ANANDAK  - AE2011-FIX
      *                   1.THE ORIGINAL COVERAGE DATE POPLUATION IS REM
      *                     FOR LTD AND STD DROP
      *                   2.CODE CHANGES MADE TO POPULATE NEXT PLAN YEAR
      *                     WHEN ASSOCIATE INCREASES/PICKS UP ALI VIA FO
      *                     AE SESSION
      * 09/20/11 LRAJA0   -AE2011 BENEFITS RAPID RESPONSE TEAM FIX
      *                    MODIFIED CODE FOR STD AND LTD
      *                    SESSION DATE IS ASSIGNED TO COVERAGE START DA
      *                    WHEN THERE IS A COVERAGE DROP FOR STD AND LTD
      * 11/21/11 LRAJA0   -AE2011 BENEFITS RAPID RESPONSE TEAM FIX
      *                    COMMENTED CODE THAT GAVE A 1 YR WAIT FOR MGMT
      *                    ASSOCIATES ENROLLING FOR LTD
      * 11/25/11 RMERUVA  -CODE FIXED TO CHANGE NOT TO HAVE PROVIDER ID
      *                    WHEN THERE IS NO MEDCIAL COVERAGE *
      * 10/11/13 AGOVIN2  -AE2014 CHANGES (PROJ. ID 315177) TO INCLUDE
      *                    VISION PLAN CHANGES
      *          KBALAT0  -LIFE CHANGES TO HANDLE NEW ALI PLANS (E & F)
      *                    ADDED AN AUTOAPPROVAL LOGIC FOR 2013 TO APPRO
      *                    LIFE PLANS FOR ASC CHANGING COV FROM 200K TO
      *                    AND FROM 750K TO 1M
      *          ABALU    -PARTNER CHANGES TO PROVIDE GUARANTEED COVERAG
      *                    AMOUNT FOR DLI PLAN IF A PARTNER OR
      *                    (SPOUSE WITH SAME GENDER AS THAT OF ASSSOCIAT
      *                    (SPOUSE WITH UNION TYPE AS COMMON LAW IS PRES
      * 10/11/13 SJAYAPE  -CHANGED THE CODE TO LOOK AT THE DEPENDENT_CHA
      *                    WHEN DP IS NOT FOUND IN INSURED RELATION TABL
      *                    THIS CHANGE IS DONE TO GIVE THE  GURANTED AMO
      *                    FOR ASSOCIATES WHO ENROLLED DLI FOR DP IN AE
      *                    CHANGES CAN BE FOUND WITH COMMENT TAG
      *                    AE2014-DP-FIX      * VERSION 42
      * 10/30/13 SJAYAPE  -CHANGED THE CODE TO LOOK AT THE DEPENDENT_CHA
      *                    FOR ALL THE RELATION TYPES INSTEAD OF DP AND
      *                    CHANGES CAN BE FOUND WITH COMMENT TAG
      *                    43-AE2014-DP-FIX
      * 02/04/14 AAGORE   -CHANGED AS PER PCR# 10025822
      *                    CHANGE IS TO RETAIN COVERAGE START DATE ON
      *                    ALI/DLI PLANS FOR A FT ASSOCIATE WHO HAS PERF
      *                    -ED NE SESSION AND SELECTED ALI/DLI COVERAGE
      * 10/15/14 TVIVA1   -AE2015- CHANGE TO REMOVE THE AUTO APPROVAL FO
      *                    LIFE PLANS FOR ASC CHANGING COV FROM 200K TO
      *                    AND FROM 750K TO 1M
      * 01/01/15 AMANI1  CLARITY ID - 320243, YEYB2015 CHANGES
      *                  POPULATE COVERAGE START DATE FOR TRANSITIONED
      *                  ASSOCIATES AND CALCULATE WAIT DAYS FOR UPDATING
      *                  THE BENEFIT_ENROLLMENT TABLE.
      *                  TAG - ***YEYB-2015 CHANGES
      * 02/05/15 MBHAR1  CLARITY ID - 320243, YEYB2015 CHANGES
      *                  PROCESS THE ASSOCIATES WHO GOT TRANSITIONED FRO
      *                  PT/TEMP TO FT/MGMT BY RETAINING THE COVERAGE TH
      *                  ASSOCIATE WAS ELIGIBLE PRIOR TO ASSOCIATE'S
      *                  TRANSITION TAG - **JOB TRANSITION CHANGES
      * 02/16/15 MBHAR1  CLARITY ID - 320243, YEYB2015 JOB TRANSITION FI
      *                  FULL TIME - 89 ASSOCIATES WHOSE WAIT DAY QUANTI
      *                  IS GREATER THAN 60 IS SET AS 60 DAYS.
      *                  TAG - **JOB TRANSITION FIX BEGINS
      * 03/20/15 AMANI1  CLARITY ID - 320243, JOB TRANITION CODE FIX
      *                  WAIT DAY QUANTITY IS NOT CALCULATED FOR ASSOCIA
      *                  WHO GET TRANSITIONED FROM PT/TEMP TO FT-89 WITH
      *                  89 DAYS OF HIRE.
      * 06/04/15 SSUND1  AS PART OF AE2016 PROJECT , THE CODE IS HANDLED
      *                  FOR PART TIME / TEMPORARY ASSOCAITES.P/T ASSOCI
      *                  ARE ELIGIBLE FOR ADD,DEN,ALI & DLI PLANS FROM 2
      *                  IF THEY CHOOSE THE LIFE PLANS IN 2015 IT WILL B
      *                  EFFECTIVE ONLY ON 01-01-2016.
      * 06/24/15 SSUND1  PROJECT ID 322293, PRODUCTION ISSUE FIX
      *                  PROVIDE GUARANTEED AMOUNT FOR THE MANAGEMENT AN
      *                  FULL TIME HOURLY ASSOCIATES WHEN THEY OPT FOR
      *                  GUARANTEED AMOUNT IN MARRIAGE SESSION(FSC)
      * 07/04/15 AMANI1  CHANGED ASSOCIATE TRANSITION QUERY TO FETCH THE
      *                  ASSOCIATES WHO ARE DOING SESSION ON 60TH DAY
      * 07/15/15 SSUND1  AS PART OF AE2016 PROJECT , THE CODE IS HANDLED
      *                  FOR PART TIME / TEMPORARY TRUCK DRIVERS AND THE
      *                  ARE ELIGIBLE FOR ADD,DEN,ALI & DLI PLANS FROM 2
      *                  IF THEY CHOOSE THE LIFE PLANS IN 2015 IT WILL B
      *                  EFFECTIVE ONLY ON 01-01-2016.
      *                  ALSO REMOVED 2 UNUSED CURSOR (OFFCRCHK,MGMTCHK)
      *                  DECLARATION FROM THE CODE.
      * 07/20/15 MNEMA1  AS PART OF AE2016 PROJECT, THE CODE IS HANDLED
      *                  FOR CHANGING THE COVERAGE OPTION CODES FOR STD
      *                  LTD PLANS.
      * 10/05/15 LABEN1  AS PART OF YEYB CHANGES, INCLUDED NEW CURSOR TO
      *                  FETCH FUTURE RECORDS FROM ASSOC INS ENROLL TABL
      *                  INCLUDED LOGIC TO CALCULATE THE COV START DATE,
      *                  WHEN INC/DEC IN COV THEN ADD 365 DAYS. DROP IN
      *                  SESSION DATE + 1 DAY, WHEN FSC OR LD SESSION IS
      *                  PERFORMED FOR STD AND LTD PLANS.
      * 10/28/15 SSUND1  AS PART OF YEYB2016 JOB TRANSITION CHANGES
      *                  CODE IS HANDLED TO HAVE FIRST AUTHORIZED SESSIO
      *                  FOR ALREADY ELIGIBLE PLANS SUCH AS ADD,DEN,ALI
      *                  DLI FOR P/T TO MGMT/DRIVER/FULL TIME
      * 11/23/15 KNAIR1  AS PART OF YEYB16, ADDED LOGIC TO SET LE INDICT
      *                  TO 'N' EVEN IF THE ASSOCIATE INCREASES HIS COV
      *                  ANY SESSION DURING HIS NE PERIOD AND FOR FSC/LD
      *                  THE LE INDICATOR IS SET TO 'N' AND WHEN AN ASSO
      *                  INCREASES HIS COV FROM BASIC TO ENHANCED, THE O
      *                  COV STRT DT VALUE IS SAME AS COV STRT DT VALUE
      *                  ANY SESSION FOR DISABILITY PLANS.
      * 01/06/16 LABEN1  ADDED RESET LOGIC FOR THE NEW ELIGIBLE FLAG FOR
      *                  DISABILITY PLANS INTRODUCED AS PART OF YEYB2016
      * 01/18/16 MAMANOH ADDED CONDITION NOT TO CHANGE THE COV START DAT
      *                  IF THE BENEFIT AND SESSION COV OPTIONS ARE SAME
      * 01/22/16 LABEN1  ADDED NEW LOGIC TO BYPASS PROCESSING SSN FOR ST
      *                  PLAN IF COVERAGE OPTIONS ARE EQUAL
      * 01/23/16 SSUND1  AS PART OF YEYB2016 JOB TRANSITION CHANGES
      *                  CODE IS HANDLED TO HAVE FIRST AUTHORIZED SESSIO
      *                  FOR ALREADY ELIGIBLE PLANS SUCH AS ADD,DEN,ALI
      *                  DLI FOR ALL REHIRED AND REINSTATED ASSOCIATES
      * 02/08/16 LABEN1  AS PART OF SPECIAL ENROLLMENT, ADDED NEW CODE
      *                  LOGIC TO PROCESS THE RECORDS OF ASSOCIATE WHO H
      *                  DONE AN SE SESSION AND TO HANDLE MULTIPLE SESSI
      *                  DONE ON THE SAME SESSION DAY FOR FSC/LD, FSC/SE
      *                  CHANGED THE DEFAULTING OF STATUTORY STATES LOGI
      *                  TO DEFAULT BASED ON WORK STATE INSTEAD OF STATE
      *                  PROVINCE CD
      * 02/24/16 LABEN1  MODIFIED EXISTING FSC/LD, FSC/SE CONDITIONS TO
      *                  MOVE CHANGE REASON CODES ONLY IF THE PREV SESSI
      *                  ON THE SAME DAY IS FSC
      *                  DECOMMISSIONED THE STP PROCESS FOR AE
      * 03/16/16 LABEN1  MODIFIED THE CURSOR7 AND ADDED NEW CURSOR PREVS
      *                  TO FECTH PREV SESSION ON SAME DAY AND ADDED CON
      *                  TO RETAIN THE COV START DATE WHEN THERE IS A CH
      *                  FROM COV OPTION 6 TO 5 FOR STD PLAN
      * 03/23/16 LABEN1  OBSOLETED THE EXISTING CODE FOR INCREASE IN COV
      *                  FOR LD SESSION AND ADDED LOGIC TO HANLDE NE/LD
      *                  THE SAME DAY
      * 10/12/16 LABEN1  AS PART OF AE2017 CHANGES HAVE MADE THE BELOW
      *                  CHANGES.
      *                  -IF LD/FSC ARE DONE ON SAME DAY FOR MED,DEN,VIS
      *                   THEN FOR FSC, THE COV STRT DT WILL BE CHANGE E
      *                   DATE
      *                  -ADDED NEW CODE LOGIC FOR AE SESSION FOR DISABI
      *                   PLANS AND OBSOLETED REDUNDANT CODE LOGIC
      *                  -ADDED LOGIC TO HANDLE AE SESSION DONE DURING N
      *                   PERIOD,TO RETAIN THE SAME COV STRT DT AS OF PC
      *                   AND UPDATE THE COV OPTION ONLY
      * 10/12/16 LUSUL1  AS PART OF AE2017 CHANGES HAVE MADE THE BELOW
      *                  CHANGES.
      *                  -WHEN DLI/MA SESSION IS DONE TO ADD SPOUSE WITH
      *                   CHANGE IN CHILD COVERAGE, FOR A NON-NE ASSOCIA
      *                   CHANGES MADE DURING AE WILL BE REFLECTED
      *                  -GUARANTED AMOUNT WILL BE GIVEN FOR SPOUSE WHEN
      *                   OPTION >=2 IS CHOSEN BY A NON-NE ASSOCIATE DUR
      *                   DLI/MA SESSION
      * 10/18/16 LABEN1   ADDED LOGIC TO HANDLE NE-AE SESSION
      * 10/25/16 LABEN1   AS PART OF YEYB17 CHANGES HAVE MADE THE BELOW
      *                   CHANGES.
      *                   -HANDLING MANAGMENT AND HOURLY ASSOCIATES DURI
      *                    NE EFFECTIVE WINDOW FOR FSC SESSIONS
      * 07/25/17 JEDHA1   AS PART OF BA2018 MADE THE BELOW CHANGES.
      *                   -HANDLE NEWLY INTRODUCED FSC SESSION CALLED
      *                    ADOPTION IN OE APPLICATION.
      *                   -HANDLE NEWLY INTRODUCED FSC SESSION CALLED
      *                    GAIN OF CUSTODY IN OE APPLICATION.
      *                   -HANDLE NEWLY INTRODUCED FSC SESSION CALLED
      *                    DEATH OF SPOUSE IN OE APPLICATION.
      *
      *                   ALSO, ADDED CHANGES TO BYPASS LAST CHANGE DATE
      *                   UPDATE IN ENROLLMENT TABLE IF THERE IS NO CHAN
      *                   IN COVERAGE FROM THE PREVIOUS SESSION.
      *                   TAGGED CHANGES AS *69*
      * 09/01/17 JBASK1   AS PART OF BA2018 MADE THE BELOW CHANGES.
      *                   -HANDLE NEWLY INTRODUCED FSC SESSION CALLED
      *                    LEAVE OF ABSENCE IN OE APPLICATION.
      *                   -HANDLE NEWLY INTRODUCED FSC SESSION CALLED
      *                    RETURN TO WORK IN OE APPLICATION.
      *                   TAGGED CHANGES AS *70*
      * 06/06/18 VN62441   CLARITY ID: 403411
      *                    CHANGES HAVE BEEN MADE TO PROCESS NEWLY HIRED
      *                    MANAGEMENT ASSOCIATES EVEN IF THE HIRING
      *                    INFORMATION IS AVAILABLE IN HRDW ALONE AND AL
      *                    TO ALLOW THE FT DRIVER INTO LTD PROCESSING.
      * 03/10/19 VN503FB   CHANGES HAVE BEEN MADE TO SKIP THE RECORDS OF
      *                    PT/TEMP MEDICAL INELIGIBLE ASSOCIATES WHO HAV
      *                    DEFAULTED NO COVERAGE FOR MED PLAN TO BE WRIT
      *                    IN THE UPDATE FILE
      * 01/28/20 VN62441   CHANGES HAVE BEEN MADE TO DO SYSTEMATIC CHANG
      *                    FOR BROKER 23 WHICH INCLUDES UPDATE THE COVER
      *                    START DATE AS 01/01/2020, IF THE COVERAGE STA
      *                    DATE IN 2019.
      * 08/08/21 VN50FCV   CHANGES HAVE BEEN MADE TO DO COVERAGE START D
      *                    DROP FOR STDE/LTD PLANS AS PART OF AE2022 INS
      * 06/07/22 J0V05KB   CHANGES HAVE BEEN MADE TO FIX -305 ABEND BY
      *                    INTRODUCING NULL INDICATOR FOR EVENT DATE.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEPDATA-FILE ASSIGN SYS-UT-DEPDATA
           .
           SELECT BEUPDATE-FILE ASSIGN SYS-UT-BEUPDATE
           .
           SELECT ENRDATES-FILE ASSIGN SYS-UT-ENRDATES
           .
           SELECT FAMCHANG-FILE ASSIGN SYS-UT-FAMCHANG
           .
           SELECT SYSPRINT-FILE ASSIGN SYS-UT-SYSPRINT
           .
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
           COPY BE3000DX.
      *
       FD  BEUPDATE-FILE.
       01  BEUPDATE.
           03  BEUPDATE-RECORD.
             05  FILLER                        PIC X(190).

      ****AE2014-CHANGE BEGIN
       FD  ENRDATES-FILE.
       01  ENRDATES.
           03  ENRDATES-RECORD.
             05  FILLER                        PIC X(80).
           03 ENRDATES-RECORD009 REDEFINES ENRDATES-RECORD.
             05 ENRDATES-REC                   PIC X(80).
           03 ENRDATES-RECORD010 REDEFINES ENRDATES-RECORD.
             05 ENROLL-START-DATE              PIC X(10).
             05 ENROLL-FILLER-1                PIC X.
             05 ENROLL-END-DATE                PIC X(10).
      ****AE2014-CHANGE END

       FD  FAMCHANG-FILE.
       01  FAMCHANG.
           03  FAMCHANG-RECORD.
             05  FILLER                        PIC X(80).
           03 FAMCHANG-RECORD011 REDEFINES FAMCHANG-RECORD.
             05 FAMCHG-REC                     PIC X(80).
           03 FAMCHANG-RECORD012 REDEFINES FAMCHANG-RECORD.
             05 FAMCHG-PLAN-YEAR               PIC 9(4).
             05 FAMCHG-FILL-1                  PIC XX.
             05 FAMCHG-SSN                     PIC 9(10).
             05 FAMCHG-FILL-2                  PIC XX.
             05 FAMCHG-CHANGE-DATE             PIC X(10).
             05 FAMCHG-FILL-3                  PIC X.
             05 FAMCHG-FSC-DATE                PIC X(10).
             05 FAMCHG-FILL-4                  PIC X.
             05 FAMCHG-FSC-REASON              PIC XX.
             05 FAMCHG-FILL-5                  PIC X.
             05 FAMCHG-UPDATE-CODE             PIC X.
      *
       FD  SYSPRINT-FILE.
       01  SYSPRINT.
           03  SYSPRINT-REC                    PIC X(132).
      *
      *
      *
       WORKING-STORAGE SECTION.
       01  EZT-RETURN-CODE                     PIC S9(9) COMP.
       01  EZT-EOF-FLAGS.
           03 DEPDATA-EOF                      PIC X VALUE 'N'.
           03 ENRDATES-EOF                     PIC X VALUE 'N'.
       01  EZT-RECORD-COUNTS.
           03 DEPDATA-REK-COUNT                PIC S9(9) COMP VALUE 0.
           03 BEUPDATE-REK-COUNT               PIC S9(9) COMP VALUE 0.
           03 ENRDATES-REK-COUNT               PIC S9(9) COMP VALUE 0.
           03 FAMCHANG-REK-COUNT               PIC S9(9) COMP VALUE 0.
       01 EZT-GOTO-STOP-JOB                    PIC X VALUE 'N'.
       01 EZT-TEMP-DATE                        PIC X(8).
       01 EZT-TEMP-TIME                        PIC X(8).
       01 EZT-SYSDATE                          PIC X(8) VALUE SPACES.
       01 EZT-SYSDATE-LONG                     PIC X(10) VALUE SPACES.
       01 EZT-SYSTIME                          PIC X(8) VALUE SPACES.
       01 EZT-WS-TOTAL-WAGES                   PIC S9(13) COMP-3
                                                                VALUE 0.

           COPY WMDATEX.
      *
       01 WEB-COUNT                            PIC S9(10) VALUE 0.
       01 AE-RECS                              PIC S9(10) VALUE 0.
       01 FSC-RECS                             PIC S9(10) VALUE 0.
       01 AE-RECS-GENERATED                    PIC S9(10) VALUE 0.
       01 OE-RECS                              PIC S9(10) VALUE 0.
       01 TOT-RECS-WRITTEN                     PIC S9(10) VALUE 0.
       01 FIRST-FLAG                           PIC X VALUE 'Y'.
       01 WS-SESSION-MONTH                     PIC 99 VALUE 0.
       01 WS-DENIAL-LETTER                     PIC X VALUE 'N'.
       01 WS-EVENT-DATE                        PIC X(10) VALUE SPACES.
       01 WS-EVENT-DATE013 REDEFINES WS-EVENT-DATE.
         03 WS-EVENT-YEAR                      PIC 9(4).
         03 WS-EVENT-FILL1                     PIC X.
         03 WS-EVENT-MONTH                     PIC 99.
         03 WS-EVENT-FILL2                     PIC X.
         03 WS-EVENT-DAY                       PIC 99.
      ** CHANGE 14, CREATE NEXT MONTH FOR ADVANCED FSC KEYS
       01 WS-EVENT-DATE1                       PIC X(10) VALUE SPACES.
       01 WS-EVENT-DATE1014 REDEFINES WS-EVENT-DATE1.
         03 WS-EVENT-YEAR1                     PIC 9(4).
         03 WS-EVENT-FILL11                    PIC X.
         03 WS-EVENT-MONTH1                    PIC 99.
         03 WS-EVENT-FILL21                    PIC X.
         03 WS-EVENT-DAY1                      PIC 99.
      ** CHANGE 14, END OF CHANGE
       01 WS-SQL-DISPLAY                       PIC S9(4) VALUE 0.
       01 ANNUAL-ENROLLMENT                    PIC X VALUE 'N'.
       01 PAY-PERIOD-BEGIN                     PIC X(10) VALUE SPACES.
       01 NEXT-PAY-PERIOD-BEGIN                PIC X(10) VALUE SPACES.
       01 NEXT-PLAN-YR-START                   PIC X(10) VALUE SPACES.
       01 NEXT-PLAN-YR-START015 REDEFINES NEXT-PLAN-YR-START.
         03 NEXT-PLAN-YY                       PIC 9(4).
         03 NEXT-PLAN-F1                       PIC X.
         03 NEXT-PLAN-MN                       PIC 99.
         03 NEXT-PLAN-F2                       PIC X.
         03 NEXT-PLAN-DD                       PIC 99.
       01 CURRENT-PLAN-YEAR                    PIC S9(4) COMP VALUE 0.
       01 NEXT-PLAN-YEAR                       PIC S9(4) COMP VALUE 0.
       01 LOOKUP-PLAN-YEAR                     PIC S9(4) COMP VALUE 0.
       01 LOOKUP-PLAN-YEAR-N                   PIC S9(5) VALUE 0.
       01 C-URRENT-DATE                        PIC X(10) VALUE ' '.
       01 WCOVERAGE-OPT-PREV                   PIC X VALUE ' '.
       01 WCOVERAGE-CAT-PREV                   PIC X VALUE ' '.
       01 SESSION-TIMESTAMP                    PIC X(26) VALUE SPACES.
       01 SESSION-TIMESTAMP016 REDEFINES SESSION-TIMESTAMP.
         03 SESSION-DATE                       PIC X(10).
         03 SESSION-TIME                       PIC X(16).
      *
       01  WS-BEUPDATE.
           03 BEUPDATE-RECORD005.
             05 BEN-REC                        PIC X(190).
           03 BEUPDATE-RECORD006 REDEFINES BEUPDATE-RECORD005.
             05 BEN-DATA                       PIC X(190).
           03 BEUPDATE-RECORD007 REDEFINES BEUPDATE-RECORD005.
             05 SSN-O                          PIC S9(9) COMP.
             05 PLAN-NAME-O                    PIC XXX.
             05 PLAN-YEAR-O                    PIC S9(4) COMP.
             05 PAYROLL-DATE-O                 PIC X(10).
             05 HISTORY-TYPE-O                 PIC X.
             05 COV-START-DATE-O               PIC X(10).
             05 COV-END-DATE-O                 PIC X(10).
             05 COV-OPTION-O                   PIC X.
             05 COV-CATEGORY-O                 PIC X.
             05 BEN-OPTION-O                   PIC X.
             05 ENROLL-DATE-O                  PIC X(10).
             05 ENROLL-RCPT-DATE-O             PIC X(10).
             05 CONFIRM-DATE-O                 PIC X(10).
             05 REMINDER-DATE-O                PIC X(10).
             05 ELIGIBILITY-DATE-O             PIC X(10).
             05 ORIG-COV-DATE-O                PIC X(10).
             05 ARREARS-CANCEL-DT-O            PIC X(10).
             05 PEAK-TIME-DATE-O               PIC X(10).
             05 DEFAULT-DATE-O                 PIC X(10).
             05 COPY-TO-PAYROLL-O              PIC X.
             05 CHANGE-REASON-CD-O             PIC XX.
             05 LAST-CHANGE-DATE-O             PIC X(10).
             05 LAST-CHANGE-ID-O               PIC X(8).
             05 PROVIDER-ID-O                  PIC S9(4) COMP.
             05 ENROLL-LATE-INT-O              PIC X.
             05 WAIT-DAY-QTY-O                 PIC S9(4) COMP.
             05 BEN-BKRID-O                    PIC S9(4) COMP.
             05 OUT-POCKET-MAX-CD              PIC X.
             05 UP-FRONT-ALLOW-CD              PIC X.
             05 BEN-FILLER                     PIC X(26).
           03 BEUPDATE-RECORD008 REDEFINES BEUPDATE-RECORD005.
             05 INIT1                          PIC X(20).
             05 INIT2                          PIC X(20).
             05 INIT3                          PIC X(20).
             05 INIT4                          PIC X(20).
             05 INIT5                          PIC X(20).
             05 INIT6                          PIC X(20).
             05 INIT7                          PIC X(20).
             05 INIT8                          PIC X(20).
             05 INIT9                          PIC X(20).
             05 INIT10                         PIC X(10).
      *
       01 WSSN                                 PIC S9(17) COMP-3
                                                             VALUE 0.
       01 WPLAN-NAME                           PIC XXX VALUE SPACES.
       01 WPLAN-YEAR                           PIC S9(7) COMP-3 VALUE 0.
       01 WPAYROLL-DATE                        PIC X(10) VALUE SPACES.
       01 WPAYROLL-DATE-IND                    PIC S9(4) COMP VALUE 0.
       01 WHISTORY-TYPE-CD                     PIC X VALUE SPACES.
       01 WCOVERAGE-START-DT                   PIC X(10) VALUE SPACES.
       01 WCOVERAGE-START-DT-IND               PIC S9(4) COMP VALUE 0.
       01 WCOVERAGE-END-DT                     PIC X(10) VALUE SPACES.
       01 WCOVERAGE-END-DT-IND                 PIC S9(4) COMP VALUE 0.
       01 WCOVERAGE-OPTION                     PIC X VALUE SPACES.
       01 WCOVERAGE-CATEGORY                   PIC X VALUE SPACES.
       01 WBENEFIT-OPTION                      PIC X VALUE SPACES.
       01 WENROLLMENT-DATE                     PIC X(10) VALUE SPACES.
       01 WENROLLMENT-DATE-IND                 PIC S9(4) COMP VALUE 0.
       01 WENROLL-RCPT-DATE                    PIC X(10) VALUE SPACES.
       01 WENROLL-RCPT-DATE-IND                PIC S9(4) COMP VALUE 0.
       01 WCONFIRM-DATE                        PIC X(10) VALUE SPACES.
       01 WCONFIRM-DATE-IND                    PIC S9(4) COMP VALUE 0.
       01 WREMINDER-DATE                       PIC X(10) VALUE SPACES.
       01 WREMINDER-DATE-IND                   PIC S9(4) COMP VALUE 0.
       01 WELIGIBILITY-DATE                    PIC X(10) VALUE SPACES.
       01 WELIGIBILITY-DATE-IND                PIC S9(4) COMP VALUE 0.
       01 WORIG-COVERAGE-DATE                  PIC X(10) VALUE SPACES.
       01 WORIG-COVERAGE-DATE-IND              PIC S9(4) COMP VALUE 0.
       01 WORIG-COVERAGE-DATE-PLUS15           PIC X(10) VALUE SPACES.
       01 WORIG-COVERAGE-DATE-PLUS15-IND       PIC S9(4) COMP VALUE 0.
       01 WARREARS-CANCEL-DT                   PIC X(10) VALUE SPACES.
       01 WARREARS-CANCEL-DT-IND               PIC S9(4) COMP VALUE 0.
       01 WPEAK-TIME-DATE                      PIC X(10) VALUE SPACES.
       01 WPEAK-TIME-DATE-IND                  PIC S9(4) COMP VALUE 0.
       01 WDEFAULT-DATE                        PIC X(10) VALUE SPACES.
       01 WDEFAULT-DATE-IND                    PIC S9(4) COMP VALUE 0.
       01 WCOPY-TO-PAYROLL-FL                  PIC X VALUE SPACES.
       01 WCHANGE-REASON-CODE                  PIC XX VALUE SPACES.
       01 WCHANGE-REASON-CODE-IND              PIC S9(4) COMP VALUE 0.
       01 WLAST-CHANGE-DATE                    PIC X(10) VALUE SPACES.
       01 WLAST-CHANGE-DATE-IND                PIC S9(4) COMP VALUE 0.
       01 WLAST-CHANGE-ID                      PIC X(8) VALUE SPACES.
       01 WLAST-CHANGE-ID-IND                  PIC S9(4) COMP VALUE 0.
       01 WORIG-COVERAGE-DATE-PLUS60           PIC X(10) VALUE SPACES.
       01 WORIG-COVERAGE-DATE-PLUS60-IND       PIC S9(4) COMP VALUE 0.
       01 WORIG-COVERAGE-DATE-PLUS75           PIC X(10) VALUE SPACES.
       01 WORIG-COVERAGE-DATE-PLUS75-IND       PIC S9(4) COMP VALUE 0.
       01 WENROLL-LATE-INT                     PIC X VALUE SPACES.
       01 WASC-PAY-CODE                        PIC X VALUE SPACES.
       01 WASC-PAY-CODE-IND                    PIC S9(4) COMP VALUE 0.
       01 WASC-PAY-CD                          PIC X VALUE SPACES.
       01 WASC-PAY-CD-IND                      PIC S9(4) COMP VALUE 0.
       01 WASC-JOB-CODE                        PIC X VALUE SPACES.
       01 WASC-JOB-CODE-IND                    PIC S9(4) COMP VALUE 0.
       01 WSTATE-PROVINCE-CD-IND               PIC S9(4) COMP VALUE 0.
      ***AE2016 CHANGES BEGIN
       01 WASC-TYPE-CODE                       PIC X VALUE SPACES.
      ***AE2016 CHANGES END
       01 WPEAK-NEWLY-ELIGIBLE-DATE-STRT       PIC X(10) VALUE SPACES.
       01 WPEAK-NEWLY-ELIGIBLE-DATE-END        PIC X(10) VALUE SPACES.
       01 WMGMT-NEWLY-ELIGIBLE-DATE            PIC X(10) VALUE SPACES.
       01 WHRLY-NEWLY-ELIGIBLE-DATE            PIC X(10) VALUE SPACES.
       01 WHRLY-NEWLY-ELIGIBLE-DTPLUS90        PIC X(10) VALUE SPACES.
       01 WHRLY-NEWLY-ELIGIBLE-DTPLUS194       PIC X(10) VALUE SPACES.
       01 WWAIT-DAY-QTY                        PIC S9(4) COMP VALUE 0.
       01 WWAIT-DAY-QTY-IND                    PIC S9(4) COMP VALUE 0.
       01 WSTATE-PROVINCE-CD                   PIC XX VALUE SPACES.
       01 TPLAN-NAME                           PIC XXX VALUE SPACES.
       01 TPLAN-YEAR                           PIC S9(4) COMP VALUE 0.
       01 TOPTION-CODE                         PIC X VALUE SPACES.
       01 TOPTION-AMT                          PIC S9(9) COMP VALUE 0.
       01 WS-NULL-IND                          PIC S9(4) COMP VALUE 0.
       01 WS-ORIG-EVENT-DATE                   PIC X(10) VALUE SPACES.
       01 WS-LAST-CHANGE-ID                    PIC X VALUE SPACES.
       01 WINS-PROVIDER-ID                     PIC S9(4) COMP VALUE 0.
       01 WINS-PROVIDER-ID-IND                 PIC S9(4) COMP VALUE 0.
       01 WINS-BROKER-ID                       PIC S9(4) COMP VALUE 0.
       01 WINS-BROKER-ID-IND                   PIC S9(4) COMP VALUE 0.
       01 WOUT-POCKET-MAX-CD                   PIC X VALUE SPACES.
       01 WUP-FRONT-ALLOW-CD                   PIC X VALUE SPACES.
      *
       01 WSSN1                                PIC S9(9) COMP VALUE 0.
      *
       01 WPLAN-YEAR-PREV                      PIC S9(4) COMP VALUE 0.
       01 WPAYROLL-DATE-PREV                   PIC X(10) VALUE SPACES.
       01 WS-DI-LE-EVENT-DATE                  PIC X(10) VALUE SPACES.
       01 WS-DAYS                              PIC 9(7) VALUE 0.
      *
       01 WS-MY-BEN-USERID                     PIC X(8) VALUE SPACES.
       01 WS-MY-BEN-USERID017 REDEFINES WS-MY-BEN-USERID.
         03 WS-USERID-CHG-REASON               PIC XX.
         03 WS-USERID-CONST                    PIC X(6).
       01 WS-OUT-REC-SAVE                      PIC X(190) VALUE SPACES.
       01 WS-ENROLLEE-SSN                      PIC X(10) VALUE SPACES.
       01 WS-NEWLY-ELIGIBLE                    PIC X VALUE SPACES.
       01 WS-SESSION-TS                        PIC X(26) VALUE SPACES.
       01 WS-SESSION-TS018 REDEFINES WS-SESSION-TS.
         03 WS-SESSION-DATE                    PIC X(10).
         03 WS-SESSION-DATE019 REDEFINES WS-SESSION-DATE.
           05 WS-SESSION-DATE-YY               PIC 9(4).
           05 FILLER                           PIC X.
           05 WS-SESSION-DATE-MM               PIC 99.
           05 FILLER                           PIC X.
           05 WS-SESSION-DATE-DD               PIC 99.
         03 FILLER                             PIC X(16).
       01 WS-WORK-STATE                        PIC S999 COMP-3 VALUE 0.
       01 WS-ALI-OPT                           PIC XXX VALUE SPACES.
       01 WS-ALI-SSN                           PIC S9(10) VALUE 0.
       01 WS-TOTAL-WAGES                       PIC S9(13)V99 COMP-3
                                                             VALUE 0.
       01 WS-TOTAL-WAGES-4                     PIC 9(15) VALUE 0.
       01 WS-TOTAL-WAGES-1 REDEFINES WS-TOTAL-WAGES-4.
          05 WS-TOTAL-WAGES-2                  PIC 9(10).
          05 WS-TOTAL-WAGES-3                  PIC 9(5).
       01 WS-TOTAL-PAY-PERIOD                  PIC S9(4) COMP VALUE 0.
       01 WS-ALI-COVERAGE-OPTION               PIC X VALUE SPACES.
       01 WS-ALI-AMT                           PIC S9(10) VALUE 0.
       01 WS-MAX-DLI-AMT                       PIC S9(10) VALUE 0.
       01 WS-MAX-DLI-OPT                       PIC X VALUE SPACES.
       01 WS-MAX-DLI-CATG                      PIC X VALUE SPACES.
       01 WS-HRS-WRK                           PIC S9(6)V999 COMP-3
                                                             VALUE 0.
       01 WS-EVENT-DATE-CHANGED                PIC X VALUE SPACES.
       01 WS-CHANGE-REASON-CODE                PIC XX VALUE ' '.
       01 WS-EVENT-TYPE-CODE                   PIC S9(4) COMP VALUE 0.
       01 WS-GOOD-HEALTH-IND                   PIC X VALUE SPACES.
       01 WS-LETTER-NBR                        PIC S9(4) COMP VALUE 0.
       01 WS-MSG-TXT                           PIC X(45) VALUE SPACES.
      *****AE2011-CHANGE BEGINS
       01 WS-ENROLLEE-SSN-BI                   PIC S9(9) COMP VALUE 0.
       01 SESSION-DATE-PLUS1                   PIC X(10) VALUE ' '.
       01 COVRG-DATE-NYEAR                     PIC X(10) VALUE ' '.
      ***70  AE2018-LOA,RW CHANGES BEGINS
       01 EVENT-DATE-PLUS1                     PIC X(10) VALUE ' '.
       01 EVENT-DATE-PLUS1-IND                 PIC S9(4) COMP VALUE 0.
      ***70  AE2018-LOA,RW CHANGES ENDS
       01 WS-WAIT-PERIOD-END                   PIC X(10) VALUE SPACES.
       01 WS-SPOUSE-COV-START-DATE             PIC X(10) VALUE SPACES.
       01 WS-CHILD-COV-START-DATE              PIC X(10) VALUE SPACES.
       01 WS-NEW-PICKUP-IND                    PIC X VALUE 'N'.
       01 WS-OPT-CAT-DECREASE                  PIC X VALUE 'N'.
       01 TWO-RECORDS-FLAG                     PIC X VALUE 'N'.
       01 FIRST-REC-FLAG                       PIC X VALUE 'N'.
      ***AE2016 CHANGES BEGINS
       01 WS-PT-TRUCK                          PIC X VALUE 'N'.
       01 WS-TEMP-SSN                          PIC X(10) VALUE SPACES.
      ***AE2016 CHANGES END
       01 COV-OPTION-O1                        PIC X VALUE SPACES.
       01 COV-CATEGORY-O1                      PIC X VALUE SPACES.
       01 HISTORY-TYPE-O1                      PIC X VALUE SPACES.
       01 COV-START-DATE-O1                    PIC X(10) VALUE SPACES.
       01 COV-OPTION-O2                        PIC X VALUE SPACES.
       01 COV-CATEGORY-O2                      PIC X VALUE SPACES.
       01 HISTORY-TYPE-O2                      PIC X VALUE SPACES.
       01 COV-START-DATE-O2                    PIC X(10) VALUE SPACES.
      ****YEYB-2015 CHANGES BEGINS
       01 WTRANS-DATE                          PIC X(10) VALUE SPACES.
       01 WTRANS-RSTRCT-CODE                   PIC S9(4) COMP VALUE 0.
       01 WS-WIN                               PIC S9(9) COMP VALUE 0.
       01 WS-WAIT-DAYS                         PIC S9(4) COMP VALUE 0.
      ****YEYB-2015 CHANGES ENDS
      *****AE2011-CHANGE END
      ***JOB TRANSITION CHANGES BEGINS **
       01 WTRAN-CHANGE-REASON-CODE             PIC XX VALUE SPACES.
       01 WTRANS-TYPE-CODE                     PIC S9(4) COMP VALUE 0.
      ***JOB TRANSITION CHANGES ENDS   **
      *
      **56 YEYB2016 CHANGES START
       01 WS-STD-LTD-NW-ELIG                   PIC X VALUE SPACES.
      **56 YEYB2016 CHANGES END
      *
      **57 YEYB2016 FIX CHANGES START
       01 WS-DEN-COV-STRT-DT                   PIC X(10) VALUE SPACES.
      **57 YEYB2016 FIX CHANGES END
      *
      **61*63
       01 WS-EVENT-DATE-LD-SE                  PIC X(10) VALUE SPACES.
       01 WS-EVENT-DATE-LD-SE-IND              PIC S9(4) COMP VALUE 0.
      **61*63
      *
      **65 LABEN1 AE2017 CHANGES
       01 WS-EVENT-DATE-LD                     PIC X(10) VALUE SPACES.
       01 WS-EVENT-DATE-LD-IND                 PIC S9(4) COMP VALUE 0.
      **65 LABEN1 AE2017 CHANGES END
      *
       01 WS-DIS-COV-STRT-DT                   PIC X(10) VALUE SPACES.
      *
       01 SSN-CURR-YEAR                        PIC S9(10) VALUE 0.
       01 SSN-PREV                             PIC S9(10) VALUE 0.
       01 PLAN-NAME-PREV                       PIC XXX VALUE SPACES.
       01 PLAN-YEAR-PREV                       PIC S9(4) COMP VALUE 0.
       01 COV-START-DATE-PREV                  PIC X(10) VALUE SPACES.
       01 COV-OPTION-PREV                      PIC X VALUE SPACES.
       01 COV-CATEGORY-PREV                    PIC X VALUE SPACES.
       01 ENROLL-RCPT-DATE-PREV                PIC X(10) VALUE SPACES.
       01 ORIG-COV-DATE-PREV                   PIC X(10) VALUE SPACES.
       01 CHANGE-REASON-CD-PREV                PIC XX VALUE SPACES.
       01 DC-ENROLL-TYPE-PREV                  PIC S9(5) VALUE 0.
      *
       01 WS-ALI-OPT-IND-TY                    PIC S99 VALUE 0.
       01 WS-ALI-OPT-IND-NY                    PIC S99 VALUE 0.
       01 WS-DLI-OPT-IND-TY                    PIC S99 VALUE 0.
       01 WS-DLI-OPT-IND-NY                    PIC S99 VALUE 0.
       01 WS-DLI-CATG-IND-TY                   PIC S99 VALUE 0.
       01 WS-DLI-CATG-IND-NY                   PIC S99 VALUE 0.
      *
       01 SW-DEBUG-SWITCH                      PIC X VALUE 'N'.
       01 SW-CURSOR2A-ACTIV                    PIC X VALUE 'N'.
      *
       01 SW-POST-STD-LTD                      PIC X VALUE SPACES.
       01 SW-LOAD-ALI-DLI                      PIC X VALUE SPACES.
       01 WS-FOUND-SW                          PIC X VALUE SPACES.
       01 WS-COUNTER                           PIC 99 VALUE 0.
      ****AE2014 CHANGES **
       01 WS-MGMT-ASSOC                        PIC X VALUE SPACES.
       01 WS-OFFICERS                          PIC X VALUE SPACES.
       01 WS-AUTO-APPRVL-FLAG                  PIC X VALUE SPACES.
       01 WS-JOB-CODE                          PIC X VALUE SPACES.
       01 WS-PAY-CODE                          PIC X VALUE SPACES.
       01 WS-PAY-CD                            PIC X VALUE SPACES.
       01 WS-MGMT-ASSOC1                       PIC X VALUE SPACES.
       01 WS-OFFCR-CNT                         PIC S9(4) COMP VALUE 0.
       01 WS-SSN                               PIC S9(17) COMP-3
                                                             VALUE 0.
       01 WS-COVERAGE-OPT-CMP-ONE              PIC X VALUE SPACES.
       01 WS-COVERAGE-OPT-CMP-TWO              PIC X VALUE SPACES.
       01 WS-COVERAGE-OPT-ONE-INCREASED        PIC X VALUE SPACES.
       01 WS-COVERAGE-OPT-ONE-DECREASED        PIC X VALUE SPACES.
       01 WS-COVERAGE-OPT-ONE-EQUAL            PIC X VALUE SPACES.
       01 COV-OPT-PREV                         PIC X VALUE SPACES.
       01 WS-ENR-START-DT                      PIC X(10) VALUE SPACES.
       01 WS-ENR-END-DT                        PIC X(10) VALUE SPACES.
      ***AE2014 CHANGES END **
      ***AE2014-DP CHANGES BEGINS **
       01 WS-WIN-NBR                           PIC S9(9) COMP VALUE 0.
       01 WS-RELATION-TYPE-CODE                PIC X(5) VALUE '     '.
       01 WS-DP-FOUND-SW                       PIC X VALUE 'N'.
      ***43-AE2014-DP-FIX CHANGES BEGINS **
       01 WS-SP-FOUND-SW                       PIC X VALUE 'N'.
       01 WS-LEG-DEP-FOUND-SW                  PIC X VALUE 'N'.
       01 WS-DP-DEP-FOUND-SW                   PIC X VALUE 'N'.
       01 WS-GENDER-SAME-SW                    PIC X VALUE 'N'.
       01 WS-SP-COMMN-LAW-SW                   PIC X VALUE 'N'.
       01 WS-REL-TABLE-EOF-SW                  PIC X VALUE 'N'.
       01 WS-DEP-CHG-TABLE-EOF-SW              PIC X VALUE 'N'.
      ***43-AE2014-DP-FIX CHANGES ENDS **
      ***AE2014-DP CHANGES ENDS **
       01 ALI-TY-ARRAY-RLEVEL.
         03 ALI-TY-ARRAY                       PIC X(11) OCCURS 15.
       01 ALI-TY-ARRAY-RLEVEL020 REDEFINES ALI-TY-ARRAY-RLEVEL.
         03 ALI-TY-ARRAY-ARRAY                 OCCURS 15.
          05 TABLE-ALI-OPT-TY                  PIC X.
          05 TABLE-ALI-AMT-TY                  PIC 9(10).
      *
       01 ALI-NY-ARRAY-RLEVEL.
         03 ALI-NY-ARRAY                       PIC X(11) OCCURS 15.
       01 ALI-NY-ARRAY-RLEVEL021 REDEFINES ALI-NY-ARRAY-RLEVEL.
         03 ALI-NY-ARRAY-ARRAY                 OCCURS 15.
          05 TABLE-ALI-OPT-NY                  PIC X.
          05 TABLE-ALI-AMT-NY                  PIC 9(10).
      *
       01 DLI-TY-OPT-ARRAY-RLEVEL.
         03 DLI-TY-OPT-ARRAY                   PIC X(11) OCCURS 15.
       01 DLI-TY-OPT-ARRAY-RLEVEL022 REDEFINES
                                              DLI-TY-OPT-ARRAY-RLEVEL.
         03 DLI-TY-OPT-ARRAY-ARRAY             OCCURS 15.
          05 TABLE-DLI-OPT-TY                  PIC X.
          05 TABLE-DLI-OPT-AMT-TY              PIC 9(10).
      *
       01 DLI-NY-OPT-ARRAY-RLEVEL.
         03 DLI-NY-OPT-ARRAY                   PIC X(11) OCCURS 15.
       01 DLI-NY-OPT-ARRAY-RLEVEL023 REDEFINES
                                              DLI-NY-OPT-ARRAY-RLEVEL.
         03 DLI-NY-OPT-ARRAY-ARRAY             OCCURS 15.
          05 TABLE-DLI-OPT-NY                  PIC X.
          05 TABLE-DLI-OPT-AMT-NY              PIC 9(10).
      *
       01 DLI-TY-CATG-ARRAY-RLEVEL.
         03 DLI-TY-CATG-ARRAY                  PIC X(11) OCCURS 15.
       01 DLI-TY-CATG-ARRAY-RLEVEL024 REDEFINES
                                             DLI-TY-CATG-ARRAY-RLEVEL.
         03 DLI-TY-CATG-ARRAY-ARRAY            OCCURS 15.
          05 TABLE-DLI-CATG-TY                 PIC X.
          05 TABLE-DLI-CATG-AMT-TY             PIC 9(10).
      *
       01 DLI-NY-CATG-ARRAY-RLEVEL.
         03 DLI-NY-CATG-ARRAY                  PIC X(11) OCCURS 15.
       01 DLI-NY-CATG-ARRAY-RLEVEL025 REDEFINES
                                             DLI-NY-CATG-ARRAY-RLEVEL.
         03 DLI-NY-CATG-ARRAY-ARRAY            OCCURS 15.
          05 TABLE-DLI-CATG-NY                 PIC X.
          05 TABLE-DLI-CATG-AMT-NY             PIC 9(10).
      *
       01 WS-WAGES-N                           PIC 9(5) VALUE 0.
       01 WS-WAGES-N026 REDEFINES WS-WAGES-N.
         03 WS-WAGES-X1                        PIC 99.
         03 WS-WAGES-X2                        PIC 999.
      *
       01 WS-STP-OPTION                        PIC X VALUE SPACES.
       01 WS-STP-CATEGORY                      PIC X VALUE SPACES.
       01 WS-STP-NOCHANGE                      PIC X VALUE SPACES.
       01 WS-STP-RECTYPE-CD                    PIC X VALUE SPACES.
      ***YEYB 2016 CHANGES START
       01 WS-BEN-OPTION                        PIC X VALUE SPACES.
       01 WS-SSN1                              PIC S9(17) COMP-3
                                                             VALUE 0.
       01 WS-PLAN-NAME                         PIC XXX VALUE SPACES.
       01 WS-COV-END-DT                        PIC X(10) VALUE SPACES.
       01 WS-COVERAGE-START-DT                 PIC X(10) VALUE SPACES.
       01 WS-COV-END-DT-IND                    PIC S9(4) COMP VALUE 0.
       01 WS-ORIG-COV-DT                       PIC X(10) VALUE SPACES.
       01 WS-ORIG-COV-DT-IND                   PIC S9(4) COMP VALUE 0.
       01 WS-EFF-DT                            PIC X(10) VALUE SPACES.
       01 WS-WAIT-DAY-QTY                      PIC S9(4) COMP VALUE 0.
       01 WS-ENROLL-LT-IND                     PIC X VALUE SPACES.
      **68 YEYB17 CHANGES START
       01 WS-DISTRANS-SW                       PIC X VALUE 'N'.
       01 EZT-DC-PLAN-NAME                     PIC XXX VALUE SPACES.
       01 EZT-DC-SESSION-TS                    PIC X(26) VALUE SPACES.
       01 EZT-COV-START-DATE-O                 PIC X(10) VALUE SPACES.
       01 EZT-DC-CHANGE-REASON-CODE            PIC XX VALUE SPACES.
       01 EZT-DISPLAY-STRING                   PIC X(1024) VALUE SPACES.
       01 EZT-TMP-NUM-FLD-B-10-0               PIC 9(10) VALUE 0.
       01 EZT-DISP-EZ-5-0                      PIC ZZ,ZZZ-.
       01 EZT-DISP-EZ-10-0                     PIC Z,ZZZ,ZZZ,ZZZ-.
       01 EZT-DISP-EZ-17-0                     PIC
                                                ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ-.
       01 EZT-DISP-EZ-4-0                      PIC Z,ZZZ-.
       01 EZT-DISP-EZ-5-0-1                    PIC ZZ,ZZZ-.
       01 EZT-DISP-EZ-5-0-2                    PIC ZZ,ZZZ-.
       01 EZT-DISP-EZ-5-0-3                    PIC ZZ,ZZZ-.
       01 EZT-DISP-EZ-2-0                      PIC ZZ-.
       01 EZT-DISP-N9-10-0                     PIC 9(10).
       01 EZT-DISP-N9-2-0                      PIC 99.
       01 EZT-DISP-EZ-10-0-1                   PIC Z,ZZZ,ZZZ,ZZZ-.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      *
      *68 YEYB17 CHANGES END
           EXEC SQL DECLARE CURSOR1 CURSOR FOR
           SELECT ANNUAL_ENROLL_FLAG,
                  PLAN_YEAR,
                  PLAN_YEAR + 1,
                  LAST_PAYROLL_DATE + 1 DAY,
                  LAST_PAYROLL_DATE + 15 DAYS,
                  CURRENT DATE
           FROM   PLAN_YEAR
           WHERE  CURRENT_YEAR_FLAG = 'Y'
           WITH UR
           END-EXEC

           EXEC SQL DECLARE CURSOR2B CURSOR FOR
           SELECT (SUBSTR(CHAR(PLAN_YEAR+1),1,4)
                  || '-01-01')
           FROM   PLAN_YEAR
           WHERE  CURRENT_YEAR_FLAG = 'Y'
           WITH UR
           END-EXEC

           EXEC SQL DECLARE CURSOR2 CURSOR FOR
           SELECT A.SSN                                     ,
                  A.PLAN_NAME                               ,
                  A.PLAN_YEAR                               ,
                  A.PAYROLL_DATE                            ,
                  A.HISTORY_TYPE_CD                         ,
                  A.COVERAGE_START_DT                       ,
                  A.COVERAGE_END_DT                         ,
                  A.COVERAGE_OPTION                         ,
                  A.COVERAGE_CATEGORY                       ,
                  A.BENEFIT_OPTION                          ,
                  A.ENROLLMENT_DATE                         ,
                  A.ENROLL_RCPT_DATE                        ,
                  A.CONFIRM_DATE                            ,
                  A.REMINDER_DATE                           ,
                  A.ELIGIBILITY_DATE                        ,
                  A.ORIG_COVERAGE_DATE                      ,
                  A.ORIG_COVERAGE_DATE + 15 DAYS            ,
                  A.ARREARS_CANCEL_DT                       ,
                  A.PEAK_TIME_DATE                          ,
                  A.DEFAULT_DATE                            ,
                  A.COPY_TO_PAYROLL_FL                      ,
                  A.CHANGE_REASON_CODE                      ,
                  A.LAST_CHANGE_DATE                        ,
                  A.LAST_CHANGE_ID                          ,
                  A.INS_PROVIDER_ID                         ,
                  A.INS_BROKER_ID                           ,
                  A.OUT_POCKET_MAX_CD                       ,
                  A.UP_FRONT_ALLOW_CD                       ,
                  A.ORIG_COVERAGE_DATE + 60 DAYS            ,
                  A.ORIG_COVERAGE_DATE + 75 DAYS            ,
                  A.ENROLL_LATE_IND                         ,
                  B.ASC_PAY_CODE                            ,
                  B.ASC_JOB_CODE                            ,
                  A.ELIGIBILITY_DATE                            ,
                  A.ELIGIBILITY_DATE + C.PEAK_TIME_DAY_CNT DAYS ,
                  A.ELIGIBILITY_DATE + C.MGT_ENROLL_DAY_CNT DAYS ,
                  A.ELIGIBILITY_DATE + C.FT_ENROLL_DAY_CNT DAYS  ,
                  B.ASC_WORK_STATE                               ,
                  A.ELIGIBILITY_DATE + 90 DAYS                   ,
                  DATE(SUBSTR(CHAR(DATE(A.ELIGIBILITY_DATE) + 89 DAYS),
                      1, 8)
                  || '01' )                                      ,
                  A.ELIGIBILITY_DATE + C.FT_ENROLL_DAY_CNT DAYS + 90
                      DAYS,
                  A.WAIT_DAY_QTY     ,
                  B.STATE_PROVINCE_CD
           FROM   BENEFIT_ENROLLMENT A
                , ASSOCIATE_ID_T B
                , ENROLLMENT_PLAN_YR C
           WHERE   A.PLAN_YEAR = :LOOKUP-PLAN-YEAR
              AND  A.PLAN_NAME = :EZT-DC-PLAN-NAME
              AND  A.HISTORY_TYPE_CD = 'E'
              AND  A.SSN =:WSSN
              AND  B.ASC_SS_NBR =:WSSN
              AND  C.PLAN_YEAR = YEAR(CURRENT DATE)
           WITH UR
           END-EXEC

      ***** REAL TIME UPDATE AE2019 CHANGES BEGIN ***

           EXEC SQL DECLARE CURSOR2A CURSOR FOR
           SELECT A.SSN                                     ,
                  A.PLAN_NAME                               ,
                  A.PLAN_YEAR                               ,
                  A.PAYROLL_DATE                            ,
                  A.HISTORY_TYPE_CD                         ,
                  A.COVERAGE_START_DT                       ,
                  A.COVERAGE_END_DT                         ,
                  A.COVERAGE_OPTION                         ,
                  A.COVERAGE_CATEGORY                       ,
                  A.BENEFIT_OPTION                          ,
                  A.ENROLLMENT_DATE                         ,
                  A.ENROLL_RCPT_DATE                        ,
                  A.CONFIRM_DATE                            ,
                  A.REMINDER_DATE                           ,
                  A.ELIGIBILITY_DATE                        ,
                  A.ORIG_COVERAGE_DATE                      ,
                  A.ORIG_COVERAGE_DATE + 15 DAYS            ,
                  A.ARREARS_CANCEL_DT                       ,
                  A.PEAK_TIME_DATE                          ,
                  A.DEFAULT_DATE                            ,
                  A.COPY_TO_PAYROLL_FL                      ,
                  A.CHANGE_REASON_CODE                      ,
                  A.LAST_CHANGE_DATE                        ,
                  A.LAST_CHANGE_ID                          ,
                  A.INS_PROVIDER_ID                         ,
                  A.INS_BROKER_ID                           ,
                  A.OUT_POCKET_MAX_CD                       ,
                  A.UP_FRONT_ALLOW_CD                       ,
                  A.ORIG_COVERAGE_DATE + 60 DAYS            ,
                  A.ORIG_COVERAGE_DATE + 75 DAYS            ,
                  A.ENROLL_LATE_IND                         ,
                  CASE WHEN PAY_TYPE_CODE = 'S'
                     THEN '6'
                     ELSE '1'
                     END                                   ,
                  B.PAY_TYPE_CODE                            ,
                  B.ASC_TYPE_CODE                            ,
                  A.ELIGIBILITY_DATE                            ,
                  A.ELIGIBILITY_DATE + C.PEAK_TIME_DAY_CNT DAYS ,
                  A.ELIGIBILITY_DATE + C.MGT_ENROLL_DAY_CNT DAYS ,
                  A.ELIGIBILITY_DATE + C.FT_ENROLL_DAY_CNT DAYS  ,
                  CAST(D.STATE_NBR AS DECIMAL(2,0))              ,
                  A.ELIGIBILITY_DATE + 90 DAYS                   ,
                  DATE(SUBSTR(CHAR(DATE(A.ELIGIBILITY_DATE) + 89 DAYS),
                      1, 8)
                  || '01' )                                      ,
                  A.ELIGIBILITY_DATE + C.FT_ENROLL_DAY_CNT DAYS + 90
                      DAYS,
                  A.WAIT_DAY_QTY     ,
                  STR_STATE_PROV_CD
           FROM  BENEFIT_ENROLLMENT A
                ,V3_WIN_ASC_NID_US B
                ,ENROLLMENT_PLAN_YR C
                ,STATE D
           WHERE   A.PLAN_YEAR = :LOOKUP-PLAN-YEAR
              AND  A.PLAN_NAME = :EZT-DC-PLAN-NAME
              AND  A.HISTORY_TYPE_CD = 'E'
              AND  A.SSN =:WSSN
              AND  NATIONAL_ID =CAST(SUBSTR(DIGITS(A.SSN),2,9) AS
                  CHAR(23))
              AND  STR_STATE_PROV_CD = STATE_CODE
              AND  C.PLAN_YEAR = YEAR(CURRENT DATE)
           WITH UR
           END-EXEC

      ***** REAL TIME UPDATE AE2019 CHANGES END***

           EXEC SQL DECLARE CURSOR3 CURSOR FOR
           SELECT A.PLAN_YEAR                               ,
                  A.PAYROLL_DATE
           FROM   BENEFIT_ENROLLMENT A
           WHERE   A.PLAN_YEAR = :WPLAN-YEAR-PREV
              AND  A.PLAN_NAME = :EZT-DC-PLAN-NAME
              AND  A.HISTORY_TYPE_CD = 'E'
              AND  A.SSN =:WSSN
           WITH UR
           END-EXEC

           EXEC SQL DECLARE CURSOR4 CURSOR FOR
           SELECT ENROLLEE_SSN
           FROM   ENROLLMENT_LETTER
           WHERE  ENROLLEE_SSN = :WSSN
             AND  SESSION_TS = :EZT-DC-SESSION-TS
             AND  LETTER_NBR = 1
             WITH UR
           END-EXEC

           EXEC SQL DECLARE CURSOR4A CURSOR FOR
           SELECT ENROLLEE_SSN
           FROM   ENROLLMENT_LETTER
           WHERE  ENROLLEE_SSN = :WSSN
             AND  SESSION_TS = :EZT-DC-SESSION-TS
             AND  LETTER_NBR = :WS-LETTER-NBR
             WITH UR
           END-EXEC

      **AE2016 CHANGES BEGIN
      ** REMOVED THE BELOW CONDITION FROM THE ABOVE QUERY (CURSOR4A) AND
      ** ADDED THE CONDITION WITH SESSION_TS
      *  AND  DATE(SESSION_TS) = :DC-SESSION-DATE
      **AE2016 CHANGES END

           EXEC SQL DECLARE CURSOR5      CURSOR FOR
           SELECT SUM(ASC_BNFT_WAGES_AMT)
           FROM   WAGE_HISTORY_T            A
                , PLAN_YEAR                 B
                , ASSOCIATE_ID_T            C
           WHERE  A.ASC_SS_NBR=  :WSSN
             AND  A.ASC_SS_NBR=  C.ASC_SS_NBR
             AND  CURRENT_YEAR_FLAG = 'Y'
             AND  C.ASC_HIRE_DATE <= A.ASC_PAYROLL_DATE
             AND  A.ASC_PAYROLL_DATE <= LAST_PAYROLL_DATE
             AND  A.ASC_PAYROLL_DATE >= LAST_PAYROLL_DATE - 350 DAYS
           WITH UR
           END-EXEC

           EXEC SQL DECLARE CURSOR5A     CURSOR FOR
           SELECT COUNT(*)
           FROM   WAGE_HISTORY_T            A
                , PLAN_YEAR                 B
                , ASSOCIATE_ID_T            C
                , ASSOC_CLASS_ELIG          D
           WHERE  A.ASC_SS_NBR=   :WSSN
               AND A.ASC_PAY_CODE =  D.PAY_CODE
           AND A.ASC_JOB_CODE = D.JOB_CODE
           AND A.ASC_STATUS_CODE =  D.STATUS_CODE
             AND  A.ASC_SS_NBR=  C.ASC_SS_NBR
             AND  CURRENT_YEAR_FLAG = 'Y'
             AND  A.ASC_PAYROLL_DATE >= C.ASC_HIRE_DATE
               AND  A.ASC_PAYROLL_DATE <= LAST_PAYROLL_DATE
               AND  A.ASC_PAYROLL_DATE >= LAST_PAYROLL_DATE - 350 DAYS
               AND ((A.ASC_BNFT_HRS_WRK >= 0 AND SHORT_CLASS =
                   'MANAGEMENT')
                   OR (A.ASC_BNFT_HRS_WRK >= 10 AND SHORT_CLASS =
                       'HOURLY'))
               AND  A.ASC_PAYROLL_TYPE =  ' '
             WITH UR
           END-EXEC

           EXEC SQL DECLARE CURSOR6      CURSOR FOR
           SELECT PLAN_NAME
                , PLAN_YEAR
                , OPTION_CODE
                , OPTION_AMT
           FROM   COVERAGE_OPTION           A
           WHERE  PLAN_YEAR IN (:CURRENT-PLAN-YEAR, :NEXT-PLAN-YEAR)
             AND  PLAN_NAME IN ('ALI','DLI')
                  ORDER BY PLAN_NAME , PLAN_YEAR, OPTION_CODE
           WITH UR
           END-EXEC
      *61*63
           EXEC SQL DECLARE CURSOR7      CURSOR FOR
            SELECT VALUE(A.CHANGE_REASON_CODE,' ')
                       , A.CHANGE_EVENT_DATE
                       , A.ENROLL_TYPE_CODE
            FROM    ENROLLMENT_SESSION A
            WHERE A.ENROLLEE_SSN = :WSSN
            AND     A.CHANGE_REASON_CODE NOT IN ('AE','NE','LD')
            AND     A.PAYROLL_PROCESS_TS IS NULL
            AND     A.PROCESS_IND = 'Y'
            AND     A.SESSION_TS =
                         (SELECT MAX(B.SESSION_TS)
                           FROM ENROLLMENT_SESSION B
                           WHERE B.ENROLLEE_SSN = :WSSN
                           AND B.CHANGE_REASON_CODE NOT IN ('AE','NE',
                               'LD')
                           AND B.PAYROLL_PROCESS_TS IS NULL
                           AND B.PROCESS_IND = 'Y'
                           AND B.SESSION_TS < :EZT-DC-SESSION-TS)
           WITH UR
           END-EXEC

      ****************************************************          +
      *   CHANGE 34  - CHGD  AND CHANGE_REASON_CODE NOT IN          +
      *                      'HE','SE','FP','PE','LD'               +
      ****************************************************          +
      *   CHANGE 69  - CHGD  A.CHANGE_REASON_CODE IN                +
      *                      'AP','SC','DS'                         +
      *   CHANGE 70  - CHGD  A.CHANGE_REASON_CODE IN                +
      *                      'LA','RW'                              +
      ****************************************************          +
           EXEC SQL DECLARE CURSOR8      CURSOR FOR
            SELECT VALUE(A.CHANGE_REASON_CODE,' ')
                       , A.ENROLL_TYPE_CODE
            FROM    ENROLLMENT_SESSION A
            WHERE A.ENROLLEE_SSN = :WSSN
            AND    A.CHANGE_REASON_CODE IN  ('BI','AP','SC','MA','GS',
                   'LE','DI','NE','DS','LA','RW','TC')
            AND    A.PAYROLL_PROCESS_TS IS NULL
            AND    A.PROCESS_IND = 'Y'
            AND    A.SESSION_TS =
                    (SELECT MAX(B.SESSION_TS)
                    FROM ENROLLMENT_SESSION B
                    WHERE B.ENROLLEE_SSN = :WSSN
                     AND B.CHANGE_REASON_CODE NOT IN ('HE','SE','FP',
                         'PE', 'LD')
                     AND B.PAYROLL_PROCESS_TS IS NULL
                     AND B.PROCESS_IND = 'Y'
                     AND B.SESSION_TS < :EZT-DC-SESSION-TS)
           WITH UR
           END-EXEC

      ***** ADDED CODE FOR MY BENEFITS USER FLAG -- HJMUNI ********
           EXEC SQL DECLARE CURSOR9      CURSOR FOR
            SELECT EXTRN_SITE_IND
            FROM    ENROLLMENT_SESSION A
            WHERE A.ENROLLEE_SSN = :WSSN
            AND     A.PROCESS_IND = 'Y'
            AND     A.SESSION_TS = :EZT-DC-SESSION-TS
           WITH UR
           END-EXEC
      ***** CHANGES ENDED      ******

      ***** ADDED CODE FOR STD PLUS VALIDATION   -- RDLEON ********
      *091808 - AE2009 CHANGED - CHECK IF F RECORD EXISTS IF DOING
      *         AE SESSION. CHANGED TO USE VARIABLE RECORD-TYPE-CD TO
      *         INDICATE CURRENT OR FUTURE RECORD ---RDLEON
      *  AE2016  MNEMA1  CHANGE BEGINS        **************************
           EXEC SQL DECLARE CURSOR10     CURSOR FOR
            SELECT BEN_OPTION_CODE
                 , BENEFIT_CATG_CODE
              FROM ASSOC_INS_ENROLL
             WHERE ASC_SS_NBR     = :WSSN
               AND PLAN_NAME      = 'STP'
               AND RECORD_TYPE_CD = :WS-STP-RECTYPE-CD
           WITH UR
           END-EXEC
      *  AE2016  MNEMA1  CHANGE ENDS          **************************

      ***AE2011-CHANGES BEGIN
           EXEC SQL DECLARE CURSOR11 CURSOR FOR
             SELECT DATE(SESSION_TS + 1 DAY)
             FROM   ENROLLMENT_SESSION
             WHERE  ENROLLEE_SSN = :WS-ENROLLEE-SSN-BI
             AND    SESSION_TS  = :SESSION-TIMESTAMP
           WITH UR
           END-EXEC

      ***AE2011-CHANGES END
      ***70 AE2018-LOA,RW CHANGES BEGINS
           EXEC SQL DECLARE CURSOR12 CURSOR FOR
             SELECT DATE(CHANGE_EVENT_DATE + 1 DAY)
             FROM   ENROLLMENT_SESSION
             WHERE  ENROLLEE_SSN = :WSSN
             AND    SESSION_TS  = :SESSION-TIMESTAMP
           WITH UR
           END-EXEC

      ***70 AE2018-LOA,RW CHANGES ENDS

      **AE2014-DP CHANGES BEGINS ***
      ******************************************************************
      ** CURSOR TO GET THE DEPENDENT DETAILS
      ******************************************************************
           EXEC SQL DECLARE GETRELCD CURSOR FOR
           SELECT DISTINCT R.RELATION_TYPE_CODE
           FROM   INSURED_PERSON               P
                 ,INSURED_RELATION             R
           WHERE  P.NATIONAL_ID                =  :WSSN
           AND    P.PERSON_ID                  =  R.PRI_PERSON_ID
           AND (R.RELATION_STOP_DATE IS NULL OR
           R.RELATION_STOP_DATE >= CURRENT DATE )
           WITH UR
           END-EXEC
      **********AE2014-DP CHANGES ENDS *********************************
      ** CURSOR TO GET THE DEPENDENT DETAILS FROM DEPENDENT_CHANGE
      ******************************************************************
           EXEC SQL DECLARE GETRELDP CURSOR FOR
           SELECT D.RELATION_TYPE
           FROM  DEPENDENT_CHANGE  D , ENROLLMENT_SESSION E
           WHERE D.ENROLLEE_SSN = :WSSN
           AND   D.ENROLLEE_SSN = E.ENROLLEE_SSN
           AND   D.SESSION_TS  = E.SESSION_TS
           AND   E.PROCESS_IND = 'Y'
           AND   D.DEP_COVERED_IND = 'Y'
           AND   D.SESSION_TS = ( SELECT
                                MAX(DC.SESSION_TS)
                                FROM DEPENDENT_CHANGE DC
                                WHERE
                                DC.ENROLLEE_SSN = :WSSN
                           AND  DC.PROCESS_IND = 'Y' )
           WITH UR
           END-EXEC
      ******************************************************************
      * CURSOR TO FETCH WIN NBR OF AN ASSOCAITE
      ******************************************************************
      **AE2016 CHANGES BEGIN
           EXEC SQL DECLARE GETWINNBR     CURSOR FOR
           SELECT DISTINCT(WIN_NBR) , ASC_TYPE_CODE
           FROM  V3_WIN_ASC_NID_US,ASSOCIATE_ID_T
           WHERE NATIONAL_ID =
                         CAST(SUBSTR(DIGITS(ASC_SS_NBR),2,9) AS
                             CHAR(23))
           AND ASC_SS_NBR = :WSSN
           WITH UR
           END-EXEC

      ***** REAL TIME UPDATE AE2019 CHANGES BEGIN ***

           EXEC SQL DECLARE GETWINNBRNEW  CURSOR FOR
           SELECT DISTINCT(WIN_NBR) , ASC_TYPE_CODE
           FROM  V3_WIN_ASC_NID_US
           WHERE NATIONAL_ID =
                         CAST(SUBSTR(DIGITS(:WSSN1),2,9) AS CHAR(23))
           WITH UR
           END-EXEC

      ***** REAL TIME UPDATE AE2019 CHANGES END ***

      **ADDED ASC_TYPE_CODE IN THE SELECT CLAUSE OF ABOVE QUERY
      **AE2016 CHANGES END
      ******************************************************************
      ** CURSOR TO CHECK IF SPOUSE AND ASSOCIATE ARE OF SAME GENDER
      ******************************************************************
           EXEC SQL DECLARE GETGENDER CURSOR FOR
           SELECT 'Y'
           FROM  V3_WIN_ASC_NID_US V ,
                 DEPENDENT_CHANGE  D
           WHERE D.ENROLLEE_SSN = :WSSN
           AND   V.NATIONAL_ID
                       = CAST(SUBSTR(DIGITS(D.ENROLLEE_SSN),2,9) AS
                           CHAR(23))
           AND   D.RELATION_TYPE = 'SP'
           AND   V.GENDER_CODE   = D.DEP_GENDER_IND
           AND   D.SESSION_TS    = (
                                    SELECT MAX(D1.SESSION_TS)
                                    FROM DEPENDENT_CHANGE D1
                                    WHERE D1.ENROLLEE_SSN  = :WSSN
                                    AND D1.RELATION_TYPE   = 'SP'
                                    AND D1.PROCESS_IND = 'Y'
                                    AND D1.DEP_COVERED_IND = 'Y')
           WITH UR
           END-EXEC
      ******************************************************************
      ** CURSOR TO CHECK IF THE UNION TYPE OF SPOUSE IS COMMON LAW
      ******************************************************************
           EXEC SQL DECLARE GETUNIONCD CURSOR FOR
           SELECT 'Y'
           FROM   ASSOC_PARTNER_SESSION P1
           WHERE  P1.UNION_TYPE_CD    = 2
           AND    P1.ENROLLEE_WIN_NBR = :WS-WIN-NBR
           AND    P1.SESSION_TS IN (
                                    SELECT  MAX(P2.SESSION_TS)
                                    FROM    ASSOC_PARTNER_SESSION  P2
                                    WHERE   P2.UNION_TYPE_CD = 2
                                    AND     P2.ENROLLEE_WIN_NBR
                                            = P1.ENROLLEE_WIN_NBR)
           WITH UR
           END-EXEC
      **AE2014-DP CHANGES ENDS ***
      ***YEYB-2015 CHANGES BEGINS
      *
      **JOB TRANSITION CHANGES BEGINS**
           EXEC SQL    DECLARE TRANSITION CURSOR FOR
           SELECT WIN_NBR
                 ,COALESCE(DATE(MAX(LAST_CHG_TS)),'0001-01-01')
                 ,TRANS_RSTRCTN_CODE
                 ,CHANGE_REASON_CODE
                 ,ASSOC_TRANS_TYPE_CODE
           FROM   ASSOCIATE_TRANSITION
           WHERE  WIN_NBR               = :WS-WIN-NBR
           AND    FIRST_SESSN_CPLT_IND  = 'Y'
           AND    ((TRANS_OBSOLETE_DATE IS NULL
           AND    CURRENT DATE BETWEEN TRANS_EFFECTIVE_DATE
           AND    (TRANS_EFFECTIVE_DATE + 61 DAYS))
           OR     (TRANS_OBSOLETE_DATE IS NOT NULL
           AND    TRANS_OBSOLETE_DATE > CURRENT DATE))
           AND    CURRENT_DATE          >= TRANS_EFFECTIVE_DATE
           AND    CURRENT_DATE          <=
                  CASE WHEN TRANS_OBSOLETE_DATE
                                        IS NULL
                            THEN TRANS_EFFECTIVE_DATE + 61 DAYS
                       WHEN TRANS_OBSOLETE_DATE
                                        IS NOT NULL
                            THEN TRANS_OBSOLETE_DATE
                  END
           GROUP  BY
                  WIN_NBR
                 ,TRANS_RSTRCTN_CODE
                 ,CHANGE_REASON_CODE
                 ,ASSOC_TRANS_TYPE_CODE
           WITH UR
           END-EXEC
      **JOB TRANSITION CHANGES ENDS  **
      *
           EXEC SQL    DECLARE WAITDAYS CURSOR FOR
           SELECT DAYS(DATE(:EZT-COV-START-DATE-O))
                - DAYS(DATE(:WELIGIBILITY-DATE))
           FROM   SYSIBM.SYSDUMMY1
           WITH   UR
           END-EXEC
      *
      *54 YEYB2016 CHANGES TO ADD 365DAYS FOR LATE ENROLLE
           EXEC SQL    DECLARE GETCOVST CURSOR FOR
           SELECT DATE(:WS-COVERAGE-START-DT) + 365 DAYS
           FROM   SYSIBM.SYSDUMMY1
           WITH   UR
           END-EXEC
      *54 YEYB2016 CHANGES END

      *54 YEYB-2016 CHANGES TO FETCH FUTURE EFF COV
           EXEC SQL DECLARE STDLTD CURSOR FOR
           SELECT   ASC_SS_NBR,
                    BEN_OPTION_CODE,
                    PLAN_NAME,
                    EFFECTIVE_DT,
                    ORIG_COVERAGE_DATE,
                    COVERAGE_END_DT,
                    WAIT_DAY_QTY,
                    ENROLL_LATE_IND
           FROM   ASSOC_INS_ENROLL
           WHERE    ASC_SS_NBR = :WSSN AND
                    RECORD_TYPE_CD = 'F' AND
                    CREATE_TS =
                               (SELECT MAX(B.CREATE_TS)
                               FROM ASSOC_INS_ENROLL B
                               WHERE B.ASC_SS_NBR = :WSSN
                               AND PLAN_NAME = :EZT-DC-PLAN-NAME
                               AND RECORD_TYPE_CD = 'F')
           WITH UR
           END-EXEC
      *54 YEYB-2016 CHANGES END

      *63 SPECIAL ENROLLMENT CHANGES START
           EXEC SQL DECLARE PREVSES  CURSOR FOR
            SELECT VALUE(A.CHANGE_REASON_CODE,' ')
                       , A.ENROLL_TYPE_CODE
                       , A.CHANGE_EVENT_DATE
                       , A.SESSION_TS
            FROM    ENROLLMENT_SESSION A
            WHERE   A.ENROLLEE_SSN = :WSSN
            AND     A.CHANGE_REASON_CODE
                        NOT IN ('AE','NE','AD','PD','RS','PH','DT')
            AND     A.CHANGE_REASON_CODE ^=  :EZT-DC-CHANGE-REASON-CODE
            AND     A.PAYROLL_PROCESS_TS IS NULL
            AND     A.PROCESS_IND = 'Y'
            AND     A.SESSION_TS =
                         (SELECT MAX(B.SESSION_TS)
                           FROM ENROLLMENT_SESSION B
                           WHERE B.ENROLLEE_SSN = :WSSN
                           AND B.CHANGE_REASON_CODE
                           NOT IN ('AE','NE','AD','PD','RS','PH','DT')
                           AND B.CHANGE_REASON_CODE ^=
                               :EZT-DC-CHANGE-REASON-CODE
                           AND B.PAYROLL_PROCESS_TS IS NULL
                           AND B.PROCESS_IND = 'Y'
                           AND B.SESSION_TS < :EZT-DC-SESSION-TS)
           WITH UR
           END-EXEC

      *68 YEYB17 CHANGES START
           EXEC SQL    DECLARE DISTRANS CURSOR FOR
           SELECT 'Y'
           FROM   ASSOCIATE_TRANSITION
           WHERE  WIN_NBR               = :WS-WIN-NBR
           AND    ((TRANS_OBSOLETE_DATE IS NULL
           AND    CURRENT DATE BETWEEN TRANS_EFFECTIVE_DATE
           AND    (TRANS_EFFECTIVE_DATE + 61 DAYS))
           OR     (TRANS_OBSOLETE_DATE IS NOT NULL
           AND    TRANS_OBSOLETE_DATE > CURRENT DATE))
           WITH UR
           END-EXEC
      /
       PROCEDURE DIVISION.
       EZT--MAIN SECTION.
           MOVE 0 TO EZT-RETURN-CODE
           MOVE '-' TO WS-EVENT-FILL1 OF WS-EVENT-DATE013
           MOVE '-' TO WS-EVENT-FILL2 OF WS-EVENT-DATE013
           MOVE '-' TO WS-EVENT-FILL11 OF WS-EVENT-DATE1014
           MOVE '-' TO WS-EVENT-FILL21 OF WS-EVENT-DATE1014
           MOVE '-' TO NEXT-PLAN-F1 OF NEXT-PLAN-YR-START015
           MOVE 01 TO NEXT-PLAN-MN OF NEXT-PLAN-YR-START015
           MOVE '-' TO NEXT-PLAN-F2 OF NEXT-PLAN-YR-START015
           MOVE 01 TO NEXT-PLAN-DD OF NEXT-PLAN-YR-START015
           MOVE 'MBO   ' TO WS-USERID-CONST OF WS-MY-BEN-USERID017
           ACCEPT EZT-TEMP-DATE FROM DATE YYYYMMDD
           MOVE EZT-TEMP-DATE (3:2) TO EZT-SYSDATE (7:2)
           MOVE EZT-TEMP-DATE (5:2) TO EZT-SYSDATE (1:2)
           MOVE EZT-TEMP-DATE (7:2) TO EZT-SYSDATE (4:2)
           MOVE EZT-TEMP-DATE (1:4) TO EZT-SYSDATE-LONG (7:4)
           MOVE EZT-TEMP-DATE (5:2) TO EZT-SYSDATE-LONG (1:2)
           MOVE EZT-TEMP-DATE (7:2) TO EZT-SYSDATE-LONG (4:2)
           MOVE '/' TO EZT-SYSDATE (3:1)
           MOVE '/' TO EZT-SYSDATE (6:1)
           MOVE '/' TO EZT-SYSDATE-LONG (3:1)
           MOVE '/' TO EZT-SYSDATE-LONG (6:1)
           ACCEPT EZT-TEMP-TIME FROM TIME
           MOVE EZT-TEMP-TIME (1:2) TO EZT-SYSTIME (1:2)
           MOVE EZT-TEMP-TIME (3:2) TO EZT-SYSTIME (4:2)
           MOVE EZT-TEMP-TIME (5:2) TO EZT-SYSTIME (7:2)
           MOVE '.' TO EZT-SYSTIME (3:1)
           MOVE '.' TO EZT-SYSTIME (6:1)
           OPEN OUTPUT SYSPRINT-FILE
      *68 YEYB17 CHANGES END

           PERFORM 1-JOB-CONTROL
           CLOSE SYSPRINT-FILE
           MOVE EZT-RETURN-CODE TO RETURN-CODE
           GOBACK
           .
       1-JOB-CONTROL SECTION.
           MOVE 'N' TO EZT-GOTO-STOP-JOB
           OPEN INPUT ENRDATES-FILE
           PERFORM 1-PROCESS-HOUSEKEEPING
           OPEN INPUT DEPDATA-FILE
           OPEN OUTPUT FAMCHANG-FILE
           MOVE SPACES TO FAMCHANG
           OPEN OUTPUT BEUPDATE-FILE
           MOVE SPACES TO BEUPDATE
           PERFORM Z-READ-DEPDATA-FILE
           PERFORM UNTIL DEPDATA-EOF = 'Y' OR
                         EZT-GOTO-STOP-JOB = 'S'
              PERFORM 1-JOB-PROC
              IF EZT-GOTO-STOP-JOB NOT = 'S'
                 MOVE 'N' TO EZT-GOTO-STOP-JOB
                 PERFORM Z-READ-DEPDATA-FILE
              END-IF
           END-PERFORM
           PERFORM 1-DISPLAY-COUNTS
           CLOSE DEPDATA-FILE
           CLOSE FAMCHANG-FILE
           CLOSE BEUPDATE-FILE
           CLOSE ENRDATES-FILE
           EXEC SQL COMMIT END-EXEC
           .
      *
       1-JOB-PROC SECTION.

           MOVE DC-SESSION-TS TO SESSION-TIMESTAMP
           MOVE DC-ENROLLEE-SSN TO WS-ENROLLEE-SSN

           IF DC-PLAN-YEAR = CURRENT-PLAN-YEAR
              MOVE DC-ENROLLEE-SSN TO SSN-CURR-YEAR
           END-IF
      **AE2016 CHANGES BEGIN
            MOVE SESSION-DATE TO WS-SESSION-DATE
      **AE2016 CHANGES END

      ****AE2011-CHANGE BEGIN
           MOVE 'N' TO TWO-RECORDS-FLAG
           MOVE 'N' TO FIRST-REC-FLAG
      ****AE2011-CHANGE END


           COMPUTE WEB-COUNT = WEB-COUNT + 1

              COMPUTE WS-SESSION-MONTH = DC-SESSION-TS-MONTH + 1
              MOVE WS-SESSION-MONTH TO WS-EVENT-MONTH
              MOVE DC-SESSION-TS-YEAR TO WS-EVENT-YEAR
              MOVE 01 TO WS-EVENT-DAY
              IF WS-EVENT-MONTH = 13
                 MOVE 01 TO WS-EVENT-MONTH
                 COMPUTE WS-EVENT-YEAR = WS-EVENT-YEAR + 1
              END-IF
      * CHANGE 14, ADDED FOR ADVANCED FSC KEYS
              COMPUTE WS-SESSION-MONTH = DC-SESSION-TS-MONTH + 2
              MOVE WS-SESSION-MONTH TO WS-EVENT-MONTH1
              MOVE DC-SESSION-TS-YEAR TO WS-EVENT-YEAR1
              MOVE 01 TO WS-EVENT-DAY1
              IF WS-EVENT-MONTH1 = 13
                 MOVE 01 TO WS-EVENT-MONTH1
                 COMPUTE WS-EVENT-YEAR1 = WS-EVENT-YEAR1 + 1
              END-IF
              IF WS-EVENT-MONTH1 = 14
                 MOVE 02 TO WS-EVENT-MONTH1
                 COMPUTE WS-EVENT-YEAR1 = WS-EVENT-YEAR1 + 1
              END-IF
      * CHANGE 14, END
              IF FIRST-FLAG = 'Y'
                 PERFORM 1-GET-PLAN-YEAR
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
                 MOVE 'N' TO SW-LOAD-ALI-DLI
                 PERFORM 1-GET-ALI-DLI-AMOUNTS
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
                 MOVE
                     '**************************************************
      -             '***' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE CURRENT-PLAN-YEAR TO EZT-DISP-EZ-5-0
                 STRING 'CURRENT-PLAN-YEAR = ' EZT-DISP-EZ-5-0
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE NEXT-PLAN-YEAR TO EZT-DISP-EZ-5-0
                 STRING 'NEXT-PLAN-YEAR    = ' EZT-DISP-EZ-5-0
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'ANNUAL-ENROLLMENT = ' ANNUAL-ENROLLMENT
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'SESSION-DATE      = ' SESSION-DATE
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'DC-CHANGE-EVENT-DATE     = ' WS-EVENT-DATE
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '***' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 'N' TO FIRST-FLAG
              END-IF

               IF DC-PAYROLL-PROCESS-TS-NULL-IND NOT = '?'
                  IF SW-DEBUG-SWITCH = 'Y'
                     MOVE SPACES TO EZT-DISPLAY-STRING
                     MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                     STRING 'DC-PAYROLL-PROCESS-TS NOT NULL '
                         EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                         INTO EZT-DISPLAY-STRING
                     WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                         AFTER ADVANCING 1 LINE
                  END-IF
      *> GO TO JOB
                  GO TO SECTION-EXIT
               END-IF


           IF DC-CHANGE-REASON-CODE = 'NC'
              MOVE 'AE' TO DC-CHANGE-REASON-CODE
              MOVE 4 TO DC-ENROLL-TYPE-CODE
           END-IF
               IF SW-DEBUG-SWITCH = 'Y'
                  MOVE SPACES TO EZT-DISPLAY-STRING
                  MOVE DC-ENROLL-TYPE-CODE TO EZT-DISP-EZ-5-0
                  STRING 'FLAGS ' WS-ENROLLEE-SSN ' ' DC-PLAN-NAME
                      ' ES ' DC-ES-PROCESS-IND ' EC ' DC-EC-PROCESS-IND
                      ' ' ' PAY ' DC-PAYROLL-LOAD-IND ' RC '
                      DC-CHANGE-REASON-CODE ' TC ' EZT-DISP-EZ-5-0
                      DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                  WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                      AFTER ADVANCING 1 LINE
               END-IF
               MOVE DC-ENROLLEE-SSN TO WSSN
               MOVE DC-PLAN-YEAR TO LOOKUP-PLAN-YEAR
               PERFORM 1-GET-BENEFIT-DATA
               IF EZT-GOTO-STOP-JOB NOT = 'N'
                   GO TO SECTION-EXIT
               END-IF
      *54 YEYB 2016 CHANGES START
               IF DC-PLAN-NAME = 'STD' OR 'LTD'
                  PERFORM 1-GET-ASSOC-INS-DATA
                  IF EZT-GOTO-STOP-JOB NOT = 'N'
                      GO TO SECTION-EXIT
                  END-IF
               END-IF
      *54 YEYB 2016 CHANGES END
      **27- IF LD AND PREV SAME DAY SESSION IS FSC, CHANGE THE LD TO THE
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
      *65 LABEN1 AE2017 CHANGES
               IF DC-CHANGE-REASON-CODE = 'LD' AND (DC-PLAN-NAME =
                   'MED' OR 'DEN' OR 'ADD' OR 'VIS')
                  MOVE ' ' TO WS-CHANGE-REASON-CODE
                  MOVE 0 TO WS-EVENT-TYPE-CODE
                  PERFORM 1-GET-PREV-REASON-SAME-DAY
                  IF EZT-GOTO-STOP-JOB NOT = 'N'
                      GO TO SECTION-EXIT
                  END-IF
                  IF WS-CHANGE-REASON-CODE NOT = ' '
                     MOVE WS-CHANGE-REASON-CODE TO
                         DC-CHANGE-REASON-CODE
                     MOVE WS-EVENT-TYPE-CODE TO DC-ENROLL-TYPE-CODE
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
                     IF (WS-CHANGE-REASON-CODE = 'MA' OR 'GS' OR 'DI'
                         OR 'LE' OR 'BI' OR 'AP' OR 'SC' OR 'DS' OR
                         'LA' OR 'RW' OR 'TC')
                         MOVE WS-EVENT-DATE-LD TO DC-CHANGE-EVENT-DATE
                         MOVE WS-EVENT-DATE-LD TO COV-START-DATE-O
                         MOVE WS-EVENT-DATE-LD TO WS-ORIG-EVENT-DATE
                     END-IF
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE SPACES TO EZT-DISPLAY-STRING
                        MOVE WS-EVENT-TYPE-CODE TO EZT-DISP-EZ-5-0
                        STRING 'LD CHANGED TO ' WS-CHANGE-REASON-CODE
                            ' TYPE CHANGED TO ' EZT-DISP-EZ-5-0
                            DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
                  END-IF
               END-IF
      **27- END OF CHANGE
      *65 LABEN1 AE2017 CHANGES END

      *61*63- IF LD, FETCH THE DETAILS FOR ANY SESSION DONE ON THE SAME
               IF DC-CHANGE-REASON-CODE = 'LD' AND (DC-PLAN-NAME =
                   'STD' OR 'LTD')
                  MOVE ' ' TO WS-CHANGE-REASON-CODE
                  MOVE 0 TO WS-EVENT-TYPE-CODE
                  PERFORM 1-GET-PREV-SESSION-SAME-DAY
                  IF EZT-GOTO-STOP-JOB NOT = 'N'
                      GO TO SECTION-EXIT
                  END-IF
      *62
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
                  IF WS-CHANGE-REASON-CODE NOT = ' '
                     IF ((WS-CHANGE-REASON-CODE = 'MA' OR 'GS' OR 'DI'
                         OR 'LE' OR 'BI' OR 'AP' OR 'SC' OR 'DS' OR
                         'LA' OR 'RW' OR 'TC') OR
                         (WS-CHANGE-REASON-CODE = 'SE' AND DC-PLAN-NAME
                         = 'STD'))
                        MOVE WS-CHANGE-REASON-CODE TO
                            DC-CHANGE-REASON-CODE
                        MOVE WS-EVENT-TYPE-CODE TO DC-ENROLL-TYPE-CODE
                        MOVE WS-EVENT-DATE-LD-SE TO
                            DC-CHANGE-EVENT-DATE
                     ELSE
                        MOVE 'LD' TO DC-CHANGE-REASON-CODE
                     END-IF
      *62
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE SPACES TO EZT-DISPLAY-STRING
                        MOVE WS-EVENT-TYPE-CODE TO EZT-DISP-EZ-5-0
                        STRING 'LD CHANGED TO ' WS-CHANGE-REASON-CODE
                            ' TYPE CHANGED TO ' EZT-DISP-EZ-5-0
                            DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
                  END-IF
               END-IF

      *61*63 IF SE, FETCH THE DETAILS FOR ANY SESSION DONE ON THE SAME D
               IF DC-CHANGE-REASON-CODE = 'SE' AND DC-PLAN-NAME NOT =
                   'STD'
                  MOVE ' ' TO WS-CHANGE-REASON-CODE
                  MOVE 0 TO WS-EVENT-TYPE-CODE
                  PERFORM 1-GET-PREV-SESSION-SAME-DAY
                  IF EZT-GOTO-STOP-JOB NOT = 'N'
                      GO TO SECTION-EXIT
                  END-IF
      *62
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
                  IF WS-CHANGE-REASON-CODE NOT = ' '
                     IF (WS-CHANGE-REASON-CODE = 'MA' OR 'GS' OR 'DI'
                         OR 'LE' OR 'TC' OR 'BI' OR 'AP' OR 'SC' OR
                         'LD' OR 'DS' OR 'LA' OR 'RW')
                        MOVE WS-CHANGE-REASON-CODE TO
                            DC-CHANGE-REASON-CODE
                        MOVE WS-EVENT-TYPE-CODE TO DC-ENROLL-TYPE-CODE
                        IF (WS-CHANGE-REASON-CODE = 'LD')
                            IF DC-PLAN-NAME = 'ALI' OR 'DLI'
                             CONTINUE
                            ELSE
                             IF DC-PLAN-NAME = 'MED' OR 'DEN' OR 'ADD'
                                 OR 'VIS'
                             MOVE ' ' TO WS-CHANGE-REASON-CODE
                             MOVE 0 TO WS-EVENT-TYPE-CODE
                             PERFORM 1-GET-PREV-REASON-SAME-DAY
                             IF EZT-GOTO-STOP-JOB NOT = 'N'
                                 GO TO SECTION-EXIT
                             END-IF
                             IF WS-CHANGE-REASON-CODE NOT = ' '
                             MOVE WS-CHANGE-REASON-CODE TO
                                 DC-CHANGE-REASON-CODE
                             MOVE WS-EVENT-TYPE-CODE TO
                                 DC-ENROLL-TYPE-CODE
                             END-IF
                            ELSE
                             MOVE WS-SESSION-TS TO DC-SESSION-TS
                             PERFORM 1-GET-PREV-SESSION-SAME-DAY
                             IF EZT-GOTO-STOP-JOB NOT = 'N'
                                 GO TO SECTION-EXIT
                             END-IF
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
                             IF (WS-CHANGE-REASON-CODE = 'MA' OR 'GS'
                                 OR 'DI' OR 'LE' OR 'TC' OR 'RW' OR
                                 'BI' OR 'SC' OR 'AP' OR 'DS' OR 'LA')
                             MOVE WS-CHANGE-REASON-CODE TO
                                 DC-CHANGE-REASON-CODE
                             MOVE WS-EVENT-TYPE-CODE TO
                                 DC-ENROLL-TYPE-CODE
                             MOVE WS-EVENT-DATE-LD-SE TO
                                 DC-CHANGE-EVENT-DATE
                             ELSE
                             MOVE 'SE' TO DC-CHANGE-REASON-CODE
                             END-IF
                            END-IF
                            END-IF
                        ELSE
                            MOVE WS-EVENT-DATE-LD-SE TO
                                DC-CHANGE-EVENT-DATE
                        END-IF
                     ELSE
                        MOVE 'SE' TO DC-CHANGE-REASON-CODE
                     END-IF
      *62
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE SPACES TO EZT-DISPLAY-STRING
                        MOVE WS-EVENT-TYPE-CODE TO EZT-DISP-EZ-5-0
                        STRING 'SE CHANGED TO ' WS-CHANGE-REASON-CODE
                            ' TYPE CHANGED TO ' EZT-DISP-EZ-5-0
                            DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
                  END-IF
               END-IF
      *61
      **31- CODE ADJUSTMENTS START HERE
      **34
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
           IF (DC-CHANGE-REASON-CODE = 'HE' OR 'PE') AND (DC-PLAN-NAME
               = 'MED' OR 'VIS')
              MOVE 'SE' TO DC-CHANGE-REASON-CODE
              MOVE 68 TO DC-ENROLL-TYPE-CODE
           END-IF
      **34
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
           IF DC-CHANGE-REASON-CODE = 'FP' AND (DC-PLAN-NAME = 'MED' OR
               'VIS')
              MOVE 'FP' TO DC-CHANGE-REASON-CODE
              MOVE 17 TO DC-ENROLL-TYPE-CODE
           END-IF
      **34
           IF (DC-CHANGE-REASON-CODE = 'HE' OR 'FP' OR 'PE') AND
               (DC-PLAN-NAME = 'ALI' OR 'DLI' OR 'STD' OR 'LTD')
              MOVE 'LD' TO DC-CHANGE-REASON-CODE
              MOVE 64 TO DC-ENROLL-TYPE-CODE
           END-IF
      **34
           IF (DC-CHANGE-REASON-CODE = 'SE' OR 'HE' OR 'FP' OR 'PE')
               AND (DC-PLAN-NAME = 'DEN' OR 'ADD')
              MOVE ' ' TO WS-CHANGE-REASON-CODE
              MOVE 0 TO WS-EVENT-TYPE-CODE
              PERFORM 1-GET-PREV-REASON-HE
              IF EZT-GOTO-STOP-JOB NOT = 'N'
                  GO TO SECTION-EXIT
              END-IF
              IF WS-CHANGE-REASON-CODE NOT = ' '
                 MOVE WS-CHANGE-REASON-CODE TO DC-CHANGE-REASON-CODE
                 MOVE WS-EVENT-TYPE-CODE TO DC-ENROLL-TYPE-CODE
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE WS-EVENT-TYPE-CODE TO EZT-DISP-EZ-5-0
                    STRING 'HE CHANGED TO ' WS-CHANGE-REASON-CODE
                        ' TYPE CHANGED TO ' EZT-DISP-EZ-5-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    END-IF
                 END-IF
           END-IF
      **31- CODE ADJUSTMENTS END HERE
      **21-  CODE STARTS HERE
               MOVE ' ' TO WS-ORIG-EVENT-DATE
               MOVE 'N' TO WS-EVENT-DATE-CHANGED
               IF DC-CHANGE-REASON-CODE = 'LE' AND (DC-PLAN-NAME NOT =
                   'STD' AND 'LTD')
                  MOVE DC-CHANGE-EVENT-DATE TO WS-ORIG-EVENT-DATE
                  PERFORM 1-GET-EVENT-DATE
                  MOVE WS-DI-LE-EVENT-DATE TO DC-CHANGE-EVENT-DATE
                  IF SW-DEBUG-SWITCH = 'Y'
                      MOVE 'ONE-EVENT DATE FOR  LE ' TO
                          EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                      MOVE SPACES TO EZT-DISPLAY-STRING
                      STRING 'WS-ORIG-EVENT-DATE = ' WS-ORIG-EVENT-DATE
                          DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                  END-IF
               END-IF
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'DS'
               IF DC-CHANGE-REASON-CODE = 'DI' OR 'DS'
      *31 - ADD 'ADD' TO CONDTION NOW THAT CATEGORY CODE IS USED
                  IF (DC-PLAN-NAME = 'MED' OR 'DEN' OR 'STD' OR 'DLI'
                      OR 'ADD' OR 'VIS') AND DC-CATEGORY-CODE <
                      WCOVERAGE-CATEGORY
                     MOVE DC-CHANGE-EVENT-DATE TO WS-ORIG-EVENT-DATE
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE 'TWO-EVENT DATE FOR DI ' TO
                            EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                        MOVE SPACES TO EZT-DISPLAY-STRING
                        STRING 'WS-ORIG-EVENT-DATE = '
                            WS-ORIG-EVENT-DATE DELIMITED BY SIZE
                            INTO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
                     PERFORM 1-GET-EVENT-DATE
                     MOVE WS-DI-LE-EVENT-DATE TO DC-CHANGE-EVENT-DATE
                     MOVE 'Y' TO WS-EVENT-DATE-CHANGED
                  END-IF
      ***AE2014 CHANGES BEGINS***
                  MOVE DC-OPTION-CODE TO WS-COVERAGE-OPT-CMP-ONE
                  MOVE WCOVERAGE-OPTION TO WS-COVERAGE-OPT-CMP-TWO
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-INCREASED
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-DECREASED
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-EQUAL
                  PERFORM 1-COMPARE-COVERAGE-OPTIONS
                  IF (DC-PLAN-NAME = 'ADD' OR 'ALI' OR 'DLI') AND
                      WS-COVERAGE-OPT-ONE-DECREASED = 'Y' AND
                      WS-EVENT-DATE-CHANGED = 'N'
      *         DC-OPTION-CODE   < WCOVERAGE-OPTION  AND +
                      MOVE DC-CHANGE-EVENT-DATE TO WS-ORIG-EVENT-DATE
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE 'THREE-EVENT DATE FOR DI ' TO
                            EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                        MOVE SPACES TO EZT-DISPLAY-STRING
                        STRING 'WS-ORIG-EVENT-DATE = '
                            WS-ORIG-EVENT-DATE DELIMITED BY SIZE
                            INTO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                      END-IF
                      PERFORM 1-GET-EVENT-DATE
                      MOVE WS-DI-LE-EVENT-DATE TO DC-CHANGE-EVENT-DATE
                  END-IF
               END-IF
      **
              IF DC-ENROLLEE-SSN = SSN-PREV AND DC-PLAN-NAME =
                  PLAN-NAME-PREV AND CHANGE-REASON-CD-PREV = 'LE' AND
                  DC-CHANGE-REASON-CODE = 'AE' AND DC-ENROLL-TYPE-PREV
                  = 67 AND DC-ENROLL-TYPE-CODE = 67
                      PERFORM 1-GET-EVENT-DATE
                      MOVE WS-DI-LE-EVENT-DATE TO DC-CHANGE-EVENT-DATE
                  IF SW-DEBUG-SWITCH = 'Y'
                      MOVE 'THREE-EVENT DATE FOR  LE ' TO
                          EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                  END-IF
              END-IF
              IF DC-ENROLLEE-SSN = SSN-PREV AND DC-PLAN-NAME =
                  PLAN-NAME-PREV AND CHANGE-REASON-CD-PREV = 'DI' AND
                  DC-CHANGE-REASON-CODE = 'AE' AND DC-ENROLL-TYPE-PREV
                  = 13 AND DC-ENROLL-TYPE-CODE = 13 AND DC-OPTION-CODE
                  <= COV-OPTION-O AND DC-CATEGORY-CODE < COV-CATEGORY-O

                      PERFORM 1-GET-EVENT-DATE
                      MOVE WS-DI-LE-EVENT-DATE TO DC-CHANGE-EVENT-DATE
                  IF SW-DEBUG-SWITCH = 'Y'
                      MOVE 'FOUR-EVENT DATE FOR DI ' TO
                          EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                  END-IF
              END-IF

      *69 JEDHA1 BA2018 CHANGES BEGINS
              IF DC-ENROLLEE-SSN = SSN-PREV AND DC-PLAN-NAME =
                  PLAN-NAME-PREV AND CHANGE-REASON-CD-PREV = 'DS' AND
                  DC-CHANGE-REASON-CODE = 'AE' AND DC-ENROLL-TYPE-PREV
                  = 16 AND DC-ENROLL-TYPE-CODE = 16 AND DC-OPTION-CODE
                  <= COV-OPTION-O AND DC-CATEGORY-CODE < COV-CATEGORY-O

                      PERFORM 1-GET-EVENT-DATE
                      MOVE WS-DI-LE-EVENT-DATE TO DC-CHANGE-EVENT-DATE
                  IF SW-DEBUG-SWITCH = 'Y'
                      MOVE 'FOUR-EVENT DATE FOR DS ' TO
                          EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                  END-IF
              END-IF
      *69 JEDHA1 BA2018 CHANGES ENDS

              IF SW-DEBUG-SWITCH = 'Y'
                  MOVE SPACES TO EZT-DISPLAY-STRING
                  STRING 'DC-CHANGE-EVENT-DATE ' WS-ENROLLEE-SSN ' '
                      DC-CHANGE-EVENT-DATE ' ' DC-PLAN-NAME
                      DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                  WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                      AFTER ADVANCING 1 LINE
              END-IF
      **21-  CODE END HERE

               IF (DC-ES-PROCESS-IND = 'Y' AND DC-EC-PROCESS-IND = 'Y'
                   AND DC-PAYROLL-LOAD-IND = 'Y')

                   IF SW-DEBUG-SWITCH = 'Y'
                      MOVE 'ZERO' TO EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                   END-IF
               ELSE
                  IF (DC-PLAN-NAME = 'ALI' AND DC-ES-PROCESS-IND = 'Y'
                      AND DC-EC-PROCESS-IND = 'Y' AND
                      DC-PAYROLL-LOAD-IND NOT = ' ' AND
                      WS-NEWLY-ELIGIBLE = 'Y')
                      IF SW-DEBUG-SWITCH = 'Y'
                         MOVE 'ZERO-1' TO EZT-DISPLAY-STRING
                         WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                             AFTER ADVANCING 1 LINE
                      END-IF
                  ELSE
                    IF DC-PLAN-NAME = 'DLI'
                       MOVE 'N' TO WS-DENIAL-LETTER
                       MOVE DC-ENROLLEE-SSN TO WSSN
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WSSN TO EZT-DISP-EZ-17-0
                          STRING 'WSSN ' EZT-DISP-EZ-17-0 ' '
                              DC-SESSION-TS DELIMITED BY SIZE
                              INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       PERFORM 1-CHECK-FOR-DENIAL-LETTER
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          STRING 'WS-DENIAL-LETTER ' WS-DENIAL-LETTER
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                    END-IF
                    IF (DC-PLAN-NAME = 'DLI' AND DC-ES-PROCESS-IND =
                        'Y' AND DC-EC-PROCESS-IND = 'Y' AND
                        DC-PAYROLL-LOAD-IND NOT = ' ' AND
                        WS-DENIAL-LETTER = 'N')

                        IF SW-DEBUG-SWITCH = 'Y'
                           MOVE 'ZERO-2' TO EZT-DISPLAY-STRING
                           WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                               AFTER ADVANCING 1 LINE
                        END-IF
                    ELSE
      ***AE2014 CHANGES BEGINS***
                  MOVE DC-OPTION-CODE TO WS-COVERAGE-OPT-CMP-ONE
                  MOVE WCOVERAGE-OPTION TO WS-COVERAGE-OPT-CMP-TWO
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-INCREASED
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-DECREASED
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-EQUAL
                  PERFORM 1-COMPARE-COVERAGE-OPTIONS
      **AE2014 CHANGES ENDS**
                       IF DC-PLAN-NAME = 'ALI' AND
                           WS-COVERAGE-OPT-ONE-DECREASED = 'Y'
                           CONTINUE
      *              DC-OPTION-CODE < WCOVERAGE-OPTION
                       ELSE
      **AE2016 CHANGE BEGIN
      *62
                        IF (((WASC-PAY-CODE = '1' OR '3' OR '6') AND
                            WASC-JOB-CODE = 'T' OR 'P') OR WS-PT-TRUCK
                            = 'Y') AND DC-PLAN-NAME = 'ALI' AND
                            WS-SESSION-DATE-YY = 2016 AND
                            DC-CHANGE-REASON-CODE = 'AE'
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'ZERO-4 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                        ELSE
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'ZERO-3' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             MOVE SPACES TO EZT-DISPLAY-STRING
                             MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                             STRING
                                 'DOES NOT QUALIFY FOR PROCESSING     '
                                 EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                                 INTO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'CLOSE-BENEFIT-DATE ONE' TO
                                 EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          IF SW-CURSOR2A-ACTIV = 'Y'
                             PERFORM 1-CLOSE-BENEFIT-DATAN
                             IF EZT-GOTO-STOP-JOB NOT = 'N'
                                 GO TO SECTION-EXIT
                             END-IF
                          END-IF
                          PERFORM 1-CLOSE-BENEFIT-DATA
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
      *> GO TO JOB
                          GO TO SECTION-EXIT
      **AE2016 CHANGES BEGIN
      *             END-IF
                        END-IF
      **AE2016 CHANGES END
                       END-IF
                    END-IF
                  END-IF
               END-IF

      ***AE2011-CHANGES BEGIN
               MOVE DC-ENROLLEE-SSN TO WS-ENROLLEE-SSN-BI
               PERFORM 1-GET-SESSION-DATE-PLUS1
               IF EZT-GOTO-STOP-JOB NOT = 'N'
                   GO TO SECTION-EXIT
               END-IF
      ***AE2011-CHANGES END

               PERFORM 1-BUILD-UPDATE-YEAR-RECORD
               IF EZT-GOTO-STOP-JOB NOT = 'N'
                   GO TO SECTION-EXIT
               END-IF
               IF SW-DEBUG-SWITCH = 'Y'
                  MOVE 'CLOSE-BENEFIT-DATE MAIN' TO EZT-DISPLAY-STRING
                  WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                      AFTER ADVANCING 1 LINE
               END-IF
               IF SW-CURSOR2A-ACTIV = 'Y'
                  PERFORM 1-CLOSE-BENEFIT-DATAN
                  IF EZT-GOTO-STOP-JOB NOT = 'N'
                      GO TO SECTION-EXIT
                  END-IF
               END-IF
               PERFORM 1-CLOSE-BENEFIT-DATA
           .
       SECTION-EXIT.

       1-GET-BENEFIT-DATA SECTION.
            MOVE DC-PLAN-NAME TO EZT-DC-PLAN-NAME
            EXEC SQL OPEN CURSOR2
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 STRING 'FAILED TO OPEN BENEFIT_ENROLLMENT, SQLCODE = '
                     EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
      *    .
      *1-WS-SQL-DISPLAY.
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            MOVE 0 TO WWAIT-DAY-QTY
            EXEC SQL FETCH CURSOR2
                INTO :WSSN ,
                     :WPLAN-NAME ,
                     :WPLAN-YEAR ,
                     :WPAYROLL-DATE :WPAYROLL-DATE-IND,
                     :WHISTORY-TYPE-CD ,
                     :WCOVERAGE-START-DT :WCOVERAGE-START-DT-IND,
                     :WCOVERAGE-END-DT :WCOVERAGE-END-DT-IND,
                     :WCOVERAGE-OPTION ,
                     :WCOVERAGE-CATEGORY ,
                     :WBENEFIT-OPTION ,
                     :WENROLLMENT-DATE :WENROLLMENT-DATE-IND,
                     :WENROLL-RCPT-DATE :WENROLL-RCPT-DATE-IND,
                     :WCONFIRM-DATE :WCONFIRM-DATE-IND,
                     :WREMINDER-DATE :WREMINDER-DATE-IND,
                     :WELIGIBILITY-DATE :WELIGIBILITY-DATE-IND,
                     :WORIG-COVERAGE-DATE :WORIG-COVERAGE-DATE-IND,
                     :WORIG-COVERAGE-DATE-PLUS15
                     :WORIG-COVERAGE-DATE-PLUS15-IND,
                     :WARREARS-CANCEL-DT :WARREARS-CANCEL-DT-IND,
                     :WPEAK-TIME-DATE :WPEAK-TIME-DATE-IND,
                     :WDEFAULT-DATE :WDEFAULT-DATE-IND,
                     :WCOPY-TO-PAYROLL-FL ,
                     :WCHANGE-REASON-CODE :WCHANGE-REASON-CODE-IND,
                     :WLAST-CHANGE-DATE :WLAST-CHANGE-DATE-IND,
                     :WLAST-CHANGE-ID :WLAST-CHANGE-ID-IND,
                     :WINS-PROVIDER-ID :WINS-PROVIDER-ID-IND,
                     :WINS-BROKER-ID :WINS-BROKER-ID-IND,
                     :WOUT-POCKET-MAX-CD,
                     :WUP-FRONT-ALLOW-CD,
                     :WORIG-COVERAGE-DATE-PLUS60
                     :WORIG-COVERAGE-DATE-PLUS60-IND,
                     :WORIG-COVERAGE-DATE-PLUS75
                     :WORIG-COVERAGE-DATE-PLUS75-IND,
                     :WENROLL-LATE-INT,
                     :WASC-PAY-CODE,
                     :WASC-JOB-CODE,
                     :WPEAK-NEWLY-ELIGIBLE-DATE-STRT,
                     :WPEAK-NEWLY-ELIGIBLE-DATE-END,
                     :WMGMT-NEWLY-ELIGIBLE-DATE,
                     :WHRLY-NEWLY-ELIGIBLE-DATE,
                     :WS-WORK-STATE,
                     :WHRLY-NEWLY-ELIGIBLE-DTPLUS90,
                     :WS-DIS-COV-STRT-DT,
                     :WHRLY-NEWLY-ELIGIBLE-DTPLUS194,
                     :WWAIT-DAY-QTY:WWAIT-DAY-QTY-IND ,
                     :WSTATE-PROVINCE-CD
            END-EXEC

              IF SQLCODE = 0
                 IF WPLAN-NAME = 'ALI'
                     MOVE WCOVERAGE-OPTION TO WS-ALI-COVERAGE-OPTION
                 END-IF
                 MOVE 'N' TO WS-NEWLY-ELIGIBLE
      *57 YEYB2016 FIX CHANGES START
                 MOVE 'N' TO WS-STD-LTD-NW-ELIG
      *57 YEYB2016 FIX CHANGES END
      **AE2016 CHANGES BEGIN
                 IF WS-TEMP-SSN NOT = WS-ENROLLEE-SSN
                    MOVE 'N' TO WS-PT-TRUCK
                    IF WASC-JOB-CODE = 'D'
                       PERFORM 1-GET-WIN-NBR
                    END-IF
                 END-IF
      *
                 IF ((WASC-PAY-CODE = '1' OR '3' OR '6') AND
                     WASC-JOB-CODE = 'P' OR 'T' OR WS-PT-TRUCK = 'Y')
                     AND SESSION-DATE <= WCOVERAGE-START-DT
      *        SESSION-DATE >= WPEAK-NEWLY-ELIGIBLE-DATE-START   AND +
      *        SESSION-DATE <= WPEAK-NEWLY-ELIGIBLE-DATE-END
                    MOVE 'Y' TO WS-NEWLY-ELIGIBLE
      ****AE2011-CHANGE BEGIN
      *        WS-WAIT-PERIOD-END    = WPEAK-NEWLY-ELIGIBLE-DATE-END
                    MOVE WCOVERAGE-START-DT TO WS-WAIT-PERIOD-END
      *56*57 YEYB2016 CHANGES START
                    IF WPLAN-NAME = 'DEN'
                        MOVE WCOVERAGE-START-DT TO WS-DEN-COV-STRT-DT
                    END-IF
                    IF (WPLAN-NAME = 'STD' OR 'LTD') AND SESSION-DATE
                        <= WS-DEN-COV-STRT-DT
                        MOVE 'Y' TO WS-STD-LTD-NW-ELIG
                    END-IF
      *56*57 YEYB2016 CHANGES END
      **AE2016 CHANGES END
      ****AE2011-CHANGE END
                 ELSE
      ****PCR# 10025822 BEGIN
                    IF WASC-PAY-CODE = '1' AND WASC-JOB-CODE = 'F' AND
                        SESSION-DATE <= WCOVERAGE-START-DT
                       MOVE 'Y' TO WS-NEWLY-ELIGIBLE
      *67 LABEN1 AE2017 CHANGES
                    IF (WPLAN-NAME = 'STD' OR 'LTD') AND SESSION-DATE
                        <= WS-DIS-COV-STRT-DT
                        MOVE 'Y' TO WS-STD-LTD-NW-ELIG
                    END-IF
      *67 LABEN1 AE2017 CHANGES END
      ****AE2011-CHANGE BEGIN
                       MOVE WCOVERAGE-START-DT TO WS-WAIT-PERIOD-END
      ****PCR# 10025822 END
      ****AE2011-CHANGE END
                    ELSE
                       IF SESSION-DATE <= WMGMT-NEWLY-ELIGIBLE-DATE
                          MOVE 'Y' TO WS-NEWLY-ELIGIBLE
      *56*57 YEYB2016 CHANGES START
                       IF (WPLAN-NAME = 'STD' OR 'LTD')
                             MOVE 'Y' TO WS-STD-LTD-NW-ELIG
                       END-IF
      *56*57 YEYB2016 CHANGES END
      ****AE2011-CHANGE BEGIN
                          MOVE SESSION-DATE TO WS-WAIT-PERIOD-END
      ****AE2011-CHANGE END
                       ELSE
                          MOVE 'N' TO WS-NEWLY-ELIGIBLE
                          MOVE 'N' TO WS-STD-LTD-NW-ELIG
                       END-IF
                    END-IF
                 END-IF
      *AE2016 CHANGES BEGIN
      ******************************************************************
      *ADDED CODE FOR HOURLY ASSOCIATE '1F' HAVING 180 DAY WAIT_DAY_QTY
      *FOR WS-NEWLY-ELIGIBLE
      ******************************************************************
      *     IF WASC-PAY-CODE = '1'    AND      +
      *        WASC-JOB-CODE = 'F'    AND      +
      *        WWAIT-DAY-QTY = 180
      *         IF SESSION-DATE  >= WHRLY-NEWLY-ELIGIBLE-DTPLUS90  AND +
      *            SESSION-DATE  <= WHRLY-NEWLY-ELIGIBLE-DTPLUS194
      *            WS-NEWLY-ELIGIBLE = 'Y'
      *         ELSE
      *            WS-NEWLY-ELIGIBLE = 'N'
      *         END-IF
      ****AE2011-CHANGE BEGIN
      *         WS-WAIT-PERIOD-END = WHRLY-NEWLY-ELIGIBLE-DTPLUS194
      ****AE2011-CHANGE END
      *     END-IF
      *AE2016 CHANGES END
      ******************************************************************
      *CHANGE ENDED
      ******************************************************************
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                    STRING 'NEWLY ELIG FLAG ' EZT-DISP-EZ-10-0 ' '
                        WS-NEWLY-ELIGIBLE ' ' WASC-PAY-CODE ' '
                        WASC-JOB-CODE ' ES ' SESSION-DATE ' PTB '
                        WPEAK-NEWLY-ELIGIBLE-DATE-STRT ' PTE '
                        WPEAK-NEWLY-ELIGIBLE-DATE-END ' MED '
                        WMGMT-NEWLY-ELIGIBLE-DATE ' HED '
                        WHRLY-NEWLY-ELIGIBLE-DATE DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
              ELSE

      ***** REAL TIME UPDATE AE2019 CHANGES BEGIN *****

                 IF SQLCODE = 100
                    PERFORM 1-GET-BENEFIT-DATAN
                    IF EZT-GOTO-STOP-JOB NOT = 'N'
                        GO TO SECTION-EXIT
                    END-IF

      ***** REAL TIME UPDATE AE2019 CHANGES END *****
                 ELSE
                    MOVE SQLCODE TO WS-SQL-DISPLAY
                    MOVE LOOKUP-PLAN-YEAR TO LOOKUP-PLAN-YEAR-N
                    MOVE
                        '***********************************************
      -                '****' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                    STRING
                        'FAILED TO READ BENEFIT_ENROLLMENT, SQLCODE = '
                        EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                        ' PLAN YEAR = ' EZT-DISP-EZ-5-0 ' PLAN NAME = '
                        DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '****' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 END-IF
              END-IF
           .
       SECTION-EXIT.

       1-CLOSE-BENEFIT-DATA SECTION.

              EXEC SQL CLOSE CURSOR2
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE BENEFIT_ENROLLMENT, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .

      ***** REAL TIME UPDATE AE2019 CHANGES BEGIN *****

       1-GET-BENEFIT-DATAN SECTION.
            MOVE DC-PLAN-NAME TO EZT-DC-PLAN-NAME
            EXEC SQL OPEN CURSOR2A
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN BENEFIT_ENROLLMENT_N, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              ELSE
                 MOVE 'Y' TO SW-CURSOR2A-ACTIV
              END-IF
            MOVE 0 TO WWAIT-DAY-QTY
            EXEC SQL FETCH CURSOR2A
                INTO :WSSN ,
                     :WPLAN-NAME ,
                     :WPLAN-YEAR ,
                     :WPAYROLL-DATE :WPAYROLL-DATE-IND,
                     :WHISTORY-TYPE-CD ,
                     :WCOVERAGE-START-DT :WCOVERAGE-START-DT-IND,
                     :WCOVERAGE-END-DT :WCOVERAGE-END-DT-IND,
                     :WCOVERAGE-OPTION ,
                     :WCOVERAGE-CATEGORY ,
                     :WBENEFIT-OPTION ,
                     :WENROLLMENT-DATE :WENROLLMENT-DATE-IND,
                     :WENROLL-RCPT-DATE :WENROLL-RCPT-DATE-IND,
                     :WCONFIRM-DATE :WCONFIRM-DATE-IND,
                     :WREMINDER-DATE :WREMINDER-DATE-IND,
                     :WELIGIBILITY-DATE :WELIGIBILITY-DATE-IND,
                     :WORIG-COVERAGE-DATE :WORIG-COVERAGE-DATE-IND,
                     :WORIG-COVERAGE-DATE-PLUS15
                     :WORIG-COVERAGE-DATE-PLUS15-IND,
                     :WARREARS-CANCEL-DT :WARREARS-CANCEL-DT-IND,
                     :WPEAK-TIME-DATE :WPEAK-TIME-DATE-IND,
                     :WDEFAULT-DATE :WDEFAULT-DATE-IND,
                     :WCOPY-TO-PAYROLL-FL ,
                     :WCHANGE-REASON-CODE :WCHANGE-REASON-CODE-IND,
                     :WLAST-CHANGE-DATE :WLAST-CHANGE-DATE-IND,
                     :WLAST-CHANGE-ID :WLAST-CHANGE-ID-IND,
                     :WINS-PROVIDER-ID :WINS-PROVIDER-ID-IND,
                     :WINS-BROKER-ID :WINS-BROKER-ID-IND,
                     :WOUT-POCKET-MAX-CD,
                     :WUP-FRONT-ALLOW-CD,
                     :WORIG-COVERAGE-DATE-PLUS60
                     :WORIG-COVERAGE-DATE-PLUS60-IND,
                     :WORIG-COVERAGE-DATE-PLUS75
                     :WORIG-COVERAGE-DATE-PLUS75-IND,
                     :WENROLL-LATE-INT,
                     :WASC-PAY-CODE :WASC-PAY-CODE-IND,
                     :WASC-PAY-CD :WASC-PAY-CD-IND,
                     :WASC-JOB-CODE :WASC-JOB-CODE-IND,
                     :WPEAK-NEWLY-ELIGIBLE-DATE-STRT,
                     :WPEAK-NEWLY-ELIGIBLE-DATE-END,
                     :WMGMT-NEWLY-ELIGIBLE-DATE,
                     :WHRLY-NEWLY-ELIGIBLE-DATE,
                     :WS-WORK-STATE,
                     :WHRLY-NEWLY-ELIGIBLE-DTPLUS90,
                     :WS-DIS-COV-STRT-DT,
                     :WHRLY-NEWLY-ELIGIBLE-DTPLUS194,
                     :WWAIT-DAY-QTY:WWAIT-DAY-QTY-IND ,
                     :WSTATE-PROVINCE-CD :WSTATE-PROVINCE-CD-IND
            END-EXEC

              IF SQLCODE = 0 AND WASC-PAY-CODE-IND = 0 AND
                  WASC-JOB-CODE-IND = 0
                 IF WPLAN-NAME = 'ALI'
                     MOVE WCOVERAGE-OPTION TO WS-ALI-COVERAGE-OPTION
                 END-IF
                 MOVE 'N' TO WS-NEWLY-ELIGIBLE
                 MOVE 'N' TO WS-STD-LTD-NW-ELIG
                 IF WS-TEMP-SSN NOT = WS-ENROLLEE-SSN
                    MOVE 'N' TO WS-PT-TRUCK
                    IF WASC-PAY-CD = 'D'
                       PERFORM 1-GET-WIN-NBR
                    END-IF
                 END-IF
      *
                 IF ((WASC-PAY-CODE = '1' OR '6') AND WASC-JOB-CODE =
                     'P' OR 'T' OR WS-PT-TRUCK = 'Y') AND SESSION-DATE
                     <= WCOVERAGE-START-DT
                    MOVE 'Y' TO WS-NEWLY-ELIGIBLE
                    MOVE WCOVERAGE-START-DT TO WS-WAIT-PERIOD-END
                    IF WPLAN-NAME = 'DEN'
                        MOVE WCOVERAGE-START-DT TO WS-DEN-COV-STRT-DT
                    END-IF
                    IF (WPLAN-NAME = 'STD' OR 'LTD') AND SESSION-DATE
                        <= WS-DEN-COV-STRT-DT
                        MOVE 'Y' TO WS-STD-LTD-NW-ELIG
                    END-IF
                 ELSE
                    IF WASC-PAY-CODE = '1' AND WASC-JOB-CODE = 'F' AND
                        SESSION-DATE <= WCOVERAGE-START-DT
                       MOVE 'Y' TO WS-NEWLY-ELIGIBLE
                    IF (WPLAN-NAME = 'STD' OR 'LTD') AND SESSION-DATE
                        <= WS-DIS-COV-STRT-DT
                        MOVE 'Y' TO WS-STD-LTD-NW-ELIG
                    END-IF
                       MOVE WCOVERAGE-START-DT TO WS-WAIT-PERIOD-END
                    ELSE
                       IF SESSION-DATE <= WMGMT-NEWLY-ELIGIBLE-DATE
                          MOVE 'Y' TO WS-NEWLY-ELIGIBLE
                       IF (WPLAN-NAME = 'STD' OR 'LTD')
                             MOVE 'Y' TO WS-STD-LTD-NW-ELIG
                       END-IF
                          MOVE SESSION-DATE TO WS-WAIT-PERIOD-END
                       ELSE
                          MOVE 'N' TO WS-NEWLY-ELIGIBLE
                          MOVE 'N' TO WS-STD-LTD-NW-ELIG
                       END-IF
                    END-IF
                 END-IF
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                    STRING 'NEWLY ELIG FLAG ' EZT-DISP-EZ-10-0 ' '
                        WS-NEWLY-ELIGIBLE ' ' WASC-PAY-CODE ' '
                        WASC-JOB-CODE ' ES ' SESSION-DATE ' PTB '
                        WPEAK-NEWLY-ELIGIBLE-DATE-STRT ' PTE '
                        WPEAK-NEWLY-ELIGIBLE-DATE-END ' MED '
                        WMGMT-NEWLY-ELIGIBLE-DATE ' HED '
                        WHRLY-NEWLY-ELIGIBLE-DATE DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
              ELSE
              IF SQLCODE = 100 OR WASC-PAY-CODE-IND = -1 OR
                  WASC-JOB-CODE-IND = -1 OR WSTATE-PROVINCE-CD-IND = -1


                    MOVE SQLCODE TO WS-SQL-DISPLAY
                    MOVE LOOKUP-PLAN-YEAR TO LOOKUP-PLAN-YEAR-N
                    MOVE
                        '***********************************************
      -                '*****' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                    STRING 'STATE-PROVINCE-CD IS NULL, SQLCODE = '
                        EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                        ' PLAN YEAR = ' EZT-DISP-EZ-5-0 ' PLAN NAME = '
                        DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '*****' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    IF SW-CURSOR2A-ACTIV = 'Y'
                       PERFORM 1-CLOSE-BENEFIT-DATAN
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
                    END-IF
                    PERFORM 1-CLOSE-BENEFIT-DATA
                    IF EZT-GOTO-STOP-JOB NOT = 'N'
                        GO TO SECTION-EXIT
                    END-IF
      *> GO TO JOB
                 MOVE 'J' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              ELSE
                    MOVE SQLCODE TO WS-SQL-DISPLAY
                    MOVE LOOKUP-PLAN-YEAR TO LOOKUP-PLAN-YEAR-N
                    MOVE
                        '***********************************************
      -                '*****' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                    STRING
                        'FAILED TO READ BENEFIT_ENROLLMENT_N, SQLCODE =
      -                '' EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                        ' PLAN YEAR = ' EZT-DISP-EZ-5-0 ' PLAN NAME = '
                        DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '*****' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                    MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
              END-IF
           .
       SECTION-EXIT.

       1-CLOSE-BENEFIT-DATAN SECTION.

              EXEC SQL CLOSE CURSOR2A
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE BENEFIT_ENROLLMENT_N, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              ELSE
                 MOVE 'N' TO SW-CURSOR2A-ACTIV
              END-IF
           .
       SECTION-EXIT.

      ***** REAL TIME UPDATE AE2019 CHANGES END *****

       1-GET-BENEFIT-DATA-PREV SECTION.
            MOVE DC-PLAN-NAME TO EZT-DC-PLAN-NAME
            EXEC SQL OPEN CURSOR3
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN BENEFIT_ENROLLMENT-2, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL FETCH CURSOR3
                INTO :WPLAN-YEAR-PREV,
                     :WPAYROLL-DATE-PREV
            END-EXEC

              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 COMPUTE LOOKUP-PLAN-YEAR-N = LOOKUP-PLAN-YEAR - 1
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO READ BENEFIT_ENROLLMENT-2, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                 STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                     ' PLAN YEAR = ' EZT-DISP-EZ-5-0 ' PLAN NAME = '
                     DC-PLAN-NAME DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF

              EXEC SQL CLOSE CURSOR3
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE BENEFIT_ENROLLMENT-2, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.

      *65 LABEN1 AE2017 CHANGES
       1-GET-PREV-REASON-SAME-DAY SECTION.
            MOVE DC-SESSION-TS TO EZT-DC-SESSION-TS
            EXEC SQL OPEN CURSOR7
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN ENROLLMENT SESSION  , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL FETCH CURSOR7
                INTO :WS-CHANGE-REASON-CODE,
                     :WS-EVENT-DATE-LD :WS-EVENT-DATE-LD-IND,
                     :WS-EVENT-TYPE-CODE
            END-EXEC

              IF SQLCODE NOT = 0 AND +100
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO READ ENROLLMENT SESSION  , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                     ' SESSION TS= ' DC-SESSION-TS DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
              IF SQLCODE = +100
                 MOVE ' ' TO WS-CHANGE-REASON-CODE
                 MOVE 0 TO WS-EVENT-TYPE-CODE
              END-IF

              EXEC SQL CLOSE CURSOR7
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE ENROLLMENT_SESSION  , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
      *65 LABEN1 AE2017 CHANGES END

       1-GET-PREV-REASON-HE SECTION.
            MOVE DC-SESSION-TS TO EZT-DC-SESSION-TS
            EXEC SQL OPEN CURSOR8
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     'ERROR IN GET-PREV-REASON-HE
      -             '  ' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN ENROLLMENT SESSION  , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL FETCH CURSOR8
                INTO :WS-CHANGE-REASON-CODE
                     :WS-EVENT-TYPE-CODE
            END-EXEC

              IF SQLCODE NOT = 0 AND +100
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     'ERROR IN GET-PREV-REASON-HE
      -             '  ' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO READ ENROLLMENT SESSION  , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                     ' SESSION TS= ' DC-SESSION-TS DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
              IF SQLCODE = +100
                 MOVE ' ' TO WS-CHANGE-REASON-CODE
                 MOVE 0 TO WS-EVENT-TYPE-CODE
              END-IF

              EXEC SQL CLOSE CURSOR8
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     'ERROR IN GET-PREV-REASON-HE
      -             '  ' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE ENROLLMETN_SESSION  , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.

       1-BUILD-UPDATE-YEAR-RECORD SECTION.
              MOVE X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' TO
                  INIT1
              MOVE X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' TO
                  INIT2
              MOVE X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' TO
                  INIT3
              MOVE X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' TO
                  INIT4
              MOVE X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' TO
                  INIT5
              MOVE X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' TO
                  INIT6
              MOVE X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' TO
                  INIT7
              MOVE X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' TO
                  INIT8
              MOVE X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' TO
                  INIT9
              MOVE X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' TO
                  INIT10

              MOVE 'N' TO SW-POST-STD-LTD
              MOVE 'N' TO WS-STP-NOCHANGE
      **AE2014-DP CHANGES BEGINS***
              MOVE 'N' TO WS-DP-FOUND-SW
      **AE2014-DP CHANGES ENDS ***
      **43-AE2014-DP-FIX CHANGES BEGINS **
              MOVE 'N' TO WS-SP-FOUND-SW
              MOVE 'N' TO WS-LEG-DEP-FOUND-SW
              MOVE 'N' TO WS-DP-DEP-FOUND-SW
      **43-AE2014-DP-FIX CHANGES ENDS** **

      *  BYPASS PLANS THAT HAVE NO CHANGE IN EITHER OPTION OR CATEGORY

      **25- ALLOW AE TO GO THROUGH FOR ORIG-DATE-CHANGES IF NEEDED
      * JLSMELT ADDED CODE TO LET STD GO ON THROUGH
      * DMODALI ADDED OUT-POCKET-MAX-CD AND UP-FRONT-ALLOW-CD
      *59 ADDED LOGIC TO BYPASS STD PLAN IF THE COVERAGE OPTIONS ARE EQU
      * *70* CHANGES TO BYPASS THE LAST CHANGE DATE UPDATE IF THE COVERA
      * *70* OPTIONS ARE SAME FOR MED FOR NO COVERAGE OPTION AND FOR THE
      * *70* PLAN AS WELL
              IF (DC-PLAN-NAME = 'MED' AND DC-OPTION-CODE =
                  WCOVERAGE-OPTION AND DC-OPTION-CODE = '1') OR
                  (DC-PLAN-NAME = 'DLI' AND DC-OPTION-CODE >
                  WCOVERAGE-OPTION AND DC-CATEGORY-CODE =
                  WCOVERAGE-CATEGORY AND WCOVERAGE-OPTION >= '2')
                 IF SW-CURSOR2A-ACTIV = 'Y'
                    PERFORM 1-CLOSE-BENEFIT-DATAN
                    IF EZT-GOTO-STOP-JOB NOT = 'N'
                        GO TO SECTION-EXIT
                    END-IF
                 END-IF
                 PERFORM 1-CLOSE-BENEFIT-DATA
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
      *> GO TO JOB
                 MOVE 'J' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
      * *70*  CHANGES ENDS

              IF ((DC-OPTION-CODE = WCOVERAGE-OPTION AND
                  DC-CATEGORY-CODE = WCOVERAGE-CATEGORY AND
                  DC-INS-PROV-ID = WINS-PROVIDER-ID AND
                  DC-INS-BROKER-ID = WINS-BROKER-ID AND
                  DC-OUT-POCKET-MAX-CD = WOUT-POCKET-MAX-CD AND
                  DC-UP-FRONT-ALLOW-CD = WUP-FRONT-ALLOW-CD) OR
                  (DC-OPTION-CODE = '1' AND WCOVERAGE-OPTION = '4' AND
                  DC-PLAN-NAME = 'STD'))
      * JSTST
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-INS-PROV-ID TO EZT-DISP-EZ-5-0
                    MOVE WINS-PROVIDER-ID TO EZT-DISP-EZ-5-0-1
                    MOVE LOOKUP-PLAN-YEAR TO EZT-DISP-EZ-5-0-2
                    MOVE DC-PLAN-YEAR TO EZT-DISP-EZ-5-0-3
                    STRING 'NO CHANGE, BYPASSING ' WS-ENROLLEE-SSN ' '
                        DC-PLAN-NAME ' DCO ' DC-OPTION-CODE ' WCO '
                        WCOVERAGE-OPTION ' DCC ' DC-CATEGORY-CODE
                        ' WCC ' WCOVERAGE-CATEGORY ' DCC '
                        EZT-DISP-EZ-5-0 ' WCC ' EZT-DISP-EZ-5-0-1
                        ' PLAN YEAR ' EZT-DISP-EZ-5-0-2 ' DC PLAN YR '
                        EZT-DISP-EZ-5-0-3 DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                   MOVE SPACES TO EZT-DISPLAY-STRING
                   STRING ' DC OPCT MX ' DC-OUT-POCKET-MAX-CD
                       ' WC OPCT MX ' WOUT-POCKET-MAX-CD ' DC UFRT AL '
                       DC-UP-FRONT-ALLOW-CD ' WC UFRT AL '
                       WUP-FRONT-ALLOW-CD DELIMITED BY SIZE
                       INTO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
                 END-IF
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'CLOSE-BENEFIT-DATE TWO' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF SW-CURSOR2A-ACTIV = 'Y'
                    PERFORM 1-CLOSE-BENEFIT-DATAN
                    IF EZT-GOTO-STOP-JOB NOT = 'N'
                        GO TO SECTION-EXIT
                    END-IF
                 END-IF
                 PERFORM 1-CLOSE-BENEFIT-DATA
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
      *> GO TO JOB
                 MOVE 'J' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF

      *  FORMAT BASIC UPDATE RECORD FOR BENEFIT ENROLLMENT

              MOVE WSSN TO SSN-O
              MOVE WPLAN-YEAR TO PLAN-YEAR-O
              MOVE WPLAN-NAME TO PLAN-NAME-O
              MOVE WPAYROLL-DATE TO PAYROLL-DATE-O
              MOVE WHISTORY-TYPE-CD TO HISTORY-TYPE-O
              MOVE DC-OPTION-CODE TO COV-OPTION-O
              MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O
              MOVE C-URRENT-DATE TO ENROLL-RCPT-DATE-O
              MOVE C-URRENT-DATE TO LAST-CHANGE-DATE-O
              PERFORM 1-SET-LAST-CHANGE-ID
              IF EZT-GOTO-STOP-JOB NOT = 'N'
                  GO TO SECTION-EXIT
              END-IF
                 MOVE DC-INS-PROV-ID TO PROVIDER-ID-O
              MOVE DC-CHANGE-REASON-CODE TO CHANGE-REASON-CD-O
              MOVE DC-OUT-POCKET-MAX-CD TO OUT-POCKET-MAX-CD
              MOVE DC-UP-FRONT-ALLOW-CD TO UP-FRONT-ALLOW-CD
              MOVE ' ' TO COPY-TO-PAYROLL-O

              IF SW-DEBUG-SWITCH = 'Y'
                 MOVE '***********************************************'
                     TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'DC-CHANGE-REASON-CODE    '
                     DC-CHANGE-REASON-CODE DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 STRING 'DC-ENROLLEE-SSN          ' EZT-DISP-EZ-10-0
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-PLAN-YEAR TO EZT-DISP-EZ-5-0-3
                 STRING 'DC-PLAN-YEAR             ' EZT-DISP-EZ-5-0-3
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'DC-PLAN-NAME             ' DC-PLAN-NAME
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'DC-OPTION-CODE           ' DC-OPTION-CODE
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'WCOVERAGE-OPTION         ' WCOVERAGE-OPTION
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'DC-CATEGORY-CODE         ' DC-CATEGORY-CODE
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'WCOVERAGE-CATEGORY       ' WCOVERAGE-CATEGORY
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'DC-CHANGE-EVENT-DATE     '
                     DC-CHANGE-EVENT-DATE DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'DC-CHANGE-EVENT-DATE     '
                     DC-CHANGE-EVENT-DATE DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'DC-SESSION-TS            ' DC-SESSION-TS
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-INS-PROV-ID TO EZT-DISP-EZ-5-0
                 STRING 'DC-INS-PROV-ID           ' EZT-DISP-EZ-5-0
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WINS-PROVIDER-ID TO EZT-DISP-EZ-5-0-1
                 STRING 'WINS-PROVIDER-ID         ' EZT-DISP-EZ-5-0-1
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'PAY-PERIOD-BEGIN         ' PAY-PERIOD-BEGIN
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'NEXT-PAY-PERIOD-BEGIN    '
                     NEXT-PAY-PERIOD-BEGIN DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE '***********************************************'
                     TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              END-IF
              IF PLAN-NAME-O = 'ALI'
                 IF CHANGE-REASON-CD-O = 'NE'
                    MOVE DC-OPTION-CODE TO WS-COVERAGE-OPT-CMP-ONE
                    MOVE WCOVERAGE-OPTION TO WS-COVERAGE-OPT-CMP-TWO
                    MOVE 'N' TO WS-COVERAGE-OPT-ONE-INCREASED
                    MOVE 'N' TO WS-COVERAGE-OPT-ONE-DECREASED
                    MOVE 'N' TO WS-COVERAGE-OPT-ONE-EQUAL
                    PERFORM 1-COMPARE-COVERAGE-OPTIONS
                    IF WS-COVERAGE-OPT-ONE-INCREASED = 'Y'
                       IF WCOVERAGE-OPTION = '1'
                          MOVE '2' TO COV-OPTION-O
                          MOVE 'Y' TO WS-NEW-PICKUP-IND
                          MOVE 'N' TO WS-OPT-CAT-DECREASE
                          PERFORM 1-POPULATE-COV-START-DT
                       ELSE
                          IF SW-CURSOR2A-ACTIV = 'Y'
                             PERFORM 1-CLOSE-BENEFIT-DATAN
                             IF EZT-GOTO-STOP-JOB NOT = 'N'
                                 GO TO SECTION-EXIT
                             END-IF
                          END-IF
                          PERFORM 1-CLOSE-BENEFIT-DATA
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
      *> GO TO JOB
                          MOVE 'J' TO EZT-GOTO-STOP-JOB
                          GO TO SECTION-EXIT
                       END-IF
                    ELSE
                       IF WS-COVERAGE-OPT-ONE-DECREASED = 'Y' AND
                           SESSION-DATE >= WCOVERAGE-START-DT
                          MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                       END-IF
                    END-IF
                 END-IF
                 IF CHANGE-REASON-CD-O NOT = 'NE'
      ****AE2011-FIX TO WRITE 2011 AE RECORD
                    IF PLAN-NAME-PREV = 'ALI' AND SSN-O = SSN-PREV AND
                        CHANGE-REASON-CD-O = 'AE' AND
                        CHANGE-REASON-CD-PREV = 'OE' AND
                        (COV-OPTION-PREV > '1' OR COV-OPT-PREV
                        ALPHABETIC)
      ***AE2014 CHANGES BEGINS***
                          MOVE DC-OPTION-CODE TO
                              WS-COVERAGE-OPT-CMP-ONE
                          MOVE COV-OPTION-PREV TO
                              WS-COVERAGE-OPT-CMP-TWO
                          MOVE 'N' TO WS-COVERAGE-OPT-ONE-INCREASED
                          MOVE 'N' TO WS-COVERAGE-OPT-ONE-DECREASED
                          MOVE 'N' TO WS-COVERAGE-OPT-ONE-EQUAL
                          PERFORM 1-COMPARE-COVERAGE-OPTIONS
                          IF WS-COVERAGE-OPT-ONE-INCREASED = 'Y' OR
                              WS-COVERAGE-OPT-ONE-EQUAL = 'Y'
      *           IF DC-OPTION-CODE >= COV-OPTION-PREV
      **AE2014 CHANGES ENDS**
                             MOVE COV-OPTION-PREV TO COV-OPTION-O
                             MOVE COV-START-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                          END-IF
                    ELSE
                      MOVE COV-OPTION-O TO WS-COVERAGE-OPT-CMP-ONE
                      MOVE WCOVERAGE-OPTION TO WS-COVERAGE-OPT-CMP-TWO
                      MOVE 'N' TO WS-COVERAGE-OPT-ONE-INCREASED
                      MOVE 'N' TO WS-COVERAGE-OPT-ONE-DECREASED
                      MOVE 'N' TO WS-COVERAGE-OPT-ONE-EQUAL
                      PERFORM 1-COMPARE-COVERAGE-OPTIONS
                      IF WS-COVERAGE-OPT-ONE-INCREASED = 'Y'
      **AE2016 CHANGES BEGIN
      *62
                         IF (((WASC-PAY-CODE = '1' OR '3' OR '6') AND
                             WASC-JOB-CODE = 'T' OR 'P') OR WS-PT-TRUCK
                             = 'Y') AND DC-PLAN-NAME = 'ALI' AND
                             WS-SESSION-DATE-YY = 2016 AND
                             DC-CHANGE-REASON-CODE = 'AE'
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'ZERO-5 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
      **AE2016 CHANGES END
                         ELSE
                             MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                             IF SW-CURSOR2A-ACTIV = 'Y'
                             PERFORM 1-CLOSE-BENEFIT-DATAN
                             IF EZT-GOTO-STOP-JOB NOT = 'N'
                                 GO TO SECTION-EXIT
                             END-IF
                             END-IF
                             PERFORM 1-CLOSE-BENEFIT-DATA
                             IF EZT-GOTO-STOP-JOB NOT = 'N'
                                 GO TO SECTION-EXIT
                             END-IF
      *> GO TO JOB
                             MOVE 'J' TO EZT-GOTO-STOP-JOB
                             GO TO SECTION-EXIT
                         END-IF
                      ELSE
                        IF WS-COVERAGE-OPT-ONE-DECREASED = 'Y'
                          MOVE 'N' TO WS-NEW-PICKUP-IND
                          MOVE 'Y' TO WS-OPT-CAT-DECREASE
                          PERFORM 1-POPULATE-COV-START-DT
                        END-IF
                      END-IF
                    END-IF
      ****AE2011-FIX TO WRITE 2011 AE RECORD
                 END-IF
              IF COV-OPTION-O = WCOVERAGE-OPTION
                 IF SW-CURSOR2A-ACTIV = 'Y'
                    PERFORM 1-CLOSE-BENEFIT-DATAN
                    IF EZT-GOTO-STOP-JOB NOT = 'N'
                        GO TO SECTION-EXIT
                    END-IF
                 END-IF
                 PERFORM 1-CLOSE-BENEFIT-DATA
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
      *> GO TO JOB
                 MOVE 'J' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
              END-IF
      ***AE2014-DP CHANGES BEGINS****
              IF (PLAN-NAME-O = 'DLI' AND DC-SESSION-DATE >=
                  WS-ENR-START-DT AND DC-SESSION-DATE <= WS-ENR-END-DT)

                   MOVE 'N' TO WS-DP-FOUND-SW
      **43-AE2014-DP-FIX CHANGES BEGINS **
                   MOVE 'N' TO WS-SP-FOUND-SW
                   MOVE 'N' TO WS-LEG-DEP-FOUND-SW
                   MOVE 'N' TO WS-DP-DEP-FOUND-SW
      **43-AE2014-DP-FIX CHANGES ENDS** **
                   PERFORM 1-GET-RELATION-TYPE-CD-CURSOR
      ***DP*FIX******
                   IF WS-DP-FOUND-SW = 'N'
                   PERFORM 1-GET-RELTN-TYPE-CD-CRSR-CHNGE
                   END-IF
      **43-AE2014-DP-FIX CHANGES BEGINS **
                   IF WS-DP-FOUND-SW = 'Y' AND WS-SP-FOUND-SW = 'Y'
                      MOVE 'N' TO WS-SP-FOUND-SW
                   END-IF
      **43-AE2014-DP-FIX CHANGES ENDS** **
                   IF WS-SP-FOUND-SW = 'Y'
                      PERFORM 1-GET-WIN-NBR
                      PERFORM 1-CHECK-IF-SPOUSE-IS-DP
                   END-IF
              END-IF

      ***AE2014-DP CHANGES ENDS****
      ********** JLS ADDED 'MA' TO PICK UP MARRIAGE FSC
      **********     TO CORRECT HOST BROKEN ISSUE.  CHANGE 19
      ****AE2011-CHANGE BEGIN
              IF (PLAN-NAME-O = 'DLI' AND (CHANGE-REASON-CD-O = 'AE' OR
                  'NE' OR 'MA') AND (DC-ENROLL-TYPE-CODE = 4 OR 34 OR
                  30))
      ****AE2011-CHANGE END
                  IF SW-DEBUG-SWITCH = 'Y'
                     MOVE 'TWO' TO EZT-DISPLAY-STRING
                     WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                         AFTER ADVANCING 1 LINE
                  END-IF
      ****AE2011-FIX TO INCLUDE WCOVERAGE-CATEGORY = 'A'
                  IF COV-OPTION-O >= '2' AND WCOVERAGE-OPTION = '1' AND
                      WCOVERAGE-CATEGORY = 'A'
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE 'TWO-1' TO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
                     IF CHANGE-REASON-CD-O = 'AE'
                        MOVE '1' TO COV-OPTION-O
                     ELSE
                        MOVE '2' TO COV-OPTION-O
                     END-IF
                     MOVE 'Y' TO WS-NEW-PICKUP-IND
                     MOVE 'N' TO WS-OPT-CAT-DECREASE
                     PERFORM 1-POPULATE-COV-START-DT
      **AE2014-DP CHANGES BEGINS**
                        IF WS-DP-FOUND-SW = 'Y'
                             MOVE '2' TO COV-OPTION-O
                        END-IF
      **AE2014-DP CHANGES ENDS **
                  ELSE
                     IF COV-OPTION-O > WCOVERAGE-OPTION OR COV-OPTION-O
                         = X'00'
                        IF SW-DEBUG-SWITCH = 'Y'
                           MOVE 'TWO-2' TO EZT-DISPLAY-STRING
                           WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                               AFTER ADVANCING 1 LINE
                        END-IF
                        MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                        MOVE 'N' TO WS-NEW-PICKUP-IND
                        MOVE 'N' TO WS-OPT-CAT-DECREASE
                     PERFORM 1-POPULATE-COV-START-DT
      **AE2014-DP CHANGES BEGINS**
                        IF WS-DP-FOUND-SW = 'Y' AND WCOVERAGE-OPTION =
                            '1'
                             MOVE '2' TO COV-OPTION-O
                        END-IF
      **AE2014-DP CHANGES ENDS **
                     END-IF
                  END-IF
                  IF COV-OPTION-O < WCOVERAGE-OPTION
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE 'TWO-3' TO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
                     MOVE 'N' TO WS-NEW-PICKUP-IND
                     MOVE 'Y' TO WS-OPT-CAT-DECREASE
                     PERFORM 1-POPULATE-COV-START-DT
                     MOVE COV-OPTION-O TO COV-OPTION-O
                  END-IF
                  IF COV-CATEGORY-O = X'00'
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE 'TWO-4' TO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
                     MOVE WCOVERAGE-CATEGORY TO COV-CATEGORY-O
                  END-IF
      *   01
                  IF COV-CATEGORY-O = ' '
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE 'TWO-5' TO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
      *   01
                     MOVE WCOVERAGE-CATEGORY TO COV-CATEGORY-O
      *   01
                  END-IF
              END-IF

      *  CHECK DLI FOR SPOUSE COVERAGE CUTBACKS

      * NOT NEWLY ELIGIBLE DLI
              IF PLAN-NAME-O = 'DLI' AND (CHANGE-REASON-CD-O = 'AE' OR
                  'LD') AND WS-NEWLY-ELIGIBLE = 'N' AND
                  DC-ENROLL-TYPE-CODE NOT = 30
                  IF SW-DEBUG-SWITCH = 'Y'
                     MOVE 'TWO-FIVE' TO EZT-DISPLAY-STRING
                     WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                         AFTER ADVANCING 1 LINE
                  END-IF
                  IF DC-OPTION-CODE > WCOVERAGE-OPTION
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE 'TWO-FIVE-1' TO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
      **AE2014-DP CHANGES BEGINS **
                     IF COV-CATEGORY-O = WCOVERAGE-CATEGORY AND
                         WS-DP-FOUND-SW = 'N'
                         CONTINUE
      **AE2014-DP CHANGES ENDS ***
      *66 **AE2017-LUSUL1-CHANGES BEGINS ***
      **           PERFORM CLOSE-BENEFIT-DATA
      **           GO TO JOB
      *66 **AE2017-LUSUL1-CHANGES ENDS ***
                     ELSE
                        MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                     END-IF
      **AE2014-DP CHANGES BEGINS**
                     IF WS-DP-FOUND-SW = 'Y' AND WCOVERAGE-OPTION = '1'

                         MOVE '2' TO COV-OPTION-O
                     END-IF
      **AE2014-DP CHANGES ENDS **
                  END-IF
                  IF COV-CATEGORY-O > WCOVERAGE-CATEGORY
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE 'TWO-FIVE-1' TO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
                        IF WCOVERAGE-OPTION = '1' AND
                            WCOVERAGE-CATEGORY = 'A'
                             MOVE SESSION-DATE TO COV-START-DATE-O
                             MOVE SESSION-DATE TO ORIG-COV-DATE-O
                        ELSE
                           MOVE SESSION-DATE TO COV-START-DATE-O
                        END-IF
                  ELSE
                        MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                  END-IF
              END-IF
      **26- IF AE AND PREV REC NOT AE AND SAME SSN ALLOW <= COVERAGE
              IF PLAN-NAME-O = 'DLI' AND PLAN-NAME-PREV = 'DLI' AND
                  SSN-O = SSN-PREV AND CHANGE-REASON-CD-O = 'AE' AND
                  CHANGE-REASON-CD-PREV = 'MA' AND COV-OPTION-PREV >
                  '1'
                  IF SW-DEBUG-SWITCH = 'Y'
                     MOVE 'ONE-THREE-ONE' TO EZT-DISPLAY-STRING
                     WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                         AFTER ADVANCING 1 LINE
                  END-IF
                  IF DC-OPTION-CODE > COV-OPTION-PREV
                     IF SW-DEBUG-SWITCH = 'Y'
                        MOVE 'ONE-THREE-12' TO EZT-DISPLAY-STRING
                        WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                            AFTER ADVANCING 1 LINE
                     END-IF
                     MOVE COV-OPTION-PREV TO COV-OPTION-O
                  END-IF
              END-IF
      **26- END CHANGES


      *  SET SPECIFIC DATES DEPENDING ON CONDITIONS

      *  PRORATED PLANS BASED ON FIRST OF NEXT MONTH
      *  CHANGE 14 ADDED 'GS' AND 'LE'
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODE 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'DI' OR 'MA' OR
                  'GS' OR 'LE' OR 'DS' OR 'LA' OR 'RW' OR 'TC') AND
                  (DC-PLAN-NAME = 'MED' OR 'DEN' OR 'ADD' OR 'VIS')
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'FOUR' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-OPTION-CODE NOT = WCOVERAGE-OPTION OR
                     DC-CATEGORY-CODE NOT = WCOVERAGE-CATEGORY
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'FOUR-1' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF WCOVERAGE-OPTION = '1'
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'FOUR-2' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                          MOVE DC-CHANGE-EVENT-DATE TO
                              COV-START-DATE-O
                          MOVE DC-CHANGE-EVENT-DATE TO ORIG-COV-DATE-O
                    ELSE
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'FOUR-3' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                          MOVE DC-CHANGE-EVENT-DATE TO
                              COV-START-DATE-O
                    END-IF
                 END-IF
              END-IF


      ****************************************************
      *   CHANGE 35  - CHG
      *       CHANGE-REASON-CD-O = 'BI' 'SE' AND      +
      ****************************************************
      *  MED DEN AND ADD FOR FAMILY STATUS CHANGE BI AND SE
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'BI' OR 'AP' OR
                  'SC' OR 'SE' OR 'FP') AND (DC-PLAN-NAME = 'MED' OR
                  'DEN' OR 'ADD' OR 'VIS')
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'FIVE' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-OPTION-CODE NOT = WCOVERAGE-OPTION OR
                     DC-CATEGORY-CODE NOT = WCOVERAGE-CATEGORY
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'FIVE-1' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF WCOVERAGE-OPTION = '1'
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'FIVE-2' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE DC-CHANGE-EVENT-DATE TO COV-START-DATE-O
                       MOVE DC-CHANGE-EVENT-DATE TO ORIG-COV-DATE-O
                    ELSE
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'FIVE-3' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE DC-CHANGE-EVENT-DATE TO COV-START-DATE-O
                    END-IF
                 END-IF
              END-IF

      *
      *  DIVORCE CONDITIONS FOR LIFE AND DISABILITY DECREASES FOR DLI
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODE 'DS'
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'DI' OR 'DS')
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'SEVEN' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-PLAN-NAME = 'DLI'
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'SEVEN-1' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE < WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'SEVEN-2' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       PERFORM 1-CREATE-TWO-DLI-COV-RECORDS
                    END-IF
                    IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'SEVEN-5' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE DC-CHANGE-EVENT-DATE TO COV-START-DATE-O
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE < WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'SEVEN-8' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'SEVEN-11' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'CLOSE-BENEFIT-DATE THREE' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF
                 END-IF
              END-IF

      *  DIVORCE CONDITIONS FOR LIFE AND DISABILITY INCREASES FOR DLI
      *  FOR HOURLY AND NON NEWLY ELIGIBLE MANAGEMENT
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODE 'DS'
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'DI' OR 'DS')
                  AND ((WASC-PAY-CODE = '1' AND (WASC-JOB-CODE = 'F' OR
                  'T' OR 'P')) OR (WASC-PAY-CODE = '6' AND
                  WS-NEWLY-ELIGIBLE = 'N') OR (WASC-PAY-CODE = '1' AND
                  (WASC-JOB-CODE NOT = 'F' AND 'T' AND 'P') AND
                  WS-NEWLY-ELIGIBLE = 'N'))
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'NINE ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-PLAN-NAME = 'DLI'
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'NINE-1' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'NINE-2' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'CLOSE-BENEFIT-DATE FIVE' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF
                    IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'NINE-3' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                          IF WCOVERAGE-OPTION = '1' AND
                              WCOVERAGE-CATEGORY = 'A'
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'NINE-5' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             PERFORM 1-CREATE-TWO-DLI-COV-RECORDS
                          ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'NINE-6' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             PERFORM 1-CREATE-TWO-DLI-COV-RECORDS
                          END-IF
                    END-IF
                    IF DC-OPTION-CODE >= WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'NINE-9' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO DC-OPTION-CODE
                          IF WCOVERAGE-OPTION = '1' AND
                              WCOVERAGE-CATEGORY = 'A'
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'NINE-11' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE SESSION-DATE TO COV-START-DATE-O
                             MOVE SESSION-DATE TO ORIG-COV-DATE-O
                          ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'NINE-12' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE SESSION-DATE TO COV-START-DATE-O
                             MOVE 'N' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
                          END-IF
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE < WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'NINE-15' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'NINE-18' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
      *                COV-START-DATE-O = PAY-PERIOD-BEGIN
                             MOVE 'N' TO WS-NEW-PICKUP-IND
                             MOVE 'Y' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
      *             END-IF
                    END-IF
                 END-IF
              END-IF

      *  DIVORCE CONDITIONS FOR LIFE AND DISABILITY INCREASES FOR DLI
      *  FOR NEWLY ELIGIBLE MANAGEMENT
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODE 'DS'
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'DI' OR 'DS')
                  AND ((WASC-PAY-CODE = '6' AND WS-NEWLY-ELIGIBLE =
                  'Y') OR (WASC-PAY-CODE = '1' AND (WASC-JOB-CODE NOT =
                  'F' AND 'T' AND 'P') AND WS-NEWLY-ELIGIBLE = 'Y'))
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'NINE ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-PLAN-NAME = 'DLI'
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'THIRTEEN-1A' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-2A' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'CLOSE-BENEFIT-DATE SIX' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-4A' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF WCOVERAGE-OPTION = '1' AND WCOVERAGE-CATEGORY
                           = 'A'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-5A' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                             MOVE SESSION-DATE TO COV-START-DATE-O
                             MOVE SESSION-DATE TO ORIG-COV-DATE-O
                       ELSE
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-6A' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                             MOVE SESSION-DATE TO COV-START-DATE-O
                       END-IF
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-7A' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                       IF WCOVERAGE-OPTION = '1' AND WCOVERAGE-CATEGORY
                           = 'A'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-8A' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          MOVE SESSION-DATE TO COV-START-DATE-O
                          MOVE SESSION-DATE TO ORIG-COV-DATE-O
                       ELSE
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-9A' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                             MOVE SESSION-DATE TO COV-START-DATE-O
                       END-IF
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE < WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-10A' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                          MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                    END-IF
                    IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-11A' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                      PERFORM 1-CREATE-TWO-DLI-COV-RECORDS
                    END-IF
                 END-IF
              END-IF


      *  MARRIAGE OR BIRTH LIFE AND DISABILITY DECREASES  DLI
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'MA' OR 'BI' OR
                  'AP' OR 'SC')
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'ELEVEN' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-PLAN-NAME = 'DLI'
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'ELEVEN-1' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE < WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'ELEVEN-2' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE < WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'ELEVEN-3' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                    END-IF
                    IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'ELEVEN-4' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                    END-IF
                 END-IF
              END-IF

      *  MARRIAGE OR BIRTH LIFE AND DISABILITY INCREASES EXCEPT DLI
      *  FOR ALL HOURLY AND NOT NEWLY ELIGIBLE MANAGEMENT
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'MA' OR 'BI' OR
                  'AP' OR 'SC') AND ((WASC-PAY-CODE = '1' AND
                  WASC-JOB-CODE = 'F' OR 'T' OR 'P') OR (WASC-PAY-CODE
                  = '6' AND WS-NEWLY-ELIGIBLE = 'N') OR (WASC-PAY-CODE
                  = '1' AND (WASC-JOB-CODE NOT = 'F' AND 'T' AND 'P')
                  AND WS-NEWLY-ELIGIBLE = 'N'))
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'TWELVE  ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
              END-IF

      *  BIRTH INCREASES FOR DLI FOR ALL HOURLY AND NOT NEWLY ELIGIBLE
      *  MANAGEMENT
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'BI' OR 'AP' OR
                  'SC') AND ((WASC-PAY-CODE = '1' AND WASC-JOB-CODE =
                  'F' OR 'T' OR 'P') OR (WASC-PAY-CODE = '6' AND
                  WS-NEWLY-ELIGIBLE = 'N') OR (WASC-PAY-CODE = '1' AND
                  (WASC-JOB-CODE NOT = 'F' AND 'T' AND 'P') AND
                  WS-NEWLY-ELIGIBLE = 'N'))
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'THIRTEEN' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-PLAN-NAME = 'DLI'
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'THIRTEEN-1' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-2' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'CLOSE-BENEFIT-DATE EIGHT' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-3' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'CLOSE-BENEFIT-DATE NINE' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-4' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF WCOVERAGE-OPTION = '1' AND WCOVERAGE-CATEGORY
                           = 'A'
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-5' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE 'Y' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
                       ELSE
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-6' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                             MOVE 'N' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
                       END-IF
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-7' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                       IF WCOVERAGE-OPTION = '1' AND WCOVERAGE-CATEGORY
                           = 'A'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-8' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          MOVE 'Y' TO WS-NEW-PICKUP-IND
                          MOVE 'N' TO WS-OPT-CAT-DECREASE
                          PERFORM 1-POPULATE-COV-START-DT
                       ELSE
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-9' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          MOVE 'N' TO WS-NEW-PICKUP-IND
                          MOVE 'N' TO WS-OPT-CAT-DECREASE
                          PERFORM 1-POPULATE-COV-START-DT
                       END-IF
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE < WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-10' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                    END-IF
                    IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-11' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       PERFORM 1-CREATE-TWO-DLI-COV-RECORDS
                    END-IF
                 END-IF
              END-IF
      *  LE & GS INCREASES FOR ALI AND DLI FOR ALL HOURLY AND MANAGEMENT
      *  ARE REQUIRED TO HAVE PROOF OF GOOD HEALTH IF NOT NEWLY ELIGIBLE
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'LE' OR 'GS' OR
                  'LA' OR 'RW' OR 'TC') AND WS-NEWLY-ELIGIBLE = 'N'
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'THIRTEEN-LEGS' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-PLAN-NAME = 'DLI'
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'THIRTEEN-1-DLI-LEGS-NOT NE' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
      *****  NO CHANGE
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-2-DLI-LEGS-NOT NE' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE
                              'CLOSE-BENEFIT-DATA THIRTEEN-2-DLI-LEGS-NO
      -                      'T NE' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF
      *****  NO CHANGE IN CATEGORY DECREASE IN OPTION ADDED 02-13-2003
                    IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-2A-DLI-LEGS-NOT NE' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE DC-OPTION-CODE TO COV-OPTION-O
      *           COV-START-DATE-O = DC-CHANGE-EVENT-DATE
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                    END-IF
      *****  INCREASE OPTION NOT CATEGORY = NO CHANGE ALLOWED
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       MOVE '1' TO WCOVERAGE-OPTION
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-3-DLI-LEGS-NOT NE' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE
                              'CLOSE-BENEFIT-DATA THIRTEEN-3-DLI-LEGS-NO
      -                      'T NE' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF
      *****  INCREASE   IN OPTION AND CHANGE IN CATEGORY
      *****      CATEGORY ALLOWED, OPTION DEPENDENT ON CURRENT
                    IF DC-CATEGORY-CODE > WCOVERAGE-CATEGORY AND
                        DC-OPTION-CODE > WCOVERAGE-OPTION
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-4-DLI-LEGS-NOT NE' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                       MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O
      *         ***IF FIRST TIME COVERAGE SET BOTH DATES
                        IF WCOVERAGE-CATEGORY = 'A' AND
                            WCOVERAGE-OPTION = '1'
                            MOVE 'Y' TO WS-NEW-PICKUP-IND
                            MOVE 'N' TO WS-OPT-CAT-DECREASE
                            PERFORM 1-POPULATE-COV-START-DT
                        ELSE
                            MOVE 'N' TO WS-NEW-PICKUP-IND
                            MOVE 'N' TO WS-OPT-CAT-DECREASE
                            PERFORM 1-POPULATE-COV-START-DT

                        END-IF
                    END-IF
                    IF DC-CATEGORY-CODE < WCOVERAGE-CATEGORY AND
                        DC-OPTION-CODE > WCOVERAGE-OPTION
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-4-DLI-LEGS-NOT NE' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                       MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                     END-IF
      *****  NO CHG OR DECREASE IN OPT AND CHG IN CAT
      *****      CATEGORY AND OPTION ALLOWED
                    IF DC-CATEGORY-CODE < WCOVERAGE-CATEGORY AND
                        DC-OPTION-CODE <= WCOVERAGE-OPTION
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-4-DLI-LEGS-NOT NE' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE DC-OPTION-CODE TO COV-OPTION-O
                       MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O
      *         ***IF FIRST TIME COVERAGE SET BOTH DATES
                        MOVE 'N' TO WS-NEW-PICKUP-IND
                        MOVE 'Y' TO WS-OPT-CAT-DECREASE
                        PERFORM 1-POPULATE-COV-START-DT
                     END-IF
                     IF DC-CATEGORY-CODE > WCOVERAGE-CATEGORY AND
                         DC-OPTION-CODE = WCOVERAGE-OPTION
                        IF WCOVERAGE-CATEGORY = 'A' AND
                            WCOVERAGE-OPTION = '1'
                           MOVE NEXT-PAY-PERIOD-BEGIN TO
                               COV-START-DATE-O
                           MOVE 'Y' TO WS-NEW-PICKUP-IND
                           MOVE 'N' TO WS-OPT-CAT-DECREASE
                           PERFORM 1-POPULATE-COV-START-DT
                        ELSE
                           MOVE 'N' TO WS-NEW-PICKUP-IND
                           MOVE 'N' TO WS-OPT-CAT-DECREASE
                           PERFORM 1-POPULATE-COV-START-DT
                        END-IF
                     END-IF
                     IF DC-CATEGORY-CODE > WCOVERAGE-CATEGORY AND
                         DC-OPTION-CODE < WCOVERAGE-OPTION
                        PERFORM 1-CREATE-TWO-DLI-COV-RECORDS
                     END-IF
                 END-IF
              END-IF
      *  LE & GS INCREASES FOR ALI AND DLI FOR ALL HOURLY AND MANAGEMENT
      *  THAT ARE NEWLY ELIGIBLE CAN HAVE 25000 ALI, 5000 SPOUSE DLI.
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'LE' OR 'GS' OR
                  'LA' OR 'RW' OR 'TC') AND WS-NEWLY-ELIGIBLE = 'Y'
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'THIRTEEN-LEGS-NEWLY-ELIG' TO
                        EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-PLAN-NAME = 'DLI'
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'THIRTEEN-1-DLI-LEGS' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF DC-OPTION-CODE > '1' AND WCOVERAGE-OPTION = '1'
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-2-DLI-LEGS' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        WCOVERAGE-OPTION > '1'
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-3-DLI-LEGS' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                    END-IF
      ****AE2011-CHANGE BEGINS
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF

                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       MOVE '1' TO WCOVERAGE-OPTION
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF

                    IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       MOVE DC-OPTION-CODE TO COV-OPTION-O
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                    END-IF

                    IF DC-CATEGORY-CODE > WCOVERAGE-CATEGORY AND
                        DC-OPTION-CODE > WCOVERAGE-OPTION
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                       MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O
                       IF WCOVERAGE-CATEGORY = 'A' AND WCOVERAGE-OPTION
                           = '1'
                          MOVE 'Y' TO WS-NEW-PICKUP-IND
                          MOVE 'N' TO WS-OPT-CAT-DECREASE
                          PERFORM 1-POPULATE-COV-START-DT
                       ELSE
                          MOVE 'N' TO WS-NEW-PICKUP-IND
                          MOVE 'N' TO WS-OPT-CAT-DECREASE
                          PERFORM 1-POPULATE-COV-START-DT
                       END-IF
                    END-IF

                    IF DC-CATEGORY-CODE < WCOVERAGE-CATEGORY AND
                        DC-OPTION-CODE > WCOVERAGE-OPTION
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                       MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                    END-IF

                    IF DC-CATEGORY-CODE < WCOVERAGE-CATEGORY AND
                        DC-OPTION-CODE <= WCOVERAGE-OPTION
                       MOVE DC-OPTION-CODE TO COV-OPTION-O
                       MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                    END-IF

                    IF DC-CATEGORY-CODE > WCOVERAGE-CATEGORY AND
                        DC-OPTION-CODE <= WCOVERAGE-OPTION
                       PERFORM 1-CREATE-TWO-DLI-COV-RECORDS
                    END-IF
      ****AE2011-CHANGE ENDS
                 END-IF
              END-IF
      *  BIRTH INCREASES FOR DLI FOR NEWLY ELIGIBLE MANAGEMENT
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND (CHANGE-REASON-CD-O = 'BI' OR 'AP' OR
                  'SC') AND ((WASC-PAY-CODE = '6' AND WS-NEWLY-ELIGIBLE
                  = 'Y') OR (WASC-PAY-CODE = '1' AND (WASC-JOB-CODE
                  NOT = 'F' AND 'T' AND 'P') AND WS-NEWLY-ELIGIBLE =
                  'Y'))
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'THIRTEEN-A' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-PLAN-NAME = 'DLI'
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'THIRTEEN-1A' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-2A' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'CLOSE-BENEFIT-DATE TEN' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-4A' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF WCOVERAGE-OPTION = '1' AND WCOVERAGE-CATEGORY
                           = 'A'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-5A' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
      ****AE2011-CHANGE BEGIN
      *                 COV-START-DATE-O = DC-CHANGE-EVENT-DATE
      *                 ORIG-COV-DATE-O  = DC-CHANGE-EVENT-DATE
                             MOVE 'Y' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
      ****AE2011-CHANGE END
                       ELSE
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-6A' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
      ****AE2011-CHANGE BEGIN
      *                 COV-START-DATE-O = DC-CHANGE-EVENT-DATE
                             MOVE 'N' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
      ****AE2011-CHANGE END
                       END-IF
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-7A' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                       IF WCOVERAGE-OPTION = '1' AND WCOVERAGE-CATEGORY
                           = 'A'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'THIRTEEN-8A' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                             MOVE 'Y' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
                       ELSE
                             MOVE 'N' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
                       END-IF
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE < WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-10A' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                       MOVE 'N' TO WS-NEW-PICKUP-IND
                       MOVE 'Y' TO WS-OPT-CAT-DECREASE
                       PERFORM 1-POPULATE-COV-START-DT
                    END-IF
                    IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'THIRTEEN-11A' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                          MOVE 'N' TO WS-NEW-PICKUP-IND
                          MOVE 'Y' TO WS-OPT-CAT-DECREASE
                          PERFORM 1-POPULATE-COV-START-DT
                    END-IF
                 END-IF
              END-IF

      *  MARRIAGE INCREASES FOR DLI HOURLY AND NOT NEWLY ELIG. MGT.
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND CHANGE-REASON-CD-O = 'MA' AND
                  ((WASC-PAY-CODE = '1' AND (WASC-JOB-CODE = 'F' OR 'T'
                  OR 'P')) OR (WASC-PAY-CODE = '6' AND
                  WS-NEWLY-ELIGIBLE
                  = 'N') OR (WASC-PAY-CODE = '1' AND (WASC-JOB-CODE
                  NOT = 'F' AND 'T' AND 'P') AND WS-NEWLY-ELIGIBLE =
                  'N'))
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'FOURTEEN   ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-PLAN-NAME = 'DLI'
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'FOURTEEN-1 ' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'FOURTEEN-2 ' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'CLOSE-BENEFIT-DATE ELEVEN' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'FOURTEEN-3 ' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF WCOVERAGE-OPTION = '1' AND WCOVERAGE-CATEGORY
                           = 'A'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FOURTEEN-4 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          MOVE '2' TO COV-OPTION-O
                          IF SESSION-DATE > DC-CHANGE-EVENT-DATE
                             MOVE SESSION-DATE TO COV-START-DATE-O
                             MOVE SESSION-DATE TO ORIG-COV-DATE-O
                          ELSE
                             MOVE DC-CHANGE-EVENT-DATE TO
                                 COV-START-DATE-O
                             MOVE DC-CHANGE-EVENT-DATE TO
                                 ORIG-COV-DATE-O
                          END-IF
                       ELSE
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FOURTEEN-5 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
      *  START OF CHANGE 07
                          IF WCOVERAGE-OPTION = '1' AND DC-OPTION-CODE
                              > '1'
                             MOVE '2' TO COV-OPTION-O
                          END-IF
                          IF WCOVERAGE-OPTION > '1' AND DC-OPTION-CODE
                              > WCOVERAGE-OPTION
                             MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                          END-IF
                          IF SESSION-DATE > DC-CHANGE-EVENT-DATE
                             MOVE SESSION-DATE TO COV-START-DATE-O
                          ELSE
                             MOVE DC-CHANGE-EVENT-DATE TO
                                 COV-START-DATE-O
                          END-IF
      *  END   OF CHANGE 07
                       END-IF
                    ELSE
                       IF DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FOURTEEN-6 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          IF WCOVERAGE-OPTION = '1' AND
                              WCOVERAGE-CATEGORY = 'A'
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FOURTEEN-7 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE SESSION-DATE TO COV-START-DATE-O
                             MOVE SESSION-DATE TO ORIG-COV-DATE-O
                          ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FOURTEEN-8 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE SESSION-DATE TO COV-START-DATE-O
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF

      *  MARRIAGE INCREASES FOR NEWLY ELIGIBLE MANAGEMENT.
              IF DC-PLAN-YEAR = WPLAN-YEAR AND DC-PLAN-NAME =
                  WPLAN-NAME AND CHANGE-REASON-CD-O = 'MA' AND
                  ((WASC-PAY-CODE = '6' AND WS-NEWLY-ELIGIBLE = 'Y') OR
                  (WASC-PAY-CODE = '1' AND (WASC-JOB-CODE NOT = 'F' AND
                  'T' AND 'P') AND WS-NEWLY-ELIGIBLE = 'Y'))
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'FOURTEEN   ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF DC-PLAN-NAME = 'DLI'
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'FOURTEEN-1 ' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                        DC-CATEGORY-CODE = WCOVERAGE-CATEGORY
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'FOURTEEN-2 ' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'CLOSE-BENEFIT-DATE TWELVE' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF SW-CURSOR2A-ACTIV = 'Y'
                          PERFORM 1-CLOSE-BENEFIT-DATAN
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                       END-IF
                       PERFORM 1-CLOSE-BENEFIT-DATA
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
      *> GO TO JOB
                       MOVE 'J' TO EZT-GOTO-STOP-JOB
                       GO TO SECTION-EXIT
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'FOURTEEN-3 ' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       IF WCOVERAGE-OPTION = '1' AND WCOVERAGE-CATEGORY
                           = 'A'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FOURTEEN-4 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          MOVE '2' TO COV-OPTION-O
                             MOVE 'Y' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
                       ELSE
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FOURTEEN-5 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          IF WCOVERAGE-OPTION = '1' AND DC-OPTION-CODE
                              > '1'
                             MOVE '2' TO COV-OPTION-O
                          END-IF
                          IF WCOVERAGE-OPTION > '1' AND DC-OPTION-CODE
                              > WCOVERAGE-OPTION
                             MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                          END-IF
                             MOVE 'N' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
                       END-IF
                    ELSE
                       IF DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FOURTEEN-6 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          IF WCOVERAGE-OPTION = '1' AND
                              WCOVERAGE-CATEGORY = 'A'
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FOURTEEN-7 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE 'Y' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
                          ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FOURTEEN-8 ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE 'N' TO WS-NEW-PICKUP-IND
                             MOVE 'N' TO WS-OPT-CAT-DECREASE
                             PERFORM 1-POPULATE-COV-START-DT
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF

              IF CHANGE-REASON-CD-O = 'AE'
                 COMPUTE AE-RECS = AE-RECS + 1
              ELSE
                 IF CHANGE-REASON-CD-O = 'NE'
                    MOVE 'OE' TO CHANGE-REASON-CD-O
                    COMPUTE OE-RECS = OE-RECS + 1
                 ELSE
                    COMPUTE FSC-RECS = FSC-RECS + 1
                 END-IF
              END-IF

              COMPUTE TOT-RECS-WRITTEN = TOT-RECS-WRITTEN + 1

      *  DO A FINAL CHECK FOR PERSONS STILL IN THE 90 DAY WAIT WHEN IT I
      *  FAMILY STATUS CHANGE AND IF SO RESET THE DATES TO THE ORIGINAL
      *  COVERAGE DATE
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
              IF SW-DEBUG-SWITCH = 'Y'
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 STRING 'FINAL CHECK FOR NE FOR MA,DI,BI,AP,SC,DS'
                     EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'FINAL CHECK BEN ORIG DATE  '
                     WORIG-COVERAGE-DATE DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'FINAL CHECK NEW START DATE ' COV-START-DATE-O
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'FINAL CHECK NEW ORIG DATE  ' ORIG-COV-DATE-O
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              END-IF
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
              IF (DC-CHANGE-REASON-CODE = 'MA' OR 'DI' OR 'BI' OR 'AP'
                  OR 'GS' OR 'LE' OR 'SE' OR 'SC' OR 'DS' OR 'LA' OR
                  'RW' OR 'TC') AND WORIG-COVERAGE-DATE >
                  COV-START-DATE-O
                 MOVE WORIG-COVERAGE-DATE TO COV-START-DATE-O
              END-IF

      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
              IF (DC-CHANGE-REASON-CODE = 'MA' OR 'DI' OR 'BI' OR 'AP'
                  OR 'GS' OR 'LE' OR 'SE' OR 'SC' OR 'DS' OR 'LA' OR
                  'RW' OR 'TC') AND WORIG-COVERAGE-DATE >
                  ORIG-COV-DATE-O
                 MOVE WORIG-COVERAGE-DATE TO ORIG-COV-DATE-O
              END-IF

      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
              IF DC-CHANGE-REASON-CODE = 'MA' OR 'DI' OR 'BI' OR 'AP'
                  OR 'GS' OR 'LE' OR 'SE' OR 'SC' OR 'DS' OR 'LA' OR
                  'RW' OR 'TC'
                 PERFORM 1-FORMAT-FAMILY-CHANGE-REC
                 WRITE FAMCHANG
                 MOVE SPACES TO FAMCHANG
                 ADD 1 TO FAMCHANG-REK-COUNT
              END-IF

      *65 LABEN1 AE2017 CHANGES
      * STD/LTD TO ACCOMMODATE THE INTRODUCTION AND ON GOING PROCESSING
      *
      *AMANI1-CHANGED THE CODE TO ALLOW THE FT DRIVER INTO LTD PROCESSIN
      *  IF DC-JOB-CODE = 'D'
              IF WS-PT-TRUCK = 'Y'
                 CONTINUE
              ELSE
                 IF (PLAN-NAME-O = 'LTD' AND ((DC-CHANGE-REASON-CODE
                     NOT = 'NE') AND (DC-CHANGE-REASON-CODE NOT =
                     'AE')))
                     IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                         SESSION-DATE >= WCOVERAGE-START-DT
                         MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                     END-IF
                 END-IF
                 IF (PLAN-NAME-O = 'LTD' AND DC-CHANGE-REASON-CODE =
                     'AE')
                     IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                         SESSION-DATE >= WCOVERAGE-START-DT
                         PERFORM 1-GET-COVRG-DATE-NYEAR
                         IF EZT-GOTO-STOP-JOB NOT = 'N'
                             GO TO SECTION-EXIT
                         END-IF
                         MOVE COVRG-DATE-NYEAR TO COV-START-DATE-O
                     END-IF
                 END-IF
                 IF (PLAN-NAME-O = 'STD' AND DC-PLAN-YEAR = 2015 AND
                     COV-OPTION-O = '1' AND DC-CHANGE-REASON-CODE NOT =
                     'AE') OR (PLAN-NAME-O = 'STD' AND DC-PLAN-YEAR >=
                     2016 AND COV-OPTION-O = '4' AND
                     DC-CHANGE-REASON-CODE NOT = 'AE')
                     IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                         SESSION-DATE >= WCOVERAGE-START-DT
                         MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                     END-IF
                 END-IF
                 IF (PLAN-NAME-O = 'STD' AND DC-PLAN-YEAR = 2015 AND
                     COV-OPTION-O = '1' AND DC-CHANGE-REASON-CODE =
                     'AE') OR (PLAN-NAME-O = 'STD' AND DC-PLAN-YEAR >=
                     2016 AND COV-OPTION-O = '4' AND
                     DC-CHANGE-REASON-CODE = 'AE')
                     IF DC-OPTION-CODE < WCOVERAGE-OPTION AND
                         SESSION-DATE >= WCOVERAGE-START-DT
                         PERFORM 1-GET-COVRG-DATE-NYEAR
                         IF EZT-GOTO-STOP-JOB NOT = 'N'
                             GO TO SECTION-EXIT
                         END-IF
                         MOVE COVRG-DATE-NYEAR TO COV-START-DATE-O
                     END-IF
                 END-IF
              END-IF

      *65 LABEN1 AE2017 CHANGES
      *68 LABEN1 YEYB17 CHANGES START
      *FOR DISABILITY PLANS IF THE CHANGE REASON CD IS AE BELOW CODE EXE
                 IF (DC-CHANGE-REASON-CODE = 'AE') AND (DC-PLAN-NAME =
                     'STD' OR 'LTD')
      *        (DC-PLAN-YEAR >= 2016)
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'STD/LTD-8' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    IF DC-OPTION-CODE > WCOVERAGE-OPTION
                     IF DC-ENROLL-TYPE-CODE = 4
                       MOVE DC-SESSION-TS TO WS-SESSION-TS
                       MOVE WS-SESSION-DATE TO WS-COVERAGE-START-DT
      *16 - MAKE STD/LTD EFF 1 YEAR FROM SESSION DATE INSTEAD OF 1-1-##
      *           WS-SESSION-DATE-YY        =  WS-SESSION-DATE-YY + 1
      *           COV-START-DATE-O          =  WS-SESSION-DATE
                       PERFORM 1-GET-COVERAGE-START-DT
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
                       MOVE WS-COVERAGE-START-DT TO ORIG-COV-DATE-O
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'STD/LTD-9' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                     ELSE
                       MOVE DC-SESSION-TS TO WS-SESSION-TS
                       MOVE WS-SESSION-DATE TO WS-COVERAGE-START-DT
      *           WS-SESSION-DATE-YY        =  WS-SESSION-DATE-YY + 1
      *           COV-START-DATE-O          =  WS-SESSION-DATE
                       PERFORM 1-GET-COVERAGE-START-DT
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
                       MOVE WS-COVERAGE-START-DT TO ORIG-COV-DATE-O
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'STD/LTD-10' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                     END-IF
                       IF WS-STD-LTD-NW-ELIG = 'N'
                          MOVE 'Y' TO ENROLL-LATE-INT-O
                       ELSE
                          MOVE 'N' TO ENROLL-LATE-INT-O
                          MOVE WCOVERAGE-START-DT TO COV-START-DATE-O
                       END-IF
                       IF PLAN-NAME-O = 'STD'
                          MOVE WWAIT-DAY-QTY TO WAIT-DAY-QTY-O
                       ELSE
                          MOVE 365 TO WAIT-DAY-QTY-O
                       END-IF
                    END-IF
                    IF DC-OPTION-CODE < WCOVERAGE-OPTION
      *           PERFORM GET-SESSION-DATE-PLUS1
                       PERFORM 1-GET-COVRG-DATE-NYEAR
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
                       IF WCOVERAGE-START-DT > COVRG-DATE-NYEAR
                           MOVE WCOVERAGE-START-DT TO COV-START-DATE-O
                       ELSE
                       MOVE COVRG-DATE-NYEAR TO COV-START-DATE-O
                       END-IF
                       MOVE 'N' TO ENROLL-LATE-INT-O
                    END-IF
                 END-IF
      *62
      *65 LABEN1 AE2017 CHANGES END
      *68 LABEN1 YEYB17 CHANGES END
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'

      *AMANI1-CHANGED THE CODE TO ALLOW THE FT DRIVER INTO LTD PROCESSIN
      *  IF  (DC-JOB-CODE ^= 'D')  AND +
              IF (WS-PT-TRUCK NOT = 'Y') AND (DC-CHANGE-REASON-CODE =
                  'LE' OR 'GS' OR 'DI' OR 'MA' OR 'BI' OR 'AP' OR 'SC'
                  OR 'RW' OR 'LA' OR 'SE' OR 'DS' OR 'TC') AND
                  (DC-PLAN-NAME = 'STD' OR 'LTD') AND (WCOVERAGE-OPTION
                  < COV-OPTION-O)
      *      (DC-PLAN-YEAR >= 2016)
      *68 YEYB17 CHANGES START
                  PERFORM 1-GET-WIN-NBR
                  PERFORM 1-CHECK-DISTRANS-CURSOR
                  IF EZT-GOTO-STOP-JOB NOT = 'N'
                      GO TO SECTION-EXIT
                  END-IF
                  IF WS-STD-LTD-NW-ELIG = 'Y' OR WS-DISTRANS-SW = 'Y'
                     MOVE 'N' TO ENROLL-LATE-INT-O
                     IF WS-DISTRANS-SW = 'Y'
                        MOVE WS-SESSION-DATE TO COV-START-DATE-O
                     ELSE
                        MOVE WCOVERAGE-START-DT TO COV-START-DATE-O
                     END-IF
                     MOVE 0 TO WAIT-DAY-QTY-O
                  ELSE
                     IF DC-CHANGE-REASON-CODE NOT = 'SE'
                        MOVE DC-CHANGE-EVENT-DATE TO
                            WS-COVERAGE-START-DT
                        PERFORM 1-GET-COVERAGE-START-DT
                        IF EZT-GOTO-STOP-JOB NOT = 'N'
                            GO TO SECTION-EXIT
                        END-IF
                        MOVE 'Y' TO ENROLL-LATE-INT-O
                        MOVE 365 TO WAIT-DAY-QTY-O
                     END-IF
                  END-IF
      *68 YEYB17 CHANGES END
      *61
                  IF DC-CHANGE-REASON-CODE = 'SE'
                     IF WCOVERAGE-START-DT < '2016-03-19'
                        MOVE '2016-03-19' TO COV-START-DATE-O
                        MOVE 'N' TO ENROLL-LATE-INT-O
                        MOVE 0 TO WAIT-DAY-QTY-O
                     ELSE
                        MOVE WCOVERAGE-START-DT TO COV-START-DATE-O
                     END-IF
                  END-IF
                  MOVE 'Y' TO SW-POST-STD-LTD
              END-IF
      *54 YEYB2016 CHANGES START
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
              IF (DC-CHANGE-REASON-CODE = 'LE' OR 'GS' OR 'DI' OR 'MA'
                  OR 'BI' OR 'AP' OR 'SC' OR 'TC' OR 'RW' OR 'LA' OR
                  'SE' OR 'DS') AND (DC-PLAN-NAME = 'STD' OR 'LTD') AND
                  (WCOVERAGE-OPTION > COV-OPTION-O) AND (DC-PLAN-YEAR
                  >= 2016)
      *61
                 IF DC-CHANGE-REASON-CODE NOT = 'SE'
                     MOVE DC-CHANGE-EVENT-DATE TO WS-COVERAGE-START-DT
      *58
                     IF WCOVERAGE-OPTION NOT = '1' AND '4'
                        PERFORM 1-GET-COVERAGE-START-DT
                        IF EZT-GOTO-STOP-JOB NOT = 'N'
                            GO TO SECTION-EXIT
                        END-IF
                     END-IF
      *58
                     IF (WS-COVERAGE-START-DT > WORIG-COVERAGE-DATE)
                         AND (DC-PLAN-NAME = 'LTD')
                         PERFORM 1-GET-SESSION-DATE-PLUS1
                         IF EZT-GOTO-STOP-JOB NOT = 'N'
                             GO TO SECTION-EXIT
                         END-IF
                         MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                     END-IF
                 END-IF
      *58*61
                     IF (DC-OPTION-CODE = '1')
                        PERFORM 1-GET-SESSION-DATE-PLUS1
                        IF EZT-GOTO-STOP-JOB NOT = 'N'
                            GO TO SECTION-EXIT
                        END-IF
                        MOVE 'N' TO ENROLL-LATE-INT-O
                        IF WCOVERAGE-OPTION NOT = '1' AND '4'
                           MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                        END-IF
                       IF PLAN-NAME-O = 'STD' AND (DC-JOB-CODE NOT =
                           'P' AND 'T') AND (WS-WORK-STATE NOT = 5 AND
                           12 AND 31 AND 40)
                             MOVE '4' TO COV-OPTION-O
                        END-IF
                     END-IF
              END-IF
      *58*61
              IF (DC-CHANGE-REASON-CODE = 'LD') AND (DC-PLAN-NAME =
                  'LTD') AND (DC-PLAN-YEAR >= 2016)
      *62
                  IF (DC-OPTION-CODE NOT = '1')
                      MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                  END-IF
      *62
              END-IF
              IF (DC-CHANGE-REASON-CODE = 'LD') AND (DC-PLAN-YEAR >=
                  2016)
      *58*61
                     IF ((DC-OPTION-CODE = '1') AND (DC-PLAN-NAME =
                         'LTD' OR ((WCOVERAGE-OPTION NOT = '1' AND '4')
                         AND DC-PLAN-NAME = 'STD')))
                          PERFORM 1-GET-SESSION-DATE-PLUS1
                          IF EZT-GOTO-STOP-JOB NOT = 'N'
                              GO TO SECTION-EXIT
                          END-IF
                          MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                     END-IF
                     IF PLAN-NAME-O = 'STD' AND (DC-JOB-CODE NOT = 'P'
                         AND 'T' AND 'S') AND (WS-WORK-STATE NOT = 5
                         AND 12 AND 31 AND 40) AND (DC-OPTION-CODE =
                         '1')
                             MOVE '4' TO COV-OPTION-O
                     END-IF
      *58*61
              END-IF
      *54 YEYB2016 CHANGES END

      *63 SPECIAL ENROLLMENT CHANGES START
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
              IF (DC-JOB-CODE NOT = 'D') AND (DC-CHANGE-REASON-CODE =
                  'LE' OR 'GS' OR 'DI' OR 'MA' OR 'BI' OR 'AP' OR 'SC'
                  OR 'SE' OR 'LD' OR 'DS' OR 'LA' OR 'RW' OR 'TC') AND
                  (DC-PLAN-NAME = 'STD') AND (WCOVERAGE-OPTION >
                  COV-OPTION-O) AND (DC-PLAN-YEAR >= 2016)
                  IF (WCOVERAGE-OPTION = '6' AND COV-OPTION-O = '5')
                      MOVE WCOVERAGE-START-DT TO COV-START-DATE-O
                      MOVE WENROLL-LATE-INT TO ENROLL-LATE-INT-O
                      MOVE WWAIT-DAY-QTY TO WAIT-DAY-QTY-O
                  END-IF
              END-IF
      *64
              IF (DC-JOB-CODE NOT = 'D') AND (DC-CHANGE-REASON-CODE =
                  'LD') AND (DC-PLAN-NAME = 'STD' OR 'LTD') AND
                  (WCOVERAGE-OPTION < COV-OPTION-O) AND (DC-PLAN-YEAR
                  >= 2016)
                     MOVE WCOVERAGE-START-DT TO COV-START-DATE-O
                     MOVE WENROLL-LATE-INT TO ENROLL-LATE-INT-O
                     MOVE WWAIT-DAY-QTY TO WAIT-DAY-QTY-O
              END-IF
      *63 SPECIAL ENROLLMENT CHANGES END

      * FINAL CHECK FOR DLI LIMITS DUE TO ALI SELECTIONS FOR FL,MD,WA ST
      *      1)  SAVE ALL ALI SELECTIONS FOR DLI LIIT CHECKS.
              IF PLAN-NAME-O = 'ALI'
                 MOVE COV-OPTION-O TO WS-ALI-OPT
                 MOVE DC-ENROLLEE-SSN TO WS-ALI-SSN
              END-IF

      * FINAL CHECK FOR CHANGES AFTER CUTBACKS AND IF NO CHANGES THEN DO
      * NOT WRITE THE RECORD
      * JLSMELT ADDED CODE TO PASS ALL STD RECORDS ON TO THE NEXT PROGRA

              IF COV-OPTION-O = WCOVERAGE-OPTION AND COV-CATEGORY-O =
                  WCOVERAGE-CATEGORY AND PROVIDER-ID-O =
                  WINS-PROVIDER-ID AND BEN-BKRID-O = WINS-BROKER-ID AND
                  PLAN-NAME-O NOT = 'STD'
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                    MOVE DC-PLAN-YEAR TO EZT-DISP-EZ-5-0-3
                    MOVE PROVIDER-ID-O TO EZT-DISP-EZ-5-0-1
                    STRING 'NO CHANGES AFTER CUTBACKS, GO TO JOB '
                        EZT-DISP-EZ-10-0 ' ' PLAN-NAME-O ' '
                        EZT-DISP-EZ-5-0-3 ' ' COV-OPTION-O ' '
                        COV-CATEGORY-O ' ' ORIG-COV-DATE-O ' '
                        CHANGE-REASON-CD-O ' ' EZT-DISP-EZ-5-0-1
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'CLOSE-BENEFIT-DATE THIRTEEN' TO
                        EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 IF SW-CURSOR2A-ACTIV = 'Y'
                    PERFORM 1-CLOSE-BENEFIT-DATAN
                    IF EZT-GOTO-STOP-JOB NOT = 'N'
                        GO TO SECTION-EXIT
                    END-IF
                 END-IF
                 PERFORM 1-CLOSE-BENEFIT-DATA
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
      *> GO TO JOB
                 MOVE 'J' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF


      * MODIFY CHANGE REASON CODE AND START AND ORIGINAL DATES BASED ON
      * ENROLLMENT TYPE CODE FROM THE WEB FOR AE RECORDS.

              IF CHANGE-REASON-CD-O = 'AE'
                    IF PLAN-NAME-O = 'STD' AND DC-PLAN-YEAR >= 2016
                       IF COV-OPTION-O = '1' AND WASC-PAY-CODE = '1'
                           AND WASC-JOB-CODE = 'F' AND (WS-WORK-STATE
                           NOT = 5 AND 12 AND 31 AND 40)
                          MOVE '4' TO COV-OPTION-O
                       END-IF
                    END-IF

      * SET DATES FOR MED DEN ADD FOR MA AND DI TO PRORATED DATE  ****
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
                 IF ((DC-ENROLL-TYPE-CODE = 66 OR 67 OR 27 OR 50) AND
                     (PLAN-NAME-O = 'MED' OR 'DEN' OR 'ADD' OR 'VIS'))
                    IF WCOVERAGE-OPTION = '1'
                       MOVE DC-CHANGE-EVENT-DATE TO COV-START-DATE-O
                       MOVE DC-CHANGE-EVENT-DATE TO ORIG-COV-DATE-O
                    ELSE
                       MOVE DC-CHANGE-EVENT-DATE TO COV-START-DATE-O
                    END-IF
                 END-IF
      * SET DATES FOR MED DEN ADD FOR BI TO EVENT DATE ***************
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
                 IF ((DC-ENROLL-TYPE-CODE = 7 OR 5 OR 51) AND
                    (PLAN-NAME-O = 'MED' OR 'DEN' OR 'ADD' OR 'VIS'))
                    IF WCOVERAGE-OPTION = '1'
                       MOVE DC-CHANGE-EVENT-DATE TO COV-START-DATE-O
                       MOVE DC-CHANGE-EVENT-DATE TO ORIG-COV-DATE-O
                    ELSE
                       MOVE DC-CHANGE-EVENT-DATE TO COV-START-DATE-O
                    END-IF
                 END-IF
      * SET CHANGE REASON CODES *********************************
                 IF DC-ENROLL-TYPE-CODE = 13
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  DI' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'DI' TO CHANGE-REASON-CD-O
                 END-IF
                 IF DC-ENROLL-TYPE-CODE = 30
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  MA' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'MA' TO CHANGE-REASON-CD-O
                 END-IF
                 IF DC-ENROLL-TYPE-CODE = 7
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  BI' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'BI' TO CHANGE-REASON-CD-O
                 END-IF
                 IF DC-ENROLL-TYPE-CODE = 5
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  AP' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'AP' TO CHANGE-REASON-CD-O
                 END-IF
                 IF DC-ENROLL-TYPE-CODE = 51
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  SC' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'SC' TO CHANGE-REASON-CD-O
                 END-IF
      *69 JEDHA1 BA2018 CHANGES BEGINS
                 IF DC-ENROLL-TYPE-CODE = 16
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  DS' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'DS' TO CHANGE-REASON-CD-O
                 END-IF
      *69 JEDHA1 BA2018 CHANGES ENDS
                 IF DC-ENROLL-TYPE-CODE = 64
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  LD' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'LD' TO CHANGE-REASON-CD-O
                 END-IF
                 IF DC-ENROLL-TYPE-CODE = 66
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  GS' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'GS' TO CHANGE-REASON-CD-O
                 END-IF
                 IF DC-ENROLL-TYPE-CODE = 67
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  LE' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'LE' TO CHANGE-REASON-CD-O
                 END-IF
      **AE2018-LOA,RW CHANGES BEGINS
                 IF DC-ENROLL-TYPE-CODE = 27
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  LA' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'LA' TO CHANGE-REASON-CD-O
                 END-IF
                 IF DC-ENROLL-TYPE-CODE = 50
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'MODIFIED CHANGE REASON TO  RW' TO
                           EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
                    MOVE 'RW' TO CHANGE-REASON-CD-O
                 END-IF
      **AE2018-LOA,RW CHANGES ENDS
              END-IF

              IF (PLAN-NAME-O = 'DLI' AND CHANGE-REASON-CD-O = 'OE' AND
                  DC-ENROLL-TYPE-CODE = 34)
                 IF DC-OPTION-CODE > WCOVERAGE-OPTION AND
                     DC-CATEGORY-CODE >= WCOVERAGE-CATEGORY
                     IF (WCOVERAGE-OPTION = '1' AND WCOVERAGE-CATEGORY
                         = 'A')
                         MOVE 'Y' TO WS-NEW-PICKUP-IND
                         MOVE 'N' TO WS-OPT-CAT-DECREASE
                         PERFORM 1-POPULATE-COV-START-DT
                     ELSE
                         MOVE 'N' TO WS-NEW-PICKUP-IND
                         MOVE 'N' TO WS-OPT-CAT-DECREASE
                         PERFORM 1-POPULATE-COV-START-DT
                     END-IF
                 END-IF
                 IF DC-OPTION-CODE = WCOVERAGE-OPTION AND
                     DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                     IF (WCOVERAGE-OPTION = '1' AND WCOVERAGE-CATEGORY
                         = 'A')
                         MOVE 'Y' TO WS-NEW-PICKUP-IND
                         MOVE 'N' TO WS-OPT-CAT-DECREASE
                         PERFORM 1-POPULATE-COV-START-DT
                     ELSE
                         MOVE 'N' TO WS-NEW-PICKUP-IND
                         MOVE 'N' TO WS-OPT-CAT-DECREASE
                         PERFORM 1-POPULATE-COV-START-DT
                     END-IF
                 END-IF
                 IF (DC-OPTION-CODE < WCOVERAGE-OPTION) AND
                     (DC-CATEGORY-CODE < WCOVERAGE-CATEGORY)
                     MOVE 'N' TO WS-NEW-PICKUP-IND
                     MOVE 'Y' TO WS-OPT-CAT-DECREASE
                     PERFORM 1-POPULATE-COV-START-DT
                 END-IF
                 IF (DC-OPTION-CODE = WCOVERAGE-OPTION) AND
                     (DC-CATEGORY-CODE < WCOVERAGE-CATEGORY)
                     MOVE 'N' TO WS-NEW-PICKUP-IND
                     MOVE 'Y' TO WS-OPT-CAT-DECREASE
                     PERFORM 1-POPULATE-COV-START-DT
                 END-IF
                 IF (DC-OPTION-CODE < WCOVERAGE-OPTION) AND
                     (DC-CATEGORY-CODE < WCOVERAGE-CATEGORY)
                     MOVE 'N' TO WS-NEW-PICKUP-IND
                     MOVE 'Y' TO WS-OPT-CAT-DECREASE
                     PERFORM 1-POPULATE-COV-START-DT
                 END-IF
                 IF (DC-OPTION-CODE > WCOVERAGE-OPTION) AND
                     (DC-CATEGORY-CODE < WCOVERAGE-CATEGORY)
                     IF WCOVERAGE-OPTION = '1' AND ((WASC-PAY-CODE =
                         '6') OR (WASC-PAY-CODE = '1' AND WASC-JOB-CODE
                         NOT = 'F' AND 'T' AND 'P'))
                         PERFORM 1-CREATE-TWO-DLI-COV-RECORDS
                     ELSE
                         MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                         MOVE 'N' TO WS-NEW-PICKUP-IND
                         MOVE 'Y' TO WS-OPT-CAT-DECREASE
                         PERFORM 1-POPULATE-COV-START-DT
                     END-IF
                 END-IF
                 IF (DC-OPTION-CODE < WCOVERAGE-OPTION) AND
                     (DC-CATEGORY-CODE > WCOVERAGE-CATEGORY)
                     IF WCOVERAGE-OPTION = '1' AND ((WASC-PAY-CODE =
                         '6') OR (WASC-PAY-CODE = '1' AND WASC-JOB-CODE
                         NOT = 'F' AND 'T' AND 'P'))
                         PERFORM 1-CREATE-TWO-DLI-COV-RECORDS
                     ELSE
                         MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                         MOVE 'N' TO WS-NEW-PICKUP-IND
                         MOVE 'Y' TO WS-OPT-CAT-DECREASE
                         PERFORM 1-POPULATE-COV-START-DT
                     END-IF
                 END-IF
              END-IF

              MOVE SESSION-DATE TO CONFIRM-DATE-O
              IF SW-DEBUG-SWITCH = 'Y'
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-PLAN-YEAR TO EZT-DISP-EZ-5-0-3
                 MOVE PROVIDER-ID-O TO EZT-DISP-EZ-5-0-1
                 STRING 'BEREC ' WS-ENROLLEE-SSN ' ' PLAN-NAME-O ' '
                     EZT-DISP-EZ-5-0-3 ' ' COV-START-DATE-O ' '
                     COV-OPTION-O ' ' COV-CATEGORY-O ' '
                     ENROLL-RCPT-DATE-O ' ' ORIG-COV-DATE-O ' '
                     CHANGE-REASON-CD-O ' ' EZT-DISP-EZ-5-0-1 ' '
                     ENROLL-LATE-INT-O DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              END-IF

      ********************* START CODING CHANGES HERE ******************
      ** ADD LAST CHECKS HERE FOR FSC CARRY OVER OF DATES AND OPTS FROM
      ** PREVIOUS RECORD BEFORE SAVING AND WRITING CURRENT RECORD

      ** SINCE WE CAN ONLY HAVE THE MAX (LAST ENTERED AND UNPROCESSED)
      ** RECORD FOR SSN AND PLAN THEN WE NEED TO DO FURTHER CHECKS
      ** BY ENTRY TYPE

              IF (SSN-O = SSN-PREV AND PLAN-NAME-O = PLAN-NAME-PREV)
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE 'FIFTY ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
      ** THIS IS FSC FOR BOTH RECORDS SO CARRY OPT,CAT,EFF-DATE,AND
      ** ORIG-DATE OVER TO NEW PLAN YEAR FOR ALL PLANS SINCE NEW YEAR RE
      ** GENERATED BY WEB ENTRY AND SHOULD BE SAME.

      ** CHANGE 20 - ELIMINATE THIS CODE.  BO008 UPDATES PLAN_SELECTION
      **             WAY THE DATA SHOULD LOOK FOR BOTH PLAN YEARS.
      *65 LABEN1 AE2017 CHANGES
      *ADDED LOGIC TO BYPASS DISABILITY PLANS
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
                 IF (PLAN-NAME-O NOT = 'STD' AND 'LTD')
                 IF ((CHANGE-REASON-CD-PREV = 'MA' OR 'BI' OR 'DI' OR
                     'LD' OR 'GS' OR 'LE' OR 'AP' OR 'SC' OR 'SE' OR
                     'DS' OR 'LA' OR 'RW' OR 'TC') AND
                     (DC-ENROLL-TYPE-PREV = 30 OR 13 OR 16 OR 7 OR 5 OR
                     51 OR 64 OR 66 OR 67 OR 68 OR 27 OR 50) AND
                     (CHANGE-REASON-CD-O = 'MA' OR 'BI' OR 'AP' OR 'DI'
                     OR 'LD' OR 'GS' OR 'LE' OR 'SC' OR 'SE' OR 'DS' OR
                     'LA' OR 'RW' OR 'TC') AND (DC-ENROLL-TYPE-CODE =
                     30 OR 13 OR 16 OR 7 OR 5 OR 51 OR 64 OR 66 OR 67
                     OR 68 OR 27 OR 50) AND CHANGE-REASON-CD-O =
                     CHANGE-REASON-CD-PREV AND DC-ENROLL-TYPE-CODE =
                     DC-ENROLL-TYPE-PREV)
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'FIFTY ONE ' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODE 'DS'
                    IF (CHANGE-REASON-CD-O = 'DI' OR 'DS') AND
                        PLAN-NAME-O = 'DLI' AND COV-CATEGORY-O >
                        COV-CATEGORY-PREV
                       MOVE COV-START-DATE-PREV TO COV-START-DATE-O
                    END-IF
                    MOVE ORIG-COV-DATE-PREV TO ORIG-COV-DATE-O
                 END-IF

      ** THIS IS FSC OR NE OR LD FOR THE CURRENT YEAR AND THEN FOLLOWED
      ** BY AN 'AE' TYPE 4 RECORD FOR THE
      ** NEW YEAR. IF COVERAGE IS SAME FOR THE PLAN THEN CARRY DATES OVE
      ** IF COVERAGE IS DIFFERENT FOR NEW YEAR AND NOT FIRST TIME COVERA
      ** USE CURR ORIG-DATE AND 01-01-XXXX(NEW PLAN YEAR) EFF-DATE
      ** IF COVERAGE IS DIFFERENT FOR NEW YEAR AND IS FIRST TIME  COVERA
      ** USE GREATER OF 01-01-XXXX OR ORIG-DATE FOR BOTH EFF AND ORIG DA
      ** NOTE THAT FIRST TIME COVERAGE DETERMINED FROM CURR YEAR ENTRY S
      ** THE 'MA' MAY HAVE PICKED UP FIRST TIME COV AND THE NEW YEAR REC
      ** BENEFITS WILL NOT BE UPDATED YET.
      ** NEXT-PLAN-YR-START - IS START DATE FOR NEW PLAN YEAR

      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
                 IF ((CHANGE-REASON-CD-PREV = 'MA' OR 'BI' OR 'DI' OR
                     'LD' OR 'NE' OR 'GS' OR 'LE' OR 'TC' OR 'SE' OR
                     'AP' OR 'SC' OR 'DS' OR 'LA' OR 'RW') AND
                     (DC-ENROLL-TYPE-PREV = 30 OR 13 OR 16 OR 7 OR 5 OR
                     34 OR 51 OR 64 OR 66 OR 67 OR 68 OR 27 OR 50) AND
                     CHANGE-REASON-CD-O = 'AE' AND DC-ENROLL-TYPE-CODE
                     = 4)
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'FIFTY TWO ' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
      ** SAME COVERAGES, CARRY SAME DATES
                    IF (COV-OPTION-O = COV-OPTION-PREV AND
                        COV-CATEGORY-O = COV-CATEGORY-PREV)
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'FIFTY THREE ' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                        MOVE COV-START-DATE-PREV TO COV-START-DATE-O
                        MOVE ORIG-COV-DATE-PREV TO ORIG-COV-DATE-O
                    ELSE
      ** IF PREV COVERAGE NOT NO COVERAGE THEN AE REC IS NOT PICKING
      ** UP FOR FIRST TIME. POST DATES DEPENDING ON THIS.
                       IF PLAN-NAME-O = 'DLI'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FIFTY FOUR ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          IF (COV-OPTION-PREV = '1' AND
                              COV-CATEGORY-PREV = 'A')
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FIFTY FIVE ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE ORIG-COV-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                          ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'FIFTY EIGHT ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE COV-START-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                          END-IF
                       END-IF
                       IF PLAN-NAME-O = 'ALI'
                          MOVE COV-START-DATE-PREV TO COV-START-DATE-O
                          MOVE ORIG-COV-DATE-PREV TO ORIG-COV-DATE-O
                       END-IF
                       IF PLAN-NAME-O NOT = 'DLI'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SIXTY ONE ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          IF COV-OPTION-PREV = '1'
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SIXTY TWO ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             IF ORIG-COV-DATE-PREV <=
                                 NEXT-PLAN-YR-START
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SIXTY THREE ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE NEXT-PLAN-YR-START TO
                                 COV-START-DATE-O
                             MOVE NEXT-PLAN-YR-START TO
                                 ORIG-COV-DATE-O
                             ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SIXTY FOUR ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE ORIG-COV-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                             END-IF
                          ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SIXTY FIVE ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             IF ORIG-COV-DATE-PREV <=
                                 NEXT-PLAN-YR-START
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SIXTY SIX ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE NEXT-PLAN-YR-START TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                             ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SIXTY SEVEN ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE ORIG-COV-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF

      ** THIS IS AE REC FOR MEW YEAR FOLLOWED BY FSC OR LD IN CURRENT YE
      ** THEN FSC SHOULD OVER WRITE DATES IF OPTION/CAT ARE SAME.  IF OP
      ** OR CATEGORY ARE DIFFERENT THEN ONLY START DATE OVER WRITTEN.
      ** NEXT-PLAN-YR-START - IS START DATE FOR NEW PLAN YEAR

      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
                 IF (CHANGE-REASON-CD-PREV = 'AE' AND
                     DC-ENROLL-TYPE-PREV = 4 AND (CHANGE-REASON-CD-O =
                     'MA' OR 'BI' OR 'AP' OR 'SC' OR 'DI' OR 'LD' OR
                     'GS' OR 'TC' OR 'LE' OR 'SE' OR 'DS' OR 'LA' OR
                     'RW') AND (DC-ENROLL-TYPE-CODE = 30 OR 13 OR 16 OR
                     7 OR 5 OR 51 OR 64 OR 66 OR 67 OR 68 OR 27 OR 50))
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'SIXTY  EIGHT' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
      ** SAME COVERAGES, CARRY SAME DATES
                    IF (COV-OPTION-O = COV-OPTION-PREV AND
                        COV-CATEGORY-O = COV-CATEGORY-PREV)
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'SIXTY NINE  ' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                        MOVE COV-START-DATE-PREV TO COV-START-DATE-O
                        MOVE ORIG-COV-DATE-PREV TO ORIG-COV-DATE-O
                    ELSE
      ** IF PREV COVERAGE NOT NO COVERAGE THEN AE REC IS NOT PICKING
      ** UP FOR FIRST TIME. POST DATES DEPENDING ON THIS.
                       IF PLAN-NAME-O = 'DLI'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SEVENTY    ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          IF (COV-OPTION-PREV = '1' AND
                              COV-CATEGORY-PREV = 'A')
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SEVENTY ONE' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE ORIG-COV-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                          ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SEVENTY FOUR' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE COV-START-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                          END-IF
                       END-IF
                       IF PLAN-NAME-O = 'ALI'
                          IF COV-OPTION-PREV = '1'
                             MOVE ORIG-COV-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                          ELSE
                             MOVE COV-START-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                          END-IF
      *    .
      *1-ENDIF.
                       IF PLAN-NAME-O NOT = 'DLI' AND PLAN-NAME-O =
                           'ALI'
                          IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SEVENTY SEVEN' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                          END-IF
                          IF COV-OPTION-PREV = '1'
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SEVENTY EIGHT' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             IF ORIG-COV-DATE-PREV <=
                                 NEXT-PLAN-YR-START
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'SEVENTY NINE' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE NEXT-PLAN-YR-START TO
                                 COV-START-DATE-O
                             MOVE NEXT-PLAN-YR-START TO
                                 ORIG-COV-DATE-O
                             ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'EIGHTY     ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE ORIG-COV-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                             END-IF
                          ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'EIGHTY ONE ' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             IF ORIG-COV-DATE-PREV <=
                                 NEXT-PLAN-YR-START
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'EIGHTY TWO' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE NEXT-PLAN-YR-START TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                             ELSE
                             IF SW-DEBUG-SWITCH = 'Y'
                             MOVE 'EIGHTY THREE' TO EZT-DISPLAY-STRING
                             WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                                 AFTER ADVANCING 1 LINE
                             END-IF
                             MOVE ORIG-COV-DATE-PREV TO
                                 COV-START-DATE-O
                             MOVE ORIG-COV-DATE-PREV TO
                                 ORIG-COV-DATE-O
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
                 END-IF
              END-IF
      *65 LABEN1 AE2017 CHANGES END

      ** THIS IS AN FSC RECORD FOR THE NEW YEAR AND THERE WAS NOT ANOTHE
      ** RECORD FOR THE CURRENT YEAR FOR THIS ASSOCIATE.  DETERMINE IF
      ** THE COVERAGE NEEDS TO HAVE A CURRENT YEAR RECORD CREATED AND IF
      ** SO WE HAVE TO CREATE IT.
      **JLSMELT
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'AP' 'SC' 'DS'
      *70 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODES 'LA' 'RW'
              IF SSN-CURR-YEAR NOT = DC-ENROLLEE-SSN
                 IF (CHANGE-REASON-CD-O = 'MA' OR 'BI' OR 'AP' OR 'SC'
                     OR 'DI' OR 'GS' OR 'LE' OR 'DS' OR 'LA' OR 'RW' OR
                     'TC') AND (DC-ENROLL-TYPE-CODE = 30 OR 13 OR 16 OR
                     7 OR 5 OR 51 OR 66 OR 67 OR 27 OR 50)
                    IF SW-DEBUG-SWITCH = 'Y'
                       MOVE 'EIGHTY FOUR ' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                    END-IF
      ** IF NOT MED, DEN, ADD WE NEED TO CREATE CURR YEAR REC.
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
                    IF DC-PLAN-NAME NOT = 'MED' AND 'DEN' AND 'ADD' AND
                        'VIS'
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE 'EIGHTY FIVE ' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                          STRING 'DC-ENROLLEE-SSN  ' EZT-DISP-EZ-10-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE DC-PLAN-YEAR TO EZT-DISP-EZ-5-0-3
                          STRING 'DC-PLAN-YEAR     ' EZT-DISP-EZ-5-0-3
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE BEN-REC TO WS-OUT-REC-SAVE
                       MOVE DC-ENROLLEE-SSN TO WSSN
                       COMPUTE WPLAN-YEAR-PREV = DC-PLAN-YEAR - 1
                       PERFORM 1-GET-BENEFIT-DATA-PREV
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WPLAN-YEAR-PREV TO EZT-DISP-EZ-5-0
                          STRING 'WPLAN-YEAR-PREV ' EZT-DISP-EZ-5-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          STRING 'WPAYROLL-DATE-PREV '
                              WPAYROLL-DATE-PREV DELIMITED BY SIZE
                              INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       MOVE WPLAN-YEAR-PREV TO PLAN-YEAR-O
                       MOVE WPAYROLL-DATE-PREV TO PAYROLL-DATE-O
                       IF SW-DEBUG-SWITCH = 'Y'
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE PLAN-YEAR-O TO EZT-DISP-EZ-5-0
                          MOVE PROVIDER-ID-O TO EZT-DISP-EZ-5-0-1
                          STRING
                              'CREATED FOR FSC FOR OTHER THAN MED DEN AD
      -                      'D ' ' ' WS-ENROLLEE-SSN ' ' PLAN-NAME-O
                              ' ' EZT-DISP-EZ-5-0 ' ' PAYROLL-DATE-O
                              ' ' COV-START-DATE-O ' ' COV-OPTION-O ' '
                              COV-CATEGORY-O ' ' ENROLL-RCPT-DATE-O ' '
                              ORIG-COV-DATE-O ' ' CHANGE-REASON-CD-O
                              ' ' EZT-DISP-EZ-5-0-1 ' '
                              ENROLL-LATE-INT-O DELIMITED BY SIZE
                              INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                       END-IF
                       PERFORM 1-WRITE-BEUPDATE
                       IF EZT-GOTO-STOP-JOB NOT = 'N'
                           GO TO SECTION-EXIT
                       END-IF
                       MOVE WS-OUT-REC-SAVE TO BEN-REC
                    END-IF
                 END-IF
              END-IF
              END-IF
              IF SW-DEBUG-SWITCH = 'Y'
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-PLAN-YEAR TO EZT-DISP-EZ-5-0-3
                 MOVE PROVIDER-ID-O TO EZT-DISP-EZ-5-0-1
                 STRING 'BEREC AFTER CHECKING FOR DATE CHANGNES' ' '
                     WS-ENROLLEE-SSN ' ' PLAN-NAME-O ' '
                     EZT-DISP-EZ-5-0-3 ' ' COV-START-DATE-O ' '
                     COV-OPTION-O ' ' COV-CATEGORY-O ' '
                     ENROLL-RCPT-DATE-O ' ' ORIG-COV-DATE-O ' '
                     CHANGE-REASON-CD-O ' ' EZT-DISP-EZ-5-0-1 ' '
                     ENROLL-LATE-INT-O DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              END-IF

      *********************  END  CODING CHANGES HERE ******************
              MOVE SSN-O TO SSN-PREV
              MOVE PLAN-NAME-O TO PLAN-NAME-PREV
              MOVE COV-START-DATE-O TO COV-START-DATE-PREV
              MOVE COV-OPTION-O TO COV-OPTION-PREV
              MOVE COV-CATEGORY-O TO COV-CATEGORY-PREV
              MOVE ENROLL-RCPT-DATE-O TO ENROLL-RCPT-DATE-PREV
              MOVE ORIG-COV-DATE-O TO ORIG-COV-DATE-PREV
              MOVE CHANGE-REASON-CD-O TO CHANGE-REASON-CD-PREV
              MOVE DC-ENROLL-TYPE-CODE TO DC-ENROLL-TYPE-PREV
              MOVE WCOVERAGE-OPTION TO WCOVERAGE-OPT-PREV
              MOVE WCOVERAGE-CATEGORY TO WCOVERAGE-CAT-PREV

              IF SW-DEBUG-SWITCH = 'Y'
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE SSN-PREV TO EZT-DISP-EZ-10-0
                 MOVE DC-ENROLL-TYPE-PREV TO EZT-DISP-EZ-5-0
                 STRING 'SAVED DATA ' EZT-DISP-EZ-10-0 ' '
                     PLAN-NAME-PREV ' ' COV-START-DATE-PREV ' '
                     COV-OPTION-PREV ' ' COV-CATEGORY-PREV ' '
                     ENROLL-RCPT-DATE-PREV ' ' ORIG-COV-DATE-PREV ' '
                     CHANGE-REASON-CD-PREV ' ' EZT-DISP-EZ-5-0
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              END-IF

              IF TWO-RECORDS-FLAG = 'Y'
                 MOVE COV-OPTION-O1 TO COV-OPTION-O
                 MOVE COV-CATEGORY-O1 TO COV-CATEGORY-O
                 MOVE HISTORY-TYPE-O1 TO HISTORY-TYPE-O
                 MOVE COV-START-DATE-O1 TO COV-START-DATE-O
                 PERFORM 1-GET-NEW-BROKER-COV-ST-DATE
                 PERFORM 1-WRITE-BEUPDATE
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
                 MOVE 'N' TO FIRST-REC-FLAG
                 MOVE COV-OPTION-O2 TO COV-OPTION-O
                 MOVE COV-CATEGORY-O2 TO COV-CATEGORY-O
                 MOVE HISTORY-TYPE-O2 TO HISTORY-TYPE-O
                 MOVE COV-START-DATE-O2 TO COV-START-DATE-O

                 PERFORM 1-GET-NEW-BROKER-COV-ST-DATE
                 PERFORM 1-WRITE-BEUPDATE
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
               ELSE
                 PERFORM 1-GET-NEW-BROKER-COV-ST-DATE
                 PERFORM 1-WRITE-BEUPDATE
              END-IF
           .
       SECTION-EXIT.
      **AE2020-NEW BROKER CHANGES****
       1-GET-NEW-BROKER-COV-ST-DATE SECTION.
                 IF CHANGE-REASON-CD-O = 'OE' AND DC-PLAN-YEAR = 2020
                     AND DC-PLAN-NAME = 'MED' AND DC-INS-BROKER-ID = 23
                     AND WCOVERAGE-OPTION = '1' AND COV-OPTION-O NOT =
                     WCOVERAGE-OPTION AND WCOVERAGE-START-DT <=
                     '2020-01-01'
                    MOVE '2020-01-01' TO COV-START-DATE-O
                    MOVE '2020-01-01' TO ORIG-COV-DATE-O
                 END-IF
                 IF DC-PLAN-YEAR = 2020 AND DC-PLAN-NAME = 'MED' AND
                     DC-INS-BROKER-ID = 23 AND COV-START-DATE-O <=
                     '2020-01-01'
                    MOVE '2020-01-01' TO COV-START-DATE-O
                 END-IF
           .
      **AE2014-DP CHANGES BEGINS*****
      *=================================================================
      * 1. OPEN THE CURSOR
      * 2. FETCH RELATION_TYPE_CODE TO SEE IF THERE IS A DP
      * 3. CLOSE THE CURSOR
      *=================================================================
       1-GET-RELATION-TYPE-CD-CURSOR SECTION.

                 EXEC SQL OPEN GETRELCD
                 END-EXEC
                 IF SQLCODE = +0
                     CONTINUE
                 ELSE
                    MOVE '== BAD OPEN ON GETRELCD  CURSOR ==' TO
                        EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' PLAN YEAR = ' EZT-DISP-EZ-5-0
                        ' PLAN NAME = ' DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '*****' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE ' ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                    EXEC SQL ROLLBACK END-EXEC
                    MOVE EZT-RETURN-CODE TO RETURN-CODE
                    STOP RUN
                 END-IF

                 MOVE '     ' TO WS-RELATION-TYPE-CODE
      *****43-AE2014-DP-FIX CHANGES BEGINS****
                 MOVE 'N' TO WS-REL-TABLE-EOF-SW
                 PERFORM UNTIL NOT (WS-REL-TABLE-EOF-SW = 'N')
                 EXEC SQL FETCH GETRELCD
                     INTO :WS-RELATION-TYPE-CODE
                 END-EXEC
                 IF SQLCODE = 0
                   EVALUATE WS-RELATION-TYPE-CODE
                     WHEN 'DP   '
                       MOVE 'Y' TO WS-DP-FOUND-SW
                     WHEN 'SP   '
                       MOVE 'Y' TO WS-SP-FOUND-SW
                     WHEN 'S    '
                     WHEN 'D    '
                     WHEN 'SS   '
                     WHEN 'SD   '
                       MOVE 'Y' TO WS-LEG-DEP-FOUND-SW
                     WHEN 'PS   '
                     WHEN 'PD   '
                       MOVE 'Y' TO WS-DP-DEP-FOUND-SW
                   END-EVALUATE
                 ELSE
                    IF SQLCODE = +100
                       MOVE 'Y' TO WS-REL-TABLE-EOF-SW
                    ELSE
                       MOVE 'ABEND WHILE FETCHING  RELATION TYPE CODES'
                           TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE SPACES TO EZT-DISPLAY-STRING
                       MOVE SQLCODE TO EZT-DISP-EZ-10-0
                       STRING 'GETRELCD CURSOR SQLCODE: '
                           EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                           INTO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE ' ' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE SPACES TO EZT-DISPLAY-STRING
                       MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                       STRING 'PLAN YEAR = ' EZT-DISP-EZ-5-0
                           ' PLAN NAME = ' DC-PLAN-NAME
                           DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE
                           '********************************************
      -                   '*****' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                       EXEC SQL ROLLBACK END-EXEC
                       MOVE EZT-RETURN-CODE TO RETURN-CODE
                       STOP RUN
                    END-IF
                 END-IF
                 END-PERFORM

      *****43-AE2014-DP-FIX CHANGES ENDS  ****
                 EXEC SQL CLOSE GETRELCD
                 END-EXEC
                 IF SQLCODE = +0
                     CONTINUE
                 ELSE
                    MOVE '== BAD CLOSE ON GETRELCD CURSOR ==' TO
                        EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE ' ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' PLAN YEAR = ' EZT-DISP-EZ-5-0
                        ' PLAN NAME = ' DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '**' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                    EXEC SQL ROLLBACK END-EXEC
                    MOVE EZT-RETURN-CODE TO RETURN-CODE
                    STOP RUN
                 END-IF
           .

      *****************AE2014-DP CHANGES BEGINS*************************
       1-GET-RELTN-TYPE-CD-CRSR-CHNGE SECTION.

                 EXEC SQL OPEN GETRELDP
                 END-EXEC
                 IF SQLCODE = +0
                     CONTINUE
                 ELSE
                    MOVE '== BAD OPEN ON GETRELDP  CURSOR ==' TO
                        EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' PLAN YEAR = ' EZT-DISP-EZ-5-0
                        ' PLAN NAME = ' DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '*****' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE ' ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                    EXEC SQL ROLLBACK END-EXEC
                    MOVE EZT-RETURN-CODE TO RETURN-CODE
                    STOP RUN
                 END-IF

                 MOVE '     ' TO WS-RELATION-TYPE-CODE
      ******** 43-AE2014-DP-FIX CHANGES BEGINS***
                 MOVE 'N' TO WS-DEP-CHG-TABLE-EOF-SW
                 PERFORM UNTIL NOT (WS-DEP-CHG-TABLE-EOF-SW = 'N')
                 EXEC SQL FETCH GETRELDP
                     INTO :WS-RELATION-TYPE-CODE
                 END-EXEC
                 IF SQLCODE = 0
                   EVALUATE WS-RELATION-TYPE-CODE
                     WHEN 'DP   '
                       MOVE 'Y' TO WS-DP-FOUND-SW
                     WHEN 'SP   '
                       MOVE 'Y' TO WS-SP-FOUND-SW
                     WHEN 'S    '
                     WHEN 'D    '
                     WHEN 'SS   '
                     WHEN 'SD   '
                       MOVE 'Y' TO WS-LEG-DEP-FOUND-SW
                     WHEN 'PS   '
                     WHEN 'PD   '
                       MOVE 'Y' TO WS-DP-DEP-FOUND-SW
                   END-EVALUATE
                 ELSE
                    IF SQLCODE = +100
                       MOVE 'Y' TO WS-DEP-CHG-TABLE-EOF-SW
                    ELSE
                       MOVE 'ABEND WHILE FETCHING  RELATION TYPE CODES'
                           TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE SPACES TO EZT-DISPLAY-STRING
                       MOVE SQLCODE TO EZT-DISP-EZ-10-0
                       STRING 'GETRELCD CURSOR SQLCODE: '
                           EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                           INTO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE ' ' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE SPACES TO EZT-DISPLAY-STRING
                       MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                       STRING 'PLAN YEAR = ' EZT-DISP-EZ-5-0
                           ' PLAN NAME = ' DC-PLAN-NAME
                           DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE
                           '********************************************
      -                   '*****' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                       EXEC SQL ROLLBACK END-EXEC
                       MOVE EZT-RETURN-CODE TO RETURN-CODE
                       STOP RUN
                    END-IF
                 END-IF
                 END-PERFORM

      *****************43-AE2014-DP-FIX CHANGES ENDS****
                 EXEC SQL CLOSE GETRELDP
                 END-EXEC
                 IF SQLCODE = +0
                     CONTINUE
                 ELSE
                    MOVE '== BAD CLOSE ON GETRELDP CURSOR ==' TO
                        EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE ' ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' PLAN YEAR = ' EZT-DISP-EZ-5-0
                        ' PLAN NAME = ' DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '**' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                    EXEC SQL ROLLBACK END-EXEC
                    MOVE EZT-RETURN-CODE TO RETURN-CODE
                    STOP RUN
                 END-IF
           .

      * PROCESSING OF GETWINNBR CURSOR
      *=================================================================
       1-GET-WIN-NBR SECTION.
             EXEC SQL OPEN GETWINNBR
             END-EXEC
             IF SQLCODE = +0
                 CONTINUE
             ELSE
                MOVE '== BAD OPEN ON WIN_ASSOCIATE        TABLE ==' TO
                    EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE SPACES TO EZT-DISPLAY-STRING
                MOVE SQLCODE TO EZT-DISP-EZ-10-0
                STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                    DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE SPACES TO EZT-DISPLAY-STRING
                MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                STRING '== NATIONAL ID: ' EZT-DISP-EZ-10-0
                    DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE ' ' TO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                EXEC SQL ROLLBACK END-EXEC
                MOVE EZT-RETURN-CODE TO RETURN-CODE
                STOP RUN
             END-IF
      **AE2016 CHANGES BEGIN
             EXEC SQL FETCH GETWINNBR
                 INTO :WS-WIN-NBR,
                      :WASC-TYPE-CODE
             END-EXEC
             IF SQLCODE = +0
      ***YEYB-2015 CHANGES BEGINS
                IF WASC-TYPE-CODE = 'P' OR 'T'
                   MOVE 'Y' TO WS-PT-TRUCK
                END-IF
             ELSE
                IF SQLCODE = -305
                   MOVE 'N' TO WS-PT-TRUCK
      **AE2016 CHANGES END
      ***YEYB-2015 CHANGES ENDS
             ELSE
      ***** REAL TIME UPDATE AE2019 CHANGES BEGIN ***
                IF SQLCODE = 100
                   MOVE WSSN TO WSSN1
                   PERFORM 1-GET-WIN-NBRN
      ***** REAL TIME UPDATE AE2019 CHANGES END ***
                ELSE
                   MOVE SPACES TO EZT-DISPLAY-STRING
                   MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                   STRING
                       'ABEND WHILE FETCHING  WIN_NBR FOR NATIONAL ID  '
                        EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                       INTO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
                   MOVE SPACES TO EZT-DISPLAY-STRING
                   MOVE SQLCODE TO EZT-DISP-EZ-10-0
                   STRING 'GETWINNBR : SQL CODE ' EZT-DISP-EZ-10-0
                       DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
                   MOVE ' ' TO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
                   MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                   EXEC SQL ROLLBACK END-EXEC
                   MOVE EZT-RETURN-CODE TO RETURN-CODE
                   STOP RUN
                END-IF
      **AE2016 CHANGES BEGIN
                END-IF
      **AE2016 CHANGES END
             END-IF

             EXEC SQL CLOSE GETWINNBR
             END-EXEC
             IF SQLCODE = +0
                 CONTINUE
             ELSE
                MOVE '== BAD CLOSE ON WIN_ASSOCIATE        TABLE ==' TO
                    EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE SPACES TO EZT-DISPLAY-STRING
                MOVE SQLCODE TO EZT-DISP-EZ-10-0
                STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                    DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE ' ' TO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE SPACES TO EZT-DISPLAY-STRING
                MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                STRING '== NATIONAL ID: ' EZT-DISP-EZ-10-0
                    DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                EXEC SQL ROLLBACK END-EXEC
                MOVE EZT-RETURN-CODE TO RETURN-CODE
                STOP RUN
             END-IF
           .

      ***** REAL TIME UPDATE AE2019 CHANGES BEGIN ***

       1-GET-WIN-NBRN SECTION.
             EXEC SQL OPEN GETWINNBRNEW
             END-EXEC
             IF SQLCODE = +0
                 CONTINUE
             ELSE
                MOVE '== BAD OPEN ON WIN_ASSOCIATE NEW    TABLE ==' TO
                    EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE SPACES TO EZT-DISPLAY-STRING
                MOVE SQLCODE TO EZT-DISP-EZ-10-0
                STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                    DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE SPACES TO EZT-DISPLAY-STRING
                MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                STRING '== NATIONAL ID: ' EZT-DISP-EZ-10-0
                    DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE ' ' TO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                EXEC SQL ROLLBACK END-EXEC
                MOVE EZT-RETURN-CODE TO RETURN-CODE
                STOP RUN
             END-IF

             EXEC SQL FETCH GETWINNBRNEW
                 INTO :WS-WIN-NBR,
                      :WASC-TYPE-CODE
             END-EXEC
             IF SQLCODE = +0

                IF WASC-TYPE-CODE = 'P' OR 'T'
                   MOVE 'Y' TO WS-PT-TRUCK
                END-IF
             ELSE
                IF SQLCODE = -305
                   MOVE 'N' TO WS-PT-TRUCK

             ELSE
                MOVE SPACES TO EZT-DISPLAY-STRING
                MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                STRING
                    'ABEND WHILE FETCHING  WIN_NBR FOR NATIONAL ID  '
                    EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                    INTO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE SPACES TO EZT-DISPLAY-STRING
                MOVE SQLCODE TO EZT-DISP-EZ-10-0
                STRING 'GETWINNBR NEW: SQL CODE ' EZT-DISP-EZ-10-0
                    DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE ' ' TO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                EXEC SQL ROLLBACK END-EXEC
                MOVE EZT-RETURN-CODE TO RETURN-CODE
                STOP RUN

                END-IF

             END-IF

             EXEC SQL CLOSE GETWINNBRNEW
             END-EXEC
             IF SQLCODE = +0
                 CONTINUE
             ELSE
                MOVE '== BAD CLOSE ON WIN_ASSOCIATE NEW    TABLE ==' TO
                    EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE SPACES TO EZT-DISPLAY-STRING
                MOVE SQLCODE TO EZT-DISP-EZ-10-0
                STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                    DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE ' ' TO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE SPACES TO EZT-DISPLAY-STRING
                MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                STRING '== NATIONAL ID: ' EZT-DISP-EZ-10-0
                    DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                    AFTER ADVANCING 1 LINE
                MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                EXEC SQL ROLLBACK END-EXEC
                MOVE EZT-RETURN-CODE TO RETURN-CODE
                STOP RUN
             END-IF
           .

      ***** REAL TIME UPDATE AE2019 CHANGES END ***
      *=================================================================
      *THIS EVALUATES IF SPOUSE AND ASSOCIATE ARE BOTH OF SAME GENDER OR
      *NOT.IF BOTH ARE OF SAME GENDER THEN THAT SPOUSE IS PROCESSED AS A
      *=================================================================
       1-CHECK-IF-SPOUSE-IS-DP SECTION.

                 EXEC SQL OPEN GETGENDER
                 END-EXEC
                 IF SQLCODE = +0
                     CONTINUE
                 ELSE
                    MOVE '== BAD OPEN ON GETGENDER CURSOR ==' TO
                        EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE ' ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                        ' PLAN YEAR = ' EZT-DISP-EZ-5-0 ' PLAN NAME = '
                        DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '*****' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                    EXEC SQL ROLLBACK END-EXEC
                    MOVE EZT-RETURN-CODE TO RETURN-CODE
                    STOP RUN
                 END-IF

                 EXEC SQL FETCH GETGENDER
                     INTO :WS-GENDER-SAME-SW
                 END-EXEC
                 IF SQLCODE = 0
                    MOVE 'Y' TO WS-DP-FOUND-SW
                 ELSE
                    IF SQLCODE = +100
                       PERFORM 1-CHECK-IF-SPOUSE-IS-COMMON-LW
                    ELSE
                       MOVE 'ABEND WHILE FETCHING  GETGENDER CURSOR '
                           TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE SPACES TO EZT-DISPLAY-STRING
                       MOVE SQLCODE TO EZT-DISP-EZ-10-0
                       STRING 'GETGENDER : SQL CODE ' EZT-DISP-EZ-10-0
                           DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE ' ' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE SPACES TO EZT-DISPLAY-STRING
                       MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                       MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                       STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                           ' PLAN YEAR = ' EZT-DISP-EZ-5-0
                           ' PLAN NAME = ' DC-PLAN-NAME
                           DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE
                           '********************************************
      -                   '*****' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                       EXEC SQL ROLLBACK END-EXEC
                       MOVE EZT-RETURN-CODE TO RETURN-CODE
                       STOP RUN
                    END-IF
                 END-IF

                 EXEC SQL CLOSE GETGENDER
                 END-EXEC
                 IF SQLCODE = +0
                     CONTINUE
                 ELSE
                    MOVE '== BAD CLOSE ON GETGENDER CURSOR ==' TO
                        EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE ' ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                        ' PLAN YEAR = ' EZT-DISP-EZ-5-0 ' PLAN NAME = '
                        DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '**' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                    EXEC SQL ROLLBACK END-EXEC
                    MOVE EZT-RETURN-CODE TO RETURN-CODE
                    STOP RUN
                 END-IF
           .
      *=================================================================
      *THIS CHECKS IF UNION TYPE CODE OF THE SPOUSE IS COMMON LAW.
      *IF THE SPOUSE IS COMMON LAW THEN CONSIDER THEM AS DP
      *=================================================================
       1-CHECK-IF-SPOUSE-IS-COMMON-LW SECTION.

                 EXEC SQL OPEN GETUNIONCD
                 END-EXEC
                 IF SQLCODE = +0
                     CONTINUE
                 ELSE
                    MOVE '== BAD OPEN ON GETUNIONCD CURSOR ==' TO
                        EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE ' ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                        ' PLAN YEAR = ' EZT-DISP-EZ-5-0 ' PLAN NAME = '
                        DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '**' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                    EXEC SQL ROLLBACK END-EXEC
                    MOVE EZT-RETURN-CODE TO RETURN-CODE
                    STOP RUN
                 END-IF

                 EXEC SQL FETCH GETUNIONCD
                     INTO :WS-SP-COMMN-LAW-SW
                 END-EXEC
                 IF SQLCODE = 0
                    MOVE 'Y' TO WS-DP-FOUND-SW
                 ELSE
                    IF SQLCODE = +100
                        CONTINUE
                    ELSE
                       MOVE 'ABEND WHILE FETCHING  GETUNIONCD CURSOR '
                           TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE SPACES TO EZT-DISPLAY-STRING
                       MOVE SQLCODE TO EZT-DISP-EZ-10-0
                       STRING 'GETUNIONCD  : SQL CODE '
                           EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                           INTO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE ' ' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE SPACES TO EZT-DISPLAY-STRING
                       MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                       MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                       STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                           ' PLAN YEAR = ' EZT-DISP-EZ-5-0
                           ' PLAN NAME = ' DC-PLAN-NAME
                           DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE
                           '********************************************
      -                   '*****' TO EZT-DISPLAY-STRING
                       WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                           AFTER ADVANCING 1 LINE
                       MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                       EXEC SQL ROLLBACK END-EXEC
                       MOVE EZT-RETURN-CODE TO RETURN-CODE
                       STOP RUN
                    END-IF
                 END-IF

                 EXEC SQL CLOSE GETUNIONCD
                 END-EXEC
                 IF SQLCODE = +0
                     CONTINUE
                 ELSE
                    MOVE '== BAD CLOSE ON GETUNIONCD CURSOR ==' TO
                        EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE ' ' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                    MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                    STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                        ' PLAN YEAR = ' EZT-DISP-EZ-5-0 ' PLAN NAME = '
                        DC-PLAN-NAME DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE
                        '***********************************************
      -                '**' TO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE 1 TO EZT-RETURN-CODE
      *> STOP-EXECUTE
                    EXEC SQL ROLLBACK END-EXEC
                    MOVE EZT-RETURN-CODE TO RETURN-CODE
                    STOP RUN
                 END-IF
           .
      **AE2014-DP CHANGES ENDS**
       1-WRITE-BEUPDATE SECTION.
      *DMODALI CHECK FOR NULL VALUES
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
              IF PLAN-NAME-O = 'MED' OR 'VIS'
                 IF WINS-PROVIDER-ID-IND < 0
                    MOVE 0 TO WINS-PROVIDER-ID
                 END-IF
                 IF WINS-BROKER-ID-IND < 0
                    MOVE 0 TO WINS-BROKER-ID
                 END-IF
              END-IF

              IF LAST-CHANGE-ID-O NOT = 'WEBSITE '
                  MOVE 'ONLINE' TO LAST-CHANGE-ID-O
              END-IF
      **AE2014-VIS-CHANGES -- ADDED VISION TO PLAN NAME **
              IF WS-NEWLY-ELIGIBLE = 'N'
                IF PLAN-NAME-O = 'MED' OR 'DEN' OR 'ALI' OR 'ADD' OR
                    'VIS'
                  IF WCOVERAGE-OPTION = '1'
                    IF COV-OPTION-O NOT = WCOVERAGE-OPTION
                       MOVE 'Y' TO ENROLL-LATE-INT-O
                    END-IF
                  END-IF
                END-IF
              END-IF
              IF WS-NEWLY-ELIGIBLE = 'N'
                IF PLAN-NAME-O = 'DLI'
                  IF WCOVERAGE-OPTION = '1'
                    IF WCOVERAGE-CATEGORY = 'A'
                       IF COV-OPTION-O > WCOVERAGE-OPTION
                          MOVE 'Y' TO ENROLL-LATE-INT-O
                       ELSE
                         IF COV-CATEGORY-O > WCOVERAGE-CATEGORY
                            MOVE 'Y' TO ENROLL-LATE-INT-O
                         END-IF
                       END-IF
                    END-IF
                  END-IF
                END-IF
              END-IF
      ****29
                  MOVE DC-OPTION-CODE TO WS-COVERAGE-OPT-CMP-ONE
                  MOVE WCOVERAGE-OPTION TO WS-COVERAGE-OPT-CMP-TWO
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-INCREASED
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-DECREASED
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-EQUAL
                  PERFORM 1-COMPARE-COVERAGE-OPTIONS
              IF (PLAN-NAME-O = 'ALI' OR 'DLI') AND
                  WS-COVERAGE-OPT-ONE-INCREASED = 'Y'
                  IF WS-NEWLY-ELIGIBLE = 'Y'
                      IF PLAN-NAME-O = 'ALI'
                          MOVE 5 TO WS-LETTER-NBR
                          MOVE 'ALI GOOD HEALTH CHECK NE' TO
                              WS-MSG-TXT
                      END-IF
                      IF PLAN-NAME-O = 'DLI'
                          MOVE 6 TO WS-LETTER-NBR
                          MOVE 'DLI GOOD HEALTH CHECK NE' TO
                              WS-MSG-TXT
                      END-IF
                      PERFORM 1-PROOF-OF-GOOD-HEALTH
                      IF EZT-GOTO-STOP-JOB NOT = 'N'
                          GO TO SECTION-EXIT
                      END-IF
                      IF WS-GOOD-HEALTH-IND = 'Y'
                          IF WCOVERAGE-OPTION = '1'
                            IF CHANGE-REASON-CD-O = 'AE'
                             MOVE '1' TO COV-OPTION-O
                            ELSE
                             MOVE '2' TO COV-OPTION-O
                            END-IF
      **AE2014-DP CHANGES BEGINS**
                            IF CHANGE-REASON-CD-O = 'AE' AND
                                PLAN-NAME-O = 'DLI' AND WS-DP-FOUND-SW
                                = 'Y'
                             MOVE '2' TO COV-OPTION-O
                            END-IF
      **AE2014-DP CHANGES ENDS**
                          ELSE
                             MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                          END-IF
                          IF PLAN-NAME-O = 'DLI' AND CHANGE-REASON-CD-O
                              = 'MA' AND DC-OPTION-CODE > '2'
                             MOVE '2' TO COV-OPTION-O
                          END-IF
                      END-IF
                      IF WS-GOOD-HEALTH-IND = 'N'
      **AE2016 CHANGES BEGIN
                         IF CHANGE-REASON-CD-O = 'AE'
                            MOVE '1' TO COV-OPTION-O
                         ELSE
                            MOVE DC-OPTION-CODE TO COV-OPTION-O
                         END-IF
      **AE2016 CHANGES END
                      END-IF
                  END-IF

                  IF WS-NEWLY-ELIGIBLE = 'N'
                      IF PLAN-NAME-O = 'ALI'
                          MOVE 5 TO WS-LETTER-NBR
                          MOVE 'ALI GOOD HEALTH CHECK NOT NE' TO
                              WS-MSG-TXT
                      END-IF
                      IF PLAN-NAME-O = 'DLI'
                          MOVE 6 TO WS-LETTER-NBR
                          MOVE 'DLI GOOD HEALTH CHECK NOT NE' TO
                              WS-MSG-TXT
                      END-IF
                      PERFORM 1-PROOF-OF-GOOD-HEALTH
                      IF EZT-GOTO-STOP-JOB NOT = 'N'
                          GO TO SECTION-EXIT
                      END-IF
      *66 **AE2017-LUSUL1-CHANGES BEGINS ***
                      IF WS-GOOD-HEALTH-IND = 'Y'
                             MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                          IF PLAN-NAME-O = 'DLI' AND CHANGE-REASON-CD-O
                              = 'MA' AND DC-OPTION-CODE >= '2'
                             MOVE '2' TO COV-OPTION-O
                          END-IF
      *66 **AE2017-LUSUL1-CHANGES ENDS ***
                          IF PLAN-NAME-O = 'DLI' AND CHANGE-REASON-CD-O
                              = 'AE' AND WS-DP-FOUND-SW = 'Y' AND
                              WCOVERAGE-OPTION = '1' AND DC-OPTION-CODE
                              >= '2'
                             MOVE '2' TO COV-OPTION-O
                          END-IF
      **AE2014-DP CHANGES ENDS***
                      END-IF
                      IF WS-GOOD-HEALTH-IND = 'N'
                          MOVE DC-OPTION-CODE TO COV-OPTION-O
                      END-IF
                  END-IF
              END-IF

      ****AE2011-CHANGE BEGIN
              IF PLAN-NAME-O = 'DLI' AND DC-OPTION-CODE <
                  WCOVERAGE-OPTION AND FIRST-REC-FLAG = 'N'
                  MOVE DC-OPTION-CODE TO COV-OPTION-O
              END-IF
      ****AE2011-CHANGE END
      ***AE2014 CHANGES BEGINS***
              IF PLAN-NAME-O = 'ALI'
                  MOVE DC-OPTION-CODE TO WS-COVERAGE-OPT-CMP-ONE
                  MOVE WCOVERAGE-OPTION TO WS-COVERAGE-OPT-CMP-TWO
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-INCREASED
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-DECREASED
                  MOVE 'N' TO WS-COVERAGE-OPT-ONE-EQUAL
                  PERFORM 1-COMPARE-COVERAGE-OPTIONS
                  IF WS-COVERAGE-OPT-ONE-DECREASED = 'Y' AND
                      FIRST-REC-FLAG = 'N'
                      MOVE DC-OPTION-CODE TO COV-OPTION-O
                  END-IF
              END-IF
              MOVE DC-INS-BROKER-ID TO BEN-BKRID-O
      ***YEYB-2015 CHANGES BEGINS
      ** CHECK IF THE ASSOCIATE HAS CHANGE REASON CODE AS NE AND GOT REH
      ** OR REINSTATED PROVIDED WITH AN OPTION TO DO TRANSITION SESSION
      ** LOOKING INTO ASSOCIATE TRANSITION TABLE
      *60*68
              IF DC-OPTION-CODE = '1' AND WCOVERAGE-OPTION = '4' AND
                  PLAN-NAME-O = 'STD'
                 CONTINUE
              ELSE
                 PERFORM 1-CHECK-TRANSITION-TABLE
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
              END-IF
      *60*68
      ***YEYB-2015 CHANGES ENDS
      ****29
      **AE2016 CHANGES BEGIN
              PERFORM 1-ALI-DLI-PARTTIME-CHANGE
      **AE2016 CHANGES END
      **AE2018-LOA,RW CHANGES BEGINS
              IF ((PLAN-NAME-O = 'MED' OR 'DEN' OR 'VIS' OR 'ADD') AND
                  (CHANGE-REASON-CD-O = 'LA' OR 'TC') AND (COV-OPTION-O
                  = '1') AND (COV-OPTION-O NOT = WCOVERAGE-OPTION))

                 PERFORM 1-GET-EVENT-DATE-PLUS1
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
      **AE2022-ABEND FIX CHANGES**
                 IF EVENT-DATE-PLUS1-IND = -1
                    MOVE WS-EVENT-DATE-LD TO COV-START-DATE-O
                 ELSE
                    MOVE EVENT-DATE-PLUS1 TO COV-START-DATE-O
                 END-IF
                 MOVE 'TC' TO CHANGE-REASON-CD-O
              END-IF
      **AE2018-LOA,RW CHANGES ENDS
      **AE2020 CHANGES BEGIN
              PERFORM 1-WRITE-UPDATED-REC
      **AE2020 CHANGES END
      **AE2016 CHANGES BEGIN
              MOVE SSN-O TO EZT-TMP-NUM-FLD-B-10-0
              MOVE EZT-TMP-NUM-FLD-B-10-0 (1:10) TO WS-TEMP-SSN
      **AE2016 CHANGES END
           .
       SECTION-EXIT.

       1-WRITE-UPDATED-REC SECTION.
              IF ((DC-PLAN-NAME = 'MED') AND (DC-CATEGORY-CODE = ' ')
                  AND (DC-OPTION-CODE = '1') AND (WCOVERAGE-END-DT-IND
                  = 0) AND (WASC-PAY-CODE = '1') AND (WASC-JOB-CODE =
                  'T' OR 'P'))
                 CONTINUE
              ELSE
                 WRITE BEUPDATE FROM BEN-DATA
                 ADD 1 TO BEUPDATE-REK-COUNT
              END-IF
           .

      ****AE2018-LOA,RW CHANGES BEGINS
       1-GET-EVENT-DATE-PLUS1 SECTION.
            EXEC SQL OPEN CURSOR12
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO OPEN EVENT DATE CURSOR, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL FETCH CURSOR12
                INTO
                   :EVENT-DATE-PLUS1:EVENT-DATE-PLUS1-IND
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING ' SSN : ' WS-ENROLLEE-SSN DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO READ EVENT DATE CURSOR, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL CLOSE CURSOR12
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO CLOSE EVENT DATE CURSOR, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
      ****AE2018-LOA,RW CHANGES ENDS
      ***AE2016 CHANGES BEGIN
      *=================================================================
      *ALI DLI CHANGES FOR PART TIME/TEMPORARY ASSOCIATES FOR AE & NE SE
      *FOR 2016 PLAN YEAR
      *=================================================================
       1-ALI-DLI-PARTTIME-CHANGE SECTION.
             IF (((WASC-PAY-CODE = '1' OR '3' OR '6') AND WASC-JOB-CODE
                 = 'T' OR 'P') OR WS-PT-TRUCK = 'Y')
                IF WS-SESSION-DATE-YY = 2015 AND WCOVERAGE-START-DT <=
                    '2016-01-01'
                    IF DC-CHANGE-REASON-CODE = 'AE'
                      IF PLAN-NAME-O = 'ALI'
                         IF DC-OPTION-CODE NOT = '1' AND '2'
                            IF WCOVERAGE-OPTION < DC-OPTION-CODE
                             IF WCOVERAGE-OPTION = '1' OR '2'
                             MOVE '2' TO COV-OPTION-O
                             ELSE
                             MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                             END-IF
                            ELSE
                             MOVE DC-OPTION-CODE TO COV-OPTION-O
                            END-IF
                         ELSE
                            MOVE DC-OPTION-CODE TO COV-OPTION-O
                         END-IF
                         MOVE '2016-01-01' TO COV-START-DATE-O
                         MOVE '2016-01-01' TO ORIG-COV-DATE-O
                      ELSE
                         IF PLAN-NAME-O = 'DLI'
                            MOVE '2016-01-01' TO COV-START-DATE-O
                            MOVE '2016-01-01' TO ORIG-COV-DATE-O
                         END-IF
                      END-IF
                    ELSE
                      IF DC-CHANGE-REASON-CODE = 'NE' AND DC-PLAN-YEAR
                          = 2016 AND (PLAN-NAME-O = 'ALI' OR 'DLI')
                            MOVE '2016-01-01' TO COV-START-DATE-O
                            MOVE '2016-01-01' TO ORIG-COV-DATE-O
                      END-IF
                    END-IF
                 END-IF
              END-IF
           .
      ***AE2016 CHANGES END
      *=================================================================
      *COMPARE-COVERAGE-OPTIONS-USED TO COMPARE ALPHABETIC,NUMERIC OPTIO
      *=================================================================
       1-COMPARE-COVERAGE-OPTIONS SECTION.
              IF WS-COVERAGE-OPT-CMP-ONE NUMERIC AND
                  WS-COVERAGE-OPT-CMP-TWO NUMERIC
                      IF WS-COVERAGE-OPT-CMP-ONE >
                          WS-COVERAGE-OPT-CMP-TWO
                         MOVE 'Y' TO WS-COVERAGE-OPT-ONE-INCREASED
                      ELSE
                         IF WS-COVERAGE-OPT-CMP-ONE <
                             WS-COVERAGE-OPT-CMP-TWO
                            MOVE 'Y' TO WS-COVERAGE-OPT-ONE-DECREASED
                         ELSE
                            MOVE 'Y' TO WS-COVERAGE-OPT-ONE-EQUAL
                         END-IF
                      END-IF
              ELSE
                 IF WS-COVERAGE-OPT-CMP-ONE NUMERIC AND
                     WS-COVERAGE-OPT-CMP-TWO ALPHABETIC
                        MOVE 'Y' TO WS-COVERAGE-OPT-ONE-DECREASED
                 ELSE
                    IF WS-COVERAGE-OPT-CMP-ONE ALPHABETIC AND
                        WS-COVERAGE-OPT-CMP-TWO NUMERIC
                        MOVE 'Y' TO WS-COVERAGE-OPT-ONE-INCREASED
                    ELSE
                       IF WS-COVERAGE-OPT-CMP-ONE ALPHABETIC AND
                           WS-COVERAGE-OPT-CMP-TWO ALPHABETIC
                           IF WS-COVERAGE-OPT-CMP-ONE >
                               WS-COVERAGE-OPT-CMP-TWO
                             MOVE 'Y' TO WS-COVERAGE-OPT-ONE-INCREASED
                           ELSE
                             IF WS-COVERAGE-OPT-CMP-ONE <
                                 WS-COVERAGE-OPT-CMP-TWO
                             MOVE 'Y' TO WS-COVERAGE-OPT-ONE-DECREASED
                             ELSE
                             MOVE 'Y' TO WS-COVERAGE-OPT-ONE-EQUAL
                             END-IF
                           END-IF
                        END-IF
                      END-IF
                   END-IF
              END-IF
           .

       1-FORMAT-FAMILY-CHANGE-REC SECTION.
              IF SW-DEBUG-SWITCH = 'Y'
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-PLAN-YEAR TO EZT-DISP-EZ-5-0-3
                 STRING 'FSCREC ' EZT-DISP-EZ-5-0-3 ' ' WS-ENROLLEE-SSN
                     ' ' SESSION-DATE DC-CHANGE-EVENT-DATE ' '
                     DC-CHANGE-REASON-CODE DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              END-IF
              MOVE ' ' TO FAMCHG-REC
              MOVE DC-PLAN-YEAR TO FAMCHG-PLAN-YEAR
              MOVE DC-ENROLLEE-SSN TO FAMCHG-SSN
              MOVE SESSION-DATE TO FAMCHG-CHANGE-DATE
              IF DC-CHANGE-REASON-CODE = 'LE'
                      MOVE SPACES TO EZT-DISPLAY-STRING
                      MOVE SSN-O TO EZT-DISP-EZ-10-0
                      STRING 'SSN = ' EZT-DISP-EZ-10-0
                          DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                      MOVE SPACES TO EZT-DISPLAY-STRING
                      STRING 'DC-CHANGE-REASON-CODE = '
                          DC-CHANGE-REASON-CODE DELIMITED BY SIZE
                          INTO EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                      MOVE SPACES TO EZT-DISPLAY-STRING
                      STRING 'WS-ORIG-EVENT-DATE = ' WS-ORIG-EVENT-DATE
                          DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                      MOVE WS-ORIG-EVENT-DATE TO FAMCHG-FSC-DATE
                  MOVE WS-ORIG-EVENT-DATE TO FAMCHG-FSC-DATE
              ELSE
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODE 'DS'
                  IF (DC-CHANGE-REASON-CODE = 'DI' OR 'DS') AND
                      WS-EVENT-DATE-CHANGED = 'Y'
                      MOVE SPACES TO EZT-DISPLAY-STRING
                      MOVE SSN-O TO EZT-DISP-EZ-10-0
                      STRING 'SSN = ' EZT-DISP-EZ-10-0
                          DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                      MOVE SPACES TO EZT-DISPLAY-STRING
                      STRING 'DC-CHANGE-REASON-CODE = '
                          DC-CHANGE-REASON-CODE DELIMITED BY SIZE
                          INTO EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                      MOVE SPACES TO EZT-DISPLAY-STRING
                      STRING 'WS-ORIG-EVENT-DATE = ' WS-ORIG-EVENT-DATE
                          DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                      WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                          AFTER ADVANCING 1 LINE
                      MOVE WS-ORIG-EVENT-DATE TO FAMCHG-FSC-DATE
                  ELSE
                      MOVE DC-CHANGE-EVENT-DATE TO FAMCHG-FSC-DATE
                  END-IF
              END-IF
              MOVE DC-CHANGE-REASON-CODE TO FAMCHG-FSC-REASON
              MOVE 'I' TO FAMCHG-UPDATE-CODE
           .

       1-GET-PLAN-YEAR SECTION.
            EXEC SQL OPEN CURSOR1
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO OPEN PLAN_YEAR TABLE, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL FETCH CURSOR1
                INTO
                   :ANNUAL-ENROLLMENT,
                   :CURRENT-PLAN-YEAR,
                   :NEXT-PLAN-YEAR,
                   :PAY-PERIOD-BEGIN,
                   :NEXT-PAY-PERIOD-BEGIN,
                   :C-URRENT-DATE
            END-EXEC
              IF SQLCODE = 0
                 MOVE NEXT-PLAN-YEAR TO NEXT-PLAN-YY
              END-IF
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO READ PLAN_YEAR TABLE, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL CLOSE CURSOR1
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO CLOSE PLAN_YEAR TABLE, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.

       1-GET-ALI-DLI-AMOUNTS SECTION.
            MOVE 1 TO WS-ALI-OPT-IND-TY
            MOVE 1 TO WS-ALI-OPT-IND-NY
            MOVE 1 TO WS-DLI-OPT-IND-TY
            MOVE 1 TO WS-DLI-OPT-IND-NY
            MOVE 1 TO WS-DLI-CATG-IND-TY
            MOVE 1 TO WS-DLI-CATG-IND-NY
            EXEC SQL OPEN CURSOR6
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN COVERAGE_OPTION TABLE,  SQLCODE = '
                      EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            PERFORM UNTIL NOT (SW-LOAD-ALI-DLI = 'N')
            EXEC SQL FETCH CURSOR6
                INTO
                   :TPLAN-NAME,
                   :TPLAN-YEAR,
                   :TOPTION-CODE,
                   :TOPTION-AMT
            END-EXEC
              IF SQLCODE = 0
                 PERFORM 1-LOAD-ALI-DLI-TABLE
              END-IF
              IF SQLCODE = 100
                 MOVE 'Y' TO SW-LOAD-ALI-DLI
              END-IF
              IF SQLCODE NOT = 0 AND 100
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO READ COVERAGE_OPTION TABLE, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            END-PERFORM
            EXEC SQL CLOSE CURSOR6
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO CLOSE COVERAGE_OPTION, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.

       1-LOAD-ALI-DLI-TABLE SECTION.
              IF TPLAN-NAME = 'ALI' AND TPLAN-YEAR = CURRENT-PLAN-YEAR
                  MOVE TOPTION-CODE TO
                      TABLE-ALI-OPT-TY(WS-ALI-OPT-IND-TY)
                  MOVE TOPTION-AMT TO
                      TABLE-ALI-AMT-TY(WS-ALI-OPT-IND-TY)
                  COMPUTE WS-ALI-OPT-IND-TY = WS-ALI-OPT-IND-TY + 1
              END-IF
              IF TPLAN-NAME = 'ALI' AND TPLAN-YEAR = NEXT-PLAN-YEAR
                  MOVE TOPTION-CODE TO
                      TABLE-ALI-OPT-NY(WS-ALI-OPT-IND-NY)
                  MOVE TOPTION-AMT TO
                      TABLE-ALI-AMT-NY(WS-ALI-OPT-IND-NY)
                  COMPUTE WS-ALI-OPT-IND-NY = WS-ALI-OPT-IND-NY + 1
              END-IF
              IF TPLAN-NAME = 'DLI' AND TPLAN-YEAR = CURRENT-PLAN-YEAR
                  IF (TOPTION-CODE >= 'A' AND <= 'Z')
                      MOVE TOPTION-CODE TO
                          TABLE-DLI-CATG-TY(WS-DLI-CATG-IND-TY)
                      MOVE TOPTION-AMT TO
                          TABLE-DLI-CATG-AMT-TY(WS-DLI-CATG-IND-TY)
                   MOVE SPACES TO EZT-DISPLAY-STRING
                   STRING ' DLI TABLE ; '
                       TABLE-DLI-CATG-TY(WS-DLI-CATG-IND-TY)
                       DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
                   MOVE SPACES TO EZT-DISPLAY-STRING
                   MOVE TABLE-DLI-CATG-AMT-TY(WS-DLI-CATG-IND-TY) TO
                       EZT-DISP-N9-10-0
                   STRING EZT-DISP-N9-10-0 DELIMITED BY SIZE
                       INTO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
                      COMPUTE WS-DLI-CATG-IND-TY =
                          WS-DLI-CATG-IND-TY + 1
                  ELSE
                      MOVE TOPTION-CODE TO
                          TABLE-DLI-OPT-TY(WS-DLI-OPT-IND-TY)
                      MOVE TOPTION-AMT TO
                          TABLE-DLI-OPT-AMT-TY(WS-DLI-OPT-IND-TY)
                      COMPUTE WS-DLI-OPT-IND-TY =
                          WS-DLI-OPT-IND-TY + 1
                  END-IF
              END-IF
              IF TPLAN-NAME = 'DLI' AND TPLAN-YEAR = NEXT-PLAN-YEAR
                  IF (TOPTION-CODE >= 'A' AND <= 'Z')
                      MOVE TOPTION-CODE TO
                          TABLE-DLI-CATG-NY(WS-DLI-CATG-IND-NY)
                      MOVE TOPTION-AMT TO
                          TABLE-DLI-CATG-AMT-NY(WS-DLI-CATG-IND-NY)
                      COMPUTE WS-DLI-CATG-IND-NY =
                          WS-DLI-CATG-IND-NY + 1
                  ELSE
                      MOVE TOPTION-CODE TO
                          TABLE-DLI-OPT-NY(WS-DLI-OPT-IND-NY)
                      MOVE TOPTION-AMT TO
                          TABLE-DLI-OPT-AMT-NY(WS-DLI-OPT-IND-NY)
                      COMPUTE WS-DLI-OPT-IND-NY =
                          WS-DLI-OPT-IND-NY + 1
                  END-IF
              END-IF
           .

       1-CHECK-FOR-DENIAL-LETTER SECTION.
            MOVE DC-SESSION-TS TO EZT-DC-SESSION-TS
            EXEC SQL OPEN CURSOR4
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN ENROLLMENT_LETTER TABLE, SQLCODE ='
                      EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
              IF SW-DEBUG-SWITCH = 'Y'
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE SQLCODE TO EZT-DISP-EZ-10-0
                 STRING 'OPEN FOR DENIAL LETTER SQLCODE = '
                     EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              END-IF
            EXEC SQL FETCH CURSOR4
                INTO
                   :WSSN
            END-EXEC

              IF SW-DEBUG-SWITCH = 'Y'
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE SQLCODE TO EZT-DISP-EZ-10-0
                 STRING 'FETCH FOR DENIAL LETTER SQLCODE = '
                     EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              END-IF
              IF SQLCODE NOT = 0 AND +100
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO READ ENROLLMENT_LETTER TABLE, SQLCODE ='
                      EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF

              IF SQLCODE = 0
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING 'FETCH FOR DENIAL LETTER SQLCODE '
                        EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 MOVE 'Y' TO WS-DENIAL-LETTER
              END-IF

            EXEC SQL CLOSE CURSOR4
            END-EXEC
              IF SQLCODE NOT = 0
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING 'CLOSE FOR DENIAL LETTER SQLCODE = '
                        EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE ENROLLMENT_LETTER TABLE, SQLCODE =
      -             '' EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
      ***** 29-
       1-CHECK-GOOD-HEALTH-LETTER SECTION.
            MOVE DC-SESSION-TS TO EZT-DC-SESSION-TS
            EXEC SQL OPEN CURSOR4A
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'OPEN FAILED ON ENROLLMENT_LETTER TABLE, SQLCODE ='
                      EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
              IF SW-DEBUG-SWITCH = 'Y'
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE SQLCODE TO EZT-DISP-EZ-10-0
                 STRING 'OPEN PROOF OF GOOD HEALTH LETTER SQLCODE = '
                     EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              END-IF
            EXEC SQL FETCH CURSOR4A
                INTO
                   :WSSN
            END-EXEC

              IF SW-DEBUG-SWITCH = 'Y'
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE SQLCODE TO EZT-DISP-EZ-10-0
                 STRING 'FETCH PROOF OF GOOD HEALTH LETTER SQLCODE = '
                     EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              END-IF
              IF SQLCODE NOT = 0 AND +100
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'READ FAILED ON ENROLLMENT_LETTER TABLE, SQLCODE ='
                      EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF

              IF SQLCODE = 0
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING 'FETCH PROOF OF GOOD HEALTH LETTER SQLCODE '
                        EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 MOVE 'Y' TO WS-GOOD-HEALTH-IND
              END-IF

            EXEC SQL CLOSE CURSOR4A
            END-EXEC
              IF SQLCODE NOT = 0
                 IF SW-DEBUG-SWITCH = 'Y'
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE SQLCODE TO EZT-DISP-EZ-10-0
                    STRING
                        'CLOSE PROOF OF GOOD HEALTH LETTER SQLCODE = '
                        EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                        INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                 END-IF
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE ENROLLMENT_LETTER TABLE, SQLCODE =
      -             '' EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
      ***** 29-
       1-CALCULATE-MAX-DLI SECTION.
                 PERFORM 1-WAGE-HISTORY-CURSOR
                 IF EZT-GOTO-STOP-JOB NOT = 'N'
                     GO TO SECTION-EXIT
                 END-IF
                 IF DC-ENROLLEE-SSN = WS-ALI-SSN
                    MOVE WS-ALI-OPT TO WS-ALI-COVERAGE-OPTION
                 END-IF
                 MOVE 'N' TO WS-FOUND-SW
                 MOVE 0 TO WS-ALI-AMT
                 IF PLAN-YEAR-O = CURRENT-PLAN-YEAR
                    COMPUTE WS-COUNTER = WS-ALI-OPT-IND-TY - 1
                    PERFORM UNTIL NOT (WS-FOUND-SW = 'N')
                       IF WS-ALI-COVERAGE-OPTION =
                           TABLE-ALI-OPT-TY(WS-COUNTER)
                           MOVE TABLE-ALI-AMT-TY(WS-COUNTER) TO
                               WS-ALI-AMT
                           MOVE 'Y' TO WS-FOUND-SW
                       ELSE
                           COMPUTE WS-COUNTER = WS-COUNTER - 1
                           IF WS-COUNTER = 0
                             MOVE 'Y' TO WS-FOUND-SW
                           END-IF
                       END-IF
                    END-PERFORM
                 ELSE
                    IF PLAN-YEAR-O = NEXT-PLAN-YEAR
                       COMPUTE WS-COUNTER = WS-ALI-OPT-IND-NY - 1
                       PERFORM UNTIL NOT (WS-FOUND-SW = 'N')
                         IF WS-ALI-COVERAGE-OPTION =
                             TABLE-ALI-OPT-NY(WS-COUNTER)
                             MOVE TABLE-ALI-AMT-NY(WS-COUNTER) TO
                                 WS-ALI-AMT
                             MOVE 'Y' TO WS-FOUND-SW
                         ELSE
                             COMPUTE WS-COUNTER = WS-COUNTER - 1
                             IF WS-COUNTER = 0
                             MOVE 'Y' TO WS-FOUND-SW
                             END-IF
                         END-IF
                       END-PERFORM
                    END-IF
                 END-IF

                 IF WS-TOTAL-PAY-PERIOD > 0 AND WS-TOTAL-PAY-PERIOD <
                     26
                     COMPUTE EZT-WS-TOTAL-WAGES ROUNDED
                       = (WS-TOTAL-WAGES * 26 / WS-TOTAL-PAY-PERIOD)
                     MOVE EZT-WS-TOTAL-WAGES TO WS-TOTAL-WAGES
                 END-IF
                 IF WS-TOTAL-WAGES > 50000
                     MOVE 50000 TO WS-TOTAL-WAGES
                 END-IF

                 IF WS-TOTAL-WAGES < 50000
      *              MOVE WS-TOTAL-WAGES TO WS-TOTAL-WAGES-4
                     MOVE WS-TOTAL-WAGES-3 TO WS-WAGES-N
                     IF WS-WAGES-X2 >= 500
                        COMPUTE WS-WAGES-X1 = WS-WAGES-X1 + 1
                        MOVE 0 TO WS-WAGES-X2
                     ELSE
                        MOVE 0 TO WS-WAGES-X2
                     END-IF
                     MOVE WS-WAGES-N TO WS-TOTAL-WAGES
                 END-IF
                 IF WSTATE-PROVINCE-CD = 'MD'
                     COMPUTE WS-MAX-DLI-AMT =
                         (WS-TOTAL-WAGES + WS-ALI-AMT)
                 ELSE
                     COMPUTE WS-MAX-DLI-AMT =
                         (WS-TOTAL-WAGES + WS-ALI-AMT) / 2
                 END-IF
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-MAX-DLI-AMT TO EZT-DISP-EZ-10-0
                 STRING ' WS-MAX-DLI-AMT : ' EZT-DISP-EZ-10-0
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 'N' TO WS-FOUND-SW
                 MOVE '1' TO WS-MAX-DLI-OPT
                 IF PLAN-YEAR-O = CURRENT-PLAN-YEAR
                    COMPUTE WS-COUNTER = WS-DLI-OPT-IND-TY - 1
                    PERFORM UNTIL NOT (WS-FOUND-SW = 'N')
                       IF WS-MAX-DLI-AMT >=
                           TABLE-DLI-OPT-AMT-TY(WS-COUNTER)
                           MOVE TABLE-DLI-OPT-TY(WS-COUNTER) TO
                               WS-MAX-DLI-OPT
                           MOVE 'Y' TO WS-FOUND-SW
                       ELSE
                           COMPUTE WS-COUNTER = WS-COUNTER - 1
                           IF WS-COUNTER = 0
                             MOVE 'Y' TO WS-FOUND-SW
                           END-IF
                       END-IF
                    END-PERFORM
                 ELSE
                    IF PLAN-YEAR-O = NEXT-PLAN-YEAR
                       COMPUTE WS-COUNTER = WS-DLI-OPT-IND-NY - 1
                       PERFORM UNTIL NOT (WS-FOUND-SW = 'N')
                         IF WS-MAX-DLI-AMT >=
                             TABLE-DLI-OPT-AMT-NY(WS-COUNTER)
                             MOVE TABLE-DLI-OPT-NY(WS-COUNTER) TO
                                 WS-MAX-DLI-OPT
                             MOVE 'Y' TO WS-FOUND-SW
                         ELSE
                             COMPUTE WS-COUNTER = WS-COUNTER - 1
                             IF WS-COUNTER = 0
                             MOVE 'Y' TO WS-FOUND-SW
                             END-IF
                         END-IF
                       END-PERFORM
                    END-IF
                 END-IF

                 MOVE 'N' TO WS-FOUND-SW
                 MOVE 'A' TO WS-MAX-DLI-CATG
                 IF PLAN-YEAR-O = CURRENT-PLAN-YEAR
                    COMPUTE WS-COUNTER = WS-DLI-CATG-IND-TY - 1
                    PERFORM UNTIL NOT (WS-FOUND-SW = 'N')
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE WS-COUNTER TO EZT-DISP-N9-2-0
                    STRING 'WS-COUNTER : ' EZT-DISP-N9-2-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE WS-DLI-CATG-IND-TY TO EZT-DISP-EZ-2-0
                    STRING 'WS-DLI-CATG-IND-TY : ' EZT-DISP-EZ-2-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE TABLE-DLI-CATG-AMT-TY(1) TO EZT-DISP-N9-10-0
                    STRING 'DLI AMT1' EZT-DISP-N9-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE TABLE-DLI-CATG-AMT-TY(2) TO EZT-DISP-N9-10-0
                    STRING 'DLI AMT2' EZT-DISP-N9-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE TABLE-DLI-CATG-AMT-TY(3) TO EZT-DISP-N9-10-0
                    STRING 'DLI AMT3' EZT-DISP-N9-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                    MOVE SPACES TO EZT-DISPLAY-STRING
                    MOVE TABLE-DLI-CATG-AMT-TY(4) TO EZT-DISP-N9-10-0
                    STRING 'DLI AMT4' EZT-DISP-N9-10-0
                        DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                    WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                        AFTER ADVANCING 1 LINE
                       IF WS-MAX-DLI-AMT >=
                           TABLE-DLI-CATG-AMT-TY(WS-COUNTER)
                           MOVE TABLE-DLI-CATG-TY(WS-COUNTER) TO
                               WS-MAX-DLI-CATG
                           MOVE 'Y' TO WS-FOUND-SW
                           MOVE SPACES TO EZT-DISPLAY-STRING
                           MOVE WS-COUNTER TO EZT-DISP-N9-2-0
                           STRING 'WS-COUNTER -X :' EZT-DISP-N9-2-0
                               DELIMITED BY SIZE
                               INTO EZT-DISPLAY-STRING
                           WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                               AFTER ADVANCING 1 LINE
                           MOVE SPACES TO EZT-DISPLAY-STRING
                           MOVE TABLE-DLI-CATG-AMT-TY(WS-COUNTER) TO
                               EZT-DISP-N9-10-0
                           STRING EZT-DISP-N9-10-0 DELIMITED BY SIZE
                               INTO EZT-DISPLAY-STRING
                           WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                               AFTER ADVANCING 1 LINE
                       ELSE
                           COMPUTE WS-COUNTER = WS-COUNTER - 1
                           IF WS-COUNTER = 0
                             MOVE 'Y' TO WS-FOUND-SW
                           END-IF
                           MOVE SPACES TO EZT-DISPLAY-STRING
                           MOVE WS-COUNTER TO EZT-DISP-N9-2-0
                           STRING 'WS-COUNTER -X :' EZT-DISP-N9-2-0
                               DELIMITED BY SIZE
                               INTO EZT-DISPLAY-STRING
                           WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                               AFTER ADVANCING 1 LINE
                           MOVE SPACES TO EZT-DISPLAY-STRING
                           MOVE TABLE-DLI-CATG-AMT-TY(WS-COUNTER) TO
                               EZT-DISP-N9-10-0
                           STRING EZT-DISP-N9-10-0 DELIMITED BY SIZE
                               INTO EZT-DISPLAY-STRING
                           WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                               AFTER ADVANCING 1 LINE
                       END-IF
                    END-PERFORM
                 ELSE
                    IF PLAN-YEAR-O = NEXT-PLAN-YEAR
                       COMPUTE WS-COUNTER = WS-DLI-CATG-IND-NY - 1
                       PERFORM UNTIL NOT (WS-FOUND-SW = 'N')
                         IF WS-MAX-DLI-AMT >=
                             TABLE-DLI-CATG-AMT-NY(WS-COUNTER)
                             MOVE TABLE-DLI-CATG-NY(WS-COUNTER) TO
                                 WS-MAX-DLI-CATG
                             MOVE 'Y' TO WS-FOUND-SW
                         ELSE
                             COMPUTE WS-COUNTER = WS-COUNTER - 1
                             IF WS-COUNTER = 0
                             MOVE 'Y' TO WS-FOUND-SW
                             END-IF
                         END-IF
                       END-PERFORM
                    END-IF
                 END-IF
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 STRING 'WS-FOUND-SW  : ' WS-FOUND-SW DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
           .
       SECTION-EXIT.

       1-WAGE-HISTORY-CURSOR SECTION.
            EXEC SQL OPEN CURSOR5
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN CURSOR5              SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            MOVE 0 TO WS-TOTAL-WAGES
            EXEC SQL FETCH CURSOR5
                INTO :WS-TOTAL-WAGES:WS-NULL-IND
            END-EXEC

              IF SQLCODE NOT = 0 AND 100
                  MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 'ERROR FETHING CURSOR5                        '
                     TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO READ WAGEF_HISTORY_T   , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                 STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                     ' PLAN YEAR = ' EZT-DISP-EZ-5-0 ' PLAN NAME = '
                     DC-PLAN-NAME DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
              EXEC SQL CLOSE CURSOR5
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE CURSOR5           , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL OPEN CURSOR5A
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN CURSOR5A             SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            MOVE 0 TO WS-TOTAL-PAY-PERIOD
            EXEC SQL FETCH CURSOR5A
                INTO :WS-TOTAL-PAY-PERIOD
            END-EXEC

              IF SQLCODE NOT = 0 AND 100
                  MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 'ERROR FETCHING CURSOR5A                      '
                     TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO READ WAGEF_HISTORY_T   , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 MOVE LOOKUP-PLAN-YEAR-N TO EZT-DISP-EZ-5-0
                 STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                     ' PLAN YEAR = ' EZT-DISP-EZ-5-0 ' PLAN NAME = '
                     DC-PLAN-NAME DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
              EXEC SQL CLOSE CURSOR5A
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE CURSOR5A          , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
      **29
       1-PROOF-OF-GOOD-HEALTH SECTION.
               MOVE 'N' TO WS-GOOD-HEALTH-IND
               MOVE DC-ENROLLEE-SSN TO WSSN
               IF SW-DEBUG-SWITCH = 'Y'
                   MOVE SPACES TO EZT-DISPLAY-STRING
                   MOVE WSSN TO EZT-DISP-EZ-17-0
                   STRING WS-MSG-TXT ' ' EZT-DISP-EZ-17-0 ' '
                       DC-SESSION-TS DELIMITED BY SIZE
                       INTO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
               END-IF
               PERFORM 1-CHECK-GOOD-HEALTH-LETTER
           .
      **29
      ***21- START
       1-GET-EVENT-DATE SECTION.
               MOVE 'A' TO WMD-FUNCT
               MOVE '1' TO WMD-FORMAT
               MOVE DC-CHANGE-EVENT-DATE TO WMD-CAL-DATE
               CALL 'WMDATE' USING WMDATE-DATA
               IF WMD-STAT = ' '
                   CONTINUE
               ELSE
                   MOVE 'CALL TO WMDATE ROUTINE FAILED #1' TO
                       EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
               END-IF
               COMPUTE WMD-DAY = WMD-DAY + 1
               MOVE 'B' TO WMD-FUNCT
               CALL 'WMDATE' USING WMDATE-DATA
               IF WMD-STAT = ' '
                   CONTINUE
               ELSE
                   MOVE 'CALL TO WMDATE ROUTINE FAILED #2' TO
                       EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
               END-IF
               MOVE WMD-FORMAT-DATE TO WS-DI-LE-EVENT-DATE
               MOVE WS-DI-LE-EVENT-DATE TO DC-CHANGE-EVENT-DATE
           .
      ***21- ENDS

       1-SET-LAST-CHANGE-ID SECTION.
            MOVE DC-SESSION-TS TO EZT-DC-SESSION-TS
            EXEC SQL OPEN CURSOR9
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN CURSOR9              SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            MOVE 'N' TO WS-LAST-CHANGE-ID
            EXEC SQL FETCH CURSOR9
                INTO :WS-LAST-CHANGE-ID
            END-EXEC
      *DISPLAY 'WSSN : '   WSSN
      *DISPLAY 'DC-SESSION-TS : ' DC-SESSION-TS
      *DISPLAY 'WS-LAST-CHANGE-ID : ' WS-LAST-CHANGE-ID
              IF SQLCODE = 0
                 IF WS-LAST-CHANGE-ID = 'Y'
                    MOVE DC-CHANGE-REASON-CODE TO WS-USERID-CHG-REASON
                    MOVE WS-MY-BEN-USERID TO LAST-CHANGE-ID-O
                 ELSE
                    MOVE 'WEBSITE ' TO LAST-CHANGE-ID-O
                 END-IF
              END-IF
              IF SQLCODE NOT = 0 AND 100
                  MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 'ERROR FETHING CURSOR9                        '
                     TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO READ ENROLLMENT_SESSION, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
              EXEC SQL CLOSE CURSOR9
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE CURSOR9           , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
      *** CHECK STP COVERAGE
       1-CHECK-STP-COVERAGE SECTION.
            EXEC SQL OPEN CURSOR10
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO OPEN ASSO_INS_ENROLL, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL FETCH CURSOR10
                INTO :WS-STP-OPTION,
                     :WS-STP-CATEGORY
            END-EXEC

              IF SQLCODE = +100
                 MOVE ' ' TO WS-STP-OPTION
                 MOVE ' ' TO WS-STP-CATEGORY
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 STRING 'NO STP RECORD FOR SSN = ' EZT-DISP-EZ-10-0
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
              ELSE
                IF SQLCODE NOT = 0
                   MOVE SQLCODE TO WS-SQL-DISPLAY
                   MOVE
                       '************************************************
      -               '****' TO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
                   MOVE SPACES TO EZT-DISPLAY-STRING
                   MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                   STRING 'FAILED TO READ ASSOC_INS_ENROLL, SQLCODE = '
                       EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                       INTO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
                   MOVE SPACES TO EZT-DISPLAY-STRING
                   MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                   STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                       DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
                   MOVE
                       '************************************************
      -               '****' TO EZT-DISPLAY-STRING
                   WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                       AFTER ADVANCING 1 LINE
                   MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                   MOVE 'S' TO EZT-GOTO-STOP-JOB
                   GO TO SECTION-EXIT
                END-IF
              END-IF

              EXEC SQL CLOSE CURSOR10
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO CLOSE ASSOC_INS_ENROLL, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.

      ****AE2011-CHANGES BEGIN
       1-GET-SESSION-DATE-PLUS1 SECTION.
            EXEC SQL OPEN CURSOR11
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN SESSION DATE CURSOR, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL FETCH CURSOR11
                INTO
                   :SESSION-DATE-PLUS1
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-ENROLLEE-SSN-BI TO EZT-DISP-EZ-10-0
                 STRING ' SSN : ' EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO READ SESSION DATE CURSOR, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL CLOSE CURSOR11
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE SESSION DATE CURSOR, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
      ****AE2011-CHANGES END

       1-GET-COVRG-DATE-NYEAR SECTION.
            EXEC SQL OPEN CURSOR2B
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN COVERAGE DATE CURSOR2B, SQLCODE = '
                      EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL FETCH CURSOR2B
                INTO
                   :COVRG-DATE-NYEAR
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-ENROLLEE-SSN-BI TO EZT-DISP-EZ-10-0
                 STRING ' SSN : ' EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO READ COVERAGE DATE CURSOR2B, SQLCODE = '
                      EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL CLOSE CURSOR2B
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE COVERAGE DATE CURSOR2B, SQLCODE =
      -             '' EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
       1-CREATE-TWO-DLI-COV-RECORDS SECTION.
      *69 JEDHA1 BA2018 CHANGES ADDED CHANGE REASON CODE 'DS'
              IF CHANGE-REASON-CD-O = 'DI' OR 'DS'
                    MOVE DC-CHANGE-EVENT-DATE TO
                        WS-SPOUSE-COV-START-DATE
                ELSE
                  IF WS-NEWLY-ELIGIBLE = 'N'
                    MOVE SESSION-DATE-PLUS1 TO
                        WS-SPOUSE-COV-START-DATE
                  ELSE
                    MOVE WS-WAIT-PERIOD-END TO
                        WS-SPOUSE-COV-START-DATE
                  END-IF
              END-IF

              IF DC-CATEGORY-CODE > WCOVERAGE-CATEGORY
                 MOVE SESSION-DATE TO WS-CHILD-COV-START-DATE
              ELSE
                 MOVE SESSION-DATE-PLUS1 TO WS-CHILD-COV-START-DATE
              END-IF

              IF WS-CHILD-COV-START-DATE < WS-SPOUSE-COV-START-DATE
                 MOVE 'Y' TO TWO-RECORDS-FLAG
                 MOVE 'Y' TO FIRST-REC-FLAG
                 MOVE WCOVERAGE-OPTION TO COV-OPTION-O1
                 MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O1
                 MOVE SPACES TO HISTORY-TYPE-O1
                 MOVE WS-CHILD-COV-START-DATE TO COV-START-DATE-O1

                 MOVE DC-OPTION-CODE TO COV-OPTION-O2
                 MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O2
                 MOVE 'E' TO HISTORY-TYPE-O2
                 MOVE WS-SPOUSE-COV-START-DATE TO COV-START-DATE-O2
              ELSE
                 IF WS-CHILD-COV-START-DATE > WS-SPOUSE-COV-START-DATE
                    MOVE 'Y' TO TWO-RECORDS-FLAG
                    MOVE DC-OPTION-CODE TO COV-OPTION-O1
                    MOVE WCOVERAGE-CATEGORY TO COV-CATEGORY-O1
                    MOVE SPACES TO HISTORY-TYPE-O1
                    MOVE WS-SPOUSE-COV-START-DATE TO COV-START-DATE-O1

                    MOVE DC-OPTION-CODE TO COV-OPTION-O2
                    MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O2
                    MOVE 'E' TO HISTORY-TYPE-O2
                    MOVE WS-CHILD-COV-START-DATE TO COV-START-DATE-O2
                 ELSE
                    MOVE DC-OPTION-CODE TO COV-OPTION-O
                    MOVE DC-CATEGORY-CODE TO COV-CATEGORY-O
                    MOVE WS-SPOUSE-COV-START-DATE TO COV-START-DATE-O
                 END-IF
              END-IF

              IF WS-NEWLY-ELIGIBLE = 'Y'
                 IF TWO-RECORDS-FLAG = 'Y'
                    IF COV-START-DATE-O1 < WS-WAIT-PERIOD-END
                       MOVE WS-WAIT-PERIOD-END TO COV-START-DATE-O1
                    END-IF
                    IF COV-START-DATE-O2 < WS-WAIT-PERIOD-END
                       MOVE WS-WAIT-PERIOD-END TO COV-START-DATE-O2
                    END-IF
                 END-IF
                 IF TWO-RECORDS-FLAG = 'N'
                    IF COV-START-DATE-O < WS-WAIT-PERIOD-END
                       MOVE WS-WAIT-PERIOD-END TO COV-START-DATE-O
                    END-IF
                 END-IF
              END-IF
           .

       1-POPULATE-COV-START-DT SECTION.
              IF WS-NEW-PICKUP-IND = 'Y'
                 IF WS-OPT-CAT-DECREASE = 'N'
                    IF SESSION-DATE >= WCOVERAGE-START-DT
                       MOVE SESSION-DATE TO COV-START-DATE-O
                       MOVE SESSION-DATE TO ORIG-COV-DATE-O
                     ELSE
                       MOVE WS-WAIT-PERIOD-END TO COV-START-DATE-O
                       MOVE WS-WAIT-PERIOD-END TO ORIG-COV-DATE-O
                    END-IF
                  ELSE
                    IF SESSION-DATE >= WCOVERAGE-START-DT
                       MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                       MOVE SESSION-DATE-PLUS1 TO ORIG-COV-DATE-O
                     ELSE
                       MOVE WS-WAIT-PERIOD-END TO COV-START-DATE-O
                       MOVE WS-WAIT-PERIOD-END TO ORIG-COV-DATE-O
                    END-IF
                  END-IF
              ELSE
                 IF WS-OPT-CAT-DECREASE = 'N'
                    IF SESSION-DATE >= WCOVERAGE-START-DT
                       MOVE SESSION-DATE TO COV-START-DATE-O
                     ELSE
                       MOVE WS-WAIT-PERIOD-END TO COV-START-DATE-O
                    END-IF
                  ELSE
                    IF SESSION-DATE >= WCOVERAGE-START-DT
                       MOVE SESSION-DATE-PLUS1 TO COV-START-DATE-O
                     ELSE
                       MOVE WS-WAIT-PERIOD-END TO COV-START-DATE-O
                    END-IF
                  END-IF
              END-IF
           .
      ***YEYB-2015 CHANGES BEGINS
      *
      ******************************************************************
      * GET THE WIN NBR OF THE ASSOCIATE PROCESSED AND PROCESS TRANSITIO
      * CURSOR.
      ******************************************************************
       1-CHECK-TRANSITION-TABLE SECTION.
      *
                IF DC-CHANGE-REASON-CODE = 'NE'
                   PERFORM 1-GET-WIN-NBR
                   PERFORM 1-PROCESS-TRANSITION-CSR
                END-IF
           .
      *
      ******************************************************************
      * THIS PARA PROCESSES THE TRANSITION CURSOR
      ******************************************************************
       1-PROCESS-TRANSITION-CSR SECTION.
      *
                PERFORM 1-OPEN-TRANSITION-CSR
                IF EZT-GOTO-STOP-JOB NOT = 'N'
                    GO TO SECTION-EXIT
                END-IF
                PERFORM 1-FETCH-TRANSITION-CSR
                IF EZT-GOTO-STOP-JOB NOT = 'N'
                    GO TO SECTION-EXIT
                END-IF
                PERFORM 1-CLOSE-TRANSITION-CSR
      *
           .
       SECTION-EXIT.
      *
      ******************************************************************
      * THIS PARA OPENS THE TRANSITION CURSOR AND EVALUATES THE SQL CODE
      ******************************************************************
       1-OPEN-TRANSITION-CSR SECTION.
      *
                EXEC SQL  OPEN TRANSITION
                END-EXEC
                EVALUATE SQLCODE
                     WHEN +0
                          CONTINUE
                     WHEN OTHER
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE '== BAD OPEN ON TRANSITION CURSOR ==' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE SQLCODE TO EZT-DISP-EZ-10-0
                          STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WSSN TO EZT-DISP-EZ-17-0
                          STRING ' ------SSN = ' EZT-DISP-EZ-17-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                          MOVE 'S' TO EZT-GOTO-STOP-JOB
                END-EVALUATE
      *
           .
      *
      ******************************************************************
      * THIS PARA FETCHES THE TRANSITION CURSOR AND EVALUATES SQL CODE
      ******************************************************************
       1-FETCH-TRANSITION-CSR SECTION.
      *
      **JOB TRANSITION CHANGES BEGINS **
                EXEC SQL FETCH  TRANSITION
                    INTO  :WS-WIN
                         ,:WTRANS-DATE
                         ,:WTRANS-RSTRCT-CODE
                         ,:WTRAN-CHANGE-REASON-CODE
                         ,:WTRANS-TYPE-CODE
                END-EXEC
      **JOB TRANSITION CHANGES ENDS   **
      *
                EVALUATE SQLCODE
                     WHEN 0
                          IF WCOVERAGE-START-DT <= WTRANS-DATE
                             PERFORM 1-OVERLOAD-COVERAGE-ST-DATE
                             IF EZT-GOTO-STOP-JOB NOT = 'N'
                                 GO TO SECTION-EXIT
                             END-IF
                          END-IF
                     WHEN +100
                          CONTINUE
                     WHEN OTHER
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              'ABEND WHILE FETCHING  TRANSITION CURSOR '
                               TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE SQLCODE TO EZT-DISP-EZ-10-0
                          STRING 'TRANSITION  : SQL CODE '
                              EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                              INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WSSN TO EZT-DISP-EZ-17-0
                          STRING ' ------SSN = ' EZT-DISP-EZ-17-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                          MOVE 'S' TO EZT-GOTO-STOP-JOB
                END-EVALUATE
      *
           .
       SECTION-EXIT.
      *
      ******************************************************************
      * THIS PARA CLOSES THE TRANSITION CURSOR AND EVALUATES THE SQL COD
      ******************************************************************
       1-CLOSE-TRANSITION-CSR SECTION.
      *
                EXEC SQL CLOSE TRANSITION
                END-EXEC
                EVALUATE SQLCODE
                     WHEN +0
                          CONTINUE
                     WHEN OTHER
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE '== BAD CLOSE ON TRANSITION CURSOR =='
                              TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE SQLCODE TO EZT-DISP-EZ-10-0
                          STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WSSN TO EZT-DISP-EZ-17-0
                          STRING ' ------SSN = ' EZT-DISP-EZ-17-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                          MOVE 'S' TO EZT-GOTO-STOP-JOB
                END-EVALUATE
      *
           .
      *
      ******************************************************************
      * POPULATES THE COVERAGE START DATE, ORIGINAL COVERAGE DATE FOR
      * ASSOCIATES WHO HAS BEEN PROVIDED WITH TRANSITION SESSION
      ******************************************************************
       1-OVERLOAD-COVERAGE-ST-DATE SECTION.
      *
                IF SESSION-DATE > WCOVERAGE-START-DT
                   MOVE WTRANS-DATE TO COV-START-DATE-O
                ELSE
                   MOVE WCOVERAGE-START-DT TO COV-START-DATE-O
                END-IF
      *
      **JOB TRANSITION CHANGES BEGINS **
                EVALUATE WTRAN-CHANGE-REASON-CODE
      *AMANI1 CHANGES STARTS
                     WHEN 'TS'
                          IF WTRANS-RSTRCT-CODE = 1 AND (PLAN-NAME-O =
                              'STD' OR 'LTD')
      *60            AND PLAN-NAME-O        NE 'MED' 'VIS'
                             MOVE COV-START-DATE-O TO ENROLL-DATE-O
                             PERFORM 1-CALCULATE-WAIT-DAYS
                             IF EZT-GOTO-STOP-JOB NOT = 'N'
                                 GO TO SECTION-EXIT
                             END-IF
                          END-IF
                          MOVE COV-START-DATE-O TO ORIG-COV-DATE-O
      *         WHEN 'TS'
      *AMANI1 CHANGES ENDS
                     WHEN 'TN'
      **JOB TRANSITION CHANGES ENDS   **
                          IF WTRANS-RSTRCT-CODE = 1 AND (PLAN-NAME-O =
                              'STD' OR 'LTD')
      *60            AND PLAN-NAME-O        NE 'MED' 'VIS'
                             MOVE COV-START-DATE-O TO ENROLL-DATE-O
                             PERFORM 1-CALCULATE-WAIT-DAYS
                             IF EZT-GOTO-STOP-JOB NOT = 'N'
                                 GO TO SECTION-EXIT
                             END-IF
                          ELSE
                             CONTINUE
                          END-IF
                          MOVE COV-START-DATE-O TO ORIG-COV-DATE-O
      **JOB TRANSITION CHANGES ENDS   **
      **JOB TRANSITION CHANGES BEGINS **
                     WHEN OTHER
      **55 JOB TRANSITION 2016 CHANGES BEGIN **
                          IF (WTRANS-RSTRCT-CODE = 3 OR
                              (WTRANS-RSTRCT-CODE = 1 AND (PLAN-NAME-O
                              = 'STD' OR 'LTD')))
      **             AND (PLAN-NAME-O        NE 'MED' 'VIS')))
                             MOVE WTRANS-DATE TO ENROLL-DATE-O
                             MOVE COV-START-DATE-O TO ORIG-COV-DATE-O
                          END-IF
      *
                          IF ((WTRAN-CHANGE-REASON-CODE = 'RS' OR 'RN')
                              AND WCOVERAGE-OPTION = '1' AND
                              (PLAN-NAME-O NOT = 'STD' AND 'LTD'))
      **             AND (PLAN-NAME-O        = 'MED' 'VIS'))
                             MOVE COV-START-DATE-O TO ORIG-COV-DATE-O
                             MOVE 'Y' TO ENROLL-LATE-INT-O
                          END-IF

                          IF (PLAN-NAME-O = 'DLI' AND DC-OPTION-CODE >
                              WCOVERAGE-OPTION)
                             IF WCOVERAGE-OPTION = '1'
                             MOVE '2' TO COV-OPTION-O
                             ELSE
                             MOVE WCOVERAGE-OPTION TO COV-OPTION-O
                             END-IF
                          END-IF
                END-EVALUATE
      **55 JOB TRANSITION 2016 CHANGES END **
      **JOB TRANSITION CHANGES ENDS   **
           .
       SECTION-EXIT.
      *
      ******************************************************************
      * THIS PARA PROCESSES THE WAITDAYS CURSOR
      ******************************************************************
       1-CALCULATE-WAIT-DAYS SECTION.
                PERFORM 1-OPEN-WAIT-DAYS-CURSOR
                IF EZT-GOTO-STOP-JOB NOT = 'N'
                    GO TO SECTION-EXIT
                END-IF
                PERFORM 1-FETCH-WAIT-DAYS-CURSOR
                IF EZT-GOTO-STOP-JOB NOT = 'N'
                    GO TO SECTION-EXIT
                END-IF
                PERFORM 1-CLOSE-WAIT-DAYS-CURSOR
      *
           .
       SECTION-EXIT.
      *
      ******************************************************************
      * THIS PARA OPENS THE WAITDAYS CURSOR AND EVALUATES THE SQL CODE
      ******************************************************************
       1-OPEN-WAIT-DAYS-CURSOR SECTION.
      *
                MOVE COV-START-DATE-O TO EZT-COV-START-DATE-O
                EXEC SQL  OPEN WAITDAYS
                END-EXEC
                EVALUATE SQLCODE
                     WHEN +0
                          CONTINUE
                     WHEN OTHER
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE '== BAD OPEN ON WAITDAYS CURSOR ==' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE SQLCODE TO EZT-DISP-EZ-10-0
                          STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WSSN TO EZT-DISP-EZ-17-0
                          STRING ' ------SSN = ' EZT-DISP-EZ-17-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                          MOVE 'S' TO EZT-GOTO-STOP-JOB
                END-EVALUATE
      *
           .
      *
      ******************************************************************
      * THIS PARA FETCHES THE WAITDAYS CURSOR AND EVALUATES THE SQL CODE
      ******************************************************************
       1-FETCH-WAIT-DAYS-CURSOR SECTION.
      *
                EXEC SQL  FETCH WAITDAYS
                    INTO :WS-WAIT-DAYS
                END-EXEC
                EVALUATE SQLCODE
                     WHEN +0
                          MOVE WS-WAIT-DAYS TO WAIT-DAY-QTY-O
                     WHEN +100
                          CONTINUE
                     WHEN OTHER
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 'ABEND WHILE FETCHING  WAITDAYS CURSOR '
                              TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE SQLCODE TO EZT-DISP-EZ-10-0
                          STRING 'WAITDAYS    : SQL CODE '
                              EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                              INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WSSN TO EZT-DISP-EZ-17-0
                          STRING ' ------SSN = ' EZT-DISP-EZ-17-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                          MOVE 'S' TO EZT-GOTO-STOP-JOB
                END-EVALUATE
      *
           .
      *
      ******************************************************************
      * THIS PARA CLOSES THE WAITDAYS CURSOR AND EVALUATES THE SQL CODE
      ******************************************************************
       1-CLOSE-WAIT-DAYS-CURSOR SECTION.
      *
                EXEC SQL  CLOSE WAITDAYS
                END-EXEC
                EVALUATE SQLCODE
                     WHEN +0
                          CONTINUE
                     WHEN OTHER
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE '== BAD CLOSE ON WAITDAYS CURSOR ==' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE SQLCODE TO EZT-DISP-EZ-10-0
                          STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WSSN TO EZT-DISP-EZ-17-0
                          STRING ' ------SSN = ' EZT-DISP-EZ-17-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                          MOVE 'S' TO EZT-GOTO-STOP-JOB
                END-EVALUATE
      *
           .
      *
      *54 YEYB2016 CHANGES START
      ******************************************************************
      *TO CALCULATE COVERAGE START DATE AS EVENT DAYS + 365 DAYS FOR STD
      ******************************************************************
       1-GET-COVERAGE-START-DT SECTION.
      *
                PERFORM 1-OPEN-GETCOVST-CURSOR
                IF EZT-GOTO-STOP-JOB NOT = 'N'
                    GO TO SECTION-EXIT
                END-IF
                PERFORM 1-FETCH-GETCOVST-CURSOR
                IF EZT-GOTO-STOP-JOB NOT = 'N'
                    GO TO SECTION-EXIT
                END-IF
                PERFORM 1-CLOSE-GETCOVST-CURSOR
      *
           .
       SECTION-EXIT.

      ******************************************************************
      * THIS PARA OPENS THE GETCOVST CURSOR AND EVALUATES THE SQL CODE
      ******************************************************************
       1-OPEN-GETCOVST-CURSOR SECTION.
      *
                EXEC SQL  OPEN GETCOVST
                END-EXEC
                EVALUATE SQLCODE
                     WHEN +0
                          CONTINUE
                     WHEN OTHER
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE '== BAD OPEN ON GETCOVST CURSOR ==' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE SQLCODE TO EZT-DISP-EZ-10-0
                          STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WSSN TO EZT-DISP-EZ-17-0
                          STRING ' ------SSN = ' EZT-DISP-EZ-17-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                          MOVE 'S' TO EZT-GOTO-STOP-JOB
                END-EVALUATE
      *
           .
      *
      ******************************************************************
      * THIS PARA FETCHES THE GETCOVST CURSOR AND EVALUATES THE SQL CODE
      ******************************************************************
       1-FETCH-GETCOVST-CURSOR SECTION.
      *
                EXEC SQL  FETCH GETCOVST
                    INTO :WS-COVERAGE-START-DT
                END-EXEC
                EVALUATE SQLCODE
                     WHEN +0
                          MOVE WS-COVERAGE-START-DT TO
                              COV-START-DATE-O
                     WHEN +100
                          CONTINUE
                     WHEN OTHER
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 'ABEND WHILE FETCHING  GETCOVST CURSOR '
                              TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE SQLCODE TO EZT-DISP-EZ-10-0
                          STRING 'GETCOVST    : SQL CODE '
                              EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                              INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WSSN TO EZT-DISP-EZ-17-0
                          STRING ' ------SSN = ' EZT-DISP-EZ-17-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                          MOVE 'S' TO EZT-GOTO-STOP-JOB
                END-EVALUATE
      *
           .
      *
      ******************************************************************
      * THIS PARA CLOSES THE GETCOVST CURSOR AND EVALUATES THE SQL CODE
      ******************************************************************
       1-CLOSE-GETCOVST-CURSOR SECTION.
      *
                EXEC SQL  CLOSE GETCOVST
                END-EXEC
                EVALUATE SQLCODE
                     WHEN +0
                          CONTINUE
                     WHEN OTHER
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE '== BAD CLOSE ON GETCOVST CURSOR ==' TO
                              EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE SQLCODE TO EZT-DISP-EZ-10-0
                          STRING '== SQLCODE: ' EZT-DISP-EZ-10-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE SPACES TO EZT-DISPLAY-STRING
                          MOVE WSSN TO EZT-DISP-EZ-17-0
                          STRING ' ------SSN = ' EZT-DISP-EZ-17-0
                              DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE
                              '*****************************************
      -                      '**' TO EZT-DISPLAY-STRING
                          WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                              AFTER ADVANCING 1 LINE
                          MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                          MOVE 'S' TO EZT-GOTO-STOP-JOB
                END-EVALUATE
      *
           .
      *54 YEYB2016 CHANGES END


      *54 YEYB2016 CHANGES START
       1-GET-ASSOC-INS-DATA SECTION.
            MOVE DC-PLAN-NAME TO EZT-DC-PLAN-NAME
            EXEC SQL OPEN STDLTD
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN STDLTD  DATE CURSOR, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
            EXEC SQL FETCH STDLTD
                INTO
                   :WS-SSN1,
                   :WS-BEN-OPTION,
                   :WS-PLAN-NAME,
                   :WS-EFF-DT,
                   :WS-ORIG-COV-DT :WS-ORIG-COV-DT-IND,
                   :WS-COV-END-DT :WS-COV-END-DT-IND,
                   :WS-WAIT-DAY-QTY,
                   :WS-ENROLL-LT-IND
            END-EXEC
              IF SQLCODE = 0
                     MOVE WS-SSN1 TO WSSN
                     MOVE WS-BEN-OPTION TO WCOVERAGE-OPTION
                     MOVE WS-PLAN-NAME TO WPLAN-NAME
                     MOVE WS-COV-END-DT TO WCOVERAGE-END-DT
                     MOVE WS-EFF-DT TO WCOVERAGE-START-DT
                     MOVE WS-ORIG-COV-DT TO WORIG-COVERAGE-DATE
                     MOVE WS-WAIT-DAY-QTY TO WWAIT-DAY-QTY
                     MOVE WS-ENROLL-LT-IND TO WENROLL-LATE-INT

              ELSE
                  IF SQLCODE = +100
                     CONTINUE
                  ELSE
                     MOVE SQLCODE TO WS-SQL-DISPLAY
                     MOVE SPACES TO EZT-DISPLAY-STRING
                     MOVE WSSN TO EZT-DISP-EZ-17-0
                     STRING ' SSN : ' EZT-DISP-EZ-17-0
                         DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                     WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                         AFTER ADVANCING 1 LINE
                     MOVE
                         '**********************************************
      -                 '******' TO EZT-DISPLAY-STRING
                     WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                         AFTER ADVANCING 1 LINE
                     MOVE SPACES TO EZT-DISPLAY-STRING
                     MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                     STRING 'FAILED TO READ STLTD CURSOR, SQLCODE = '
                         EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                         INTO EZT-DISPLAY-STRING
                     WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                         AFTER ADVANCING 1 LINE
                     MOVE
                         '**********************************************
      -                 '******' TO EZT-DISPLAY-STRING
                     WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                         AFTER ADVANCING 1 LINE
                     MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                     MOVE 'S' TO EZT-GOTO-STOP-JOB
                     GO TO SECTION-EXIT
                  END-IF
              END-IF

            EXEC SQL CLOSE STDLTD
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING 'FAILED TO CLOSE STDLTD CURSOR, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
      *54 YEYB2016 CHANGES END
      *63 SPECIAL ENROLLMENT CHANGES START
       1-GET-PREV-SESSION-SAME-DAY SECTION.

            MOVE DC-CHANGE-REASON-CODE TO EZT-DC-CHANGE-REASON-CODE
            MOVE DC-SESSION-TS TO EZT-DC-SESSION-TS
            EXEC SQL OPEN PREVSES
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN ENROLLMENT SESSION  , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF

            EXEC SQL FETCH PREVSES
                INTO :WS-CHANGE-REASON-CODE,
                     :WS-EVENT-TYPE-CODE,
                     :WS-EVENT-DATE-LD-SE :WS-EVENT-DATE-LD-SE-IND,
                     :WS-SESSION-TS
            END-EXEC
              IF SQLCODE NOT = 0 AND +100
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO READ ENROLLMENT SESSION  , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                     ' SESSION TS= ' DC-SESSION-TS DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
              IF SQLCODE = +100
                 MOVE ' ' TO WS-CHANGE-REASON-CODE
                 MOVE 0 TO WS-EVENT-TYPE-CODE
              END-IF

              EXEC SQL CLOSE PREVSES
              END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE ENROLLMENT_SESSION  , SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
      *63 SPECIAL ENROLLMENT CHANGES END

      *68 YEYB17 CHANGES START
       1-CHECK-DISTRANS-CURSOR SECTION.

            EXEC SQL OPEN DISTRANS
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO OPEN ASSOCIATE_TRANSITION, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF

            EXEC SQL FETCH DISTRANS
                INTO :WS-DISTRANS-SW
            END-EXEC
              IF SQLCODE NOT = 0 AND +100
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO READ ASSOCIATE_TRANSITION, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE DC-ENROLLEE-SSN TO EZT-DISP-EZ-10-0
                 MOVE WS-WIN-NBR TO EZT-DISP-EZ-10-0-1
                 STRING ' ------SSN = ' EZT-DISP-EZ-10-0
                     ' --WIN NBR = ' EZT-DISP-EZ-10-0-1
                     DELIMITED BY SIZE INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
                 GO TO SECTION-EXIT
              END-IF
              IF SQLCODE = +100
                 MOVE 'N' TO WS-DISTRANS-SW
              END-IF

            EXEC SQL CLOSE DISTRANS
            END-EXEC
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO WS-SQL-DISPLAY
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE SPACES TO EZT-DISPLAY-STRING
                 MOVE WS-SQL-DISPLAY TO EZT-DISP-EZ-4-0
                 STRING
                     'FAILED TO CLOSE ASSOCIATE_TRANSITION, SQLCODE = '
                     EZT-DISP-EZ-4-0 DELIMITED BY SIZE
                     INTO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE
                     '**************************************************
      -             '**' TO EZT-DISPLAY-STRING
                 WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                     AFTER ADVANCING 1 LINE
                 MOVE 1 TO EZT-RETURN-CODE
      *> STOP
                 MOVE 'S' TO EZT-GOTO-STOP-JOB
              END-IF
           .
       SECTION-EXIT.
      *68 YEYB17 CHANGES END

      ****AE2014-CHANGE BEGIN
       1-PROCESS-HOUSEKEEPING SECTION.
               PERFORM Z-READ-ENRDATES-FILE
               MOVE ENROLL-START-DATE TO WS-ENR-START-DT
               MOVE ENROLL-END-DATE TO WS-ENR-END-DT
           .
      ****AE2014-CHANGE END

       1-DISPLAY-COUNTS SECTION.
              MOVE
                  '*****************************************************
      -          '**' TO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
              MOVE
                  'S U C C E S S F U L   E N D   O F   B E 3 0 1 0
      -          '  ' TO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
              MOVE
                  '*****************************************************
      -          '**' TO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
              MOVE
                  '        PROGRAM RECORD COUNTS
      -          '  ' TO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
              MOVE
                  '-----------------------------------------------------
      -          '--' TO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
              MOVE SPACES TO EZT-DISPLAY-STRING
              MOVE WEB-COUNT TO EZT-DISP-EZ-10-0
              STRING 'WEB ENROLLMENT RECORDS READ   = '
                  EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                  INTO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
              MOVE SPACES TO EZT-DISPLAY-STRING
              MOVE AE-RECS TO EZT-DISP-EZ-10-0
              STRING 'RECORDS FROM ANNUAL ENROLL    = '
                  EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                  INTO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
              MOVE SPACES TO EZT-DISPLAY-STRING
              MOVE OE-RECS TO EZT-DISP-EZ-10-0
              STRING 'RECORDS FROM ONGOING ENROLL   = '
                  EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                  INTO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
              MOVE SPACES TO EZT-DISPLAY-STRING
              MOVE FSC-RECS TO EZT-DISP-EZ-10-0
              STRING 'FAMILY STATUS CHANGE RECORDS  = '
                  EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                  INTO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
              MOVE SPACES TO EZT-DISPLAY-STRING
              MOVE TOT-RECS-WRITTEN TO EZT-DISP-EZ-10-0
              STRING 'TOTAL RECORDS WRITTEN         = '
                  EZT-DISP-EZ-10-0 DELIMITED BY SIZE
                  INTO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
              MOVE
                  '*****************************************************
      -          '**' TO EZT-DISPLAY-STRING
              WRITE SYSPRINT FROM EZT-DISPLAY-STRING
                  AFTER ADVANCING 1 LINE
           .
       Z-READ-DEPDATA-FILE SECTION.
           READ DEPDATA-FILE
              AT END MOVE 'Y' TO DEPDATA-EOF
              NOT AT END MOVE 'N' TO DEPDATA-EOF
                     ADD 1 TO DEPDATA-REK-COUNT
           END-READ
           .
       Z-READ-ENRDATES-FILE SECTION.
           READ ENRDATES-FILE
              AT END MOVE 'Y' TO ENRDATES-EOF
              NOT AT END MOVE 'N' TO ENRDATES-EOF
                     ADD 1 TO ENRDATES-REK-COUNT
           END-READ
           .
