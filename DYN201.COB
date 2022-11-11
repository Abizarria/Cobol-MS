       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN201.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCTR        ASSIGN  TO  DISK.

           SELECT      CADCXA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CXA.

           SELECT      CADPRS        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-PRS.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCTR      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCTR.DAT".
       01  REG-CTR.
           03  DIA-CTR          PIC  9(02).
           03  DTI-CTR          PIC  9(06).
           03  DTC-CTR          PIC  9(06).
           03  DTD-CTR          PIC  9(06).
           03  FAT-CTR          PIC  9(06).
           03  PCT-CTR          PIC  9(02)V99.

       FD  CADCXA      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCXA.DAT".
       01  REG-CXA.
           03  CHV-CXA.
               05  TUR-CXA      PIC  9(01).
               05  NUM-CXA      PIC  9(01).
           03  POS-CXA          PIC  9(01).
           03  DTA-CXA          PIC  9(06).
           03  DES-CXA          PIC  X(15).

       FD  CADPRS      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADPRS.DAT".
       01  REG-PRS.
           03  CHV-PRS.
               05  TUR-PRS      PIC  9(01).
               05  CXA-PRS      PIC  9(01).
               05  ATV-PRS      PIC  9(01).
               05  LIN-PRS      PIC  9(03).
           03  DES-PRS          PIC  X(15).
           03  PRX-PRS          PIC  9(01).
           03  QTD-PRS          PIC  9(06)V9.
           03  CTD-PRS          PIC  9(10)V99.
           03  VDD-PRS          PIC S9(10)V99.
           03  QTM-PRS          PIC  9(07)V9.
           03  CTM-PRS          PIC  9(11)V99.
           03  VDM-PRS          PIC S9(11)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  NUM-OPC          PIC  9(01).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  TUR-AUX          PIC  X(05).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  TAB-TUR.
               05  FILLER       PIC  X(05)  VALUE  "MANHA".
               05  FILLER       PIC  X(05)  VALUE  "TARDE".
               05  FILLER       PIC  X(05)  VALUE  "NOITE".
           03  RED-TUR          REDEFINES   TAB-TUR.
               05  TUR-TAB      PIC  X(05)  OCCURS  03.

           03  TAB-POS.
               05  FILLER       PIC  X(07)  VALUE  "ABERTO ".
               05  FILLER       PIC  X(07)  VALUE  "FECHADO".
           03  RED-POS          REDEFINES   TAB-POS.
               05  POS-TAB      PIC  X(07)  OCCURS  02.

           03  TAB-CXA          OCCURS  10.
               05  CHV-TBC      PIC  9(02).
               05  TUR-TBC      PIC  X(05).
               05  DES-TBC      PIC  X(15).
               05  POS-TBC      PIC  X(07).

           03  DET-001.
               05  NUM-DT1      PIC  9(02).
               05  FILLER       PIC  X(03)  VALUE  SPACES.
               05  DES-DT1      PIC  X(18).
               05  TUR-DT1      PIC  X(08).
               05  POS-DT1      PIC  X(07).

           03  LIN-001          PIC  X(38).
           03  LIN-002          PIC  X(38).
           03  LIN-003          PIC  X(38).
           03  LIN-004          PIC  X(38).
           03  LIN-005          PIC  X(38).
           03  LIN-006          PIC  X(38).
           03  LIN-007          PIC  X(38).
           03  LIN-008          PIC  X(38).
           03  LIN-009          PIC  X(38).

       01  PRM-001              PIC  9(01).

       01  PRM-002.
           03  TUR-PR2          PIC  9(01).
           03  CXA-PR2          PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 01 PIC X(67) FROM
               " MOVIMENTO DE CAIXAS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 05 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 04 PIC X(74) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 08 COLUMN 17 PIC X(46) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 21 PIC X(38) FROM
               "N§   DESCRICAO         TURNO   POSICAO"
               BACKGROUND-COLOR  01        FOREGROUND-COLOR 14.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(46) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 19 PIC X(46) FROM SPACES.

       01  TLA-003.
           03  LINE 08 COLUMN 17 PIC X(46) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 21 PIC X(38) FROM
               "N§   DESCRICAO         TURNO   POSICAO"
               BACKGROUND-COLOR  01        FOREGROUND-COLOR 14.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(46) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 19 PIC X(46) FROM SPACES.

       01  TLA-004.
           03  LINE 07 COLUMN 17 PIC X(46) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 21 PIC X(38) FROM
               "N§   DESCRICAO         TURNO   POSICAO"
               BACKGROUND-COLOR  01        FOREGROUND-COLOR 14.
           03  LINE 08 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(46) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 19 PIC X(46) FROM SPACES.

       01  TLA-005.
           03  LINE 07 COLUMN 17 PIC X(46) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 21 PIC X(38) FROM
               "N§   DESCRICAO         TURNO   POSICAO"
               BACKGROUND-COLOR  01        FOREGROUND-COLOR 14.
           03  LINE 08 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 17 PIC X(46) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 19 PIC X(46) FROM SPACES.

       01  TLA-006.
           03  LINE 06 COLUMN 17 PIC X(46) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 21 PIC X(38) FROM
               "N§   DESCRICAO         TURNO   POSICAO"
               BACKGROUND-COLOR  01        FOREGROUND-COLOR 14.
           03  LINE 07 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 17 PIC X(46) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 19 PIC X(46) FROM SPACES.

       01  TLA-007.
           03  LINE 06 COLUMN 17 PIC X(46) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 21 PIC X(38) FROM
               "N§   DESCRICAO         TURNO   POSICAO"
               BACKGROUND-COLOR  01        FOREGROUND-COLOR 14.
           03  LINE 07 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 17 PIC X(46) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 19 PIC X(46) FROM SPACES.

       01  TLA-008.
           03  LINE 06 COLUMN 17 PIC X(46) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 21 PIC X(38) FROM
               "N§   DESCRICAO         TURNO   POSICAO"
               BACKGROUND-COLOR  01        FOREGROUND-COLOR 14.
           03  LINE 07 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 17 PIC X(46) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR 01.
           03  LINE 18 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 19 PIC X(46) FROM SPACES.

       01  TLA-009.
           03  LINE 05 COLUMN 17 PIC X(46) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR 01.
           03  LINE 06 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 06 COLUMN 21 PIC X(38) FROM
               "N§   DESCRICAO         TURNO   POSICAO"
               BACKGROUND-COLOR  01        FOREGROUND-COLOR 14.
           03  LINE 06 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 07 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 17 PIC X(46) FROM " ³
               "                     ³"    BACKGROUND-COLOR 01.
           03  LINE 17 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 17 PIC X(46) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR 01.
           03  LINE 18 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 19 PIC X(46) FROM SPACES.

       01  TLA-010.
           03  LINE 09 COLUMN 28 PIC X(25) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "¿"  BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 28 PIC X(25) FROM " ³
               "³"  BACKGROUND-COLOR 04.
           03  LINE 10 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 28 PIC X(25) FROM " ³  CAIXA FECHADO !!!
               "³"  BACKGROUND-COLOR 04.
           03  LINE 11 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 28 PIC X(25) FROM " ³
               "³"  BACKGROUND-COLOR 04.
           03  LINE 12 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 28 PIC X(25) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "Ù"  BACKGROUND-COLOR 04.
           03  LINE 13 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 30 PIC X(25) FROM SPACES.

       01  TLA-011.
           03  LINE 10 COLUMN 28 PIC X(24) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 28 PIC X(24) FROM " ³  Data .:   /  /    ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 28 PIC X(24) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 52 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 30 PIC X(24) FROM SPACES.

       01  TLA-012.
           03  LINE 03 COLUMN 02 PIC X(14) FROM "TURNO .:"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.
           03  LINE 03 COLUMN 11 PIC X(06) USING TUR-AUX
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.
           03  LINE 03 COLUMN 17 PIC X(11) FROM "- CAIXA .:"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.
           03  LINE 03 COLUMN 28 PIC X(15) USING DES-CXA
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.

       01  TLA-013.
           03  LINE 03 COLUMN 72 PIC 9(02) USING DIA-SYS
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.
           03  LINE 03 COLUMN 74 PIC X(01) FROM "/"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.
           03  LINE 03 COLUMN 75 PIC 9(02) USING MES-SYS
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.
           03  LINE 03 COLUMN 77 PIC X(01) FROM "/"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.
           03  LINE 03 COLUMN 78 PIC 9(02) USING ANO-SYS
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.

       01  ENT-001.
           03  DIA-ENT LINE 11 COLUMN 40 PIC 9(02) USING DIA-SYS
                       BACKGROUND-COLOR  04.
           03  MES-ENT LINE 11 COLUMN 43 PIC 9(02) USING MES-SYS
                       BACKGROUND-COLOR  04.
           03  ANO-ENT LINE 11 COLUMN 46 PIC 9(02) USING ANO-SYS
                       BACKGROUND-COLOR  04.

       01  ENT-002.
           03  LINE 11 COLUMN 21 PIC X(38) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 21 PIC X(38) USING LIN-002
               BACKGROUND-COLOR  01.

       01  ENT-003.
           03  LINE 11 COLUMN 21 PIC X(38) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 21 PIC X(38) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 21 PIC X(38) USING LIN-003
               BACKGROUND-COLOR  01.

       01  ENT-004.
           03  LINE 10 COLUMN 21 PIC X(38) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 21 PIC X(38) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 21 PIC X(38) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 21 PIC X(38) USING LIN-004
               BACKGROUND-COLOR  01.

       01  ENT-005.
           03  LINE 10 COLUMN 21 PIC X(38) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 21 PIC X(38) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 21 PIC X(38) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 21 PIC X(38) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 21 PIC X(38) USING LIN-005
               BACKGROUND-COLOR  01.

       01  ENT-006.
           03  LINE 09 COLUMN 21 PIC X(38) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 21 PIC X(38) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 21 PIC X(38) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 21 PIC X(38) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 21 PIC X(38) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 21 PIC X(38) USING LIN-006
               BACKGROUND-COLOR  01.

       01  ENT-007.
           03  LINE 09 COLUMN 21 PIC X(38) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 21 PIC X(38) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 21 PIC X(38) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 21 PIC X(38) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 21 PIC X(38) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 21 PIC X(38) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 21 PIC X(38) USING LIN-007
               BACKGROUND-COLOR  01.

       01  ENT-008.
           03  LINE 09 COLUMN 21 PIC X(38) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 21 PIC X(38) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 21 PIC X(38) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 21 PIC X(38) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 21 PIC X(38) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 21 PIC X(38) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 21 PIC X(38) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 21 PIC X(38) USING LIN-008
               BACKGROUND-COLOR  01.

       01  ENT-009.
           03  LINE 08 COLUMN 21 PIC X(38) USING LIN-001
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 21 PIC X(38) USING LIN-002
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 21 PIC X(38) USING LIN-003
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 21 PIC X(38) USING LIN-004
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 21 PIC X(38) USING LIN-005
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 21 PIC X(38) USING LIN-006
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 21 PIC X(38) USING LIN-007
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 21 PIC X(38) USING LIN-008
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 21 PIC X(38) USING LIN-009
               BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LPA-MES LINE 11 COLUMN 43 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  04.
           03  LPA-ANO LINE 11 COLUMN 46 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  04.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!! PESQUIS
                   "ANDO CAIXAS ...                           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DO
                   "CAIXA ? [ ]  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "CAIXA FECHADO !!!
                   "-  [ESC] RETORNA ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DO CA
                   "IXA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "DATA DO CAIXA INCOE
                   "RENTE !!!  -  [ESC] RETORNA ...           "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ESC] P/ RETO
                   "RNAR ...                                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001          MEN-001
           ACCEPT   DTA-SYS  FROM    DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           OPEN     I-O     CADCXA
           OPEN     INPUT   CADCTR   CADPRS
           READ     CADCTR           DISPLAY     TLA-013
           MOVE     ZEROS     TO     IND1
           MOVE     SPACES    TO     CHV-CXA
           START    CADCXA    KEY    GREATER     CHV-CXA.

       ROT-100-000.
           READ     CADCXA   NEXT AT END
                    GO  TO   ROT-100-010.
           ADD      1        TO  IND1
           MOVE     CHV-CXA  TO  CHV-TBC (IND1)
           MOVE     DES-CXA  TO  DES-TBC (IND1)
           MOVE     TUR-TAB (TUR-CXA) TO  TUR-TBC (IND1)
           MOVE     POS-TAB (POS-CXA) TO  POS-TBC (IND1)
           GO  TO   ROT-100-000.

       ROT-100-010.
           MOVE   IND1  TO  IND2
           MOVE   1     TO  IND1
           IF  IND2  >  01
               GO  TO   ROT-100-020.
           MOVE   " "   TO  RSP-OPC
           IF  POS-CXA  =   2
               DISPLAY      TLA-010
                            MEN-007
               ACCEPT       OPC-001
               GO  TO       ROT-200-050.
           MOVE     1   TO  NUM-OPC
           GO  TO   ROT-200-000.

       ROT-100-020.
           MOVE     IND1           TO NUM-DT1
           MOVE     TUR-TBC (IND1) TO TUR-DT1
           MOVE     DES-TBC (IND1) TO DES-DT1
           MOVE     POS-TBC (IND1) TO POS-DT1
           IF  IND1  =  01   MOVE  DET-001 TO  LIN-001.
           IF  IND1  =  02   MOVE  DET-001 TO  LIN-002.
           IF  IND1  =  03   MOVE  DET-001 TO  LIN-003.
           IF  IND1  =  04   MOVE  DET-001 TO  LIN-004.
           IF  IND1  =  05   MOVE  DET-001 TO  LIN-005.
           IF  IND1  =  06   MOVE  DET-001 TO  LIN-006.
           IF  IND1  =  07   MOVE  DET-001 TO  LIN-007.
           IF  IND1  =  08   MOVE  DET-001 TO  LIN-008.
           IF  IND1  =  09   MOVE  DET-001 TO  LIN-009.
           ADD    1     TO   IND1
           IF  IND1 NOT  >   IND2
               GO  TO        ROT-100-020.

       ROT-100-030.
           IF  IND2  =  02   DISPLAY  TLA-002  ENT-002.
           IF  IND2  =  03   DISPLAY  TLA-003  ENT-003.
           IF  IND2  =  04   DISPLAY  TLA-004  ENT-004.
           IF  IND2  =  05   DISPLAY  TLA-005  ENT-005.
           IF  IND2  =  06   DISPLAY  TLA-006  ENT-006.
           IF  IND2  =  07   DISPLAY  TLA-007  ENT-007.
           IF  IND2  =  08   DISPLAY  TLA-008  ENT-008.
           IF  IND2  =  09   DISPLAY  TLA-009  ENT-009.

       ROT-100-040.
           DISPLAY  MEN-002
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   "01"
               GO  TO        ROT-200-050.
           IF  NUM-OPC  < 1  OR  >  IND2
               GO  TO        ROT-100-040.
           MOVE     SPACES   TO  RSP-OPC
           MOVE     CHV-TBC (NUM-OPC) TO  CHV-CXA
           READ     CADCXA
           IF  POS-CXA   =   2
               DISPLAY       MEN-003
               ACCEPT        OPC-001
               GO  TO        ROT-100-040.

       ROT-200-000.
           IF  DTD-CTR   =   ZEROS
               MOVE DTI-CTR  TO  DTA-SYS
           ELSE
               MOVE DTD-CTR  TO  DTA-SYS
               ADD  1        TO  DIA-SYS.
           DISPLAY  TLA-011      MEN-004
           MOVE     CHV-CXA  TO  PRM-002.

       ROT-200-010.
           DISPLAY  LPA-MES
           ACCEPT   (11 40)  DIA-SYS  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               IF  IND2  =   01
                   GO  TO    ROT-200-050
               ELSE
                   GO  TO    ROT-100-030.
           IF  DIA-SYS   <   1 OR  >  31
               GO  TO        ROT-200-010.
           DISPLAY  DIA-ENT.

       ROT-200-020.
           DISPLAY  LPA-ANO
           ACCEPT   (11 43)  MES-SYS  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-010.
           IF  MES-SYS   <   1 OR  >  12
               GO  TO        ROT-200-020.
           DISPLAY  MES-ENT.

       ROT-200-030.
           DISPLAY  MEN-004
           ACCEPT   (11 46)  ANO-SYS  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-020.
           DISPLAY  ANO-ENT
           MOVE     SPACES   TO  RSP-OPC
           IF  DTA-SYS   <   DTI-CTR
               DISPLAY       MEN-006
               ACCEPT        OPC-001
               GO  TO        ROT-200-030.
           DISPLAY  MEN-005
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   "01"
               GO  TO        ROT-200-030.
           MOVE     TUR-TAB (TUR-PR2) TO  TUR-AUX
           MOVE     DTA-SYS  TO  DTA-CXA
           REWRITE  REG-CXA
           DISPLAY  TLA-012      TLA-013.

       ROT-200-040.
           MOVE     SPACES   TO  CHV-PRS
           MOVE     TUR-CXA  TO  TUR-PRS
           MOVE     NUM-CXA  TO  CXA-PRS
           START    CADPRS   KEY GREATER  CHV-PRS
           READ     CADPRS   NEXT
           IF  ATV-PRS   =   1    CHAIN  "DYN202"  USING  PRM-002.
           IF  ATV-PRS   =   2    CHAIN  "DYN203"  USING  PRM-002.
           IF  ATV-PRS   =   3    CHAIN  "DYN204"  USING  PRM-002.
           IF  ATV-PRS   =   4    CHAIN  "DYN205"  USING  PRM-002.
           IF  ATV-PRS   =   5    CHAIN  "DYN206"  USING  PRM-002.
           IF  ATV-PRS   =   6    CHAIN  "DYN207"  USING  PRM-002.
           IF  ATV-PRS   =   7    CHAIN  "DYN208"  USING  PRM-002.
           IF  ATV-PRS   =   8    CHAIN  "DYN209"  USING  PRM-002.

       ROT-200-050.
           CLOSE    CADCTR
                    CADCXA   CADPRS
           MOVE     1   TO   PRM-001
           CHAIN   "DYN200"  USING  PRM-001.
