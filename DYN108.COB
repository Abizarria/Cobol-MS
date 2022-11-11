       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN108.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADATV        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  SEQUENTIAL
                       RECORD        KEY     IS  CHV-ATV.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADATV      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADATV.DAT".
       01  REG-ATV.
           03  CHV-ATV          PIC  9(01).
           03  DES-ATV          PIC  X(15).

       FD  CADEST      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADEST.DAT".
       01  REG-EST.
           03  CHV-EST.
               05  PRX-EST      PIC  9(01).
               05  SUF-EST      PIC  9(03).
           03  DES-EST          PIC  X(25).
           03  MIN-EST          PIC  9(06).
           03  SDO-EST          PIC S9(06)V9.
           03  CST-EST          PIC  9(07)V9999.
           03  CTM-EST          PIC  9(07)V9999.
           03  VDA-EST          PIC  9(07)V99.
           03  PCT-EST          PIC S9(03)V99.
           03  QVM-EST          PIC  9(07)V9.
           03  QVA-EST          PIC  9(08)V9.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-QTD          PIC  9(03).
           03  TOT-QTD          PIC  9(05).
           03  ACM-CST          PIC S9(11)V99.
           03  TOT-CST          PIC S9(12)V99.
           03  ACM-VDA          PIC S9(11)V99.
           03  TOT-VDA          PIC S9(12)V99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "POSICAO ATUAL DO ESTOQUE  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 05 COLUMN 07 PIC X(67) FROM
               " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 11 PIC X(59) FROM "ATIVIDADE       ITENS
               "   TOTAL DE CUSTO     TOTAL DE VENDA"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 06 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 74 PIC X(02) FROM  SPACES.
           03  LINE 18 COLUMN 07 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 07 PIC X(67) FROM
               " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 74 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 09 PIC X(67) FROM SPACES.

       01  ENT-001.
           03  ATV-000 LINE 08 COLUMN 11 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  QTD-000 LINE 08 COLUMN 29 PIC   ZZ9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  CST-000 LINE 08 COLUMN 33 PIC  ---.---.---.--9,99 USING
               ACM-CST BACKGROUND-COLOR  01.
           03  VDA-000 LINE 08 COLUMN 52 PIC  ---.---.---.--9,99 USING
               ACM-VDA BACKGROUND-COLOR  01.
           03  ATV-001 LINE 09 COLUMN 11 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  QTD-001 LINE 09 COLUMN 29 PIC   ZZ9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  CST-001 LINE 09 COLUMN 33 PIC  ---.---.---.--9,99 USING
               ACM-CST BACKGROUND-COLOR  01.
           03  VDA-001 LINE 09 COLUMN 52 PIC  ---.---.---.--9,99 USING
               ACM-VDA BACKGROUND-COLOR  01.
           03  ATV-002 LINE 10 COLUMN 11 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  QTD-002 LINE 10 COLUMN 29 PIC   ZZ9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  CST-002 LINE 10 COLUMN 33 PIC  ---.---.---.--9,99 USING
               ACM-CST BACKGROUND-COLOR  01.
           03  VDA-002 LINE 10 COLUMN 52 PIC  ---.---.---.--9,99 USING
               ACM-VDA BACKGROUND-COLOR  01.
           03  ATV-003 LINE 11 COLUMN 11 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  QTD-003 LINE 11 COLUMN 29 PIC   ZZ9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  CST-003 LINE 11 COLUMN 33 PIC  ---.---.---.--9,99 USING
               ACM-CST BACKGROUND-COLOR  01.
           03  VDA-003 LINE 11 COLUMN 52 PIC  ---.---.---.--9,99 USING
               ACM-VDA BACKGROUND-COLOR  01.
           03  ATV-004 LINE 12 COLUMN 11 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  QTD-004 LINE 12 COLUMN 29 PIC   ZZ9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  CST-004 LINE 12 COLUMN 33 PIC  ---.---.---.--9,99 USING
               ACM-CST BACKGROUND-COLOR  01.
           03  VDA-004 LINE 12 COLUMN 52 PIC  ---.---.---.--9,99 USING
               ACM-VDA BACKGROUND-COLOR  01.
           03  ATV-005 LINE 13 COLUMN 11 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  QTD-005 LINE 13 COLUMN 29 PIC   ZZ9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  CST-005 LINE 13 COLUMN 33 PIC  ---.---.---.--9,99 USING
               ACM-CST BACKGROUND-COLOR  01.
           03  VDA-005 LINE 13 COLUMN 52 PIC  ---.---.---.--9,99 USING
               ACM-VDA BACKGROUND-COLOR  01.
           03  ATV-006 LINE 14 COLUMN 11 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  QTD-006 LINE 14 COLUMN 29 PIC   ZZ9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  CST-006 LINE 14 COLUMN 33 PIC  ---.---.---.--9,99 USING
               ACM-CST BACKGROUND-COLOR  01.
           03  VDA-006 LINE 14 COLUMN 52 PIC  ---.---.---.--9,99 USING
               ACM-VDA BACKGROUND-COLOR  01.
           03  ATV-007 LINE 15 COLUMN 11 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  QTD-007 LINE 15 COLUMN 29 PIC   ZZ9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  CST-007 LINE 15 COLUMN 33 PIC  ---.---.---.--9,99 USING
               ACM-CST BACKGROUND-COLOR  01.
           03  VDA-007 LINE 15 COLUMN 52 PIC  ---.---.---.--9,99 USING
               ACM-VDA BACKGROUND-COLOR  01.
           03  ATV-008 LINE 16 COLUMN 11 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  QTD-008 LINE 16 COLUMN 29 PIC   ZZ9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  CST-008 LINE 16 COLUMN 33 PIC  ---.---.---.--9,99 USING
               ACM-CST BACKGROUND-COLOR  01.
           03  VDA-008 LINE 16 COLUMN 52 PIC  ---.---.---.--9,99 USING
               ACM-VDA BACKGROUND-COLOR  01.
           03  ATV-009 LINE 17 COLUMN 11 PIC X(15) USING DES-ATV
                       BACKGROUND-COLOR  01.
           03  QTD-009 LINE 17 COLUMN 29 PIC   ZZ9 USING ACM-QTD
                       BACKGROUND-COLOR  01.
           03  CST-009 LINE 17 COLUMN 33 PIC  ---.---.---.--9,99 USING
               ACM-CST BACKGROUND-COLOR  01.
           03  VDA-009 LINE 17 COLUMN 52 PIC  ---.---.---.--9,99 USING
               ACM-VDA BACKGROUND-COLOR  01.

       01  ENT-002.
           03  ATV-TOT LINE 18 COLUMN 11 PIC X(15) FROM "TOTAL GERAL"
                       BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  QTD-TOT LINE 18 COLUMN 29 PIC   ZZ9 USING TOT-QTD
                       BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  CST-TOT LINE 18 COLUMN 33 PIC  ---.---.---.--9,99 USING
               TOT-CST BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.
           03  VDA-TOT LINE 18 COLUMN 52 PIC  ---.---.---.--9,99 USING
               TOT-VDA BACKGROUND-COLOR  01  FOREGROUND-COLOR 15.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  CALCUL
                   "ADO TOTAL DO ESTOQUE ...                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ESC] P/ RETO
                   "RNAR ...                                        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  MEN-001
           OPEN     INPUT    CADATV  CADEST
           MOVE     0   TO   TOT-QTD
                             TOT-CST TOT-VDA.

       ROT-100-000.
           READ     CADATV   AT  END
                    GO  TO   ROT-100-050.
           MOVE     CHV-ATV  TO  PRX-EST
           MOVE     ZEROS    TO  SUF-EST  ACM-QTD
                                 ACM-CST  ACM-VDA
           IF  CHV-ATV   =   0   DISPLAY  ATV-000.
           IF  CHV-ATV   =   1   DISPLAY  ATV-001.
           IF  CHV-ATV   =   2   DISPLAY  ATV-002.
           IF  CHV-ATV   =   3   DISPLAY  ATV-003.
           IF  CHV-ATV   =   4   DISPLAY  ATV-004.
           IF  CHV-ATV   =   5   DISPLAY  ATV-005.
           IF  CHV-ATV   =   6   DISPLAY  ATV-006.
           IF  CHV-ATV   =   7   DISPLAY  ATV-007.
           IF  CHV-ATV   =   8   DISPLAY  ATV-008.
           IF  CHV-ATV   =   9   DISPLAY  ATV-009.

       ROT-100-010.
           IF  CHV-ATV   =   0   DISPLAY  QTD-000  CST-000  VDA-000.
           IF  CHV-ATV   =   1   DISPLAY  QTD-001  CST-001  VDA-001.
           IF  CHV-ATV   =   2   DISPLAY  QTD-002  CST-002  VDA-002.
           IF  CHV-ATV   =   3   DISPLAY  QTD-003  CST-003  VDA-003.
           IF  CHV-ATV   =   4   DISPLAY  QTD-004  CST-004  VDA-004.
           IF  CHV-ATV   =   5   DISPLAY  QTD-005  CST-005  VDA-005.
           IF  CHV-ATV   =   6   DISPLAY  QTD-006  CST-006  VDA-006.
           IF  CHV-ATV   =   7   DISPLAY  QTD-007  CST-007  VDA-007.
           IF  CHV-ATV   =   8   DISPLAY  QTD-008  CST-008  VDA-008.
           IF  CHV-ATV   =   9   DISPLAY  QTD-009  CST-009  VDA-009.

       ROT-100-020.
           START    CADEST   KEY GREATER  CHV-EST
                    INVALID  KEY GO  TO   ROT-100-040.

       ROT-100-030.
           READ     CADEST   NEXT AT END
                    GO  TO   ROT-100-040.
           IF  PRX-EST  NOT   =  CHV-ATV
               GO  TO        ROT-100-040.
           ADD      1        TO  ACM-QTD
           COMPUTE  ACM-CST  =   ACM-CST + SDO-EST * CST-EST
           COMPUTE  ACM-VDA  =   ACM-VDA + SDO-EST * VDA-EST
           PERFORM  ROT-100-010
           GO  TO   ROT-100-030.

       ROT-100-040.
           ADD      ACM-QTD  TO  TOT-QTD
           ADD      ACM-CST  TO  TOT-CST
           ADD      ACM-VDA  TO  TOT-VDA
           MOVE     ZEROS    TO  ACM-QTD  ACM-CST  ACM-VDA
           GO  TO   ROT-100-000.

       ROT-100-050.
           DISPLAY  ENT-002  MEN-002
           MOVE     " "  TO  RSP-OPC.

       ROT-100-060.
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO  NOT   =   01
               GO  TO        ROT-100-060.
           MOVE     4    TO  PRM-001
           CLOSE    CADATV   CADEST
           CHAIN   "DYN100"  USING   PRM-001.
