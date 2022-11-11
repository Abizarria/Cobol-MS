       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN107.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCMB        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CMB.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      CADSRT        ASSIGN  TO  DISK.

           SELECT      CADTMD        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TMD.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TNQ.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCMB      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCMB.DAT".
       01  REG-CMB.
           03  CHV-CMB          PIC  9(01).
           03  DES-CMB          PIC  X(15).
           03  VVM-CMB          PIC  9(11)V99.
           03  LUC-CMB          PIC  9(04)V9999.

       FD  CADPRM      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADPRM.DAT".
       01  REG-PRM.
           03  COD-PRM          PIC  9(05).
           03  CLI-PRM          PIC  9(02)  OCCURS  36.
           03  END-PRM          PIC  9(02)  OCCURS  35.
           03  CID-PRM          PIC  9(02)  OCCURS  20.
           03  EST-PRM          PIC  9(02)  OCCURS  02.
           03  CEP-PRM          PIC  9(08).
           03  CGC-PRM          PIC  9(14).
           03  INS-PRM          PIC  9(02)  OCCURS  15.

       SD  CADSRT.

       01  REG-SRT.
           03  TNQ-SRT          PIC  9(02).
           03  CMB-SRT          PIC  9(01).
           03  SDI-SRT          PIC S9(05)V9.
           03  SDA-SRT          PIC S9(05)V9.

       FD  CADTMD      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADTMD.DAT".
       01  REG-TMD.
           03  CHV-TMD.
               05  TNQ-TMD      PIC  9(02).
               05  LTR-TMD      PIC  9(03)V9.
           03  QTD-TMD          PIC  9(05).
           03  LTM-TMD          PIC  9(02)V9.

       FD  CADTNQ      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADTNQ.DAT".
       01  REG-TNQ.
           03  CHV-TNQ          PIC  9(02).
           03  CMB-TNQ          PIC  9(01).
           03  SDI-TNQ          PIC S9(05)V9.
           03  SDA-TNQ          PIC S9(05)V9.

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  IND3             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  LTR-AUX          PIC  9(03)V9.
           03  DNS-AUX          PIC  9(01)V999.
           03  ACM-SDI          PIC S9(06)V9.
           03  ACM-SDA          PIC S9(06)V9.
           03  ACM-DIF          PIC S9(06)V9.
           03  TOT-SDI          PIC S9(06)V9.
           03  TOT-SDA          PIC S9(06)V9.
           03  TOT-DIF          PIC S9(06)V9.

           03  NOM-TAB          PIC  X(35).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  35.

           03  TAB-LET          PIC  X(41)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  41.

           03  CTA-AUX          PIC  9(03)V999999.
           03  RED-CTA          REDEFINES  CTA-AUX.
               05  INT-AUX      PIC  9(03).
               05  DEC-AUX      PIC  9(06).

           03  TP1-DET          PIC  X(14)  VALUE  "SUB-TOTAL --->".
           03  TP2-DET          PIC  X(14)  VALUE  "TOTAL GERAL ->".

           03  REG-TAB          PIC  X(80)  OCCURS  54.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  TLA-AUX.
               05  LIN-001      PIC  X(80).
               05  LIN-002      PIC  X(80).
               05  LIN-003      PIC  X(80).
               05  LIN-004      PIC  X(80).
               05  LIN-005      PIC  X(80).
               05  LIN-006      PIC  X(80).
               05  LIN-007      PIC  X(80).
               05  LIN-008      PIC  X(80).
               05  LIN-009      PIC  X(80).
               05  LIN-010      PIC  X(80).
               05  LIN-011      PIC  X(80).
               05  LIN-012      PIC  X(80).
               05  LIN-013      PIC  X(80).
               05  LIN-014      PIC  X(80).
               05  LIN-015      PIC  X(80).
               05  LIN-016      PIC  X(80).
               05  LIN-017      PIC  X(80).
               05  LIN-018      PIC  X(80).

       01  CAB-001.
           03  FILLER           PIC  X(22)  VALUE  "-DYNAMIC-".
           03  NOM-CB1          PIC  X(48).
           03  FILLER           PIC  X(10)  VALUE  "FOLHA : 01".

       01  CAB-002.
           03  FILLER           PIC  X(72)  VALUE  "RESUMO DE TANQUES".
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  CAB-004              PIC  X(80)  VALUE
           "COMBUSTIVEL           TANQUE        ESCRITURAL          FISI
           "CO         DIFERENCA".

       01  DET-001.
           03  CMB-DT1          PIC  X(24).
           03  TNQ-DT1          PIC  9(02)  BLANK  WHEN  ZEROS.
           03  FILLER           PIC  X(08)  VALUE  SPACES.
           03  SDI-DT1          PIC  ----.--9,9.
           03  FILLER           PIC  X(08)  VALUE  SPACES.
           03  SDA-DT1          PIC  ----.--9,9.
           03  FILLER           PIC  X(08)  VALUE  SPACES.
           03  DIF-DT1          PIC  ----.--9,9.

       01  TOT-001.
           03  TIP-TT1          PIC  X(14).
           03  FILLER           PIC  X(20)  VALUE  SPACES.
           03  SDI-TT1          PIC  ----.--9,9.
           03  FILLER           PIC  X(08)  VALUE  SPACES.
           03  SDA-TT1          PIC  ----.--9,9.
           03  FILLER           PIC  X(08)  VALUE  SPACES.
           03  DIF-TT1          PIC  ----.--9,9.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "CALCULO DE TANQUES  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR 00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 27 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 62 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 62 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 63 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 29 PIC X(38) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 34 PIC X(33) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 36 PIC X(31) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 42 PIC X(25) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 48 PIC X(20) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 08 COLUMN 20 PIC X(40) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 20 PIC X(40) FROM " ³  Tanque .:     -
               "               ³"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 20 PIC X(40) FROM " ³  Volume .:
               "               ³"    BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 20 PIC X(40) FROM " ³
               "               ³"    BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 20 PIC X(40) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"    BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 22 PIC X(40) FROM SPACES.

       01  TLA-003.
           03  LINE 07 COLUMN 16 PIC X(47) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 16 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 08 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 16 PIC X(47) FROM " ³  N§ do Tanque ..:
               "  -                   ³"   BACKGROUND-COLOR 01.
           03  LINE 09 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 16 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 16 PIC X(47) FROM " ³  Leitura .......:
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 16 PIC X(47) FROM " ³  Densidade .....:
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 16 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 16 PIC X(47) FROM " ³  Volume Atual ..:
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 14 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 16 PIC X(47) FROM " ³
               "                      ³"   BACKGROUND-COLOR 01.
           03  LINE 15 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 16 PIC X(47) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR 01.
           03  LINE 16 COLUMN 63 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 18 PIC X(47) FROM SPACES.

       01  TLA-004.
           03  LINE 09 COLUMN 27 PIC X(26) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "Ä¿" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 27 PIC X(26) FROM " ³
               " ³" BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 27 PIC X(26) FROM " ³  RESUMO DOS TANQUES
               " ³" BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 27 PIC X(26) FROM " ³
               " ³" BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 27 PIC X(26) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÙ" BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 53 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 29 PIC X(26) FROM SPACES.

       01  ENT-001.
           03  TNQ-ENT LINE 10 COLUMN 34 PIC 9(02)    USING CHV-TNQ
                       BACKGROUND-COLOR  01.
           03  CMB-ENT LINE 10 COLUMN 41 PIC X(15)    USING DES-CMB
                       BACKGROUND-COLOR  01.

       01  ENT-002  LINE 12 COLUMN 33 PIC ---.--9,9   USING SDA-TNQ
                    BACKGROUND-COLOR  01.

       01  ENT-003.
           03  NUM-ENT LINE 09 COLUMN 37 PIC 9(02)    USING CHV-TNQ
                       BACKGROUND-COLOR  01.
           03  DES-ENT LINE 09 COLUMN 44 PIC X(15)    USING DES-CMB
                       BACKGROUND-COLOR  01.

       01  ENT-004.
           03  LTR-ENT LINE 11 COLUMN 37 PIC     ZZ9,9 USING LTR-AUX
                       BACKGROUND-COLOR  01.
           03  DNS-ENT LINE 12 COLUMN 37 PIC     9,999 USING DNS-AUX
                       BACKGROUND-COLOR  01.
           03  LTS-ENT LINE 14 COLUMN 36 PIC ---.--9,9 USING SDA-TNQ
                       BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LINE 04 COLUMN 01 PIC X(80) USING LIN-001
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 05 COLUMN 01 PIC X(80) USING LIN-002
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 06 COLUMN 01 PIC X(80) USING LIN-003
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 07 COLUMN 01 PIC X(80) USING LIN-004
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 08 COLUMN 01 PIC X(80) USING LIN-005
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 09 COLUMN 01 PIC X(80) USING LIN-006
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 10 COLUMN 01 PIC X(80) USING LIN-007
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 11 COLUMN 01 PIC X(80) USING LIN-008
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 12 COLUMN 01 PIC X(80) USING LIN-009
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 13 COLUMN 01 PIC X(80) USING LIN-010
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 14 COLUMN 01 PIC X(80) USING LIN-011
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 15 COLUMN 01 PIC X(80) USING LIN-012
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 16 COLUMN 01 PIC X(80) USING LIN-013
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 17 COLUMN 01 PIC X(80) USING LIN-014
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 18 COLUMN 01 PIC X(80) USING LIN-015
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 19 COLUMN 01 PIC X(80) USING LIN-016
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 20 COLUMN 01 PIC X(80) USING LIN-017
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.
           03  LINE 21 COLUMN 01 PIC X(80) USING LIN-018
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 00.

       01  LPA-002 LINE 12 COLUMN 41 PIC X(01) FROM SPACES
                   BACKGROUND-COLOR  01.

       01  LPA-003.
           03  LPA-DNS LINE 12 COLUMN 37 PIC X(05) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-LTS LINE 14 COLUMN 37 PIC X(08) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VOLUME ATU
                   "AL DO TANQUE  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ LA
                   "NCAR OUTRO TANQUE  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ORDENA
                   "NDO COMBUSTIVEIS ...                            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA CONSULTAR NA
                   " TELA ? [S/N]  [ ]  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR !!!  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO RESUMO DE TANQUES ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "LEITURA  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DENSIDADE
                   "DO PRODUTO  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "LEITURA INCOERENTE
                   "OU TABELA INCOMPLETA  -  [ESC] RETORNA ...      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CA
                   "LCULAR OUTRO TANQUE  -  [ESC] RETORNA ...       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002 LINE 22 COLUMN 49 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001.

       ROT-000-000.
           DISPLAY   TLA-001
           IF  PRM-001  =  1  DISPLAY  TLA-002.
           IF  PRM-001  =  2  DISPLAY  TLA-003.
           OPEN      I-O      CADTNQ
                     INPUT    CADCMB
                              CADPRM   CADTMD
           READ      CADPRM
           MOVE      1    TO  IND2.

       ROT-000-010.
           MOVE     CLI-PRM (IND2 + 1)  TO IND3
           MOVE     LET-TAB (IND3)  TO  LET-NOM (IND2)
           ADD      1   TO   IND2
           IF  IND2  <  36
               GO  TO        ROT-000-010.
           MOVE     SPACES   TO  LET-NOM (IND2)
           ACCEPT   DTA-SYS  FROM   DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           MOVE     DIA-SYS  TO  DIA-CB2
           MOVE     MES-SYS  TO  MES-CB2
           MOVE     ANO-SYS  TO  ANO-CB2
           IF  PRM-001   =   2
               GO  TO        ROT-200-000.

       ROT-100-000.
           MOVE     ZEROS    TO  CHV-TNQ
           START    CADTNQ   KEY GREATER  CHV-TNQ.

       ROT-100-010.
           READ     CADTNQ   NEXT AT END
                    GO  TO   ROT-300-000.
           MOVE     CMB-TNQ  TO  CHV-CMB
           READ     CADCMB
           DISPLAY  ENT-001.

       ROT-100-020.
           DISPLAY  LPA-002  MEN-001
           ACCEPT   (12 33)  SDA-TNQ  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-600-000.
           DISPLAY  ENT-002  MEN-002
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           REWRITE  REG-TNQ
           GO  TO   ROT-100-010.

       ROT-200-000.
           MOVE     ZEROS    TO  CHV-TNQ
           START    CADTNQ   KEY GREATER  CHV-TNQ.

       ROT-200-010.
           READ     CADTNQ   NEXT AT END
                    GO  TO   ROT-300-000.
           MOVE     CMB-TNQ  TO  CHV-CMB
           READ     CADCMB
           DISPLAY  ENT-003
           MOVE     ZEROS    TO  LTR-AUX  DNS-AUX.

       ROT-200-020.
           DISPLAY  LPA-DNS  MEN-009
           ACCEPT   (11 37)  LTR-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-600-000.
           IF  LTR-AUX   =   ZEROS
               GO  TO        ROT-200-010.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  LTR-ENT.

       ROT-200-030.
           DISPLAY  LPA-LTS  MEN-010
           ACCEPT   (12 37)  DNS-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   "01"
               GO  TO        ROT-200-020.
           IF  DNS-AUX   =   ZEROS
               GO  TO        ROT-200-030.
           DISPLAY  DNS-ENT
           MOVE     SPACES   TO  RSP-OPC
           MOVE     CHV-TNQ  TO  TNQ-TMD
           MOVE     ZEROS    TO  LTR-TMD
           START    CADTMD   KEY GREATER  CHV-TMD
                    INVALID  KEY GO  TO   ROT-200-040.
           READ     CADTMD   NEXT
           IF  TNQ-TMD  NOT  =   CHV-TNQ
               GO  TO            ROT-200-040.
           COMPUTE  CTA-AUX  = ((LTR-AUX  -   LTR-TMD)
                             /   DNS-AUX) +   LTR-TMD
           MOVE     CHV-TNQ  TO  TNQ-TMD
           MOVE     INT-AUX  TO  LTR-TMD
           READ     CADTMD   INVALID KEY
                    DISPLAY  MEN-011
                    ACCEPT   OPC-001
                    GO  TO   ROT-200-030.
           MOVE     ZEROS    TO  INT-AUX
           COMPUTE  SDA-TNQ  =   QTD-TMD  +   CTA-AUX
                             *   LTM-TMD  *   10
           DISPLAY  LTS-ENT  MEN-012
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-030.
           REWRITE  REG-TNQ
           DISPLAY  LPA-003
           GO  TO   ROT-200-010.

       ROT-200-040.
           DISPLAY  MEN-011
           ACCEPT   OPC-001
           GO  TO   ROT-200-030.

       ROT-300-000.
           MOVE     SPACES   TO  TLA-AUX
           DISPLAY  LPA-001      TLA-004
           SORT     CADSRT   ON  ASCENDING  KEY CMB-SRT
                                                TNQ-SRT
                    INPUT        PROCEDURE  IS  ROT-300-020
                    OUTPUT       PROCEDURE  IS  ROT-300-060.
           MOVE     1        TO  IND1.

       ROT-300-010.
           DISPLAY  MEN-004
           MOVE     SPACES   TO  RSP-OPC
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-600-000.
           IF  RSP-OPC   =   "S"
               GO  TO        ROT-400-000.
           IF  RSP-OPC   =   "N"
               GO  TO        ROT-500-000.
           GO  TO   ROT-300-010.

       ROT-300-020  SECTION.

       ROT-300-030.
           MOVE     ZEROS    TO  CHV-TNQ
           START    CADTNQ   KEY GREATER  CHV-TNQ.

       ROT-300-040.
           READ     CADTNQ   NEXT AT END
                    GO  TO   ROT-300-050.
           MOVE     REG-TNQ  TO  REG-SRT
           RELEASE  REG-SRT
           GO  TO   ROT-300-040.

       ROT-300-050.
           MOVE     NOM-TAB  TO  NOM-CB1
           MOVE     CAB-001  TO  REG-TAB (01)
           MOVE     SPACES   TO  REG-TAB (02)
           MOVE     CAB-002  TO  REG-TAB (03)
           MOVE     CAB-003  TO  REG-TAB (04)  REG-TAB (06)
           MOVE     CAB-004  TO  REG-TAB (05)
           MOVE     7        TO  IND1.

       ROT-300-060  SECTION.

       ROT-300-070.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-300-100.
           MOVE     ZEROS    TO  TOT-SDI
                                 TOT-SDA  TOT-DIF.
       ROT-300-080.
           MOVE     CMB-SRT  TO  CHV-CMB
           READ     CADCMB
           MOVE     DES-CMB  TO  CMB-DT1
           MOVE     ZEROS    TO  ACM-SDI
                                 ACM-SDA  ACM-DIF.
       ROT-300-090.
           MOVE     TNQ-SRT  TO  TNQ-DT1
           MOVE     SDI-SRT  TO  SDI-DT1
           MOVE     SDA-SRT  TO  SDA-DT1
           ADD      SDI-SRT  TO  ACM-SDI
           ADD      SDA-SRT  TO  ACM-SDA
           COMPUTE  DIF-DT1  =   SDA-SRT  -  SDI-SRT
           COMPUTE  ACM-DIF  =   ACM-DIF  +  SDA-SRT  -  SDI-SRT
           MOVE     DET-001  TO  REG-TAB     (IND1)
           ADD      1        TO  IND1
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-300-100.
           IF  CMB-SRT  NOT  =   CHV-CMB
               PERFORM       ROT-300-100
               GO  TO        ROT-300-080.
           GO  TO   ROT-300-090.

       ROT-300-100.
           MOVE     SPACES   TO  REG-TAB (IND1)
           MOVE     TP1-DET  TO  TIP-TT1
           MOVE     ACM-SDI  TO  SDI-TT1
           MOVE     ACM-SDA  TO  SDA-TT1
           MOVE     ACM-DIF  TO  DIF-TT1
           MOVE     TOT-001  TO  REG-TAB (IND1 + 1)
           MOVE     SPACES   TO  REG-TAB (IND1 + 2)
           MOVE     SPACES   TO  REG-TAB (IND1 + 3)
           ADD      ACM-SDI  TO  TOT-SDI
           ADD      ACM-SDA  TO  TOT-SDA
           ADD      ACM-DIF  TO  TOT-DIF
           ADD      4        TO  IND1.

       ROT-300-110.
           MOVE     TP2-DET  TO  TIP-TT1
           MOVE     TOT-SDI  TO  SDI-TT1
           MOVE     TOT-SDA  TO  SDA-TT1
           MOVE     TOT-DIF  TO  DIF-TT1
           MOVE     TOT-001  TO  REG-TAB (IND1 - 1)
           MOVE     IND1     TO  IND2.

       ROT-300-120  SECTION.

       ROT-400-000.
           IF  IND1 = 01 OR 19 OR 37 MOVE REG-TAB (IND1) TO LIN-001.
           IF  IND1 = 02 OR 20 OR 38 MOVE REG-TAB (IND1) TO LIN-002.
           IF  IND1 = 03 OR 21 OR 39 MOVE REG-TAB (IND1) TO LIN-003.
           IF  IND1 = 04 OR 22 OR 40 MOVE REG-TAB (IND1) TO LIN-004.
           IF  IND1 = 05 OR 23 OR 41 MOVE REG-TAB (IND1) TO LIN-005.
           IF  IND1 = 06 OR 24 OR 42 MOVE REG-TAB (IND1) TO LIN-006.
           IF  IND1 = 07 OR 25 OR 43 MOVE REG-TAB (IND1) TO LIN-007.
           IF  IND1 = 08 OR 26 OR 44 MOVE REG-TAB (IND1) TO LIN-008.
           IF  IND1 = 09 OR 27 OR 45 MOVE REG-TAB (IND1) TO LIN-009.
           IF  IND1 = 10 OR 28 OR 46 MOVE REG-TAB (IND1) TO LIN-010.
           IF  IND1 = 11 OR 29 OR 47 MOVE REG-TAB (IND1) TO LIN-011.
           IF  IND1 = 12 OR 30 OR 48 MOVE REG-TAB (IND1) TO LIN-012.
           IF  IND1 = 13 OR 31 OR 49 MOVE REG-TAB (IND1) TO LIN-013.
           IF  IND1 = 14 OR 32 OR 50 MOVE REG-TAB (IND1) TO LIN-014.
           IF  IND1 = 15 OR 33 OR 51 MOVE REG-TAB (IND1) TO LIN-015.
           IF  IND1 = 16 OR 34 OR 52 MOVE REG-TAB (IND1) TO LIN-016.
           IF  IND1 = 17 OR 35 OR 53 MOVE REG-TAB (IND1) TO LIN-017.
           IF  IND1 = 18 OR 36 OR 54 MOVE REG-TAB (IND1) TO LIN-018.
           ADD    1   TO IND1
           IF  IND1 < IND2 AND IND1 NOT = 19 AND IND1 NOT = 37
               GO  TO        ROT-400-000.
           DISPLAY  LPA-001  MEN-005
           MOVE     " "  TO  RSP-OPC.

       ROT-400-010.
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-008
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-400-020.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-400-030.
           IF  IND1  =  IND2
               GO  TO        ROT-400-010.
           MOVE     SPACES   TO  TLA-AUX
           GO  TO   ROT-400-000.

       ROT-400-020.
           IF  IND1  =  19
               GO  TO   ROT-400-010.
           IF  IND1  >  19  AND <  38
               MOVE     01  TO IND1.
           IF  IND1  =  55  OR  >  37
               MOVE     19  TO IND1.
           MOVE     SPACES  TO TLA-AUX
           GO  TO   ROT-400-000.

       ROT-400-030.
           MOVE     SPACES   TO  TLA-AUX
           DISPLAY  LPA-001      TLA-004.

       ROT-500-000.
           DISPLAY  MEN-006
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-600-000.
           DISPLAY  MEN-007
           MOVE     1   TO   IND1
           OPEN     OUTPUT   RELATO.

       ROT-500-010.
           WRITE    REG-REL FROM REG-TAB (IND1)
           ADD      1   TO  IND1
           IF  IND1  <  IND2
               GO  TO        ROT-500-010.
           CLOSE    RELATO.

       ROT-600-000.
           MOVE     3   TO   PRM-001
           CLOSE    CADCMB   CADPRM
                    CADTMD   CADTNQ
           CHAIN   "DYN100"  USING  PRM-001.
