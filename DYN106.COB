       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN106.
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
           03  IND1             PIC  9(01).
           03  IND2             PIC  9(02).
           03  IND3             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  LET-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(41)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  41.

           03  COL-TAB               OCCURS  04.
               05  LIN-TAB           OCCURS  55.
                   07  LTR-TAB  PIC  9(03)V9.
                   07  QTD-TAB  PIC  9(05).
                   07  LTM-TAB  PIC  9(02)V9.

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
           03  FILLER           PIC  X(29)  VALUE  "TABELA DE MEDIDAS".
           03  FILLER           PIC  X(09)  VALUE  "TANQUE :".
           03  NUM-CB2          PIC  X(02).
           03  FILLER           PIC  X(05)  VALUE  "  -".
           03  CMB-CB2          PIC  X(27).
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  DET-001.
           03  FILLER           PIC  X(02)  VALUE  "|".
           03  LT1-DT1          PIC  ZZ9,9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  QT1-DT1          PIC  ZZZZ9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  LM1-DT1          PIC  Z9,9.
           03  FILLER           PIC  X(04)  VALUE  " ||".
           03  LT2-DT1          PIC  ZZ9,9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  QT2-DT1          PIC  ZZZZ9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  LM2-DT1          PIC  Z9,9.
           03  FILLER           PIC  X(04)  VALUE  " ||".
           03  LT3-DT1          PIC  ZZ9,9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  QT3-DT1          PIC  ZZZZ9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  LM3-DT1          PIC  Z9,9.
           03  FILLER           PIC  X(04)  VALUE  " ||".
           03  LT4-DT1          PIC  ZZ9,9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  QT4-DT1          PIC  ZZZZ9.
           03  FILLER           PIC  X(01)  VALUE  SPACES.
           03  LM4-DT1          PIC  Z9,9.
           03  FILLER           PIC  X(02)  VALUE  " |".

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "EMISSAO DE TABELAS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 27 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 27 PIC X(29) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 62 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 62 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 62 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 62 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 27 PIC X(36) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 29 PIC X(34) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 34 PIC X(29) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 36 PIC X(27) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 42 PIC X(21) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-002.
           03  LINE 09 COLUMN 20 PIC X(40) FROM " 旼컴컴컴컴컴컴컴컴컴컴
               "컴컴컴컴컴컴컴커"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 20 PIC X(40) FROM " �
               "               �"    BACKGROUND-COLOR 01.
           03  LINE 10 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 20 PIC X(40) FROM " �  Tanque .:     -
               "               �"    BACKGROUND-COLOR 01.
           03  LINE 11 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 20 PIC X(40) FROM " �
               "               �"    BACKGROUND-COLOR 01.
           03  LINE 12 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 20 PIC X(40) FROM " 읕컴컴컴컴컴컴컴컴컴컴
               "컴컴컴컴컴컴컴켸"    BACKGROUND-COLOR 01.
           03  LINE 13 COLUMN 60 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 22 PIC X(40) FROM SPACES.

       01  ENT-001.
           03  TNQ-ENT LINE 11 COLUMN 34 PIC 9(02) USING CHV-TNQ
                       BACKGROUND-COLOR  01.
           03  CMB-ENT LINE 11 COLUMN 41 PIC X(15) USING DES-CMB
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

       01  LPA-002 LINE 11 COLUMN 41 PIC X(15) FROM SPACES
                   BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DO
                   "TANQUE  -  [ESC] RETORNA ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TANQUE INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DESEJA CONSULTAR NA
                   " TELA ? [S/N]  [ ]  -  [ESC] RETORNA ...        "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "TABELA DO TANQUE IN
                   "EXISTENTE !!!  -  [ESC] RETORNA ...             "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  CARREG
                   "ANDO TABELA DO TANQUE SOLICITADO ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "USE AS SETAS P/ ROL
                   "AR A TELA  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR !!!  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO TABELA DE MEDIDAS ...                      "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 80 PIC X(01) FROM " "
                   BACKGROUND-COLOR  05.

       01 OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                  BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01 OPC-002 LINE 22 COLUMN 49 PIC X(01) USING LET-OPC AUTO
                  BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001  TLA-002
           OPEN     INPUT    CADCMB  CADPRM
                             CADTMD  CADTNQ
           READ     CADPRM
           MOVE     1   TO   IND2.

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
           MOVE     ANO-SYS  TO  ANO-CB2.

       ROT-000-020.
           DISPLAY  LPA-002  MEN-001
           MOVE     0   TO   CHV-TNQ
           ACCEPT   (11 34)  CHV-TNQ  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-300-000.
           IF  CHV-TNQ   =   ZEROS
               GO  TO        ROT-000-020.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADTNQ   INVALID KEY
                    DISPLAY  MEN-002
                    ACCEPT   OPC-001
                    GO  TO   ROT-000-020.
           MOVE     CMB-TNQ  TO  CHV-CMB
           READ     CADCMB
           DISPLAY  ENT-001
           MOVE     SPACES   TO  CHV-TMD  RSP-OPC
           MOVE     CHV-TNQ  TO  TNQ-TMD
           START    CADTMD   KEY GREATER  CHV-TMD
                    INVALID  KEY GO  TO   ROT-000-060.
           READ     CADTMD   NEXT
           IF  TNQ-TMD  NOT  =   CHV-TNQ
               GO  TO        ROT-000-060.
           DISPLAY  MEN-005
           MOVE     1    TO  IND1   IND2.

       ROT-000-030.
           MOVE     ZEROS    TO  LTR-TAB (IND1, IND2)
                                 QTD-TAB (IND1, IND2)
                                 LTM-TAB (IND1, IND2)
           ADD      1        TO  IND2
           IF  IND2  <  56
               GO  TO            ROT-000-030.
           MOVE     1        TO  IND2
           ADD      1        TO  IND1
           IF  IND1  <  05
               GO  TO            ROT-000-030.
           MOVE     1        TO  IND1.

       ROT-000-040.
           MOVE     LTR-TMD  TO  LTR-TAB (IND1, IND2)
           MOVE     QTD-TMD  TO  QTD-TAB (IND1, IND2)
           MOVE     LTM-TMD  TO  LTM-TAB (IND1, IND2)
           READ     CADTMD   NEXT AT END
                    GO  TO   ROT-000-050.
           IF  TNQ-TMD  NOT  =   CHV-TNQ
               GO  TO        ROT-000-050.
           ADD      1   TO   IND2
           IF  IND2  >  55
               MOVE  1  TO   IND2
               ADD   1  TO   IND1.
           GO  TO            ROT-000-040.

       ROT-000-050.
           DISPLAY  MEN-003
           MOVE     " "  TO  LET-OPC
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           IF  LET-OPC  NOT  =  "S"  AND  "N"
               GO  TO        ROT-000-050.
           MOVE     1        TO  IND2
           MOVE     NOM-TAB  TO  NOM-CB1
           MOVE     CHV-TNQ  TO  NUM-CB2
           MOVE     DES-CMB  TO  CMB-CB2
           IF  LET-OPC   =   "S"
               GO  TO        ROT-100-000.
           IF  LET-OPC   =   "N"
               GO  TO        ROT-200-000.

       ROT-000-060.
           DISPLAY  MEN-004
           ACCEPT   OPC-001
           GO  TO   ROT-000-020.

       ROT-100-000.
           MOVE     SPACES   TO  TLA-AUX
           MOVE     CAB-001  TO  LIN-001
           MOVE     CAB-002  TO  LIN-003
           MOVE     CAB-003  TO  LIN-004.

       ROT-100-010.
           MOVE     LTR-TAB (1, IND2) TO  LT1-DT1
           MOVE     QTD-TAB (1, IND2) TO  QT1-DT1
           MOVE     LTM-TAB (1, IND2) TO  LM1-DT1
           MOVE     LTR-TAB (2, IND2) TO  LT2-DT1
           MOVE     QTD-TAB (2, IND2) TO  QT2-DT1
           MOVE     LTM-TAB (2, IND2) TO  LM2-DT1
           MOVE     LTR-TAB (3, IND2) TO  LT3-DT1
           MOVE     QTD-TAB (3, IND2) TO  QT3-DT1
           MOVE     LTM-TAB (3, IND2) TO  LM3-DT1
           MOVE     LTR-TAB (4, IND2) TO  LT4-DT1
           MOVE     QTD-TAB (4, IND2) TO  QT4-DT1
           MOVE     LTM-TAB (4, IND2) TO  LM4-DT1
           IF  IND2 = 01 OR 19 OR 37 MOVE DET-001 TO LIN-005.
           IF  IND2 = 02 OR 20 OR 38 MOVE DET-001 TO LIN-006.
           IF  IND2 = 03 OR 21 OR 39 MOVE DET-001 TO LIN-007.
           IF  IND2 = 04 OR 22 OR 40 MOVE DET-001 TO LIN-008.
           IF  IND2 = 05 OR 23 OR 41 MOVE DET-001 TO LIN-009.
           IF  IND2 = 06 OR 24 OR 42 MOVE DET-001 TO LIN-010.
           IF  IND2 = 07 OR 25 OR 43 MOVE DET-001 TO LIN-011.
           IF  IND2 = 08 OR 26 OR 44 MOVE DET-001 TO LIN-012.
           IF  IND2 = 09 OR 27 OR 45 MOVE DET-001 TO LIN-013.
           IF  IND2 = 10 OR 28 OR 46 MOVE DET-001 TO LIN-014.
           IF  IND2 = 11 OR 29 OR 47 MOVE DET-001 TO LIN-015.
           IF  IND2 = 12 OR 30 OR 48 MOVE DET-001 TO LIN-016.
           IF  IND2 = 13 OR 31 OR 49 MOVE DET-001 TO LIN-017.
           IF  IND2 = 14 OR 32 OR 50 MOVE DET-001 TO LIN-018.
           IF  IND2 = 15 OR 33 OR 51 MOVE DET-001 TO LIN-001.
           IF  IND2 = 16 OR 34 OR 52 MOVE DET-001 TO LIN-002.
           IF  IND2 = 17 OR 35 OR 53 MOVE DET-001 TO LIN-003.
           IF  IND2 = 18 OR 36 OR 54 MOVE DET-001 TO LIN-004.
           IF  IND2 = 55             MOVE DET-001 TO LIN-005.
           ADD    1   TO IND2
           IF  IND2 = 15 OR 33 OR 51
               GO  TO    ROT-100-020.
           IF  IND2 < 56
               GO  TO    ROT-100-010.
           MOVE     CAB-003  TO  LIN-006
           MOVE     SPACES   TO  LIN-007  LIN-008  LIN-009
                                 LIN-010  LIN-011  LIN-012
                                 LIN-013  LIN-014  LIN-015
                                 LIN-016  LIN-017  LIN-018.
       ROT-100-020.
           DISPLAY  LPA-001  MEN-006
           MOVE     " "  TO  RSP-OPC.

       ROT-100-030.
           ACCEPT   (22 80)  RSP-OPC  WITH  AUTO-SKIP
           DISPLAY  MEN-009
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   99
               GO  TO        ROT-100-050.
           IF  TECLADO  NOT  =   00
               GO  TO        ROT-100-040.
           IF  IND2  >  55
               GO  TO        ROT-100-030.
           MOVE     SPACES   TO  TLA-AUX
           GO  TO   ROT-100-010.

       ROT-100-040.
           MOVE     SPACES   TO  TLA-AUX
           DISPLAY  LPA-001
           DISPLAY  TLA-002      ENT-001
           GO  TO   ROT-200-000.

       ROT-100-050.
           IF  IND2  =  15
               GO  TO   ROT-100-030.
           IF  IND2  =  33
               MOVE     01  TO IND2
               GO  TO   ROT-100-000.
           IF  IND2  =  51
               MOVE     15  TO IND2.
           IF  IND2  =  56
               MOVE     33  TO IND2.
           MOVE     SPACES  TO TLA-AUX
           GO  TO   ROT-100-010.

       ROT-200-000.
           DISPLAY  MEN-007
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-020.
           DISPLAY  MEN-008
           OPEN     OUTPUT   RELATO
           WRITE    REG-REL FROM CAB-001  AFTER  0
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003.

       ROT-200-010.
           MOVE     LTR-TAB (1, IND2) TO  LT1-DT1
           MOVE     QTD-TAB (1, IND2) TO  QT1-DT1
           MOVE     LTM-TAB (1, IND2) TO  LM1-DT1
           MOVE     LTR-TAB (2, IND2) TO  LT2-DT1
           MOVE     QTD-TAB (2, IND2) TO  QT2-DT1
           MOVE     LTM-TAB (2, IND2) TO  LM2-DT1
           MOVE     LTR-TAB (3, IND2) TO  LT3-DT1
           MOVE     QTD-TAB (3, IND2) TO  QT3-DT1
           MOVE     LTM-TAB (3, IND2) TO  LM3-DT1
           MOVE     LTR-TAB (4, IND2) TO  LT4-DT1
           MOVE     QTD-TAB (4, IND2) TO  QT4-DT1
           MOVE     LTM-TAB (4, IND2) TO  LM4-DT1
           WRITE    REG-REL FROM DET-001
           ADD      1        TO  IND2
           IF  IND2  <  56
               GO  TO        ROT-200-010.
           WRITE    REG-REL FROM CAB-003
           CLOSE    RELATO
           GO  TO   ROT-000-020.

       ROT-300-000.
           MOVE     3    TO  PRM-001
           CLOSE    CADCMB   CADPRM
                    CADTMD   CADTNQ
           CHAIN   "DYN100"  USING  PRM-001.
