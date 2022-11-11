       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN514.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADCLI        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CLI.

           SELECT      CADPRM        ASSIGN  TO  DISK.

           SELECT      CADSRT        ASSIGN  TO  DISK.

           SELECT      RELATO        ASSIGN  TO  PRINTER.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADCLI      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCLI.DAT".
       01  REG-CLI.
           03  CHV-CLI          PIC  X(10).
           03  NOM-CLI          PIC  X(35).
           03  TEL-CLI          PIC  9(12).
           03  CNT-CLI          PIC  X(15).
           03  END-CLI          PIC  X(35).
           03  CID-CLI          PIC  X(20).
           03  CEP-CLI          PIC  9(08).
           03  EST-CLI          PIC  X(02).
           03  CGC-CLI          PIC  9(14).
           03  INS-CLI          PIC  X(15).
           03  CTG-CLI          PIC  9(01).

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
           03  CHV-SRT          PIC  X(10).
           03  NOM-SRT          PIC  X(35).
           03  TEL-SRT          PIC  9(12).
           03  CNT-SRT          PIC  X(15).
           03  END-SRT          PIC  X(35).
           03  CID-SRT          PIC  X(20).
           03  CEP-SRT          PIC  9(08).
           03  EST-SRT          PIC  X(02).
           03  CGC-SRT          PIC  9(14).
           03  INS-SRT          PIC  X(15).
           03  CTG-SRT          PIC  9(01).

       FD  RELATO      LABEL         RECORD  IS  OMITTED.

       01  REG-REL              PIC  X(80).

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  ACM-LIN          PIC  9(02).

           03  TEL-AUX.
               05  DDD-AUX      PIC  9(04).
               05  PRX-AUX      PIC  9(04).
               05  SUF-AUX      PIC  9(04).

           03  CEP-AUX.
               05  CP1-AUX      PIC  9(05).
               05  CP2-AUX      PIC  9(03).

           03  CGC-AUX.
               05  CG1-AUX      PIC  9(02).
               05  CG2-AUX      PIC  9(03).
               05  CG3-AUX      PIC  9(03).
               05  CG4-AUX      PIC  9(04).
               05  CG5-AUX      PIC  9(02).

           03  TAB-PGT.
               05  FILLER       PIC  X(09)  VALUE  "SEMANAL  ".
               05  FILLER       PIC  X(09)  VALUE  "DEZENAL  ".
               05  FILLER       PIC  X(09)  VALUE  "QUINZENAL".
               05  FILLER       PIC  X(09)  VALUE  "MENSAL   ".
               05  FILLER       PIC  X(09)  VALUE  "EVENTUAL ".
           03  RED-PGT          REDEFINES   TAB-PGT.
               05  PGT-TAB      PIC  X(09)  OCCURS  05.

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

       01  CAB-001.
           03  FILLER           PIC  X(22)  VALUE  "-DYNAMIC-".
           03  NOM-CB1          PIC  X(48).
           03  FILLER           PIC  X(08)  VALUE  "FOLHA :".
           03  PAG-CB1          PIC  9(02).

       01  CAB-002.
           03  FILLER           PIC  X(72)  VALUE
               "RELACAO COMPLETA DOS CLIENTES".
           03  DIA-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  MES-CB2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  ANO-CB2          PIC  9(02).

       01  CAB-003              PIC  X(80)  VALUE  ALL  "-".

       01  DET-001.
           03  COD-DT1          PIC  X(11).
           03  FILLER           PIC  X(02)  VALUE  "-".
           03  NOM-DT1          PIC  X(39).
           03  FILLER           PIC  X(13)  VALUE  "CATEGORIA .:".
           03  CTG-DT1          PIC  9(01).
           03  FILLER           PIC  X(05)  VALUE  "  -".
           03  PGT-DT1          PIC  X(09).

       01  DET-002.
           03  FILLER           PIC  X(13)  VALUE  SPACES.
           03  FILLER           PIC  X(11)  VALUE  "C.G.C. ..:".
           03  CG1-DT2          PIC  9(02).
           03  FILLER           PIC  X(01)  VALUE  ".".
           03  CG2-DT2          PIC  9(03).
           03  FILLER           PIC  X(01)  VALUE  ".".
           03  CG3-DT2          PIC  9(03).
           03  FILLER           PIC  X(01)  VALUE  "/".
           03  CG4-DT2          PIC  9(04).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  CG5-DT2          PIC  9(02).
           03  FILLER           PIC  X(10)  VALUE  SPACES.
           03  FILLER           PIC  X(13)  VALUE  "INSCR. EST.:".
           03  INS-DT2          PIC  X(15).

       01  DET-003.
           03  FILLER           PIC  X(13)  VALUE  "ENDERECO ..:".
           03  END-DT3          PIC  X(39).
           03  FILLER           PIC  X(14)  VALUE  "TELEFONE ..: (".
           03  DDD-DT3          PIC  9(04).
           03  FILLER           PIC  X(01)  VALUE  ")".
           03  PRX-DT3          PIC  ZZZ9.
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  SUF-DT3          PIC  9(04).

       01  DET-004.
           03  FILLER           PIC  X(13)  VALUE  SPACES.
           03  CP1-DT4          PIC  9(05).
           03  FILLER           PIC  X(01)  VALUE  "-".
           03  CP2-DT4          PIC  9(03).
           03  FILLER           PIC  X(02)  VALUE  SPACES.
           03  CID-DT4          PIC  X(22).
           03  EST-DT4          PIC  X(06).
           03  FILLER           PIC  X(13)  VALUE  "CONTATO ...:".
           03  CNT-DT4          PIC  X(15).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "RELACAO COMPLETA DOS CLIENTES  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.

       01  TLA-002.
           03  LINE 09 COLUMN 29 PIC X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 29 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 29 PIC X(21) FROM " ³  Relacao Total  ³
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 29 PIC X(21) FROM " ³                 ³
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 29 PIC X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 50 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 31 PIC X(21) FROM SPACES.

       01  LPA-001.
           03  LINE 06 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ IM
                   "PRIMIR !!!  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  IMPRIM
                   "INDO RELACAO DE CLIENTES ...                    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE CLIENTES
                   " CADASTRADOS  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ORDENA
                   "NDO O CADASTRO DE CLIENTES ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           OPEN     INPUT        CADCLI   CADPRM
           DISPLAY  TLA-001      LPA-001  TLA-002
           MOVE     SPACES   TO  CHV-CLI  RSP-OPC
           START    CADCLI   KEY GREATER  CHV-CLI
                    INVALID  KEY GO  TO   ROT-200-000.
           READ     CADPRM
           MOVE     1   TO   IND2.

       ROT-000-010.
           MOVE     CLI-PRM (IND2 + 1)  TO IND1
           MOVE     LET-TAB (IND1)  TO  LET-NOM (IND2)
           ADD      1   TO   IND2
           IF  IND2  <  36
               GO  TO        ROT-000-010.
           MOVE     SPACES   TO  LET-NOM (IND2)
           ACCEPT   DTA-SYS  FROM   DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           MOVE     NOM-TAB  TO  NOM-CB1
           MOVE     DIA-SYS  TO  DIA-CB2
           MOVE     MES-SYS  TO  MES-CB2
           MOVE     ANO-SYS  TO  ANO-CB2
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-001.

       ROT-000-020.
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM  ESCAPE   KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-030.
           DISPLAY  MEN-004
           SORT     CADSRT   ON  ASCENDING  KEY  NOM-SRT
                    INPUT        PROCEDURE  IS   ROT-100-000
                    OUTPUT       PROCEDURE  IS   ROT-200-000
           GO  TO   ROT-000-040.

       ROT-000-030.
           DISPLAY  MEN-003
           ACCEPT   OPC-001.

       ROT-000-040.
           MOVE     5    TO  PRM-001
           CLOSE    CADCLI   CADPRM
           CHAIN   "DYN500"  USING  PRM-001.

       ROT-100-000  SECTION.

       ROT-100-010.
           READ     CADCLI   NEXT AT END
                    GO  TO   ROT-100-020.
           MOVE     REG-CLI  TO  REG-SRT
           RELEASE  REG-SRT
           GO  TO   ROT-100-010.

       ROT-100-020.
           DISPLAY  MEN-002
           OPEN     OUTPUT   RELATO
           MOVE     1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  0.

       ROT-200-000  SECTION.

       ROT-200-010.
           WRITE    REG-REL FROM CAB-002  AFTER  2
           WRITE    REG-REL FROM CAB-003
           MOVE     ZEROS    TO  ACM-LIN.

       ROT-200-020.
           RETURN   CADSRT   AT  END
                    GO  TO   ROT-200-040.
           IF  ACM-LIN   <   11
               GO  TO        ROT-200-030.
           ADD      1        TO  PAG-CB1
           WRITE    REG-REL FROM CAB-001  AFTER  PAGE
           PERFORM  ROT-200-010.

       ROT-200-030.
           MOVE     CHV-SRT  TO  COD-DT1
           MOVE     NOM-SRT  TO  NOM-DT1
           MOVE     CTG-SRT  TO  CTG-DT1
           MOVE     PGT-TAB (CTG-SRT) TO  PGT-DT1
           MOVE     CGC-SRT  TO  CGC-AUX
           MOVE     CG1-AUX  TO  CG1-DT2
           MOVE     CG2-AUX  TO  CG2-DT2
           MOVE     CG3-AUX  TO  CG3-DT2
           MOVE     CG4-AUX  TO  CG4-DT2
           MOVE     CG5-AUX  TO  CG5-DT2
           MOVE     INS-SRT  TO  INS-DT2
           MOVE     END-SRT  TO  END-DT3
           MOVE     TEL-SRT  TO  TEL-AUX
           MOVE     DDD-AUX  TO  DDD-DT3
           MOVE     PRX-AUX  TO  PRX-DT3
           MOVE     SUF-AUX  TO  SUF-DT3
           MOVE     CEP-SRT  TO  CEP-AUX
           MOVE     CP1-AUX  TO  CP1-DT4
           MOVE     CP2-AUX  TO  CP2-DT4
           MOVE     CID-SRT  TO  CID-DT4
           MOVE     EST-SRT  TO  EST-DT4
           MOVE     CNT-SRT  TO  CNT-DT4
           WRITE    REG-REL FROM DET-001
           WRITE    REG-REL FROM DET-002
           WRITE    REG-REL FROM DET-003
           WRITE    REG-REL FROM DET-004
           WRITE    REG-REL FROM CAB-003
           ADD      1        TO  ACM-LIN
           GO  TO   ROT-200-020.

       ROT-200-040.
           CLOSE    RELATO.
