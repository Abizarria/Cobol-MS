       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN001.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADPRM        ASSIGN  TO  DISK.

       DATA            DIVISION.
       FILE            SECTION.

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

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  IND2             PIC  9(02).
           03  NUM-OPC          PIC  9(01).
           03  TECLADO          PIC  9(02).

           03  DTA-SYS.
               05  ANO-SYS      PIC  9(02).
               05  MES-SYS      PIC  9(02).
               05  DIA-SYS      PIC  9(02).

           03  DTA-AUX.
               05  DIA-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  MES-AUX      PIC  9(02).
               05  FILLER       PIC  X(01)  VALUE  "/".
               05  ANO-AUX      PIC  9(02).

           03  COD-AUX.
               05  CD1-AUX      PIC  9(03).
               05  CD2-AUX      PIC  9(02).

           03  NOM-TAB          PIC  X(36).
           03  RED-NOM          REDEFINES   NOM-TAB.
               05  LET-NOM      PIC  X(01)  OCCURS  36.

           03  TAB-LET          PIC  X(42)  VALUE
               "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ./-&,".
           03  RED-LET          REDEFINES   TAB-LET.
               05  LET-TAB      PIC  X(01)  OCCURS  42.

       01  PRM-001              PIC  9(01)  VALUE    9.

       SCREEN          SECTION.

       01  TLA-001.
           03  BLANK   SCREEN.
           03  LINE 01 COLUMN 01 PIC X(73) USING NOM-TAB
               BACKGROUND-COLOR  04.
           03  LINE 01 COLUMN 74 PIC 9(03) USING CD1-AUX
               BACKGROUND-COLOR  04.
           03  LINE 01 COLUMN 77 PIC X(01) FROM  "/"
               BACKGROUND-COLOR  04.
           03  LINE 01 COLUMN 78 PIC 9(02) USING CD2-AUX
               BACKGROUND-COLOR  04.
           03  LINE 01 COLUMN 80 PIC X(01) FROM  " "
               BACKGROUND-COLOR  04.
           03  LINE 03 COLUMN 01 PIC X(71) FROM
               " SISTEMA AUTO-POSTO  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 03 COLUMN 72 PIC X(09) USING  DTA-AUX
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 22 COLUMN 01 PIC X(13) FROM  " MENSAGEM ->"
               BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
           03  LINE 24 COLUMN 01 PIC X(80) FROM  "          DYNAMIC MICR
               "OINFORMATICA S/C LTDA  -  PABX: (011) 5548-7371"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.

       01  TLA-002.
           03  LINE 04 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 05 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.
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
           03  LINE 21 COLUMN 01 PIC X(80) FROM SPACES
               BACKGROUND-COLOR  07.

       01  TLA-003.
           03  LINE 06 COLUMN 27 PIC X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 27 PIC X(22) FROM " ³                  ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 29 PIC X(18) FROM "  Menu Principal"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  04.
           03  LINE 07 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 27 PIC X(22) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 27 PIC X(22) FROM " ³   1 - Estoque    ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 27 PIC X(22) FROM " ³   2 - Caixas     ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 27 PIC X(22) FROM " ³   3 - Despesas   ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 27 PIC X(22) FROM " ³   4 - Bancos     ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 27 PIC X(22) FROM " ³   5 - Clientes   ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 27 PIC X(22) FROM " ³   6 - Posicao    ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 27 PIC X(22) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 49 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 29 PIC X(22) FROM SPACES.

       01  LPA-001.
           03  BLANK   SCREEN.

       01  LPA-002.
           03  LINE 08 COLUMN 51 PIC X(05) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 51 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 51 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 51 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 51 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 51 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 51 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 51 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 51 PIC X(07) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 34 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 36 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001  LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DE
                    "SEJADA ? [ ]  -  [ESC] RETORNA ...        "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001.

       ROT-000-000.
           IF  PRM-001   =   0
               GO  TO        ROT-000-040.
           OPEN     INPUT    CADPRM
           READ     CADPRM
           MOVE     1   TO   IND1.

       ROT-000-010.
           MOVE     CLI-PRM (IND1)  TO  IND2
           MOVE     LET-TAB (IND2)  TO  LET-NOM (IND1)
           ADD      1   TO   IND1
           IF  IND1  <  37
               GO  TO        ROT-000-010.
           MOVE     COD-PRM  TO  COD-AUX
           CLOSE    CADPRM
           ACCEPT   DTA-SYS FROM DATE
           MOVE     12       TO  ANO-SYS
           IF  MES-SYS   <   12  MOVE 13  TO  ANO-SYS.
           MOVE     DIA-SYS  TO  DIA-AUX
           MOVE     MES-SYS  TO  MES-AUX
           MOVE     ANO-SYS  TO  ANO-AUX.
           DISPLAY  TLA-001      TLA-002      TLA-003.

       ROT-000-030.
           MOVE     ZEROS    TO  NUM-OPC
           DISPLAY  MEN-001      ACCEPT   OPC-001
           ACCEPT   TECLADO      FROM     ESCAPE  KEY
           IF  TECLADO   =   01  GO       ROT-000-050.
           MOVE     ZEROS    TO  PRM-001
           IF  NUM-OPC   =    1  CHAIN   "DYN100"  USING  PRM-001.
           IF  NUM-OPC   =    2  CHAIN   "DYN200"  USING  PRM-001.
           IF  NUM-OPC   =    3  CHAIN   "DYN300"  USING  PRM-001.
           IF  NUM-OPC   =    4  CHAIN   "DYN400"  USING  PRM-001.
           IF  NUM-OPC   =    5  CHAIN   "DYN500"  USING  PRM-001.
           IF  NUM-OPC   =    6  CHAIN   "DYN600"  USING  PRM-001.
           GO  ROT-000-030.

       ROT-000-040.
           DISPLAY  LPA-002      TLA-003  GO       ROT-000-030.

       ROT-000-050.
           DISPLAY  LPA-001      STOP     RUN.
