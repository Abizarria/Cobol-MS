       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN307.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADLDP        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-LDP.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADLDP      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADLDP.DAT".
       01  REG-LDP.
           03  CHV-LDP.
               05  PRX-LDP      PIC  9(01).
               05  SUF-LDP      PIC  9(03).
               05  DTA-LDP      PIC  9(06).
               05  LCT-LDP      PIC  9(03).
           03  FLG-LDP          PIC  9(01).
           03  PGT-LDP          PIC  9(06).
           03  HST-LDP          PIC  X(20).
           03  VAL-LDP          PIC S9(10)V99.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  RSP-OPC          PIC  X(01).
           03  PRX-AUX          PIC  9(01).
           03  TECLADO          PIC  9(02).
           03  ACM-002          PIC S9(12)V99.
           03  ACM-001          PIC S9(12)V99.
           03  TOT-002          PIC S9(12)V99.
           03  TOT-001          PIC S9(12)V99.

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 02 PIC X(66) FROM
               "POSICAO ATUAL DAS CONTAS  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.

       01  TLA-002.
           03  LINE 05 COLUMN 06 PIC X(67) FROM
               " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 06 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 06 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 07 COLUMN 06 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 10 PIC X(59) FROM "ORIGEM / GRUPO
               "  TOTAL LIQUIDADO     TOTAL PENDENTE"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 07 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 06 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 06 PIC X(67) FROM
               " ³  Obrig. Trabalhista .:
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 06 PIC X(67) FROM
               " ³  Obrigacoes Fiscais .:
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 06 PIC X(67) FROM
               " ³  Desp. Operacionais .:
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 06 PIC X(67) FROM
               " ³  Despesa de Capital .:
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 06 PIC X(67) FROM
               " ³  Creditos de Socios .:
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 06 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 06 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 10 PIC X(21) FROM "Total das Despesas .:"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 16 COLUMN 06 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 06 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 10 PIC X(21) FROM "Total das Compras ..:"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 18 COLUMN 06 PIC X(67) FROM
               " ³
               "         ³"   BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 06 PIC X(67) FROM
               " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 08 PIC X(67) FROM SPACES.

       01  ENT-001.
           03  OBR-001 LINE 09 COLUMN 32 PIC ---.---.---.--9,99
                       USING   ACM-001   BACKGROUND-COLOR  01.
           03  OBR-002 LINE 09 COLUMN 51 PIC ---.---.---.--9,99
                       USING   ACM-002   BACKGROUND-COLOR  01.
           03  OBF-001 LINE 10 COLUMN 32 PIC ---.---.---.--9,99
                       USING   ACM-001   BACKGROUND-COLOR  01.
           03  OBF-002 LINE 10 COLUMN 51 PIC ---.---.---.--9,99
                       USING   ACM-002   BACKGROUND-COLOR  01.
           03  DOP-001 LINE 11 COLUMN 32 PIC ---.---.---.--9,99
                       USING   ACM-001   BACKGROUND-COLOR  01.
           03  DOP-002 LINE 11 COLUMN 51 PIC ---.---.---.--9,99
                       USING   ACM-002   BACKGROUND-COLOR  01.
           03  DCP-001 LINE 12 COLUMN 32 PIC ---.---.---.--9,99
                       USING   ACM-001   BACKGROUND-COLOR  01.
           03  DCP-002 LINE 12 COLUMN 51 PIC ---.---.---.--9,99
                       USING   ACM-002   BACKGROUND-COLOR  01.
           03  CRS-001 LINE 13 COLUMN 32 PIC ---.---.---.--9,99
                       USING   ACM-001   BACKGROUND-COLOR  01.
           03  CRS-002 LINE 13 COLUMN 51 PIC ---.---.---.--9,99
                       USING   ACM-002   BACKGROUND-COLOR  01.
           03  TDP-001 LINE 15 COLUMN 31 PIC ----.---.---.--9,99
                       USING   TOT-001   BACKGROUND-COLOR  01
                                         FOREGROUND-COLOR  15.
           03  TDP-002 LINE 15 COLUMN 50 PIC ----.---.---.--9,99
                       USING   TOT-002   BACKGROUND-COLOR  01
                                         FOREGROUND-COLOR  15.
           03  CMP-001 LINE 17 COLUMN 31 PIC ----.---.---.--9,99
                       USING   ACM-001   BACKGROUND-COLOR  01
                                         FOREGROUND-COLOR  15.
           03  CMP-002 LINE 17 COLUMN 50 PIC ----.---.---.--9,99
                       USING   ACM-002   BACKGROUND-COLOR  01
                                         FOREGROUND-COLOR  15.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  ACUMUL
                   "ANDO MOVIMENTO ...                              "
                   BACKGROUND-COLOR  05   FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ESC] P/ RETO
                   "RNAR ...                                        "
                   BACKGROUND-COLOR  05   FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION.

       ROT-000-000.
           DISPLAY  TLA-001
                    TLA-002  MEN-001
           OPEN     INPUT    CADLDP
           MOVE     1        TO  PRX-AUX
           MOVE     ZEROS    TO  ACM-001  ACM-002
                                 TOT-001  TOT-002.
       ROT-100-000.
           IF  PRX-AUX   =   1   DISPLAY  OBR-001  OBR-002.
           IF  PRX-AUX   =   2   DISPLAY  OBF-001  OBF-002.
           IF  PRX-AUX   =   3   DISPLAY  DOP-001  DOP-002.
           IF  PRX-AUX   =   4   DISPLAY  DCP-001  DCP-002.
           IF  PRX-AUX   =   5   DISPLAY  CRS-001  CRS-002.
           IF  PRX-AUX   =   6   DISPLAY  CMP-001  CMP-002.

       ROT-100-010.
           MOVE     SPACES   TO  CHV-LDP
           MOVE     PRX-AUX  TO  PRX-LDP
           START    CADLDP   KEY GREATER  CHV-LDP
                    INVALID  KEY GO  TO   ROT-100-030.
       ROT-100-020.
           READ     CADLDP   NEXT AT END
                    GO  TO   ROT-100-030.
           IF  PRX-LDP  NOT   =  PRX-AUX
               GO  TO        ROT-100-030.
           IF  FLG-LDP   =   1
               ADD  VAL-LDP  TO  ACM-002
           ELSE
               ADD  VAL-LDP  TO  ACM-001.
           PERFORM  ROT-100-000
           GO  TO   ROT-100-020.

       ROT-100-030.
           ADD      ACM-001  TO  TOT-001
           ADD      ACM-002  TO  TOT-002
           MOVE     ZEROS    TO  ACM-001  ACM-002
           ADD      1        TO  PRX-AUX
           IF  PRX-AUX   =   6
               GO  TO        ROT-100-040.
           IF  PRX-AUX   >   6
               GO  TO        ROT-100-050.
           GO  TO   ROT-100-000.

       ROT-100-040.
           DISPLAY  TDP-001
                    TDP-002
           GO  TO   ROT-100-000.

       ROT-100-050.
           MOVE     SPACES   TO  RSP-OPC
           DISPLAY  MEN-002
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO  NOT   =   01
               GO  TO        ROT-100-050.
           MOVE     1        TO  PRM-001
           CLOSE    CADLDP
           CHAIN   "DYN300"  USING   PRM-001.
