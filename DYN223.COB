       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN223.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.
       FILE-CONTROL.

           SELECT      CADBCO        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-BCO.

           SELECT      CADCMB        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CMB.

           SELECT      CADCXA        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-CXA.

           SELECT      CADEST        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-EST.

           SELECT      CADTNQ        ASSIGN  TO  DISK
                       ORGANIZATION          IS  INDEXED
                       ACCESS        MODE    IS  DYNAMIC
                       RECORD        KEY     IS  CHV-TNQ.

           SELECT      CADSDO        ASSIGN  TO  DISK.

       DATA            DIVISION.
       FILE            SECTION.

       FD  CADBCO      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADBCO.DAT".
       01  REG-BCO.
           03  CHV-BCO          PIC  9(03).
           03  NOM-BCO          PIC  X(15).
           03  SDI-BCO          PIC S9(11)V99.

       FD  CADCMB      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCMB.DAT".
       01  REG-CMB.
           03  CHV-CMB          PIC  9(01).
           03  DES-CMB          PIC  X(15).
           03  VVM-CMB          PIC  9(11)V99.
           03  LUC-CMB          PIC  9(04)V9999.

       FD  CADCXA      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADCXA.DAT".
       01  REG-CXA.
           03  CHV-CXA.
               05  TUR-CXA      PIC  9(01).
               05  NUM-CXA      PIC  9(01).
           03  POS-CXA          PIC  9(01).
           03  DTA-CXA          PIC  9(06).
           03  DES-CXA          PIC  X(15).

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

       FD  CADTNQ      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADTNQ.DAT".
       01  REG-TNQ.
           03  CHV-TNQ          PIC  9(02).
           03  CMB-TNQ          PIC  9(01).
           03  SDI-TNQ          PIC S9(05)V9.
           03  SDA-TNQ          PIC S9(05)V9.

       FD  CADSDO      LABEL         RECORD  IS  STANDARD
                       VALUE    OF   FILE-ID     "CADSDO.DAT".
       01  REG-SDO.
           03  SDA-SDO          PIC  9(10)V99.
           03  CRD-SDO          PIC S9(11)V99  OCCURS  05.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  IND1             PIC  9(02).
           03  RSP-OPC          PIC  X(01).
           03  TECLADO          PIC  9(02).
           03  TQ1-AUX          PIC  9(02).
           03  TQ2-AUX          PIC  9(02).
           03  TQ3-AUX          PIC  9(02).
           03  TQ4-AUX          PIC  9(02).
           03  TQ5-AUX          PIC  9(02).
           03  NTF-AUX          PIC  9(06).
           03  QTD-AUX          PIC S9(05)V9.
           03  CST-AUX          PIC  9(04)V9999.
           03  SDO-AUX          PIC  9(10)V99.
           03  VAL-AUX          PIC S9(10)V99.
           03  ACM-TOT          PIC S9(11)V99.

           03  DTA-AUX.
               05  ANO-AUX      PIC  9(02).
               05  MES-AUX      PIC  9(02).
               05  DIA-AUX      PIC  9(02).

       01  PRM-001              PIC  9(01).

       01  PRM-002.
           03  BCO-PR2          PIC  9(03).
           03  VAL-PR2          PIC  9(10)V99.

       01  PRM-003.
           03  CMP-PR3          OCCURS  10.
               05  CMB-PR3      PIC  9(01).
               05  NTF-PR3      PIC  9(06).
               05  VCT-PR3      PIC  9(06).
               05  TQ1-PR3      PIC  9(02).
               05  TQ2-PR3      PIC  9(02).
               05  TQ3-PR3      PIC  9(02).
               05  TQ4-PR3      PIC  9(02).
               05  TQ5-PR3      PIC  9(02).
               05  QTD-PR3      PIC S9(05)V9.
               05  CST-PR3      PIC  9(04)V9999.
               05  VAL-PR3      PIC S9(10)V99.

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 01 PIC X(67) FROM
               " MOVIMENTO DIARIO  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.

       01  TLA-002.
           03  LINE 08 COLUMN 14 PIC X(52) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"   BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 14 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 14 PIC X(52) FROM " ³  PARA EMITIR O DIARI
               "O DE VENDAS, E NECESSARIO  ³"   BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 14 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 14 PIC X(52) FROM " ³  QUE SEJA CONFIRMADO
               " O MOVIMENTO DO CAIXA ...  ³"   BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 14 PIC X(52) FROM " ³
               "                           ³"   BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 14 PIC X(52) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"   BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 66 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 16 PIC X(52) FROM SPACES.

       01  TLA-003.
           03  LINE 07 COLUMN 19 PIC X(42) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"  BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 19 PIC X(42) FROM " ³
               "                 ³"  BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 27 PIC X(26) FROM "DEPOSITO DO SALDO ANTER
               "IOR"   BACKGROUND-COLOR 01 FOREGROUND-COLOR 14.
           03  LINE 09 COLUMN 19 PIC X(42) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"  BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 19 PIC X(42) FROM " ³
               "                 ³"  BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 19 PIC X(42) FROM " ³  Banco ...:      -
               "                 ³"  BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 19 PIC X(42) FROM " ³
               "                 ³"  BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 19 PIC X(42) FROM " ³  Valor ...:
               "                 ³"  BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 19 PIC X(42) FROM " ³
               "                 ³"  BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 19 PIC X(42) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"  BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 19 PIC X(42) FROM " ³
               "                 ³"  BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 23 PIC X(18) FROM "Saldo Pendente ..."
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 16 COLUMN 41 PIC Z.ZZZ.ZZZ.ZZ9,99  USING  SDA-SDO
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 19 PIC X(42) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"  BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 61 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 21 PIC X(42) FROM SPACES.

       01  TLA-004.
           03  LINE 06 COLUMN 08 PIC X(65) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 08 PIC X(65) FROM " ³           LANCAMENTO
               " DAS COMPRAS DE COMBUSTIVEIS            ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 21 PIC X(38) FROM
               "LANCAMENTO DAS COMPRAS DE COMBUSTIVEIS"
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  14.
           03  LINE 07 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 08 COLUMN 08 PIC X(65) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 09 COLUMN 08 PIC X(65) FROM " ³
               "                                        ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 08 PIC X(65) FROM " ³  Combustivel .:    -
               "                                        ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 08 PIC X(65) FROM " ³
               "                                        ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 08 PIC X(65) FROM " ³  Nota Fiscal .:
               "       Custo Litro .:                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 08 PIC X(65) FROM " ³  Vencimento ..:   /
               " /     Custo Total .:                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 08 PIC X(65) FROM " ³  Quantidade ..:
               "       Tanques .....:                   ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 08 PIC X(65) FROM " ³
               "                                        ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 08 PIC X(65) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 08 PIC X(65) FROM " ³
               "                                        ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 12 PIC X(38) FROM
               "Valor Total das Compras .............."
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 51 PIC ---.---.---.--9,99 USING  ACM-TOT
               BACKGROUND-COLOR  01  FOREGROUND-COLOR  15.
           03  LINE 17 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 08 PIC X(65) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
               "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 73 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 10 PIC X(65) FROM SPACES.

       01  ENT-001.
           03  LINE 11 COLUMN 34 PIC 9(03) USING CHV-BCO
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 42 PIC X(15) USING NOM-BCO
               BACKGROUND-COLOR  01.

       01  ENT-002  LINE 13 COLUMN 34 PIC Z.ZZZ.ZZZ.ZZ9,99 USING SDO-AUX
                    BACKGROUND-COLOR  01.

       01  ENT-003.
           03  LINE 10 COLUMN 27 PIC 9(01) USING CHV-CMB
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 33 PIC X(15) USING DES-CMB
               BACKGROUND-COLOR  01.

       01  ENT-004  LINE 12 COLUMN 27 PIC 9(06) USING NTF-AUX
                    BACKGROUND-COLOR  01.

       01  ENT-005  LINE 13 COLUMN 27 PIC 9(02) USING DIA-AUX
                    BACKGROUND-COLOR  01.

       01  ENT-006  LINE 13 COLUMN 30 PIC 9(02) USING MES-AUX
                    BACKGROUND-COLOR  01.

       01  ENT-007  LINE 13 COLUMN 33 PIC 9(02) USING ANO-AUX
                    BACKGROUND-COLOR  01.

       01  ENT-008  LINE 14 COLUMN 26 PIC ---.--9,9
                    USING QTD-AUX  BACKGROUND-COLOR  01.

       01  ENT-009  LINE 12 COLUMN 53 PIC Z.ZZ9,9999
                    USING CST-AUX  BACKGROUND-COLOR  01.

       01  ENT-010  LINE 13 COLUMN 52 PIC --.---.---.--9,99
                    USING VAL-AUX  BACKGROUND-COLOR  01.

       01  ENT-011.
           03  TQ1-ENT LINE 14 COLUMN 53 PIC 9(02) USING TQ1-AUX
                       BACKGROUND-COLOR  01.
           03  TQ2-ENT LINE 14 COLUMN 56 PIC 9(02) USING TQ2-AUX
                       BACKGROUND-COLOR  01.
           03  TQ3-ENT LINE 14 COLUMN 59 PIC 9(02) USING TQ3-AUX
                       BACKGROUND-COLOR  01.
           03  TQ4-ENT LINE 14 COLUMN 62 PIC 9(02) USING TQ4-AUX
                       BACKGROUND-COLOR  01.
           03  TQ5-ENT LINE 14 COLUMN 65 PIC 9(02) USING TQ5-AUX
                       BACKGROUND-COLOR  01.

       01  ENT-012  LINE 13 COLUMN 27 PIC X(07) FROM "A VISTA"
                    BACKGROUND-COLOR  01.

       01  LPA-001.
           03  LINE 05 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 06 PIC X(70) FROM SPACES
               BACKGROUND-COLOR  07.

       01  LPA-002  LINE 11 COLUMN 42 PIC X(15) FROM SPACES
                    BACKGROUND-COLOR  01.
       01  LPA-003  LINE 13 COLUMN 34 PIC X(16) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-004.
           03  LINE 10 COLUMN 33 PIC X(15) FROM SPACES
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 27 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  01.

       01  LPA-005  LINE 13 COLUMN 27 PIC X(02) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-006  LINE 13 COLUMN 30 PIC X(02) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-007  LINE 13 COLUMN 33 PIC X(02) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-008  LINE 14 COLUMN 26 PIC X(09) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-009  LINE 12 COLUMN 53 PIC X(10) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-010  LINE 13 COLUMN 52 PIC X(17) FROM SPACES
                    BACKGROUND-COLOR  01.

       01  LPA-011.
           03  LPA-TQ1 LINE 14 COLUMN 53 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-TQ2 LINE 14 COLUMN 56 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-TQ3 LINE 14 COLUMN 59 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-TQ4 LINE 14 COLUMN 62 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.
           03  LPA-TQ5 LINE 14 COLUMN 65 PIC X(02) FROM SPACES
                       BACKGROUND-COLOR  01.

       01  LPA-012  LINE 13 COLUMN 27 PIC X(08) FROM "  /  /"
                    BACKGROUND-COLOR  01.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "AGUARDE !!!  PESQUI
                   "SANDO MOVIMENTO ...                       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-002 LINE 22 COLUMN 14 PIC X(67) FROM "NAO EXISTE NENHUM C
                   "AIXA CONFIRMADO !!!  -  [ESC] RETORNA ... "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-003 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "BANCO  -  [ESC] RETORNA ...               "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-004 LINE 22 COLUMN 14 PIC X(67) FROM "BANCO INEXISTENTE !
                   "!!  -  [ESC] RETORNA ...                  "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-005 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O VALOR DEPO
                   "SITADO  -  [ESC] RETORNA ...              "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-006 LINE 22 COLUMN 14 PIC X(67) FROM "VALOR INCOERENTE !!
                   "!  -  [ESC] RETORNA ...                   "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-007 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O CODIGO DO
                   "COMBUSTIVEL  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-008 LINE 22 COLUMN 14 PIC X(67) FROM "COMBUSTIVEL INEXIST
                   "ENTE !!!  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-009 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O NUMERO DA
                   "NOTA FISCAL  -  [ESC] RETORNA ...         "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-010 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A DATA DO VE
                   "NCIMENTO  -  [ESC] RETORNA ...            "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-011 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE A QUANTIDADE
                   " EM LITROS  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-012 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O PRECO DE C
                   "USTO UNITARIO  -  [ESC] RETORNA ...       "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-013 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE O PRECO DE C
                   "USTO TOTAL  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-014 LINE 22 COLUMN 14 PIC X(67) FROM "DIGITE OS TANQUES D
                   "E DESCARGA  -  [ESC] RETORNA ...          "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-015 LINE 22 COLUMN 14 PIC X(67) FROM "TANQUE INEXISTENTE
                   "!!!  -  [ESC] RETORNA ...                 "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-016 LINE 22 COLUMN 14 PIC X(67) FROM "DESCARGA INCOERENTE
                   " !!!  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-017 LINE 22 COLUMN 14 PIC X(67) FROM "DESCARGA JA LANCADA
                   " !!!  -  [ESC] RETORNA ...                "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  MEN-018 LINE 22 COLUMN 14 PIC X(67) FROM "TECLE [ENTER] P/ CO
                   "NFIRMAR OS DADOS  -  [ESC] RETORNA ...    "
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       01  OPC-001 LINE 22 COLUMN 80 PIC X(01) USING RSP-OPC AUTO
                   BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001  PRM-002.

       ROT-000-000.
           DISPLAY  TLA-001  MEN-001
           OPEN     INPUT    CADBCO  CADCMB
                             CADCXA  CADEST
                             CADTNQ  CADSDO
           READ     CADSDO
           MOVE     1   TO   IND1
           IF  PRM-001   =   2
               GO  TO        ROT-200-000.
           MOVE     ZEROS    TO  PRM-002
           MOVE     SPACES   TO  CHV-CXA
           START    CADCXA   KEY GREATER  CHV-CXA.

       ROT-000-010.
           READ     CADCXA   NEXT AT END
                    GO  TO   ROT-000-020.
           IF  POS-CXA   =   1
               GO  TO        ROT-000-010.
           GO  TO   ROT-100-000.

       ROT-000-020.
           DISPLAY  LPA-001
                    TLA-002  MEN-002.

       ROT-000-030.
           MOVE     " "  TO  RSP-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE KEY
           IF  TECLADO  NOT   =   01
               GO  TO        ROT-000-030.

       ROT-000-040.
           MOVE     6   TO   PRM-001
           CLOSE    CADBCO   CADCMB  CADCXA
                    CADEST   CADTNQ  CADSDO
           CHAIN   "DYN200"  USING   PRM-001.

       ROT-100-000.
           IF  SDA-SDO   =   ZEROS
               GO  TO        ROT-200-000.

       ROT-100-010.
           DISPLAY  LPA-001      TLA-003
           MOVE     BCO-PR2  TO  CHV-BCO
           MOVE     VAL-PR2  TO  SDO-AUX.

       ROT-100-020.
           DISPLAY  LPA-003
                    LPA-002  MEN-003
           ACCEPT   (11 34)  CHV-BCO  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.
           IF  CHV-BCO   =   ZEROS
               GO  TO        ROT-200-000.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADBCO   INVALID KEY
                    DISPLAY  MEN-004
                    ACCEPT   OPC-001
                    GO  TO   ROT-100-020.
           DISPLAY  ENT-001.

       ROT-100-030.
           DISPLAY  LPA-003  MEN-005
           ACCEPT   (13 34)  SDO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-020.
           IF  SDO-AUX   =   ZEROS
               GO  TO        ROT-100-030.
           MOVE     SPACES   TO  RSP-OPC
           IF  SDO-AUX   >   SDA-SDO
               DISPLAY       MEN-006
               ACCEPT        OPC-001
               GO  TO        ROT-100-030.
           DISPLAY  ENT-002  MEN-018
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-100-030.
           MOVE     CHV-BCO  TO  BCO-PR2
           MOVE     SDO-AUX  TO  VAL-PR2.

       ROT-200-000.
           MOVE     ZEROS    TO  VAL-PR3 (IND1)
                                 CMB-PR3 (IND1)  NTF-PR3 (IND1)
                                 VCT-PR3 (IND1)  QTD-PR3 (IND1)
                                 CST-PR3 (IND1)  TQ1-PR3 (IND1)
                                 TQ2-PR3 (IND1)  TQ3-PR3 (IND1)
                                 TQ4-PR3 (IND1)  TQ5-PR3 (IND1)
           ADD      1        TO  IND1
           IF  IND1    <     11
               GO  TO        ROT-200-000.
           MOVE     1        TO  IND1
           MOVE     ZEROS    TO  ACM-TOT
           DISPLAY  LPA-001      TLA-004.

       ROT-200-010.
           MOVE     ZEROS    TO  NTF-AUX  DTA-AUX  QTD-AUX  TQ1-AUX
                                 TQ2-AUX  TQ3-AUX  TQ4-AUX  TQ5-AUX.

       ROT-200-020.
           DISPLAY  LPA-004  MEN-007
           MOVE      0  TO   CHV-CMB
           ACCEPT   (10 27)  CHV-CMB  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               IF  SDA-SDO   =  ZEROS
                   GO  TO    ROT-000-040
               ELSE
                   GO  TO    ROT-100-010.
           IF  CHV-CMB   =   ZEROS
               GO  TO        ROT-300-000.
           MOVE     SPACES   TO  RSP-OPC
           READ     CADCMB   INVALID KEY
                    DISPLAY  MEN-008
                    ACCEPT   OPC-001
                    GO  TO   ROT-200-020.
           MOVE     0        TO  PRX-EST
           MOVE     CHV-CMB  TO  SUF-EST
           READ     CADEST
           MOVE     CST-EST  TO  CST-AUX
           DISPLAY  ENT-003.

       ROT-200-030.
           DISPLAY  LPA-005  MEN-009
           ACCEPT   (12 27)  NTF-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-020.
           IF  NTF-AUX   =   ZEROS
               GO  TO        ROT-200-030.
           DISPLAY  ENT-004.

       ROT-200-040.
           DISPLAY  LPA-006  MEN-010
           ACCEPT   (13 27)  DIA-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-030.
           IF  DIA-AUX   =   0
               DISPLAY       ENT-012
               GO  TO        ROT-200-070.
           IF  DIA-AUX   >   31
               GO  TO        ROT-200-040.
           DISPLAY  ENT-005.

       ROT-200-050.
           DISPLAY  LPA-007
           ACCEPT   (13 30)  MES-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-040.
           IF  MES-AUX   <   1  OR  > 12
               GO  TO        ROT-200-050.
           DISPLAY  ENT-006.

       ROT-200-060.
           DISPLAY  LPA-008  MEN-010
           ACCEPT   (13 33)  ANO-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-050.
           DISPLAY  ENT-007.

       ROT-200-070.
           DISPLAY  LPA-008
                    LPA-009  MEN-011
           ACCEPT   (14 27)  QTD-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               IF  DIA-AUX   =   0
                   DISPLAY   LPA-012
                   GO  TO    ROT-200-040
               ELSE
                   GO  TO    ROT-200-060.
           IF  QTD-AUX   =   ZEROS
               GO  TO        ROT-200-070.
           DISPLAY  ENT-008.

       ROT-200-080.
           DISPLAY  LPA-009
                    LPA-010  MEN-012
           ACCEPT   (12 53)  CST-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-070.
           COMPUTE  VAL-AUX  =   CST-AUX
                             *   QTD-AUX
           DISPLAY  ENT-009.

       ROT-200-090.
           DISPLAY  LPA-010
                    LPA-TQ1  MEN-013
           ACCEPT   (13 53)  VAL-AUX  WITH  UPDATE
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-080.
           IF  VAL-AUX   =   ZEROS
               GO  TO        ROT-200-090.
           COMPUTE  CST-AUX  =   VAL-AUX
                             /   QTD-AUX
           DISPLAY  ENT-009      ENT-010.

       ROT-200-100.
           DISPLAY  LPA-TQ2  MEN-014
           ACCEPT   (14 53)  TQ1-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-090.
           IF  TQ1-AUX   =   ZEROS
               GO  TO        ROT-200-100.
           MOVE     TQ1-AUX  TO  CHV-TNQ
           MOVE     SPACES   TO  RSP-OPC
           READ     CADTNQ   INVALID KEY
                    DISPLAY  MEN-015
                    ACCEPT   OPC-001
                    GO  TO   ROT-200-100.
           IF  CMB-TNQ  NOT  =   CHV-CMB
               DISPLAY       MEN-016
               ACCEPT        OPC-001
               GO  TO        ROT-200-100.

       ROT-200-110.
           DISPLAY  TQ1-ENT  LPA-TQ3
           ACCEPT   (14 56)  TQ2-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-100.
           IF  TQ2-AUX   =   ZEROS
               DISPLAY       LPA-TQ2
               GO  TO        ROT-200-150.
           MOVE     SPACES   TO  RSP-OPC
           IF  TQ2-AUX   =   TQ1-AUX
               DISPLAY       MEN-017
               ACCEPT        OPC-001
               GO  TO        ROT-200-110.
           MOVE     TQ2-AUX  TO  CHV-TNQ
           READ     CADTNQ   INVALID KEY
                    DISPLAY  MEN-015
                    ACCEPT   OPC-001
                    GO  TO   ROT-200-110.
           IF  CMB-TNQ  NOT  =   CHV-CMB
               DISPLAY       MEN-016
               ACCEPT        OPC-001
               GO  TO        ROT-200-110.

       ROT-200-120.
           DISPLAY  TQ2-ENT  LPA-TQ4
           ACCEPT   (14 59)  TQ3-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-110.
           IF  TQ3-AUX   =   ZEROS
               DISPLAY       LPA-TQ3
               GO  TO        ROT-200-150.
           MOVE     SPACES   TO  RSP-OPC
           IF  TQ3-AUX   =   TQ1-AUX  OR
                             TQ2-AUX
               DISPLAY       MEN-017
               ACCEPT        OPC-001
               GO  TO        ROT-200-120.
           MOVE     TQ3-AUX  TO  CHV-TNQ
           READ     CADTNQ   INVALID KEY
                    DISPLAY  MEN-015
                    ACCEPT   OPC-001
                    GO  TO   ROT-200-120.
           IF  CMB-TNQ  NOT  =   CHV-CMB
               DISPLAY       MEN-016
               ACCEPT        OPC-001
               GO  TO        ROT-200-120.

       ROT-200-130.
           DISPLAY  TQ3-ENT  LPA-TQ5
           ACCEPT   (14 62)  TQ4-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-120.
           IF  TQ4-AUX   =   ZEROS
               DISPLAY       LPA-TQ4
               GO  TO        ROT-200-150.
           MOVE     SPACES   TO  RSP-OPC
           IF  TQ4-AUX   =   TQ1-AUX  OR
               TQ2-AUX  OR   TQ3-AUX
               DISPLAY       MEN-017
               ACCEPT        OPC-001
               GO  TO        ROT-200-130.
           MOVE     TQ4-AUX  TO  CHV-TNQ
           READ     CADTNQ   INVALID KEY
                    DISPLAY  MEN-015
                    ACCEPT   OPC-001
                    GO  TO   ROT-200-130.
           IF  CMB-TNQ  NOT  =   CHV-CMB
               DISPLAY       MEN-016
               ACCEPT        OPC-001
               GO  TO        ROT-200-130.

       ROT-200-140.
           DISPLAY  TQ4-ENT  MEN-014
           ACCEPT   (14 65)  TQ5-AUX  WITH  UPDATE  AUTO-SKIP
           ACCEPT            TECLADO  FROM  ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-200-130.
           IF  TQ5-AUX   =   ZEROS
               DISPLAY       LPA-TQ5
               GO  TO        ROT-200-150.
           MOVE     SPACES   TO  RSP-OPC
           IF  TQ5-AUX   =   TQ1-AUX  OR
               TQ2-AUX  OR   TQ3-AUX  OR  TQ4-AUX
               DISPLAY       MEN-017
               ACCEPT        OPC-001
               GO  TO        ROT-200-140.
           MOVE     TQ5-AUX  TO  CHV-TNQ
           READ     CADTNQ   INVALID KEY
                    DISPLAY  MEN-015
                    ACCEPT   OPC-001
                    GO  TO   ROT-200-140.
           IF  CMB-TNQ  NOT  =   CHV-CMB
               DISPLAY       MEN-016
               ACCEPT        OPC-001
               GO  TO        ROT-200-140.
           DISPLAY  TQ5-ENT.

       ROT-200-150.
           DISPLAY  MEN-018
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               DISPLAY       LPA-TQ3
                             LPA-TQ4
                             LPA-TQ5
               GO  TO        ROT-200-100.
           MOVE     CHV-CMB  TO  CMB-PR3 (IND1)
           MOVE     NTF-AUX  TO  NTF-PR3 (IND1)
           MOVE     DTA-AUX  TO  VCT-PR3 (IND1)
           MOVE     QTD-AUX  TO  QTD-PR3 (IND1)
           MOVE     CST-AUX  TO  CST-PR3 (IND1)
           MOVE     VAL-AUX  TO  VAL-PR3 (IND1)
           MOVE     TQ1-AUX  TO  TQ1-PR3 (IND1)
           MOVE     TQ2-AUX  TO  TQ2-PR3 (IND1)
           MOVE     TQ3-AUX  TO  TQ3-PR3 (IND1)
           MOVE     TQ4-AUX  TO  TQ4-PR3 (IND1)
           MOVE     TQ5-AUX  TO  TQ5-PR3 (IND1)
           ADD      VAL-AUX  TO  ACM-TOT
           DISPLAY  TLA-004
           ADD      1        TO  IND1
           IF  IND1   <      10
               GO  TO        ROT-200-010.

       ROT-300-000.
           MOVE     2   TO   PRM-001
           CHAIN   "DYN224"  USING   PRM-001
                             PRM-002 PRM-003.
