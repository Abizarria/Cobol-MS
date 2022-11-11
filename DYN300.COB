       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN300.
       AUTHOR.         ADILSON.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
                       DECIMAL-POINT         IS  COMMA.

       DATA            DIVISION.

       WORKING-STORAGE SECTION.

       01  AUXILIARES.
           03  NUM-OPC          PIC  9(01).
           03  TECLADO          PIC  9(02).

       01  PRM-001              PIC  9(01).

       SCREEN          SECTION.

       01  TLA-001.
           03  LINE 03 COLUMN 01 PIC X(67) FROM
               " SISTEMA AUTO-POSTO  -  Ver. 7.11"
               BACKGROUND-COLOR  02  FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 27 PIC X(22) FROM " ³                  ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 29 PIC X(18) FROM "  Menu Principal"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
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

       01  TLA-002.
           03  LINE 11 COLUMN 31 PIC X(03) FROM " 3"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.
           03  LINE 08 COLUMN 34 PIC X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 34 PIC X(22) FROM " ³                  ³
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 36 PIC X(18) FROM "     Despesas"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  01.
           03  LINE 09 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 10 COLUMN 34 PIC X(22) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 11 COLUMN 34 PIC X(22) FROM " ³  1 - Cadastro    ³
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 34 PIC X(22) FROM " ³  2 - Pendentes   ³
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 34 PIC X(22) FROM " ³  3 - Correcoes   ³
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 34 PIC X(22) FROM " ³  4 - Consultas   ³
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 34 PIC X(22) FROM " ³  5 - Posicao     ³
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 34 PIC X(22) FROM " ³  6 - Relatorios  ³
               BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 34 PIC X(22) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 56 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 36 PIC X(22) FROM SPACES.

       01  TLA-003.
           03  LINE 14 COLUMN 37 PIC X(03)  FROM  " 4"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 12 COLUMN 40 PIC X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 40 PIC X(22) FROM " ³                  ³
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 42 PIC X(18) FROM "    Consultas"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 13 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 40 PIC X(22) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 40 PIC X(22) FROM " ³  1 - Movimentos  ³
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 40 PIC X(22) FROM " ³  2 - Pendencias  ³
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 40 PIC X(22) FROM " ³  3 - Pagamentos  ³
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 40 PIC X(22) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 42 PIC X(22) FROM SPACES.

       01  TLA-004.
           03  LINE 16 COLUMN 37 PIC X(03)  FROM  " 6"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 10 COLUMN 40 PIC X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 40 PIC X(22) FROM " ³                  ³
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 42 PIC X(18) FROM "    Relatorios"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR  04.
           03  LINE 11 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 12 COLUMN 40 PIC X(22) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 13 COLUMN 40 PIC X(22) FROM " ³  1 - Movimentos  ³
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 40 PIC X(22) FROM " ³  2 - Acumulados  ³
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 40 PIC X(22) FROM " ³  3 - Extratos    ³
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 40 PIC X(22) FROM " ³  4 - Pendencias  ³
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 40 PIC X(22) FROM " ³  5 - Codigos     ³
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 40 PIC X(22) FROM " ³  6 - Pagamentos  ³
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 19 COLUMN 40 PIC X(22) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 62 PIC X(02) FROM SPACES.
           03  LINE 20 COLUMN 42 PIC X(22) FROM SPACES.

       01  TLA-005.
           03  LINE 13 COLUMN 43 PIC X(03) FROM " 1"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 01.
           03  LINE 13 COLUMN 48 PIC X(15)  FROM  "   Movimentos"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 01.

       01  TLA-006.
           03  LINE 14 COLUMN 43 PIC X(03) FROM " 2"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 01.
           03  LINE 13 COLUMN 48 PIC X(15) FROM "   Acumulado"
               BACKGROUND-COLOR  07  FOREGROUND-COLOR 01.

       01  TLA-007.
           03  LINE 12 COLUMN 46 PIC X(19) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 46 PIC X(19) FROM " ³               ³"
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 14 COLUMN 46 PIC X(19) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 15 COLUMN 46 PIC X(19) FROM " ³   1 - Total   ³"
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 16 COLUMN 46 PIC X(19) FROM " ³   2 - Grupo   ³"
               BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 17 COLUMN 46 PIC X(19) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 65 PIC X(02) FROM SPACES.
           03  LINE 18 COLUMN 48 PIC X(19) FROM SPACES.

       01  LPA-001.
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

       01  LPA-002.
           03  LINE 10 COLUMN 58 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 60 PIC X(04) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 58 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 58 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 58 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 58 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 58 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 58 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 58 PIC X(06) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 40 PIC X(24) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 42 PIC X(22) FROM SPACES
               BACKGROUND-COLOR  07.

       01  LPA-003.
           03  LINE 12 COLUMN 64 PIC X(01) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 64 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 64 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 64 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 64 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 64 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 64 PIC X(03) FROM SPACES
               BACKGROUND-COLOR  07.

       01  MEN-001 LINE 22 COLUMN 14 PIC X(67) FROM "NUMERO DA OPCAO DES
                   "EJADA ? [ ]  -  [ESC] RETORNA ...         "
                    BACKGROUND-COLOR  05 FOREGROUND-COLOR 15.

       01  OPC-001  LINE 22 COLUMN 42 PIC 9(01) USING NUM-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.
       01  OPC-002  LINE 22 COLUMN 42 PIC 9(01) USING PRM-001 AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR 15.

       PROCEDURE       DIVISION  CHAINING  PRM-001.

       ROT-000-000.
           IF  PRM-001   =   0
               GO  TO        ROT-000-010.
           DISPLAY  LPA-001  TLA-001
                             MEN-001
           IF  PRM-001   =   4
               DISPLAY       TLA-002
               GO  TO        ROT-400-000.
           IF  PRM-001   =   6
               DISPLAY       TLA-002
               GO  TO        ROT-600-000.

       ROT-000-010.
           DISPLAY  TLA-002.

       ROT-000-020.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               GO  TO        ROT-000-040.
           IF  NUM-OPC   <   1  OR  >  6
               GO  TO        ROT-000-020.
           IF  NUM-OPC   =   1   CHAIN  "DYN301".
           IF  NUM-OPC   =   2   CHAIN  "DYN302".
           IF  NUM-OPC   =   3   CHAIN  "DYN303".
           IF  NUM-OPC   =   5   CHAIN  "DYN307".
           IF  NUM-OPC   =   4
               GO  TO        ROT-400-000.
           IF  NUM-OPC   =   6
               GO  TO        ROT-600-000.

       ROT-000-030.
           DISPLAY  LPA-002
           GO  TO   ROT-000-010.

       ROT-000-040.
           MOVE     ZEROS     TO    PRM-001
           CHAIN   "DYN001"  USING  PRM-001.

       ROT-400-000.
           DISPLAY  TLA-003.

       ROT-400-010.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =    01
               GO  TO        ROT-000-030.
           IF  NUM-OPC   <   1  OR  >  3
               GO  TO        ROT-400-010.
           IF  NUM-OPC   =   1   CHAIN  "DYN304".
           IF  NUM-OPC   =   2   CHAIN  "DYN305".
           IF  NUM-OPC   =   3   CHAIN  "DYN306".

       ROT-600-000.
           DISPLAY  TLA-004.

       ROT-600-010.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =    01
               GO  TO        ROT-000-030.
           IF  NUM-OPC   <   1  OR  >  6
               GO  TO        ROT-600-010.
           IF  NUM-OPC   =   3   CHAIN  "DYN310".
           IF  NUM-OPC   =   4   CHAIN  "DYN311".
           IF  NUM-OPC   =   5   CHAIN  "DYN312".
           IF  NUM-OPC   =   6   CHAIN  "DYN313".
           DISPLAY  TLA-007
           IF  NUM-OPC   =   1  DISPLAY  TLA-005.
           IF  NUM-OPC   =   2  DISPLAY  TLA-006.

       ROT-600-020.
           MOVE     0    TO  PRM-001
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =    01
               DISPLAY       LPA-003
               GO  TO        ROT-600-000.
           IF  PRM-001   <   1  OR  >  2
               GO  TO        ROT-600-020.
           IF  NUM-OPC   =   1   CHAIN  "DYN308"  USING  PRM-001.
           IF  NUM-OPC   =   2   CHAIN  "DYN309"  USING  PRM-001.
