       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     DYN100.
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
           03  LINE 03 COLUMN 01 PIC  X(67) FROM
               " SISTEMA AUTO-POSTO  -  Ver. 7.11"
               BACKGROUND-COLOR  02   FOREGROUND-COLOR  00.
           03  LINE 06 COLUMN 27 PIC  X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 27 PIC  X(22) FROM " ³                  ³"
               BACKGROUND-COLOR  01.
           03  LINE 07 COLUMN 29 PIC  X(18) FROM "  Menu Principal"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  04.
           03  LINE 07 COLUMN 49 PIC  X(02) FROM SPACES.
           03  LINE 08 COLUMN 27 PIC  X(22) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 08 COLUMN 49 PIC  X(02) FROM SPACES.
           03  LINE 09 COLUMN 27 PIC  X(22) FROM " ³   1 - Estoque    ³"
               BACKGROUND-COLOR  01.
           03  LINE 09 COLUMN 49 PIC  X(02) FROM SPACES.
           03  LINE 10 COLUMN 27 PIC  X(22) FROM " ³   2 - Caixas     ³"
               BACKGROUND-COLOR  01.
           03  LINE 10 COLUMN 49 PIC  X(02) FROM SPACES.
           03  LINE 11 COLUMN 27 PIC  X(22) FROM " ³   3 - Despesas   ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 49 PIC  X(02) FROM SPACES.
           03  LINE 12 COLUMN 27 PIC  X(22) FROM " ³   4 - Bancos     ³"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 49 PIC  X(02) FROM SPACES.
           03  LINE 13 COLUMN 27 PIC  X(22) FROM " ³   5 - Clientes   ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 49 PIC  X(02) FROM SPACES.
           03  LINE 14 COLUMN 27 PIC  X(22) FROM " ³   6 - Posicao    ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 49 PIC  X(02) FROM SPACES.
           03  LINE 15 COLUMN 27 PIC  X(22) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 49 PIC  X(02) FROM SPACES.
           03  LINE 16 COLUMN 29 PIC  X(22) FROM SPACES.

       01  TLA-002.
           03  LINE 09 COLUMN 31 PIC  X(03) FROM " 1"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  01.
           03  LINE 08 COLUMN 34 PIC  X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 34 PIC  X(22) FROM " ³                  ³"
               BACKGROUND-COLOR  04.
           03  LINE 09 COLUMN 36 PIC  X(18) FROM "     Estoque"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  01.
           03  LINE 09 COLUMN 56 PIC  X(02) FROM SPACES.
           03  LINE 10 COLUMN 34 PIC  X(22) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  04.
           03  LINE 10 COLUMN 56 PIC  X(02) FROM SPACES.
           03  LINE 11 COLUMN 34 PIC  X(22) FROM " ³  1 - Cadastro    ³"
               BACKGROUND-COLOR  04.
           03  LINE 11 COLUMN 56 PIC  X(02) FROM SPACES.
           03  LINE 12 COLUMN 34 PIC  X(22) FROM " ³  2 - Movimento   ³"
               BACKGROUND-COLOR  04.
           03  LINE 12 COLUMN 56 PIC  X(02) FROM SPACES.
           03  LINE 13 COLUMN 34 PIC  X(22) FROM " ³  3 - Telemed     ³"
               BACKGROUND-COLOR  04.
           03  LINE 13 COLUMN 56 PIC  X(02) FROM SPACES.
           03  LINE 14 COLUMN 34 PIC  X(22) FROM " ³  4 - Posicao     ³"
               BACKGROUND-COLOR  04.
           03  LINE 14 COLUMN 56 PIC  X(02) FROM SPACES.
           03  LINE 15 COLUMN 34 PIC  X(22) FROM " ³  5 - Relatorios  ³"
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 56 PIC  X(02) FROM SPACES.
           03  LINE 16 COLUMN 34 PIC  X(22) FROM " ³  6 - Etiquetas   ³"
               BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 56 PIC  X(02) FROM SPACES.
           03  LINE 17 COLUMN 34 PIC  X(22) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 56 PIC  X(02) FROM SPACES.
           03  LINE 18 COLUMN 36 PIC  X(22) FROM SPACES.

       01  TLA-003.
           03  LINE 12 COLUMN 37 PIC  X(03) FROM " 2"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  04.
           03  LINE 12 COLUMN 40 PIC  X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 40 PIC  X(21) FROM " ³                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 42 PIC  X(17) FROM "    Movimento"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  04.
           03  LINE 13 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 14 COLUMN 40 PIC  X(21) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 15 COLUMN 40 PIC  X(21) FROM " ³  1 - Compras    ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 16 COLUMN 40 PIC  X(21) FROM " ³  2 - Acertos    ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 17 COLUMN 40 PIC  X(21) FROM " ³  3 - Reajustes  ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 18 COLUMN 40 PIC  X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 19 COLUMN 42 PIC  X(21) FROM SPACES.

       01  TLA-004.
           03  LINE 13 COLUMN 37 PIC  X(03) FROM " 3"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  04.
           03  LINE 12 COLUMN 40 PIC  X(21) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 40 PIC  X(21) FROM " ³                 ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 42 PIC  X(17) FROM "     Telemed"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  04.
           03  LINE 13 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 14 COLUMN 40 PIC  X(21) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 15 COLUMN 40 PIC  X(21) FROM " ³  1 - Cadastro   ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 16 COLUMN 40 PIC  X(21) FROM " ³  2 - Tabelas    ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 17 COLUMN 40 PIC  X(21) FROM " ³  3 - Calculos   ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 18 COLUMN 40 PIC  X(21) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 61 PIC  X(02) FROM SPACES.
           03  LINE 19 COLUMN 42 PIC  X(21) FROM SPACES.

       01  TLA-005.
           03  LINE 17 COLUMN 43 PIC  X(03)  FROM  " 3"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  01.
           03  LINE 14 COLUMN 46 PIC  X(19)  FROM  " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 46 PIC  X(19)  FROM  " ³               ³"
               BACKGROUND-COLOR  04.
           03  LINE 15 COLUMN 48 PIC  X(15)  FROM  "    Calculos"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  01.
           03  LINE 15 COLUMN 65 PIC  X(02)  FROM  SPACES.
           03  LINE 16 COLUMN 46 PIC  X(19)  FROM  " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  04.
           03  LINE 16 COLUMN 65 PIC  X(02)  FROM  SPACES.
           03  LINE 17 COLUMN 46 PIC  X(19)  FROM  " ³  1 - Regua    ³"
               BACKGROUND-COLOR  04.
           03  LINE 17 COLUMN 65 PIC  X(02)  FROM  SPACES.
           03  LINE 18 COLUMN 46 PIC  X(19)  FROM  " ³  2 - Telemed  ³"
               BACKGROUND-COLOR  04.
           03  LINE 18 COLUMN 65 PIC  X(02)  FROM  SPACES.
           03  LINE 19 COLUMN 46 PIC  X(19)  FROM  " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  04.
           03  LINE 19 COLUMN 65 PIC  X(02)  FROM  SPACES.
           03  LINE 20 COLUMN 48 PIC  X(19)  FROM  SPACES.

       01  TLA-006.
           03  LINE 15 COLUMN 37 PIC  X(03)  FROM  " 5"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  04.
           03  LINE 10 COLUMN 40 PIC  X(22) FROM " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 40 PIC  X(22) FROM " ³                  ³"
               BACKGROUND-COLOR  01.
           03  LINE 11 COLUMN 42 PIC  X(18) FROM "    Relatorios"
               BACKGROUND-COLOR  07   FOREGROUND-COLOR  04.
           03  LINE 11 COLUMN 62 PIC  X(02) FROM SPACES.
           03  LINE 12 COLUMN 40 PIC  X(22) FROM " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR  01.
           03  LINE 12 COLUMN 62 PIC  X(02) FROM SPACES.
           03  LINE 13 COLUMN 40 PIC  X(22) FROM " ³   1 - Precos     ³"
               BACKGROUND-COLOR  01.
           03  LINE 13 COLUMN 62 PIC  X(02) FROM SPACES.
           03  LINE 14 COLUMN 40 PIC  X(22) FROM " ³   2 - Critico    ³"
               BACKGROUND-COLOR  01.
           03  LINE 14 COLUMN 62 PIC  X(02) FROM SPACES.
           03  LINE 15 COLUMN 40 PIC  X(22) FROM " ³   3 - Vendas     ³"
               BACKGROUND-COLOR  01.
           03  LINE 15 COLUMN 62 PIC  X(02) FROM SPACES.
           03  LINE 16 COLUMN 40 PIC  X(22) FROM " ³   4 - Compras    ³"
               BACKGROUND-COLOR  01.
           03  LINE 16 COLUMN 62 PIC  X(02) FROM SPACES.
           03  LINE 17 COLUMN 40 PIC  X(22) FROM " ³   5 - Acertos    ³"
               BACKGROUND-COLOR  01.
           03  LINE 17 COLUMN 62 PIC  X(02) FROM SPACES.
           03  LINE 18 COLUMN 40 PIC  X(22) FROM " ³   6 - Completo   ³"
               BACKGROUND-COLOR  01.
           03  LINE 18 COLUMN 62 PIC  X(02) FROM SPACES.
           03  LINE 19 COLUMN 40 PIC  X(22) FROM " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR  01.
           03  LINE 19 COLUMN 62 PIC  X(02) FROM SPACES.
           03  LINE 20 COLUMN 42 PIC  X(22) FROM SPACES.

       01  TLA-007.
           03  LINE 13 COLUMN 44  PIC  X(03)  FROM  " 1"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.
           03  LINE 13 COLUMN 49  PIC  X(14)  FROM  "    Precos"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.

       01  TLA-008.
           03  LINE 14 COLUMN 44  PIC  X(03)  FROM  " 2"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.
           03  LINE 13 COLUMN 49  PIC  X(14)  FROM  "    Critico"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.

       01  TLA-009.
           03  LINE 15 COLUMN 44  PIC  X(03)  FROM  " 3"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.
           03  LINE 13 COLUMN 49  PIC  X(14)  FROM  "    Vendas"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.

       01  TLA-010.
           03  LINE 16 COLUMN 44  PIC  X(03)  FROM  " 4"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.
           03  LINE 13 COLUMN 49  PIC  X(14)  FROM  "    Compras"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.

       01  TLA-011.
           03  LINE 17 COLUMN 44  PIC  X(03)  FROM  " 5"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.
           03  LINE 13 COLUMN 49  PIC  X(14)  FROM  "    Acertos"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.

       01  TLA-012.
           03  LINE 18 COLUMN 44  PIC  X(03)  FROM  " 6"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.
           03  LINE 13 COLUMN 49  PIC  X(14)  FROM  "   Completo"
               BACKGROUND-COLOR   07   FOREGROUND-COLOR  01.

       01  TLA-013.
           03  LINE 12 COLUMN 47  PIC  X(18)  FROM  " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
               BACKGROUND-COLOR   04.
           03  LINE 13 COLUMN 47  PIC  X(18)  FROM  " ³              ³"
               BACKGROUND-COLOR   04.
           03  LINE 13 COLUMN 65  PIC  X(02)  FROM  SPACES.
           03  LINE 14 COLUMN 47  PIC  X(18)  FROM  " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
               BACKGROUND-COLOR   04.
           03  LINE 14 COLUMN 65  PIC  X(02)  FROM  SPACES.
           03  LINE 15 COLUMN 47  PIC  X(18)  FROM  " ³  1 - Total   ³"
               BACKGROUND-COLOR   04.
           03  LINE 15 COLUMN 65  PIC  X(02)  FROM  SPACES.
           03  LINE 16 COLUMN 47  PIC  X(18)  FROM  " ³  2 - Grupo   ³"
               BACKGROUND-COLOR   04.
           03  LINE 16 COLUMN 65  PIC  X(02)  FROM  SPACES.
           03  LINE 17 COLUMN 47  PIC  X(18)  FROM  " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
               BACKGROUND-COLOR   04.
           03  LINE 17 COLUMN 65  PIC  X(02)  FROM  SPACES.
           03  LINE 18 COLUMN 49  PIC  X(18)  FROM  SPACES.

       01  LPA-001.
           03  LINE 04 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 05 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 06 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 07 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 08 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 09 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 10 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 11 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 12 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 13 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 14 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 15 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 16 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 17 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 18 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 19 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 20 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.
           03  LINE 21 COLUMN 01 PIC  X(80)  FROM  SPACES
               BACKGROUND-COLOR  07.

       01  LPA-002.
           03  LINE 10 COLUMN 58  PIC  X(04)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 11 COLUMN 58  PIC  X(06)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 12 COLUMN 58  PIC  X(06)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 13 COLUMN 58  PIC  X(06)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 14 COLUMN 58  PIC  X(06)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 15 COLUMN 58  PIC  X(06)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 16 COLUMN 58  PIC  X(06)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 17 COLUMN 58  PIC  X(06)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 18 COLUMN 58  PIC  X(06)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 19 COLUMN 40  PIC  X(24)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 20 COLUMN 42  PIC  X(22)  FROM  SPACES
               BACKGROUND-COLOR   07.

       01  LPA-003.
           03  LINE 14 COLUMN 63  PIC  X(02)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 15 COLUMN 63  PIC  X(04)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 16 COLUMN 63  PIC  X(04)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 17 COLUMN 63  PIC  X(04)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 18 COLUMN 63  PIC  X(04)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 19 COLUMN 63  PIC  X(04)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 20 COLUMN 48  PIC  X(19)  FROM  SPACES
               BACKGROUND-COLOR   07.

       01  LPA-004.
           03  LINE 12 COLUMN 64  PIC  X(01)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 13 COLUMN 64  PIC  X(03)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 14 COLUMN 64  PIC  X(03)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 15 COLUMN 64  PIC  X(03)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 16 COLUMN 64  PIC  X(03)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 17 COLUMN 64  PIC  X(03)  FROM  SPACES
               BACKGROUND-COLOR   07.
           03  LINE 18 COLUMN 64  PIC  X(03)  FROM  SPACES
               BACKGROUND-COLOR   07.

       01  MEN-001  LINE  22  COLUMN  14  PIC  X(67)  FROM  "NUMERO DA O
                    "PCAO DESEJADA ? [ ]  -  [ESC] RETORNA ...         "
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.

       01  OPC-001  LINE  22  COLUMN  42  PIC  9(01) USING  NUM-OPC AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.
       01  OPC-002  LINE  22  COLUMN  42  PIC  9(01) USING  PRM-001 AUTO
                    BACKGROUND-COLOR  05  FOREGROUND-COLOR  15.

       PROCEDURE       DIVISION  CHAINING  PRM-001.

       ROT-000-000.
           IF  PRM-001   =   0
               GO  TO        ROT-000-010.
           DISPLAY  LPA-001  TLA-001
                    TLA-002  MEN-001
           GO  TO   ROT-000-020, ROT-200-000, ROT-300-000, ROT-000-020,
                    ROT-500-000, ROT-000-020, DEPENDING    ON  PRM-001.

       ROT-000-010.
           DISPLAY  TLA-002.

       ROT-000-020.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =   01
               MOVE      0   TO  PRM-001
               GO  TO        ROT-000-040.
           IF  NUM-OPC   <   1  OR  >  6
               GO  TO        ROT-000-020.
           GO  TO   ROT-100-000, ROT-200-000, ROT-300-000,
                    ROT-400-000, ROT-500-000, ROT-600-000,
                    DEPENDING    ON  NUM-OPC.

       ROT-000-030.
           DISPLAY  LPA-002
           GO  TO   ROT-000-010.

       ROT-000-040.
           CHAIN    "DYN001"  USING  PRM-001.

       ROT-100-000.
           CHAIN    "DYN101".

       ROT-200-000.
           DISPLAY  TLA-003.

       ROT-200-010.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =    01
               GO  TO        ROT-000-030.
           IF  NUM-OPC   <   1  OR  >  3
               GO  TO        ROT-200-010.
           IF  NUM-OPC   =   1   CHAIN  "DYN102".
           IF  NUM-OPC   =   2   CHAIN  "DYN103".
           IF  NUM-OPC   =   3   CHAIN  "DYN104".

       ROT-300-000.
           DISPLAY  TLA-004.

       ROT-300-010.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =    01
               GO  TO        ROT-000-030.
           IF  NUM-OPC   <   1  OR  >  3
               GO  TO        ROT-300-010.
           IF  NUM-OPC   =   1   CHAIN  "DYN105".
           IF  NUM-OPC   =   2   CHAIN  "DYN106".
           DISPLAY  TLA-005.

       ROT-300-020.
           MOVE     0    TO  PRM-001
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =    01
               GO  TO        ROT-300-030.
           IF  PRM-001   <   1  OR  >  2
               GO  TO        ROT-300-020.
           CHAIN   "DYN107"  USING   PRM-001.

       ROT-300-030.
           DISPLAY  LPA-003
           GO  TO   ROT-300-000.

       ROT-400-000.
           CHAIN    "DYN108".

       ROT-500-000.
           DISPLAY  TLA-006.

       ROT-500-010.
           MOVE     0    TO  NUM-OPC
           ACCEPT   OPC-001
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =    01
               GO  TO        ROT-000-030.
           IF  NUM-OPC   <   1  OR  >  6
               GO  TO        ROT-500-010.
           DISPLAY  TLA-013
           IF  NUM-OPC   =   1   DISPLAY  TLA-007.
           IF  NUM-OPC   =   2   DISPLAY  TLA-008.
           IF  NUM-OPC   =   3   DISPLAY  TLA-009.
           IF  NUM-OPC   =   4   DISPLAY  TLA-010.
           IF  NUM-OPC   =   5   DISPLAY  TLA-011.
           IF  NUM-OPC   =   6   DISPLAY  TLA-012.

       ROT-500-020.
           MOVE     0    TO  PRM-001
           ACCEPT   OPC-002
           ACCEPT   TECLADO  FROM ESCAPE  KEY
           IF  TECLADO   =    01
               GO  TO        ROT-500-030.
           IF  PRM-001   <   1  OR  >  2
               GO  TO        ROT-500-020.
           IF  NUM-OPC   =   1   CHAIN  "DYN109"  USING  PRM-001.
           IF  NUM-OPC   =   2   CHAIN  "DYN110"  USING  PRM-001.
           IF  NUM-OPC   =   3   CHAIN  "DYN111"  USING  PRM-001.
           IF  NUM-OPC   =   4   CHAIN  "DYN112"  USING  PRM-001.
           IF  NUM-OPC   =   5   CHAIN  "DYN113"  USING  PRM-001.
           IF  NUM-OPC   =   6   CHAIN  "DYN114"  USING  PRM-001.

       ROT-500-030.
           DISPLAY  LPA-004
           GO  TO   ROT-500-000.

       ROT-600-000.
           CHAIN    "DYN115".
