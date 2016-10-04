      *
      *   Collection letters data transmission file to generate:
      *
      *
      *     1.  31 days past due
      *     2.  final notice
      *
      *
      * Author: Niki - example taken from LET103 (Dana)
      *   Date: 2005-07
      *
      *
      * CHANGES:
      *
      *  WR B0204                                       2016-09-22   Dana
      *   - Change the wording for the letters.
      *
      *  WO B0105
      *
      *  WO D8F86 - remove 'cc.' and 'dd.' from 30 day letter.
      *           - uppercase sender's name.
      *
      *  20061109  Signature change: DARWIN to JANET      RDN
      *  20081020  Signature change: Janet to J245H       LRR
      *  20081020  Signature change: Hahn to C820H        LRR
      *  Note new secure libraries for signatures used as well OCT 20/08 LRR
      *  20090717  Signature change: C820H to A203J       BEN
      *  20100730  Signature enlarged: Janet,
      *            Title changed from Controller to Accounting Manager: Janet
     H Datedit(*YMD)
     H Option(*SrcStmt:*NodebugIO)

     FMGPLTR    IF   E           K DISK    UsrOpn ExtDesc('FTPLIB/MGPLTR')
     FGRPFILE   IF   E           K DISK    UsrOpn ExtDesc('PRDDTALIB/GAUSRPE1')
     FADMFILE   IF   E           K DISK    UsrOpn ExtDesc('PRDDTALIB/GAUSRPRO')


     FTMNC320P  O    E             PRINTER

     F*  Empty file...
     FQERROR    O    F  132        PRINTER UsrOpn OFLIND(*INOC)
     F*  Summary of '31 days past due' letters...
     FReport    O    F  132        PRINTER UsrOpn OFLIND(*INOA)
     F*  Summary of 'over 31 days past due' letters...
     FReport2   O    F  132        PRINTER UsrOpn OFLIND(*INOB)

      * Constants
     DFirstLine        C                   1
     DLastLine         C                   60
     DTrue             C                   *On
     DFalse            C                   *Off
     DLineLength       C                   95
     DLineSpace        C                   0.4
     DUpper            C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     DLower            C                   'abcdefghijklmnopqrstuvwxyz'
     DBoldLine         C                   .03
     DRegularLine      C                   .02
     DUnderlineOffset  C                   .05

     DPastDue          C                   '01'
     DFinalNotice      C                   '02'
     DErrPastDue       C                   '03'
     DErrFinalNotice   C                   '04'

     DSignLibM         C                   'SBCMID'
     DSignLibP         C                   'SBCPEO'
     DMgrPAGESEG       C                   'J245H'
     DClerkPAGESEG     C                   'A203J'
     DMgrNAME          C                   'Janet Houndle'
     DClerkNAME        C                   'Aija Jarvi'
     DMgrPosition      C                   'Accounting Manager'
     DClerkPosition    C                   'Group Billing'

     DTimesNewRoman    C                   '2308'
     DTimesNew...
     D RomanBold       C                   '2309'
     DTimesNew...
     D RomanItalic     C                   '2310'
     DTimesNewRom...
     D anItalicBold    C                   '2311'

     DOtherFont        C                   '416'

     DIndicatorPtr     S               *   Inz(%Addr(*In))
     DIndicator        DS                  Based(IndicatorPtr)

     DToday            S               D
     DLetterDate       S               D
     DBillDate         S               D
     DToday@           S             10
     DPgmToSys         S              5S 3
     DPosition         S              5  0
     DLibrary          S             10
     DPageSeg          S              8
     DDrawer#          S              4    INZ('0003')
     DMessage          S            130
     DLastAddress      S              1  0
     DAdminName        S             50
     DNUM8             S              8  0
     DCHAR8            S              8

     DPhraseSize       S            500
     DRePhrase         S                   Like(PhraseSize)
     DLetter1P         S                   Like(PhraseSize)
     DLetter2P         S                   Like(PhraseSize)
     DLetter3P         S                   Like(PhraseSize)
     DLetter4P         S                   Like(PhraseSize)

     DFilDS            DS
     D FilRCDCOD                      4
     D FilID                          2    OVERLAY(FilRCDCOD:1)
     D FilRCDID                       2  0 OVERLAY(FilRCDCOD:3)
     D FilCMPNY                       2
     D FilGRPID                       9
     D  FilGRPNO                      5  0 OVERLAY(FilGRPID:1)
     D  G5                            5    OVERLAY(FilGRPID:1)
     D  FilSECTION                    3  0 OVERLAY(FilGRPID:6)
     D  G3                            3    OVERLAY(FilGRPID:6)
     D  FilCHKDIGIT                   1  0 OVERLAY(FilGRPID:9)
     D FilNAME                       32
     D FilWholeAddr                 160
     D FilADDRESS1                   32    OVERLAY(FilWholeAddr:1)
     D FilADDRESS2                   32    OVERLAY(FilWholeAddr:33)
     D FilADDRESS3                   32    OVERLAY(FilWholeAddr:65)
     D FilADDRESS4                   32    OVERLAY(FilWholeAddr:97)
     D FilADDRESS5                   32    OVERLAY(FilWholeAddr:129)
     D FilPOSTALCODE                  6
     D FilBILLPMDATE                  8
     D FilBLNCDPMIND                  2
     D FilPMAMOUNT                   10  2
     D FilBILL1MDATE                  8
     D FilBLNCD1MIND                  2
     D Fil1MAMOUNT                   10  2
     D FilBILL2MDATE                  8
     D FilBLNCD2MIND                  2
     D Fil2MAMOUNT                   10  2
     D FilGRPPHONE                   10  0
     D FilLANGUAGE                    1
     D FilNUMBER                      2
     D FilBROKER                     25
     D FilGRPADMIN                   25
     D FilRUNTOTAL                   10  2

     DcDate            S             18


      *----------------------------------------------------------------
      *
      *   P R O C E D U R E    P R O T O T Y P E S
      *
      *----------------------------------------------------------------
     DTMNC320          PR

     DTMNC320          PI

     D/COPY PRDSRC/QRPGLESRC,GOESTOP

     DOpenFiles        PR              N

     DOverRideFiles    PR

     DRunCommand       PR
     DCommand@                      500    Options(*VarSize) Const

     DRequestLetters   PR

     DPrintHeaderInfo  PR
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

     DPrintPastDue...
     D Letter          PR

     DPrintPastDue...
     D Body            PR
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

     DPrintDateMoney   PR
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

     DPrintSummary     PR
     Dv                                    Like(FilNUMBER) Value

     DChkEndOfPage     PR
     Dv                                    Like(FilNUMBER) Value

     DPrintFinal...
     D NoticeLetter    PR

     DPrintFinal...
     D NoticeBody      PR
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

     DPrintSignature   PR
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value
     Dv                                    Like(FilNUMBER) Value

     DSetUpLetter...
     D Wording         PR
     Dv                                    Like(FilNUMBER) Value

     DEnglishDate      PR         32767    Varying
     DDate                             D   Value

     DGetNumber        PR             1
     DDate                            8

     DWritePhrase      PR
     DPhrase                      32767    Value Varying
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

     DCutPhrase        PR                  Like(Position)
     DPhrase                      32767    Value Varying
     DPosition                             Value Like(Position)

     DWriteLineInfo    PR
     DInfo                        32767    Value Varying
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

     DCapitalize       PR         32767    Varying
     DString                      32767    Value Varying

     DToLower          PR         32767    Varying
     DString                      32767    Value Varying
     DPosition                        5  0 Value

     DToUpper          PR         32767    Varying
     DString                      32767    Value Varying
     DPosition                        5  0 Value

     DDraw...
     D HorizontalLine  PR
     DRow_                                 Like(PgmToSys) Value
     DCol_                                 Like(PgmToSys) Value
     DLength_                              Like(PgmToSys) Value
     DWidth_                               Like(PgmToSys) Value

     DDrawBox          PR
     DStartX                               Like(PgmToSys) Value
     DStartY                               Like(PgmToSys) Value
     DEndX                                 Like(PgmToSys) Value
     DEndY                                 Like(PgmToSys) Value
     DWidth_                               Like(PgmToSys) Value

     DUnderline        PR
     DStartX                               Like(PgmToSys) Value
     DStartY                               Like(PgmToSys) Value
     DLength                               Like(PgmToSys) Value
     DWidth                                Like(PgmToSys) Value

     DNewPage          PR
     Dv                                    Like(FilNUMBER) Value

      /Free

       // make sure the files open
       If OpenFiles();

          Today = %date;
          Today@ = %char(Today);

          RequestLetters();

       Endif;

       *InLR = True;
       Return;

      /End-Free

     O*================================================================
     O*
     O*                       R E P O R T S
     O*
     O*================================================================
     OQERROR    E            Header3        2  1
     O                                           14 'TMNC320 (Niki)'
     O                                           70 'Saskatchewan Blue Cross'
     O                       Today@             100

     O          E            Header3        2
     O                                           30 'MGPLTR - File is empty'

     OReport    E            Header         2  1
     O                                           14 'TMNC320 (Acct)'
     O                                           70 'Saskatchewan Blue Cross'
     O                       Today@             100
     O                                          120 'Page:'
     O                       Page               123

     O          E            Header         2
     O                                           78 '31 Days Past Due'

     O          E            Header         2
     O                                            5 'Group'
     O                                           15 'Name'
     O                                           52 'PM Date'
     O                                           66 'PM Amount'
     O                                           76 '1M Date'
     O                                           89 '1M Amount'
     O                                           99 '2M Date'
     O                                          112 '2M Amount'

     O          E            Detail         1
     O                       G5                   5
     O                                            6 '-'
     O                       G3                   9
     O                       FilNAME             43
     O                       FilBILLPMDATE       53
     O                       FilPMAMOUNT   4     66
     O                       FilBILL1MDATE       76
     O                       Fil1MAMOUNT   4     89
     O                       FilBILL2MDATE       99
     O                       Fil2MAMOUNT   4    112

     OReport2   E            Header2        2  1
     O                                           14 'TMNC320 (Acct)'
     O                                           70 'Saskatchewan Blue Cross'
     O                       Today@             100
     O                                          120 'Page:'
     O                       Page               123

     O          E            Header2        2
     O                                           78 'Final Notice'

     O          E            Header2        2
     O                                            5 'Group'
     O                                           15 'Name'
     O                                           52 'PM Date'
     O                                           66 'PM Amount'
     O                                           76 '1M Date'
     O                                           89 '1M Amount'
     O                                           99 '2M Date'
     O                                          112 '2M Amount'

     O          E            Detail2        1
     O                       G5                   5
     O                                            6 '-'
     O                       G3                   9
     O                       FilNAME             43
     O                       FilBILLPMDATE       53
     O                       FilPMAMOUNT   4     66
     O                       FilBILL1MDATE       76
     O                       Fil1MAMOUNT   4     89
     O                       FilBILL2MDATE       99
     O                       Fil2MAMOUNT   4    112

      *****************************************************************
      *
      *           P R O C E D U R E    I N T E R F A C E S
      *
      *****************************************************************
     P*------------------------------------------------------*
     P* Open files for proccessing
     P*------------------------------------------------------*
     POpenFiles        B

     D                 PI              N
      /Free

       OverRideFiles();              // over ride files

       Monitor;

          Open MGPLTR;             // open TRANSMISSION file

          Open GRPFILE;            // group names
          Open ADMFILE;            // group names

          Open QERROR;             // open QERROR file
          Open REPORT;             // open REPORT file
          Open REPORT2;            // open REPORT2 file

       On-Error;
          Return False;              // errors, so return False
       Endmon;

       *INOA = *ON;
       *INOB = *ON;
       *INOC = *ON;
       Return True;                  // no errors, so return True

      /End-Free

     POpenFiles        E
     P*------------------------------------------------------*
     P* Over ride files
     P*------------------------------------------------------*
     POverRideFiles    B

     D                 PI

      /Free

          // over ride TRANSMISSION file
       RunCommand('OVRDBF FILE(MGPLTR) TOFILE(FTPLIB/MGPLTR)');

          // over ride GROUP file
       RunCommand('OVRDBF FILE(GRPFILE) TOFILE(PRDDTALIB/GAUSRPE1)');

          // over ride ADMIN file
       RunCommand('OVRDBF FILE(ADMFILE) TOFILE(PRDDTALIB/GAUSRPRO)');

      /End-Free

     POverRideFiles    E

     P*------------------------------------------------------*
     P* Run the command that is passed in
     P*------------------------------------------------------*
     PRunCommand       B

     D                 PI
     DCommand@                      500    Options(*VarSize) Const

      * local variables
     DCmdLength        S             15  5 Inz(%Size(Command))
     DCommand          S                   Like(Command@)

     C                   Eval      Command = Command@

     C                   Call      'QCMDEXC'
     C                   Parm                    Command
     C                   Parm                    CmdLength

     C                   Return

     PRunCommand       E

     P*------------------------------------------------------*
     P* Go through transmission file and request letters as
     P* necessary
     P*------------------------------------------------------*
     PRequestLetters   B

     D                 PI

      * local variables

      /Free

          Setll *Start MGPLTR;
          Read MGPLTR;

            Dow Not %eof(MGPLTR);

              Clear FilDS;
              G5 = '00000';
              G3 = '000';

              // Load data structure using transmission file information...
              FilDS = MLFLD;

              //  Print either '31 Days' or 'Final Notice'...
              Select;
                When FilNUMBER = PastDue;
                  PrintPastDueLetter();
                  PrintSummary(PastDue);
                When FilNUMBER = FinalNotice;
                  PrintFinalNoticeLetter();
                  PrintSummary(FinalNotice);
              Endsl;

              Read MGPLTR;
            Enddo;

      /End-Free

     PRequestLetters   E

     P*-------------------------------------------------------------*
     P* Print the 'PastDue' letter
     P*-------------------------------------------------------------*
     PPrintPastDue...
     P Letter          B

     P* PROCEDURE INTERFACE
     D                 PI

      * Local Variables
     DRow_             S                   Like(Row) INZ(3.50)
     DCol_             S                   Like(Col) INZ(2)
     DPointSize_       S                   Like(PointSize) INZ(12)
     DFontType_        S                   Like(FontType) INZ(TimesNewRoman)

      /Free

          // Set up letter fields for 'PastDue'...
          SetUpLetterWording(PastDue);

          // Common letter beginning...
          PrintHeaderInfo(Row_:Col_:PointSize_:FontType_);

          // Body of the letter...
          PrintPastDueBody(Row_:Col_:PointSize_:FontType_);

          // Closing of letter for 'PastDue'...
          PrintSignature(Row_:Col_:PointSize_:FontType_:PastDue);

          // eject the letter from the printer...
          NewPage(PastDue);

      /End-Free

     PPrintPastDue...
     P Letter          E

     P*-------------------------------------------------------------*
     P* Set up wording for the letters
     P*-------------------------------------------------------------*
     PSetUpLetter...
     P Wording         B

     P* PROCEDURE INTERFACE
     D                 PI
     DLetterType_                          Like(FilNUMBER) Value

      * Local Variables

      /Free

        Clear Letter1P;
        Clear Letter2P;
        Clear Letter3P;
        Clear Letter4P;

        Select;

          //  Load for 'PastDue'...
          When LetterType_ = PastDue;
            Letter1P = 'Our records indicate that your account is currently +
             in an arrears position.  The following billed amount is now +
             overdue.';

            Letter2P = 'When an account has been in arrears for 31 days, +
             the payment of health and dental, new life and disability, +
             as well as travel claims is suspended.';

            Letter3P = 'To ensure that there will be no interruption in the +
             payment of your claims, please forward the outstanding balance +
             to our office.  Should you require additional information or +
             have any questions regarding your account, please do not +
             hesitate to contact your Group Service Representative.';

          //  Load for 'FinalNotice'...
          When LetterType_ = FinalNotice;
            Letter1P = 'Our records indicate that your account is currently +
             in an arrears position.  The following billed amounts are now +
             outstanding.';

            Letter2P = 'When an account has been in arrears for 31 days, the +
             payment of claims is suspended.  After 60 days, your policy +
             becomes subject to cancellation.';

            Letter3P = 'To avoid cancellation and reinstate claims payment, +
             please forward the outstanding balance to our office +
             immediately. Should you require additional information or have +
             any questions regarding your account, please do not hesitate to +
             contact your Group Service Representative.';

        Endsl;

        // Third paragraph is the same for both letters...
        Letter4P =  'If your payment has already been made, please disregard +
                     this letter and accept our thanks.';

      /End-Free

     PSetUpLetter...
     P Wording         E

     P*-------------------------------------------------------------*
     P* Print the header info....name, address, etc; same for either letter
     P*-------------------------------------------------------------*
     PPrintHeaderInfo  B

     P* PROCEDURE INTERFACE
     D                 PI
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

      * Local Variables
     DFontTypeBold_    S                   Like(FontType)

     DPostalCodeDS     DS                  INZ
     DPCLeft                          3
     DPCRight                         3

      /Free

       FontTypeBold_ = TimesNewRomanBold;

       // write the date...
       WritePhrase(EnglishDate(Today):Row_:Col_:PointSize_:FontType_);

       // write the company name...
       //   force the first line of the address to always print on
       //    the same line...
       Row_ = 4.73;
       WritePhrase(FilNAME:Row_:Col_:PointSize_:FontType_);

       // write the address...
       If FilADDRESS1 <> *BLANKS;
         WritePhrase(%trim(FilADDRESS1):Row_:Col_:PointSize_:
           FontType_);
       Endif;
       If FilADDRESS2 <> *BLANKS;
         WritePhrase(%trim(FilADDRESS2):Row_:Col_:PointSize_:
           FontType_);
       Endif;
       If FilADDRESS3 <> *BLANKS;
         WritePhrase(%trim(FilADDRESS3):Row_:Col_:PointSize_:
           FontType_);
       Endif;
       If FilADDRESS4 <> *BLANKS;
         WritePhrase(%trim(FilADDRESS4):Row_:Col_:PointSize_:
           FontType_);
       Endif;
       If FilADDRESS5 <> *BLANKS;
         WritePhrase(%trim(FilADDRESS5):Row_:Col_:PointSize_:
           FontType_);
       Endif;
       If FilPOSTALCODE <> *BLANKS;
          PostalCodeDS = FilPOSTALCODE;
         WritePhrase(PCLeft + ' ' + PCRight:Row_:Col_:PointSize_:
           FontType_);
       Endif;

       // write the reference line...
       //   force the reference line to always print on same line...
       Row_ = 8.3;
       WritePhrase('RE:    Outstanding Premiums - Policy ' + G5 + '-' +
          G3:Row_:Col_:PointSize_:FontTypeBold_);

       // write the salutations line...
       AdminName = 'Group Administrator';
       Row_ += (LineSpace * 2);
       WritePhrase('Dear ' + %trim(AdminNAME) + ',':Row_:Col_:
                      PointSize_:FontType_);

      /End-Free

     PPrintHeaderInfo  E

     P*-------------------------------------------------------------*
     P* Determine if phrase is too long to fit on one line
     P* If so print on multiple lines
     P*-------------------------------------------------------------*
     PWritePhrase      B

     P* PROCEDURE INTERFACE
     D                 PI
     DPhrase                      32767    Value Varying
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

      * Local Variables
     DEndPosition      S                   Like(Position)
     DStartPosition    S                   Like(Position) Inz(1)
     DSmallPhrase      S          32767    Varying

      /Free

       // do while the start position is less than the length of the phrase
       Dow StartPosition < %len(Phrase);

          // if the start position + line length is greator than the phrase
          // length make the end position the length of the phrase
          If StartPosition + LineLength > %len(Phrase);
             EndPosition = %len(Phrase) + 1;
           Else;
             // cut the phrase into smaller pieces
             EndPosition = CutPhrase(Phrase:StartPosition + LineLength);
          Endif;

          SmallPhrase = %subst(Phrase:StartPosition:
                               EndPosition - StartPosition);
          StartPosition = EndPosition + 1;

          // if the small phrase has info in it, print it
          If SmallPhrase <> ' ';
             Row_ += LineSpace;
             WriteLineInfo(SmallPhrase:Row_:Col_:PointSize_:FontType_);
          Endif;

       Enddo;

      /End-Free

     PWritePhrase      E

     P*-------------------------------------------------------------*
     P* Find the break in the prase where a word won't get cut
     P* in half
     P*-------------------------------------------------------------*
     PCutPhrase        B

     P* PROCEDURE INTERFACE
     D                 PI                  Like(Position)
     DPhrase                      32767    Value Varying
     DBlankPosition                        Value Like(Position)

      * Local Variables

      /Free

       // find a break between words
       Dow %subst(Phrase:BlankPosition:1) <> ' ';
          BlankPosition -= 1;
       Enddo;

       // if the break is at the end of the phrase, return the length
       // of the phrase
       If BlankPosition > %len(Phrase);
          Return %len(Phrase);
        Else;
          // return the position we found a break at
          Return BlankPosition;
       Endif;

      /End-Free

     PCutPhrase        E

     P*-------------------------------------------------------------*
     P* Write the line info to the letter
     P*-------------------------------------------------------------*
     PWriteLineInfo    B

     P* PROCEDURE INTERFACE
     D                 PI
     DInfo                        32767    Value Varying
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

      * Local Variables

      /Free

       Row = Row_;
       Col = Col_;
       PointSize = PointSize_;
       FontType = FontType_;
       LineInfo = Info;
       Write PrintLine;

      /End-Free

     PWriteLineInfo    E

     P*-------------------------------------------------------------*
     P* Change the date into an english date
     P*-------------------------------------------------------------*
     PEnglishDate      B

     P* PROCEDURE INTERFACE
     D                 PI         32767    Varying
     DDate                             D   Value

      * Local Variables
     DDateDS           DS
     D CCYY                           4
     D  Dash1                         1
     D MM                             2  0
     D  Dash2                         1
     D DD                             2  0

     DMonth            S              9    Dim(12)

      /Free

       // set up the months
       Month(1) = 'January';
       Month(2) = 'February';
       Month(3) = 'March';
       Month(4) = 'April';
       Month(5) = 'May';
       Month(6) = 'June';
       Month(7) = 'July';
       Month(8) = 'August';
       Month(9) = 'September';
       Month(10) = 'October';
       Month(11) = 'November';
       Month(12) = 'December';

       // check to make sure a valid date was passed in
       Test(e) Date;

       // if the date is not valid
       If %error;
          Return 'Invalid Date: ' + %char(Date);
       Endif;

       DateDS = %char(Date);

       Return %trim(Month(MM)) + ' ' + %char(DD) + ', ' + CCYY;

      /End-Free

     PEnglishDate      E

     P*-------------------------------------------------------------*
     P* Print the 'PastDue' body of letter
     P*-------------------------------------------------------------*
     PPrintPastDue...
     P Body            B

     P* PROCEDURE INTERFACE
     D                 PI
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

      * Local Variables
     DC10              S             10

      /Free

         // First paragraph...
         Row_ += LineSpace;
         WritePhrase(Letter1P:Row_:Col_:PointSize_:FontType_);

         // Print the dates and amounts...
         Row_ += LineSpace;
         PrintDateMoney(Row_:Col_:PointSize_:FontType_);
         FontType = TimesNewRoman;

         // Second paragraph...
         Row_ += LineSpace;
         WritePhrase(Letter2P:Row_:Col_:PointSize_:FontType_);

         // Third paragraph...
         Row_ += LineSpace;
         WritePhrase(Letter3P:Row_:Col_:PointSize_:FontType_);

         // Forth paragraph...
         Row_ += LineSpace;
         WritePhrase(Letter4P:Row_:Col_:PointSize_:FontType_);

      /End-Free

     PPrintPastDue...
     P Body            E

     P*------------------------------------------------------*
     P* Print any date & amount combo that has values
     P*------------------------------------------------------*
     PPrintDateMoney   B

     D                 PI
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

      /Free

         // (PM) Billing date and amount...
         If (GetNumber(FilBILLPMDATE) = True) OR FilPMAMOUNT > 0;
           If (GetNumber(FilBILLPMDATE) = True);
             cDate = EnglishDate(%date(FilBILLPMDATE:*ISO0));
           Else;
             cDate = 'PM Date';
           Endif;
           Amount = FilPMAMOUNT;
           Row_ += LineSpace;
           Row = Row_;
           Write PrintDate;
           Write PrintAmt;
         Endif;

         // (1M) One month arrears and amount...
         If (GetNumber(FilBILL1MDATE) = True) OR Fil1MAMOUNT > 0;
           If (GetNumber(FilBILL1MDATE) = True);
             cDate = EnglishDate(%date(FilBILL1MDATE:*ISO0));
           Else;
             cDate = '1M Date';
           Endif;
           Amount = Fil1MAMOUNT;
           Row_ += LineSpace;
           Row = Row_;
           Write PrintDate;
           Write PrintAmt;
         Endif;

         // (2M) Two months arrears and amount...
         If (GetNumber(FilBILL2MDATE) = True) OR Fil2MAMOUNT > 0;
           If (GetNumber(FilBILL2MDATE) = True);
             cDate = EnglishDate(%date(FilBILL2MDATE:*ISO0));
           Else;
             cDate = '2M Date';
           Endif;
           Amount = Fil2MAMOUNT;
           Row_ += LineSpace;
           Row = Row_;
           Write PrintDate;
           Write PrintAmt;
         Endif;

      /End-Free

     PPrintDateMoney   E

     P*-------------------------------------------------------------*
     P* Print the signature
     P*-------------------------------------------------------------*
     PPrintSignature   B

     P* PROCEDURE INTERFACE
     D                 PI
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value
     DLetterType_                          Like(FilNUMBER) Value

      * Local Variables
     DName             S             50
     DPosition         S             50

      /Free

       Row_ += LineSpace;
       WritePhrase('Yours sincerely,':Row_:Col_:PointSize_:FontType_);

       // For letters '31 Days PastDue' put billing clerk's signature
       //  on the letter...
       Library = SignlibP;
       PageSeg = ClerkPageSeg;
       Name = ClerkName;
       Position = ClerkPosition;

       // If the letter is 'FinalNotice' then put manager's signature
       //  on letter...
       If LetterType_ = FinalNotice;
         Library = SignlibM;
         PageSeg = MgrPageSeg;
         Name = MgrName;
         Position = MgrPosition;
       Endif;

       // print the signature page segment...
       If LetterType_ = PastDue;
         Row = Row_ - 1.5;
       Endif;
       Col = 1.5;
       If LetterType_ = FinalNotice;
         Row = Row_ - 1.4;
         Col = Col - 0.5;

         Row_ = Row_ + 0.3;
       Endif;
       Write Signature;

       // Name and job title...
       Row_ += (LineSpace * 4);
       If LetterType_ = FinalNotice;
         Row_ = Row_ - 0.3;
       Endif;
       WritePhrase(%TRIM(Name):Row_:Col_:PointSize_:FontType_);
       WritePhrase(%TRIM(Position):Row_:Col_:PointSize_:FontType_);

      /End-Free

     PPrintSignature   E

     P*------------------------------------------------------*
     P* Determines if a new page should be started
     P*------------------------------------------------------*
     PNewPage          B

     D                 PI
     DReport                               Like(FilNUMBER) Value

      /Free

       // select which report we are printing
       Select;

          When Report = PastDue;
             Write NextPage;
             Write NextPage;

          When Report = FinalNotice;
             Write NextPage;
             Write NextPage;

       Endsl;

      /End-Free

     PNewPage          E

     P*-------------------------------------------------------------*
     P* Capitalizes the fld passed in
     P*-------------------------------------------------------------*
     PCapitalize       B

     P* PROCEDURE INTERFACE
     D                 PI         32767    Varying
     DString                      32767    Value Varying

      * Local Variables
     DPosition         S              5  0 Inz(2)
     DEndPosition      S              5  0

      /Free

       // if the string passed in is just 1 letter long,
       // return the string
       If %len(String) = 1;
          Return String;
       Endif;

       String = ToLower(String:Position);
       EndPosition = %len(String);

       Dow Position < EndPosition And Position > 0;
          Position = %check(Lower:String:Position);

          If Position > 0;
             String = ToUpper(String:Position + 1);

             // change the rest of the string to lower characters
             // and make sure we are not past the end position
             If Position + 2 <= EndPosition;
                String = ToLower(String:Position + 2);
             Endif;

             Position += 2;
          Endif;

       Enddo;

       Return String;

      /End-Free

     PCapitalize       E
     P*-------------------------------------------------------------*
     P* Changes the case of the charcters to lowercase starting at
     P* the position passed in
     P*-------------------------------------------------------------*
     PToLower          B

     P* PROCEDURE INTERFACE
     D                 PI         32767    Varying
     DString                      32767    Value Varying
     DPosition                        5  0 Value

      * Local Variables

      /Free

       Return %xlate(Upper:Lower:String:Position);

      /End-Free

     PToLower          E
     P*-------------------------------------------------------------*
     P* Changes the case of the charcters to uppercase starting at
     P* the position passed in
     P*-------------------------------------------------------------*
     PToUpper          B

     P* PROCEDURE INTERFACE
     D                 PI         32767    Varying
     DString                      32767    Value Varying
     DPosition                        5  0 Value

      * Local Variables

      /Free

       Return %xlate(Lower:Upper:String:Position);

      /End-Free

     PToUpper          E

     P*-------------------------------------------------------------*
     P* Underline
     P*-------------------------------------------------------------*
     PUnderline        B

     P* PROCEDURE INTERFACE
     D                 PI
     DStartX                               Like(PgmToSys) Value
     DStartY                               Like(PgmToSys) Value
     DLength                               Like(PgmToSys) Value
     DWidth                                Like(PgmToSys) Value

      * Local Variables

      /Free

       DrawHorizontalLine(StartX + UnderlineOffset:StartY:Length:Width);

      /End-Free

     PUnderline        E

     P*-------------------------------------------------------------*
     P* Draw a horizontal line
     P*-------------------------------------------------------------*
     PDraw...
     P HorizontalLine  B

     P* PROCEDURE INTERFACE
     D                 PI
     DRow_                                 Like(PgmToSys) Value
     DCol_                                 Like(PgmToSys) Value
     DLength_                              Like(PgmToSys) Value
     DWidth_                               Like(PgmToSys) Value


      * Local Variables

      /Free

       Row = Row_;
       Col = Col_;
       Length = Length_;
       Width = Width_;

       Write DrawHLine;

      /End-Free

     PDraw...
     P HorizontalLine  E

     P*------------------------------------------------------*
     P* Prints summary line to register
     P*------------------------------------------------------*
     PPrintSummary     B

     D                 PI
     DReport                               Like(FilNUMBER) Value

      /Free

       // check for end of page
       ChkEndOfPage(Report);

       // select which report we are printing
       Select;

          When Report = PastDue;
              EXCEPT Detail;

          When Report = FinalNotice;
              EXCEPT Detail2;

       Endsl;

      /End-Free

     PPrintSummary     E

     P*------------------------------------------------------*
     P* Goto next page when report at the bottom of a page
     P*------------------------------------------------------*
     PChkEndOfPage     B

     D                 PI
     DReport                               Like(FilNUMBER) Value

      /Free

       // select which report we are printing
       Select;

          When Report = PastDue;
            If *INOA = True;
              EXCEPT Header;
              *INOA = False;
            Endif;

          When Report = FinalNotice;
            If *INOB = True;
              EXCEPT Header2;
              *INOB = False;
            Endif;

       Endsl;

      /End-Free

     PChkEndOfPage     E

     P*-------------------------------------------------------------*
     P* Print the 'FinalNotice' letter
     P*-------------------------------------------------------------*
     PPrintFinal...
     P NoticeLetter    B

     P* PROCEDURE INTERFACE
     D                 PI

      * Local Variables
     DRow_             S                   Like(Row) INZ(3.50)
     DCol_             S                   Like(Col) INZ(2)
     DPointSize_       S                   Like(PointSize) INZ(12)
     DFontType_        S                   Like(FontType) INZ(TimesNewRoman)

      /Free

          // Set up letter fields for 'FinalNotice'...
          SetUpLetterWording(FinalNotice);

          // Common letter beginning...
          PrintHeaderInfo(Row_:Col_:PointSize_:FontType_);

          // Body of the letter...
          PrintFinalNoticeBody(Row_:Col_:PointSize_:FontType_);

          // Closing of letter for 'FinalNotice'...
          PrintSignature(Row_:Col_:PointSize_:FontType_:FinalNotice);

          // eject the letter from the printer...
          NewPage(FinalNotice);

      /End-Free

     PPrintFinal...
     P NoticeLetter    E

     P*-------------------------------------------------------------*
     P* Print the 'FinalNotice' body of letter
     P*-------------------------------------------------------------*
     PPrintFinal...
     P NoticeBody      B

     P* PROCEDURE INTERFACE
     D                 PI
     DRow_                                 Like(Row)
     DCol_                                 Like(Col)
     DPointSize_                           Like(PointSize) Value
     DFontType_                            Like(FontType) Value

      * Local Variables

      /Free

         // First paragraph...
         Row_ += LineSpace;
         WritePhrase(Letter1P:Row_:Col_:PointSize_:FontType_);

         // Print the dates and amounts...
         Row_ += LineSpace;
         FontType = OtherFont;
         PrintDateMoney(Row_:Col_:PointSize_:FontType_);
         FontType = TimesNewRoman;

         // Second paragraph...
         Row_ += LineSpace;
         WritePhrase(Letter2P:Row_:Col_:PointSize_:FontType_);

         // Third paragraph...
         Row_ += LineSpace;
         WritePhrase(Letter3P:Row_:Col_:PointSize_:FontType_);

         // Forth paragraph...
         Row_ += LineSpace;
         WritePhrase(Letter4P:Row_:Col_:PointSize_:FontType_);

      /End-Free

     PPrintFinal...
     P NoticeBody      E

     P*-------------------------------------------------------------*
     P* Ensure valid date data has been passed
     P*-------------------------------------------------------------*
     PGetNumber        B

     P* PROCEDURE INTERFACE
     D                 PI             1
     DDate                            8

      * Local Variables
     DArr              S              1    DIM(30)
     DNbrArr           S              1    DIM(10)
     DPosi             S              2  0
     DLoop             S              2  0
     DPasIndex         S              2  0 INZ(8)
     DFOUND            S              1
     DCHAR1            S              1
     C*
     D Numbers         C                   '0123456789'
     C*
     C                   MOVEA     Date          Arr
     C                   MOVEA     Numbers       NbrArr
     C*  Loop through the index value examining each character against
     C*   digit values 0-9
     C                   DO        PasIndex
     C                   EVAL      POSI = POSI + 1
     C                   MOVEA     Arr(POSI)     CHAR1
     C                   EXSR      Compare
     C                   IF        FOUND = *ON
     C                   RETURN                  False
     C                   ENDIF
     C                   ENDDO
     C                   RETURN                  True
     C*
     C************************
     CSR   CHECKPARMS    BEGSR
     C*
     C                   IF        Date = *BLANKS
     C                   RETURN                  False
     C                   ENDIF
     C*
     C                   IF        PasIndex = 0
     C                   RETURN                  False
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C**************************
     C*
     CSR   COMPARE       BEGSR
     C*
     C                   EVAL      FOUND = *ON
     C                   EVAL      LOOP = 0
     C                   DO        10
     C                   EVAL      LOOP = LOOP + 1
     C                   IF        CHAR1 = NbrArr(LOOP)
     C                   EVAL      FOUND = *OFF
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR

     PGetNumber        E
