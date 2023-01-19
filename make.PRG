CLOSE ALL
CLEAR ALL
CLEAR

ERASE fews.prg
ERASE fews.fxp
ERASE fews.err
ERASE fews.bak

LOCAL cCode
cCode = FILETOSTR("header.prg") + ;
        FILETOSTR("winsock.prg") + ;
        FILETOSTR("webserver.prg") + ;
        FILETOSTR("base64helper.prg")
        
STRTOFILE(cCode, "fews.prg")
COMP fews
IF FILE("fews.err")
	MODI FILE fews.err
ELSE
 	?"OK!"
ENDIF
        