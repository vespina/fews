DEFINE CLASS base64Helper AS Custom
 
 	Version = "1.1"
 
	* Constructor
	PROCEDURE Init
		DECLARE INTEGER CryptBinaryToString IN Crypt32;
			STRING @pbBinary, LONG cbBinary, LONG dwFlags,;
			STRING @pszString, LONG @pcchString

		DECLARE INTEGER CryptStringToBinary IN crypt32;
			STRING @pszString, LONG cchString, LONG dwFlags,;
			STRING @pbBinary, LONG @pcbBinary,;
			LONG pdwSkip, LONG pdwFlags
		RETURN


	* encodeString
	* Toma un string y lo convierte en base64
	*
	PROCEDURE encodeString(pcString)
		LOCAL nFlags, nBufsize, cDst
		nFlags = 1  && base64
		nBufsize = 0
		CryptBinaryToString(@pcString, LEN(pcString),m.nFlags, NULL, @nBufsize)
		cDst = REPLICATE(CHR(0), m.nBufsize)
		IF CryptBinaryToString(@pcString, LEN(pcString), m.nFlags,@cDst, @nBufsize) = 0
			RETURN ""
		ENDIF
		RETURN cDst
	 
	 
	* decodeString
	* Toma una cadena en BAse64 y devuelve la cadena original
	*
	FUNCTION decodeString(pcB64)
		LOCAL nFlags, nBufsize, cDst
		nFlags = 1  && base64
		nBufsize = 0
		CryptStringToBinary(@pcB64, LEN(m.pcB64),nFlags, NULL, @nBufsize, 0,0)
		cDst = REPLICATE(CHR(0), m.nBufsize)
		IF CryptStringToBinary(@pcB64, LEN(m.pcB64),nFlags, @cDst, @nBufsize, 0,0) = 0
			RETURN ""
		ENDIF
		RETURN m.cDst
	 
	 
	* encodeFile
	* Toma un archivo y lo codifica en base64
	*
	PROCEDURE encodeFile(pcFile, plWebMode)
		IF NOT FILE(pcFile)
			RETURN ""
		ENDIF
		LOCAL cB64
		cB64 = THIS.encodeString(FILETOSTR(pcFile))
		IF plWebMode
			cB64 = CHRTRAN(STRT(cB64, CHR(13)+CHR(10), "\n"),CHR(0),"")
		ENDIF
		RETURN cB64
	 
	 
	* decodeFile
	* Toma una cadena base64, la decodifica y crea un archivo con el contenido
	*
	PROCEDURE decodeFile(pcB64, pcFile)
		LOCAL cBuff
		pcB64 = STRT(STRT(STRT(pcB64,"\/","/"),"\u000d",CHR(13)),"\u000a",CHR(10))
		cBuff = THIS.decodeString(pcB64)
		STRTOFILE(cBuff, pcFile)
		RETURN
 
ENDDEFINE
