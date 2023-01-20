DEFINE CLASS wsKVP AS Custom
	cName = ""
	cValue = ""
	PROCEDURE Init(pcName, pcValue)
		THIS.cName = pcName
		THIS.cValue = pcValue
		RETURN
	PROCEDURE ToString
		RETURN THIS.cName + ": " + THIS.cValue
ENDDEFINE





*  T2GMT
*
*  RETURNS A STRING REPRESENTING THE
*  GIVEN DATETIME VALUE IN THE 
*  FORMATS: 
*
*  GMT: Thu, 27 Feb 2003 14:11:12 GMT
*  ISO: YYYY-MM-DDTHH:MM:SSZ
*
FUNCTION T2GMT(tDateTime, plISOFormat)
	DECLARE integer GetTimeZoneInformation IN Win32API ;
   		STRING @ TimeZoneStruct
   	LOCAL lcTZStruct,lnSunTime,lnUTCOffset,lnDaylightBias
	lcTZStruct = SPACE(256)
	lnSunTime = GetTimeZoneInformation(@lcTZStruct )
	lnUTCOffset = WordToInt(SUBSTR(lcTZStruct ,1,4), .t.)
	lnDaylightBias = WordToInt(RIGHT(ALLTRIM(lcTZStruct), 4), .t.)
	* Add the bias if daylight savings is active
	IF lnSunTime = 2
   		lnUTCOffset = lnUTCOffset + lnDaylightBias 
	ENDIF
	* convert the offset to seconds
	lnUTCOffset = lnUTCOffset * 60
	ltUTCTime = tDateTime + lnUTCOffset 

	LOCAL lcReturn as String
	IF plISOFormat
		lcReturn = TRANSFORM(TTOC(ltUTCTime,1), "@R 9999-99-99T99:99:99Z") 
	ELSE
		lcReturn = LEFT(PROPER(CDOW(ltUTCTime)),3) +", "
		lcReturn = lcReturn +  PADL(DAY(ltUTCTime),2,'0') + SPACE(1)
		lcReturn = lcReturn + LEFT(PROPER(CMONTH(ltUTCTime)),3) + SPACE(1)
		lcReturn = lcReturn + STR(YEAR(ltUTCTime),4)+SPACE(1)
		lcReturn = lcReturn + STUFF(STUFF(RIGHT(TTOC(ltUTCTime,1), 6), 5, 0, ":"), 3, 0, ":") + SPACE(1)
		lcReturn = lcReturn + "GMT"
	ENDIF
	RETURN lcReturn


FUNCTION WordToInt(tcWordString, tlSigned)
	LOCAL lcHexString as String, lnResult as Integer, lnChar as Integer
	lcHexString = '0x'
	FOR lnChar = LEN(tcWordString) TO 1 STEP -1
 		lcHexString = lcHexString + PADL(right(TRANSFORM(ASC(SUBSTR(tcWordString, lnChar, 1)), '@0'), 2), 2, '0')
	ENDFOR
	lnResult = EVALUATE(lcHexString)
	IF tlSigned and lnResult > 0x80000000
	lnResult = lnResult - 1 - 0xFFFFFFFF
	ENDIF
	RETURN lnResult


#IF VERSION(5) < 900
FUNCTION GETWORDNUM
	LPARAMETERS pcSource, pnToken, pcSep
	LOCAL cList,nPos,cToken
	pcSep = IIF(VARTYPE(pcSep)<>"C",",",pcSep)
	cList = pcSep + pcSource
	cToken = ""
	nPos = ATC(pcSep, cList, pnToken)
	IF nPos > 0
		cToken = SUBS(cList, nPos + LEN(pcSep))
		nPos = ATC(pcSep, cToken)
		IF nPos > 0
			cToken = LEFT(cToken, nPos - 1)
		ENDIF
	ENDIF
	RETURN cToken 
	
FUNCTION GETWORDCOUNT
	LPARAMETERS pcSource, pcSep	
	pcSep = IIF(VARTYPE(pcSep)<>"C",",",pcSep)
	RETURN OCCURS(pcSep, pcSource) + 1


FUNCTION EVL
	LPARAMETERS puValue, puDEfault
	RETURN IIF(EMPTY(puValue),puDEfault,puValue)
	
#ENDIF
