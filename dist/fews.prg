*
*  FEWS.PRG
*  100% FOXPRO EMBEDABLE WEBSERVER
*
*  AUTHOR: VICTOR ESPINA
*

****************************************************************************************************
**                                          WINSOCK.PRG                                           **
****************************************************************************************************
#DEFINE SOCKET_STATE_CLOSED				 0
#DEFINE SOCKET_STATE_OPEN				 1
#DEFINE SOCKET_STATE_LISTENING			 2
#DEFINE SOCKET_STATE_CONNECTIONPENDING	 3
#DEFINE SOCKET_STATE_RESOLVINGHOST		 4
#DEFINE SOCKET_STATE_RESOLVED			 5
#DEFINE SOCKET_STATE_CONNECTING		     6
#DEFINE SOCKET_STATE_CONNECTED		 	 7
#DEFINE SOCKET_STATE_CLOSING			 8
#DEFINE SOCKET_STATE_ERROR		 		 9
#DEFINE SOCKET_STATE_CAPTIONS            "CLOSED,OPEN,LISTENING,CONNECTIONPENDING,RESOLVINGHOST,RESOLVED,CONNECTING,CONNECTED,CLOSING,ERROR"
#DEFINE SOCKET_TIMEOUT			 		 15
#DEFINE SOCKET_EOT						 CHR(13)+CHR(10)+CHR(13)+CHR(10)
#DEFINE HTTP_PACKET_LENGTH  			 8192
#DEFINE CRLF							 CHR(13)+CHR(10)

DEFINE CLASS winsock AS Form
	DoCreate = .T.
	Caption = "Winsock"
	Name = "sck"
	Visible = .F.
	Object = NULL
	
	cHeaders = ""
	cContent = ""
	nContentLength = 0
	nState = 0
	cState = ""
	
	ADD OBJECT socket AS oleSocket WITH ;
		Top = 23, ;
		Left = 46, ;
		Height = 100, ;
		Width = 100, ;
		Name = "socket"

	PROCEDURE object_access
		RETURN THIS.Socket.object
		
	PROCEDURE cHeaders_Access
		RETURN THIS.Socket.cHeaders

	PROCEDURE cContent_Access
		RETURN THIS.Socket.cContent
		
	PROCEDURE nContentLength_Access
		RETURN THIS.Socket.nContentLength			

	PROCEDURE nState_Access
		RETURN THIS.Socket.State	
		
	PROCEDURE cState_Access
	    RETURN GETWORDNUM(SOCKET_STATE_CAPTIONS, THIS.nState + 1)		
		
	PROCEDURE SendData
		LPARAMETERS pcContent
		LOCAL cPacket,nSent,nContentLengt
		nSent = 0
		nContentLength = LEN(pcContent)
		DO WHILE nSent < nContentLength
			IF nSent + HTTP_PACKET_LENGTH <= nContentLength
				cPacket = SUBSTR(pcContent, nSent + 1, HTTP_PACKET_LENGTH)
			ELSE
				cPacket = SUBSTR(pcContent, nSent + 1)
			ENDIF
			IF THIS.Socket.State <> SOCKET_STATE_CONNECTED
				THIS.Object.Close()
				RETURN
			ENDIF
			THIS.Object.SendData(cPacket)
			nSent = nSent + LEN(cPacket)
		ENDDO
		?THIS.Object.State
   	    RETURN		


	PROCEDURE Close
		IF THIS.nState <> SOCKET_STATE_CLOSED
			THIS.Object.Close()
		ENDIF
		RETURN
ENDDEFINE

DEFINE CLASS oleSocket AS OleControl
   OleClass = "MSWinsock.Winsock"

   cHeaders = ""
   cContent = ""
   nContentLength = ""
   
   PROCEDURE Init
   	  THIS.cHeaders = ""
   	  THIS.cContent = ""
   	  THIS.nContentLength = 0
   	  RETURN
   	  
   PROCEDURE Error
      LPARAMETERS nError, cMethod, nLine
      ?"[WINSOCK][ERR][" + ALLT(STR(nError)) + "][" + cMethod + ":" + ALLT(Str(nLine)) +"] " + MESSAGE() + Chr(13) + Chr(10)
      RETURN
      
   PROCEDURE Close()
      THIS.Object.Close()
      RETURN

   PROCEDURE Destroy
      THIS.Object.Close()
      RETURN
      
   PROCEDURE DataArrival
      LPARAMETERS tnByteCount
      LOCAL lcBuffer,lnEOT
      lcBuffer = SPACE(tnByteCount)
      LOCAL cData
      This.GetData( @lcBuffer, , tnByteCount )
      
      *IF ATC("Content-Length:",lcBuffer) > 0 AND EMPTY(THIS.cReceiveBuffer)
      lnEOT = AT(SOCKET_EOT, lcBuffer)
      IF lnEOT > 0 AND EMPTY(THIS.cHeaders)
          THIS.cHeaders = TRANSFORM(CREATEBINARY(LEFT(lcBuffer, lnEOT - 1)))
          THIS.cContent = TRANSFORM(CREATEBINARY(SUBS(lcBuffer, lnEOT + 4)))
          cData = MLINE(SUBS(lcBuffer, ATC("Content-Length:",lcBuffer) + 1),1)
          cData = SUBS(cData, AT(":",cDAta) + 2)
          THIS.nContentLength = INT(VAL(cData))
          ?"[WINSOCK][DataArrival][ContentLength]",THIS.nContentLength,LEN(THIS.cHEaders)
      ELSE
          cData = TRANSFORM(CREATEBINARY(lcBuffer))  
          THIS.cContent = THIS.cContent + cData
          ?"[WINSOCK][DataArrival]",LEN(THIS.cContent),"of",THIS.nContentLength
      ENDIF        
      IF LEN(THIS.cContent) >= THIS.nContentLength
      	THIS.dataReceived()
      ENDIF       
      RETURN
      
  
   PROCEDURE DataReceived
      RETURN
      
   PROCEDURE SendComplete
      IF THIS.state = SOCKET_STATE_CONNECTED
      	THIS.Close()
      ENDIF   	           
      RETURN 
ENDDEFINE
**************************************** END OF WINSOCK.PRG ****************************************

****************************************************************************************************
**                                        FEWSREQUEST.PRG                                         **
****************************************************************************************************
DEFINE CLASS fewsRequest AS winsock
    nId = 0
	cRawHTTP = ""
	cVerb = ""
	cUrl = ""
	cQueryString = ""
	cContentType = ""
	cBoundary = ""
	cAccept = ""
	cUserAgent = ""
	DIMEN aHeaders[1]
	DIMEN aQS[1]
	DIMEN aFD[1]
	nHeadersCount = 0
	nQSCount = 0
	nFDCount = 0 
	lCORSPreflight = .F.
	cOrigin = ""
	cRawData = ""
	cError = ""
	oServer = NULL
	cFilter = ""
	lHandled = .F.
	cStatusCode = ""


	PROCEDURE Bind
		LPARAMETERS poServer
		THIS.oServer = poServer
		RETURN

		
	PROCEDURE Unbind
		THIS.oServer = NULL
		RETURN
		
		
	PROCEDURE cRawData_Access
		RETURN THIS.cContent	
			
			
	PROCEDURE socket.DataReceived
		LPARAMETERS pcHeaders,pcContent
		WITH THIS.Parent
			.oServer.cLastRequest = .cHeaders + CRLF + .cContent
			.oServer.cLastResponse = ""
			.oServer.cLastStatusCode = ""
			.parseHeaders()
			.splitQS() 
			IF INLIST(LOWER(.cContentType), "multipart/form-data", "application/x-www-form-urlencoded")
				.parseFormData()
			ENDIF
			.oServer.oHistory.beginRequest(THIS.PArent)
			.oServer.addToLog("[REQ][" + ALLT(STR(.nId)) + "] " + .cUrl,5)
			.oServer.addToLog(.cHeaders, 9)
			IF !EMPTY(.cContent)
				.oServer.addToLog(LEFT(.cContent, 8192), 9)
			ENDIF
			
			IF _VFP.StartMode = 0
	            ?REPL("*",100)			
				?"[CONN #" + ALLT(STR(.nId)) + "]",.nState
			    ?.cHeaders
		    ENDIF
		
			LOCAL oResponse
			.lHandled = .F.
			oResponse = .Handle()
			IF NOT ISNULL(oResponse)
			    .oServer.oHistory.completeRequest(oResponse)
			    .oServer.cLastResponse = oResponse.ToString()
			    .oServer.cLastStatusCode = oResponse.cStatusCode
			    .cStatusCode = oResponse.cStatusCode 
			    IF _VFP.StartMode = 0
				    ? 
				    ?"-----------"
				    ?"[RESPONSE]"
				    ?LEFT(.oServer.cLastResponse,300)
			    ENDIF
			    .oServer.addToLog("[REQ] " + oResponse.cStatusCode + " (" + oResponse.cHanler + ")",5)
			    .oServer.addToLog(LEFT(.oServer.cLastResponse,8192),9)
				.SendData( CREATEBINARY(.oServer.cLastResponse) )
			ELSE
				IF EMPTY(oResponse.cHandler)
					.oServer.addToLog("[REQ][ERR] An appropiate handler could not be found (" + THIS.PArent.cUrl + ")",1)
				ELSE
					.oServer.addToLog("[REQ][ERR][" + oREsponse.cHandler + "] an appropiate response could not be generated",1)
				ENDIF	
			ENDIF
			.lHandled = .T.
		ENDWITH
		RETURN
		


	PROCEDURE Handle
	    LOCAL oResponse,oHandler
	    oResponse = CREATE("wsResponse")
		IF ISNULL(THIS.oServer)
		    oResponse.cStatusCode = "503 Service Unavailable"
			RETURN oResponse
		ENDIF
		oResponse.cServerName = THIS.oServer.cName + " " + THIS.oServer.cVersion
		DO CASE
		   CASE THIS.lCORSPreflighrt
		        oHandler = CREATE("fewsCORSHandler")
		
		   OTHERWISE		
		   		oHandler = THIS.oServer.getHandler(THIS.cFilter)
		ENDCASE
		IF ISNULL(oHandler)
		    oResponse.cStatusCode = "501 Not Implemented"
			RETURN oResponse		
		ENDIF
		oResponse.cHandler = oHandler.Name
		oHandler.oRequest = THIS
		oHandler.oResponse = oResponse
		oHandler.Process()		
		RETURN oResponse


	PROCEDURE Header
		LPARAMETERS pcName,pcDefault
		LOCAL cValue,nHdr
		cValue = IIF(PCOUNT() = 1,"",pcDefault)
		pcName = ALLT(LOWER(pcName))
	    FOR nHdr = 1 TO THIS.nHeadersCount
	    	IF LOWER(THIS.aHeaders[nHdr].cName) == pcName
	    		cValue = THIS.aHeaders[nHdr].cValue
	    		EXIT
	    	ENDIF
	    ENDFOR
	    RETURN cValue
	    
	    
	PROCEDURE QS
		LPARAMETERS pcName,pcDefault
		LOCAL cValue,nHdr
		cValue = IIF(PCOUNT() = 1,"",pcDefault)
		pcName = ALLT(LOWER(pcName))		
	    FOR nHdr = 1 TO THIS.nQSCount
	    	IF THIS.aQS[nHdr].cName == pcName
	    		cValue = THIS.aQS[nHdr].cValue
	    		EXIT
	   	ENDIF
	    ENDFOR
	    RETURN cValue	    


	PROCEDURE Form
		LPARAMETERS pcName,pcDefault
		LOCAL cValue,nFD
		cValue = IIF(PCOUNT() = 1,"",pcDefault)
		pcName = ALLT(LOWER(pcName))	
	    FOR nFD = 1 TO THIS.nFDCount
	    	IF THIS.aFDD[nFD].cName == pcName
	    		cValue = THIS.aFD[nFD].cValue
	    		EXIT
	   	ENDIF
	    ENDFOR
	    RETURN cValue	    
		
		
	PROCEDURE addToLog
		LPARAMETERS pcText,pnLevel
		RETURN THIS.oServer.addToLog(pcText, pnLevel)

				
*!*	   PROCEDURE socket.DataArrival
*!*	      LPARAMETERS tnByteCount
*!*	      LOCAL lcBuffer
*!*	      lcBuffer = SPACE(tnByteCount)
*!*	      LOCAL cData
*!*	      This.GetData( @lcBuffer, , tnByteCount )
*!*	      *lcBuffer = STRT(lcBuffer,CRLF+CRLF,"")
*!*	      THIS.Parent.cRawData = THIS.PArent.cRawData + lcBuffer
*!*	      THIS.Parent.NQSCount=THIS.Parent.nQSCount + 1
*!*	      ?THIS.Parent.nQSCount,tnByteCount, LEN(lcBuffer), LEN(THIS.PArent.cRawData)
*!*	      STRTOFILE(THIS.PArent.cRawData, "request.txt")
*!*	      RETURN
				

	PROCEDURE parseHeaders
		LOCAL lnContentSize, lnLine, lcLine, llcHdrName, lcHdrValue, nQSPos
		LOCAL ARRAY aContent[1]
		lnContentSize = ALINES(aContent, THIS.cHeaders)  	
	    STORE "" TO lcData,lcContentType,lnContentLength,lcMethod,lcOrigin
	    THIS.nHeadersCount = 0
	    FOR lnLine = 1 TO lnContentSize
	    	lcLine = aContent[lnLine]
	    	DO CASE
	    		CASE lnLine = 1
	    			THIS.cVerb = UPPER(ALLTRIM(GETWORDNUM(lcLine, 1, " ")))
	    			THIS.cUrl = UPPER(ALLTRIM(GETWORDNUM(lcLine, 2, " ")))
	    			nQSPos = AT("?",THIS.cUrl)
	    			IF nQSPos > 0
	    				THIS.cQueryString = SUBS(THIS.cUrl, nQSPOS + 1)
	    				THIS.cUrl = LEFT(THIS.cUrl, nQSPOS - 1)
	    			ENDIF
	    			THIS.cFilter = "." + LOWER(JUSTEXT(THIS.cUrl))
	    				    		
	    	    CASE ": " $ lcLine
	    	         lcHdrName = GETWORDNUM(lcLine, 1, ": ")
	    	         lcHdrValue = GETWORDNUM(lcLine, 2, ": ")
	    	         THIS.nHeadersCount = THIS.nHeadersCount + 1
	    	         THIS.cHeaders = THIS.cHeaders + IIF(THIS.nHeadersCount > 1, CRLF, "") + lcLine
	    	         DIMEN THIS.aHeaders[THIS.nHeadersCount]
	    	         THIS.aHeaders[THIS.nHeadersCount] = CREATE("wsKVP",lcHdrName,lcHdrValue)
	    	         DO CASE	    	         
			    		CASE lcHdrName == "Content-Type"
			    			THIS.cContentType = ALLTRIM(LOWER(lcHdrValue))
			    			IF ATC("boundary",THIS.cContentType) > 0
			    				THIS.cBoundary = GETWORDNUM(GETWORDNUM(THIS.cContentType, 2,";"),2,"=")
			    				THIS.cContentType = GETWORDNUM(THIS.cContentType, 1, ";")
			    			ENDIF
			    			
			    		CASE lcHdrName == "Content-Length"
			    			THIS.nContentLength = INT(VAL(lcHdrValue))

			    	   CASE lcHdrName == "Origin"
		                    THIS.cOrigin = ALLTRIM(SUBSTR(lcLine, 8)) 
		                    
		               CASE lcHdrName == "User-Agent"
		               	    THIS.cUserAgent = lcHdrValue
                    ENDCASE
	    	ENDCASE 	    	
	    ENDFOR	    
	    IF THIS.cVerb == "OPTIONS" AND !EMPTY(THIS.cOrigin)
	    	THIS.lCORSPreFlight = .T.
	    ENDIF	    	        	        	    	    
		RETURN


	PROCEDURE parseFormData
	    LOCAL nFD,cFDName,cFDValue
	    DO CASE
	       CASE THIS.cContentType == "multipart/form-data"
			    LOCAL ARRAY aContent[1]
			    LOCAL nLines,nLine,cLine
			    nLines = ALINES(aContent, THIS.cContent)
			    THIS.nFDCount = OCCURS(UPPER(THIS.cBoundary),UPPER(THIS.cContent)) - 1
			    IF THIS.nFDCount > 0
			        DIMENSION THIS.aFD[THIS.nFDCount]
			        nFD = 0
				    FOR nLine = 1 TO nLines - 1
				    	cLine = aContent[nLine]
				    	IF ATC(THIS.cBoundary,cLine) > 0
				    		cFDName = LOWER(CHRT(GETWORDNUM(aContent[nLine + 1],2,"="),["],""))
				    		cFDValue = aContent[nLine + 3]
				    		nFD = nFD + 1
				    		THIS.aFD[nFD] = CREATE("wsKVP",cFDName,cFDValue)
				    	ENDIF
				    ENDFOR
			    ENDIF
			    
			    
			CASE THIS.cContentType == "application/x-www-form-urlencoded"
				 THIS.nFDCount = GETWORDCOUNT(THIS.cContent, "&")
				 IF THIS.nFDCount > 0
					DIMEN THIS.aFD[THIS.nFDCount]					
					FOR nFD = 1 TO THIS.nFDCount
						cFDValue = GETWORDNUM(THIS.cContent, nFD, "&")
						cFDName = LOWER(GETWORDNUM(cFDValue, 1, "="))
						cFDValue = GETWORDNUM(cFDValue, 2, "=")
						THIS.aFD[nFD] = CREATE("wsKVP", cFDName, cFDValue)
					ENDFOR
				 ENDIF			
		ENDCASE
		RETURN
					
						
	PROCEDURE splitQS
		THIS.nQSCount = GETWORDCOUNT(THIS.cQueryString, "&")
		IF THIS.nQSCount > 0
			DIMEN THIS.aQS[THIS.nQSCount]
			LOCAL nQS,cQSName,cQSValue
			FOR nQS = 1 TO THIS.nQSCount
				cQSValue = GETWORDNUM(THIS.cQueryString, nQS, "&")
				cQSName = LOWER(GETWORDNUM(cQSValue, 1, "="))
				cQSValue = GETWORDNUM(cQSValue, 2, "=")
				THIS.aQS[nQS] = CREATE("wsKVP", cQSName, cQSValue)
			ENDFOR
		ENDIF
		RETURN
						
						
							
ENDDEFINE

************************************** END OF FEWSREQUEST.PRG **************************************

****************************************************************************************************
**                                        FEWSRESPONSE.PRG                                        **
****************************************************************************************************
DEFINE CLASS fewsResponse AS Custom
	
	cStatusCode = "200 OK"
	cContentType = "text/html"
	nContentLength = 0
	cContent = ""
	cServerName = ""
	cHandler = ""
	lCORSPreflight = .F.
	cCORSOrigin = ""

	PROCEDURE Init
	  LPARAMETERS pcStatusCode, pcContentType, pcContent
	  THIS.initWithData(pcStatusCode, pcContentType, pcContent)
	  RETURN 
	  	
	  	
	PROCEDURE cContent_Assign(cValue)
		THIS.cContent = cValue
		THIS.nContentLength = LEN(cValue)
		RETURN
		  	
		  	
	PROCEDURE initWithData
	  LPARAMETERS pcStatusCode, pcContentType, pcContent
	  LOCAL cSignature
	  cSignature = VARTYPE(pcStatusCode) + VARTYPE(pcContentType) + VARTYPE(pcContent)
	  DO CASE 
	     CASE cSignature = "CLL"
	          THIS.cStatusCode = pcStatusCode
	     
	     CASE cSignature = "CCL"
	          THIS.cStatusCode = "200 OK"
	          THIS.cContentType = pcStatusCode
	          THIS.cContent = pcContentType
	          
	     CASE cSignature = "CCC"
	          THIS.cStatusCode = pcStatusCode
	          THIS.cContentType = pcContentType
	          THIS.cContent = pcContent

  	     OTHERWISE
	          THIS.cStatusCode = "200 OK"
	          THIS.cContentType = "text/html"
	          THIS.cContent = ""     	 
	  ENDCASE
	  RETURN	
	
	
		
	PROCEDURE ToString
	  LPARAMETERS pcStatusCode, pcContentType, pcContent
	  IF PCOUNT() > 0
	  	THIS.initWithData(pcStatusCode, pcContentType, pcContent)
	  ENDIF
      LOCAL cREsponse
      cResponse = ""
      DO CASE
         CASE THIS.lCORSPreFlight
              cResponse = [HTTP/1.1 200 OK] + ;
						  [Content-Length: 0] + ;
						  [Connection: keep-alive] + ;
						  [Access-Control-Allow-Origin: ] + THIS.cCORSOrigin + ;
						  [Access-Control-Allow-Methods: POST, OPTIONS] + ;
						  [Access-Control-Allow-Headers: Content-Type, Access-Control-Allow-Headers, Authorization, X-Requested-With] + ;
						  [Access-Control-Max-Age: 86400] 
						  
         OTHERWISE
			  cResponse = [HTTP/1.1 ] + THIS.cStatusCode + CRLF + ;
						  [Cache-Control: private] + CRLF + ;
						  [Content-Type: ] + THIS.cContentType + CRLF + ;
						  [Content-length: ] + ALLT(STR(THIS.nContentLength)) + CRLF + ;
						  [Server: ] + THIS.cServerName + CRLF + ;
						  [Access-Control-Allow-Origin: *] + CRLF + ;
						  [Access-Control-Allow-Headers: Content-Type] + CRLF + ;
						  [Access-Control-Allow-Methods: GET,POST,PUT,DELETE,OPTIONS] + CRLF + ;
						  [Date: ]+ T2GMT(DATETIME()) + CRLF + ;
						  CRLF + ;
						  THIS.cContent
	  ENDCASE					  
	  RETURN cResponse    
ENDDEFINE

************************************* END OF FEWSRESPONSE.PRG **************************************

****************************************************************************************************
**                                    FEWSHANDLERABSTRACT.PRG                                     **
****************************************************************************************************
DEFINE CLASS fewsHandlerAbstract AS Custom
	oRequest = NULL
	oResponse = NULL
	PROCEDURE Process
		RETURN
ENDDEFINE

********************************** END OF FEWSHANDLERABSTRACT.PRG **********************************

****************************************************************************************************
**                                  FEWSSTATICCONTENTHANDLER.PRG                                  **
****************************************************************************************************
DEFINE CLASS fewsStaticContentHandler AS fewsHandlerAbstract
    Name = "staticContentHandler"
    oB64 = NULL
    
    PROCEDURE Init
    	THIS.oB64 = CREATE("base64Helper")
    	RETURN
    	
	PROCEDURE Process
	    LOCAL oReq,oResp
	    oReq = THIS.oRequest
	    oResp = THIS.oResponse
	    LOCAL cLocalFile,cUrl
	    cUrl = oReq.cUrl
	    IF cUrl == "/"
	    	cUrl = "/INDEX.HTML"
	    ENDIF
	    cUrl = SUBS(cUrl, 2)
	    cLocalFile = ADDBS(oReq.oServer.cDocumentsRoot) + CHRT(cUrl,"/","\")
	    IF FILE(cLocalFile)
	    	LOCAL cExt,cContent
	    	cExt = LOWER(JUSTEXT(cLocalFile))
	    	cContent = FILETOSTR(cLocalFile)
	    	DO CASE
	    	   CASE INLIST(cExt,"html","htm")
	    	        oResp.initWithData("200 OK","text/html", cContent)

	    	   CASE INLIST(cExt,"html","csv")
	    	        oResp.initWithData("200 OK","text/csv", cContent)

	    	   CASE INLIST(cExt,"json")
	    	        oResp.initWithData("200 OK","application/json", cContent)	 

	    	   CASE INLIST(cExt,"pdf")
	    	        oResp.initWithData("200 OK","application/pdf", cContent)	 
	    	        	    	        
	    	   CASE INLIST(cExt,"js","txt","css")
	    	        oResp.initWithData("200 OK","text/plain", cContent)	   
	    	        
	    	   CASE INLIST(cExt,"bmp","gif","jpg","jpeg")
	    	        cExt = STRT(cExt,"jpg","jpeg")
	    	        oResp.initWithData("200 OK","image/" + cExt, cContent)	   
	    	        	    	        
	    	   OTHERWISE
	    	        cContent = THIS.oB64.encodeString(cContent)
	    	   	    oResp.initWithData("200 OK","application/octet-stream", cContent)	
	    	ENDCASE
	    ELSE
	        ?"[404] " + cLocalFile
	    	oResp.cStatusCode = "404 NOT FOUND"
	    ENDIF
		RETURN
ENDDEFINE

******************************* END OF FEWSSTATICCONTENTHANDLER.PRG ********************************

****************************************************************************************************
**                                      FEWSCORSHANDLER.PRG                                       **
****************************************************************************************************
DEFINE CLASS fewsCORSHandler AS fewsHandlerAbstract

	PROCEDURE Process
	    LOCAL oReq,oResp
	    oReq = THIS.oRequest
	    oResp = THIS.oResponse
	    WITH oResp
	    	.lCORSPreflight = .T.
	    	.lCORSOrigin = oReq.cOrigin
	    ENDWITH
	    RETURN
	
ENDDEFINE
************************************ END OF FEWSCORSHANDLER.PRG ************************************

****************************************************************************************************
**                                          SUPPORT.PRG                                           **
****************************************************************************************************
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

**************************************** END OF SUPPORT.PRG ****************************************

****************************************************************************************************
**                                        BASE64HELPER.PRG                                        **
****************************************************************************************************
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

************************************* END OF BASE64HELPER.PRG **************************************

****************************************************************************************************
**                                     FEWSHISTORYMANAGER.PRG                                     **
****************************************************************************************************
DEFINE CLASS fewsHistoryManager AS Custom
	cDataFile = ""
	oEntry = NULL
	
	PROCEDURE beginRequest
		LPARAMETERS poRequest
		THIS.oEntry = CREATE("fewsHistoryEntry")
		WITH THIS.oEntry
		    .nConnId = poRequest.nId
			.tReceived = DATETIME()
			.nSecs = SECONDS()
			.cUrl = poRequest.cUrl
			.cRemoteHost = poRequest.socket.remoteHost
			.cRemoteIP = poRequest.socket.remoteHostIP
			.nRemotePort = poRequest.socket.remotePort
			.cQS = poRequest.cQueryString
		ENDWITH
		RETURN
		
		
	PROCEDURE completeRequest
		LPARAMETERS poResponse
		IF ISNULL(THIS.oEntry)
			RETURN
		ENDIF
		WITH THIS.oEntry
			.tSent = DATETIME()
			.nSecs = SECONDS() - .nSecs
			.cResponse = poResponse.cStatusCode
			.cHandler = poResponse.cHandler
		ENDWITH
		LOCAL nFH
		IF FILE(THIS.cDataFile)
			nFH = FOPEN(THIS.cDataFile,11)
			IF nFH > 0
				FSEEK(nFH,0,2)
			ENDIF
		ELSE
			nFH = FCREATE(THIS.cDataFile)
		ENDIF
		IF nFH < 0
			RETURN
		ENDIF
		FWRITE(nFH, THIS.oEntry.ToString() + CRLF)
		FCLOSE(nFH)
		RETURN
		
		

ENDDEFINE

DEFINE CLASS fewsHistoryEntry AS Custom
    nConnID = 0
	tReceived = {//::}
	cRemoteHost = ""
	nRemotePort = ""
	cRemoteIP = ""
	cUrl = ""
	cQS = ""
	cResponse = ""
	cHandler = ""
	tSent = {//::}
	nSecs = 0
	
	PROCEDURE ToString
		RETURN TTOC(THIS.tReceived) + "|" + ;
		       ALLT(STR(THIS.nConnID)) + "|" + ;
		       THIS.cRemoteIP + ":" + ALLT(STR(THIS.nRemotePort)) + IIF(EMPTY(THIS.cRemoteHost),"|"," (" + THIS.cRemoteHost + ")|") + ;
		       LOWER(THIS.cUrl) + "|" + ;
		       THIS.cQS + "|" +;
		       THIS.cHandler + "|" + ;
		       ALLT(TRANS(THIS.nSecs,"999.999"))+"s" + "|" + ;
		       THIS.cResponse 
ENDDEFINE	
********************************** END OF FEWSHISTORYMANAGER.PRG ***********************************

****************************************************************************************************
**                                         FEWSSERVER.PRG                                         **
****************************************************************************************************

DEFINE CLASS fewsServer AS winsock

    cName = "FEWS"
	cVersion = "1.0"
	nPort = 8006
	cLocalIP = ""
	nMaxConnections = 5
	DIMEN aConnPool[1]
	DIMEN aHandlers[1]
	nHandlersCount = 0
	cDocumentsRoot = ""
	cLastRequest = ""
	cLastResponse = ""
	cLastStatusCode = ""
	oHistory = NULL
	cLogFolder = ""
	nLogLevel = 9
	
	PROCEDURE Init
		DIMENSION THIS.aConnPool[THIS.nMaxConnections]
		LOCAL nConn,oConn
		FOR nConn = 1 TO THIS.nMaxConnections
		    oConn = CREATE("fwwsRequest")
		    WITH oConn
		    	.nId = nConn
		    	.Bind(THIS)
		    ENDWITH
			THIS.aConnPool[nConn] = oConn
		ENDFOR
		THIS.addHandler("*","fewsStaticContentHandler")
		THIS.cDocumentsRoot = SET("DEFAULT") + CURDIR()
		THIS.oHistory = CREATE("fewsHistoryManager")
		THIS.cLogFolder = THIS.cDocumentsRoot
		RETURN

	PROCEDURE cLocalIP_Access
		RETURN THIS.socket.localIP
		
	PROCEDURE Error
		LPARAMETERS nError, cMethod, nLine
	     THIS.addToLog("[SRV][ERR][" + ALLT(STR(nError)) + "][" + cMethod + ":" + ALLT(Str(nLine)) +"] " + MESSAGE(),1)
	     RETURN	
		
	PROCEDURE Destroy
		LOCAL nConn,cOnError
		cOnError = ON("ERROR")
		ON ERROR RETURN
		FOR nConn = 1 TO THIS.nMaxConnections
			THIS.aConnPool[nConn].Unbind()
			THIS.aConnPool[nConn] = NULL
		ENDFOR
		IF !EMPTY(cOnError)
			ON ERROR &cOnError
		ELSE
			ON ERROR	
		ENDIF
		RETURN
			
	PROCEDURE Listen
	    THIS.oHistory.cDataFile = ADDBS(THIS.cLogFolder) + "fews_history.log" 
	    WITH THIS.socket
	    	.Protocol = 0  && TCP
	    	.LocalPort = THIS.nPort
	    	.Listen()
	    ENDWITH
	    THIS.addToLog("[SRV] Listening on http://" + THIS.cLocalIP + ":" + ALLT(STR(THIS.nPort)),5)
		RETURN
		

	PROCEDURE addHandler
		LPARAMETERS pcFilter, pcHandlerClass
		LOCAL nIndex
		nIndex = THIS.getHandlerIndex(pcFilter)
		IF nIndex = 0
			THIS.nHandlersCount = THIS.nHandlersCount + 1
			DIMEN THIS.aHandlers[THIS.nHandlersCount]
			nIndex = THIS.nHandlersCount
		ENDIF
		THIS.aHandlers[nIndex] = CREATE("wsKVP", LOWER(pcFilter), pcHandlerClass)
		RETURN
		

	PROCEDURE getHandlerIndex
		LPARAMETERS pcFilter
		LOCAL nIndex,nH
		nIndex = 0
		pcFilter = LOWER(pcFilter)
		FOR nH = 1 TO THIS.nHandlersCount
			IF THIS.aHandlers[nH].cName == pcFilter
				nIndex = nH
				EXIT
			ENDIF
		ENDFOR
		RETURN nIndex
				
		
	PROCEDURE getHandler
		LPARAMETERS pcFilter
		LOCAL nIndex,oHandler
		oHandler = NULL		
		nIndex = THIS.getHandlerIndex(pcFilter)
		IF nIndex = 0
			nIndex = THIS.getHandlerIndex("*")
		ENDIF
		IF nIndex > 0
			oHandler = CREATE( THIS.aHandlers[nIndex].cValue )	
		ENDIF
		RETURN oHandler

				
	PROCEDURE socket.ConnectionRequest
		LPARAMETERS requestid
		THIS.Parent.addToLog("[SVR] Request received from " + THIS.remoteHostHost + " (" + THIS.remoteHostIP + "), ID:" + TRANS(requestid,""),5)
		LOCAL oConn
		oConn = THISFORM.getAvailConn()
		IF !ISNULL(oConn)
			oConn.socket.Accept(requestid)
			THIS.Parent.addToLog("[SVR] Assigned to connection #" + ALLT(STR(oConn.nID)), 5)
		ELSE
			THIS.Parent.addToLog("[SVR] No available connections at this time",1)
			LOCAL ARRAY aCPS[1]
			FOR oConn = 1 TO ALINES(aCPS, THIS.Parent.getConnPoolState())
				THIS.PArent.addToLog("[SVR] " + aCPS[oConn],1)
			ENDFOR
		ENDIF
		RETURN
		


	PROCEDURE getAvailConn
		LOCAL oConn, nConn, oAvailConn
		oAvailConn = NULL
		FOR nConn = 1 TO THIS.nMaxConnections
		    oConn = THIS.aConnPool[nConn]
		    IF oConn.lHandled
		    	IF INLIST(oConn.nState,SOCKET_STATE_CONNECTED,SOCKET_STATE_CLOSING)
		    		oConn.Object.Close()
		    	ENDIF
		    	oConn.Unbind()
		    	RELEASE oConn		        
		    	oConn = CREATE("fewsRequest")
		    	oConn.nId = nConn
		    	oConn.Bind(THIS)
		    	THIS.aConnPool[nConn] = oConn
		    ENDIF
			IF oConn.nState = SOCKET_STATE_CLOSED
				oAvailConn = THIS.aConnPool[nConn]
				EXIT
			ENDIF
		ENDFOR
		RETURN oAvailConn



	PROCEDURE addToLog
		LPARAMETERS pcText,pnLevel
		LOCAL cLogFile
		cLogFile = ADDBS(THIS.cLogFolder) + "fews.log"
		pnLevel = EVL(pnLevel, 9)
		IF pnLevel > THIS.nLogLevel
			RETURN
		ENDIF
		IF !FILE(cLogFile)
			FCLOSE(FCREATE(cLogFile))
		ENDIF
		LOCAL nFH
		nFH = FOPEN(cLogFile,11)
		FSEEK(nFH,0,2)
		FWRITE(nFH,"[" + TTOC(DATETIME()) + "][" + ALLT(STR(pnLevel)) + "] " + pcText + CRLF)
		FCLOSE(nFH)
		IF _VFP.startMode = 0
			?DATETIME(),pnLevel,pcText
		ENDIF
		RETURN	
		
				
		
	PROCEDURE getConnPoolState
		LOCAL cState,nConn,oConn
		cState = ""
		FOR nConn = 1 TO THIS.nMaxConnections
		    oConn = THIS.aConnPool[nConn]
			cState = cState + ;
			         ALLT(STR(oConn.nId)) + "," + ;
			         oConn.cState + "," + ;
			         oConn.cUrl + "," + ;
			         oConn.cStatusCode + CRLF
		ENDFOR
		RETURN cState
ENDDEFINE

************************************** END OF FEWSSERVER.PRG ***************************************

