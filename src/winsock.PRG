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