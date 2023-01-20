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