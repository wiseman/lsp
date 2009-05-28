<html>
<head>
<title>LSP Example (AllegroServe)</title>
</head>
<body>
<h1>LSP Example</h1>

<h2>User Agent</h2>
<%=
  (:princ-safe (or (net.aserve:header-slot-value request
						 :user-agent)
		   "None."))
%>

<h2>Referrer</h2>
<%=
  (:princ-safe (or (net.aserve:header-slot-value request
						 :referer)
		   "None."))
%>

<h2>Query Variables</h2>
<table>
<%
(let ((queries (request-query request)))
  (if (null queries)
    (html (:tr (:td "None.")))
    (dolist (query queries)
      (html
       (:tr (:td (:princ-safe (car query)))
	    (:td (:princ-safe (cdr query))))))))
%>
</table>

<h2>Loop Of Dynamism</h2>

<% (dotimes (i (+ (random 10) 1)) %>
  Hi!<br>
<% ) %>
</body>
</html>
