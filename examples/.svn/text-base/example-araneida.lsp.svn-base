<html>
<head>
<title>LSP Example (Araneida)</title>
</head>
<body>

<h1>LSP Example</h1>

<h2>User Agent</h2>
<%
    (araneida:html-stream (araneida:request-stream request)
			  (or (araneida:request-header request :user-agent)
			      "None."))
%>

<h2>Referrer</h2>
<%
  (araneida:html-stream (araneida:request-stream request)
    (or (araneida:request-header request :referer)
	"None."))
%>

<h2>Query Variables</h2>
<table>
<%
(let ((queries (araneida:url-query-alist (araneida:request-url request))))
  (if (null queries)
    (araneida:html-stream (araneida:request-stream request)
      `(tr (td "None.")))
    (dolist (query queries)
      (araneida:html-stream (araneida:request-stream request)
        `(tr (td ,(car query))
	     (td ,(cdr query)))))))
%>
</table>

<h2>Loop Of Dynamism</h2>

<% (dotimes (i (+ (random 10) 1)) %>
  Hi!<br>
<% ) %>
</body>
</html>
