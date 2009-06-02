LSP - Lisp Server Pages
=======================

Copyright 2001, 2009 John Wiseman (jjwiseman@gmail.com)


Introduction
-------------

Java Server Pages are a way to make web pages that are dynamic, by
embedding Java code in HTML.  Similarly, Lisp Server Pages allow you
to make dynamic web pages that contain Lisp code.

LSP is designed to work with a separate web server library.  The code
comes with support for both the AllegroServe and Araneida servers, but
it shouldn't be difficult to add support for other libraries (see
``lsp-aserve.lisp`` and ``lsp-allegroserve.lisp`` for examples).

LSP itself should be portable ANSI Lisp code that will run in any Lisp
implementation.

Before loading LSP, first load the particular web server code you're
using.  Then the easiest way to load LSP is to use ASDF, by evaluting
one of the following forms::

  (asdf:operate 'asdf:load-op :lsp-aserve)   ;; For AllegroServe support
  (asdf:operate 'asdf:load-op :lsp-araneida) ;; For Araneida support


Using LSP
----------

To publish an LSP page, call the ``PUBLISH-LSP`` function::

  PUBLISH-LSP (&key path file server (package *package*))     [function]

Publishes an LSP file.  ``PATH`` is a string containing the name part
of the URL at which to publish the file,e.g. ``"/math/sum.lsp"``;
``FILE`` is a pathname that specifies the file containing the page to
publish. ``PACKAGE`` is the package the reader should use when loading
and compiling this file (``*package*`` is the default).  ``SERVER`` is
the HTTP listener on which to publish the file.

Example::

  (publish-lsp :path "/temperature.html"
               :file "/Users/wiseman/src/temperature.lsp"
               :server *my-server-or-http-listener*)

An LSP file looks just like an HTML file, except for two new tags:
``<% ... %>`` and ``<%= ... %>``.

``<% ... %>`` is a scriptlet tag (to use the JSP terminology), and
wraps lisp code.  For example, ``<% (dotimes (i 10) (beep)) %>``. The
code inside the tag is executed each time the page is requested.

``<%= ... %>`` is an expression tag, and the effect of this tag varies
according to the particular web framework you are using, but is
intended to evaluate the contents as if they were wrapped with the
HTML generation macro that is usually provided.

For example, ``lsp-aserve.lisp`` defines ``<%=...%>`` such that the
contents are wrapped with the Franz ``net.html.generator:html`` macro.
More precisely::

  <%= (:h1 "header") "hi" (:princ-safe (generate-footer)) %>

is equivalent to::

  (net.html.generator:html
    (:h1 "header")
    "hi"
    (:princ-safe (generate-footer)))

which will output something like the following HTML::

 <h1>header</h1>hi<hr>2002-06-09

The ``lsp-araneida.lisp`` file defines ``<%= %>`` to use Araneida's
HTML generation facility.

During execution of LSP code, some variables related to the HTTP
request will be bound for use by the LSP code itself.

For example, LSP's AllegroServe support binds the following two
variables:

REQUEST
  The HTTP request object containing all the information about the request.
ENTITY
  The information passed to the ``publish-lsp`` function.

(See the AllegroServe documentation for more information on these
objects.)

In Araneida, the two variables are ``METHOD`` and ``REQUEST``, which
are bound to objects representing the type of HTTP request and the
HTTP request itself, respectively.


Tips
----

Expressions can be used inside HTML attributes, e.g.::

 <img src="<%= (compute-img-url request) %>">

Scriptlets do not need to be complete lisp forms, as long as the
page as a whole is syntactically valid, e.g.::

  <% (dotimes (i 10) %>
    <img src="mr-yuck.jpg">
  <% ) %>

See the ``examples`` directory for examples of publishing and using LSP
files in both AllegroServe and Araneida.


Implementation notes and caveats
--------------------------------

LSP pages are converted to strings containing lisp code, which are
then compiled and cached.  If the source file containing the lsp code
is modified, the next time a request is made for that page the code
will be recompiled and recached.

In my first attempt to do this, I tried to construct forms instead of
strings.  That just made it trickier to separate forms across ``<% ...
%>`` tags (see the ``dotimes`` example above).  Just because it's bad
that other languages are often *limited* to manipulating code as
strings doesn't mean there aren't times where it's appropriate.

There's nothing like JSP's directives or declarations.

LSP requires Franz' AllegroServe
(http://allegroserve.sourceforge.net/), Portable AllegroServe
(http://portableaserve.sourceforge.net/) or Araneida
(http://www.cliki.net/Araneida).

See http://sourceforge.net/projects/lsp for a more serious
attempt at doing this right, by Sunil Mishra and Tim Bradshaw.
