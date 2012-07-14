cl-websocket
============

Yet another WebSocket implementation for Common Lisp. This one aspires
to be RFC6455 compliant.

Introduction
------------

I wanted a modern websocket implementation with minimal dependencies
that I could run cross platform. My primary goal is to use an HTML5
browser for the GUI of my Common Lisp applications.

I tried the other WebSocket related projects, but none seemed
suitable. IOLib is unfortunately not cross platform.

This is a pre-beta release - not fully featured but certainly good
enough for demos and prototypes. Feedback and contributions through
GitHub are welcome.

The code comes with a [BSD-style
license](http://www.opensource.org/licenses/bsd-license.php) so you
can basically do with it whatever you want.

Credit to Simon David Pratt who wrote a draft implementation in 2010
called cl-websocket at https://github.com/spratt/cl-websocket , but
this is a rewrite for RFC6455.

Implementation
--------------

Based on the HTML5 WebSocket protocol as defined in RFC6455. The
intention is that this library should be easy to use in conjunction
with any TCP socket implementation or web server framework.

There is a sample that uses usocket to provide a very simple, cross
platform, single-threaded socket server. It's sufficient to allow you
to connect with an HTML5 browser and run an echo server or, more
interestingy a REPL in your browser!

I intentionally and explicitly do not support earlier draft versions
of WebSockets - the library landscape is already littered with these,
and we need to move on.

Getting Started
---------------

The easist place to start is the sample code. Load it up using your
favourite Quicklisp enabled Lisp environment:

    > (ql:quickload "cl-websocket-sample")
    > (cl-websocket-sample:echo-server)

Then point your HTML5 web browser (probably Chrome?) at the
sample.html file on disk. Press the connect button, now anything you
type and Send should be echo'd back.

You might also like to try the REPL sample

   > (cl-websocket-sample:repl-server)

This works similarly except everything you send from the browser is
eval'd on the server!

Integration
-----------

The sample.lisp file shows a simple integration with usocket, but it
should be easy to use with IOLib or web frameworks.

Limitations
-----------

Only supports short (125 byte) messages (I'll fix that soon).  

Currently focussed on supporting WebSocket servers, but no reason why it
shouln't be a good WebSocket client too.  

Doesn't yet support extended features.

Doesn't yet support multi frame messages.

Doesn't yet support ping, pong.


Rob Blackwell

July 2012