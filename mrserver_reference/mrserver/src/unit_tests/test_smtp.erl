-module(test_smtp).

-export([sendemail/0]).

sendemail () ->
	gen_smtp_client:send({"roy@orogeny.io", ["roy.c.davies@ieee.org"], "Subject: testing\r\nFrom: Roy Davies \r\nTo: Some Dude \r\n\r\nThis is the email body"}, [{relay, "hp241.hostpapa.com"}, {username, "roy@orogeny.io"}, {password, "1gelk0tt"}, {ssl, true}]).
