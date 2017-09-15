
.PHONY: run 
run: db passwd
	racket uiki.rkt 

db:
	mkdir db
	mkdir db/main

passwd:
	echo 'Enter a password for the user `admin`':
	@htpasswd -sc passwd admin

certs:
	mkdir certs
	cd certs; openssl req  -nodes -new -x509 -keyout private-key.pem -out server-cert.pem; chmod 400 private-key.pem


