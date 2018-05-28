
build: bin/db bin/htdocs bin/static bin/passwd
	raco exe -o bin/uiki ../uiki.rkt

run: build
	cd bin; ./uiki

bin/db:
	mkdir -p bin/db

bin/htdocs:
	mkdir -p bin/htdocs

bin/static:
	cp -r ../static bin/static

bin/passwd:
	echo 'Enter a password for the user `admin`':
	@htpasswd -sc passwd admin
	mv passwd ./bin

bin/certs:
	mkdir -p bin/certs
	cd bin/certs; openssl req  -nodes -new -x509 -keyout private-key.pem -out server-cert.pem; chmod 400 private-key.pem

.PHONY: build run