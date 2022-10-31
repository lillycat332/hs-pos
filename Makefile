electron-pack:
	electron-packager ./frontend/electron/ "Triangle POS" --overwrite --out ./electron/build --prune=true

build-server:
	cabal install exe:hs-pos --installdir=./frontend/electron/bin/ --overwrite-policy=always

build-frontend:
	cd frontend &&\
	yarn bundle --mode=production &&\
	cp -r ../public ./electron/bin/
	$(MAKE) electron-pack

build: 
	$(MAKE) build-server
	$(MAKE) build-frontend