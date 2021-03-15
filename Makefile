.PHONY: all

all:
	cd IsqGet; dotnet publish -c Release -r linux-x64 -p:PublishSingleFile=true -p:PublishTrimmed=true -p:ReadyToRun=true --self-contained true

install:
	cp ./IsqGet/bin/Release/*/*/publish/IsqGet /usr/local/bin
