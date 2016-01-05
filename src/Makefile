.PHONY: all clean

PNAME=./
TESTFILE=./tests
GAMEFILE=./game
CC=fsharpc
CFLAGS= --warnaserror+ --nologo

all: $(PNAME).dll

$(PNAME).dll: 
	$(CC) $(CFLAGS) $(GAMEFILE).fsx && $(CC) $(CFLAGS) $(TESTFILE).fsx


withdocs: 
	$(CC) $(CFLAGS) --doc:./simpleJackDocs.xml $(GAMEFILE).fsx  &&  $(CC) $(CFLAGS) $(TESTFILE).fsx

clean:
	rm -f $(TESTFILE).exe && rm -f $(GAMEFILE).exe
