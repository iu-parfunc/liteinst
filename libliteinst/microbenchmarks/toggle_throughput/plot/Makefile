
all: ToggleThroughput_DataCleaner.exe



ToggleThroughput_DataCleaner.exe: ToggleThroughput_DataCleaner.hs
	time stack --no-system-ghc --verbosity silent --resolver lts-3.8 --install-ghc ghc --package cassava --package text --package bytestring --package vector --package turtle  -- -O2 ToggleThroughput_DataCleaner.hs  -o ToggleThroughput_DataCleaner.exe -main-is ToggleThroughput_DataCleaner
