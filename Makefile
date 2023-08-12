all: build

build:
	sbt ';compile ;package'
	sbt ';test:compile ;test:package'

doc:
	sbt doc

run:
	sbt 'test:runMain pillars.Pillars'

simpleAdd:
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:./lib sbt 'test:runMain pillars.examples.SimpleAddExample'

end2end:
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:./lib sbt 'test:runMain pillars.examples.Tutorial'

clean:
	rm -f *.json *.fir *.v
	rm -rf test_run_dir/

cleanall: clean
	sbt clean
