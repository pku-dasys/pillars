all: build

build:
	sbt ';compile ;package'
	sbt ';test:compile ;test:package'

doc:
	sbt doc

run:
	sbt 'test:runMain tetriski.pillars.Pillars'

end2end:
	sbt 'test:runMain tetriski.pillars.examples.Tutorial'

clean:
	rm -f *.json *.fir *.v
	rm -rf test_run_dir/

cleanall: clean
	sbt clean