all: build

build:
	sbt ';compile ;package'
	sbt ';test:compile ;test:package'

doc:
	sbt doc

run:
	sbt 'test:runMain tetriski.pillars.Pillars'

mapping:
	sbt 'test:runMain tetriski.pillars.mapping.Mapping'

clean:
	rm -f *.json *.fir *.v
	rm -rf test_run_dir/

cleanall: clean
	sbt clean