all: minijs

minijs: syntax.scala values.scala domains.scala interpreter.scala util.scala
	fsc -d build -unchecked syntax.scala values.scala domains.scala interpreter.scala 		util.scala

clean:
	rm -rf build/* *.out
test:
	python testAll.py
