SCALA = ./scala/bin/scala
SCALA_OPTS = -deprecation

run:
	$(SCALA) $(SCALA_OPTS) src/$(CLASS)/$(CLASS).scala