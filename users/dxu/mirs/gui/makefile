include ../setup/paths

all:
	${JAVA_HOME}/bin/javac -sourcepath ../src/lib_java  -d . Monitor.java
	${JAVA_HOME}/bin/jar cvfm  ../bin/Monitor.jar  Monitor.mf  *.class
	rm -f *.class

version:
	${JAVA_HOME}/bin/javac -sourcepath ../src/lib_java  -d . Monitor.java
	${JAVA_HOME}/bin/jar cvfm  ../bin/Monitor.jar  Monitor.mf  *.class
	rm -f *.class
	cd .. ;  svn status -u | tail -1 | awk '{print $$4}' > version.txt
	
run:
	${JAVA_HOME}/bin/java -Dawt.toolkit=sun.awt.X11.XToolkit -jar ../bin/Monitor.jar &


clean:
	rm  -f *.class ../bin/Monitor.jar
