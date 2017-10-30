To build with a development version of this plugin, enter the directory
containing this README and repeat as follows:

* write some code
* `gradle build install`
* `mvn install:install-file -Dfile=build/libs/bond-gradle-1.0.jar -DpomFile=build/poms/pom-default.xml`

This will install the plugin to your local maven repository, which the other
Java projects' build.gradles will use in preference to maven central.
