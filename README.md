[hsmsim](http://github.com/leachbj/hsmsim) is a simple HSM simulator providing a number of commands compatible
with a Thales 8000/9000 HSM.  The simulator only supports a small number of commands and can only use test LMKs
so should not be considered a replacement for a real HSM however it may be useful during a development of software
that interacts with a HSM.

## Quick start

The simulator runs as a java process.

Apache Maven is required to compile the code for this project.

### Compile
  `mvn package`

### Run the packaged jar
  `java -jar target/hsmsim-akka/hsmsim.jar`

Alternatively the simulator can be deployed as a web application, deploy the target/hsmsim-war/hsmsim.war file to a suitable
servlet container.

## Contributing

The simulator supports a very small number of commands and only supports the test LMKs.  Contributions of
additional command support welcomed.

## License
Copyright 2013 Bernard Leach

Licensed under the MIT license [http://opensource.org/licenses/MIT]
