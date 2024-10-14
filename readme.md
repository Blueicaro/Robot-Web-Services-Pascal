# Robot Web Services Pascal

This a repository that contents a free pascal library to access a ABB robot using [Robot Web Services](https://developercenter.robotstudio.com/api/RWS).


## Robot Web services
Robot Web Services is a library written in 100% free pascal that enables developers to create their own custom applications to interact with the ABB robots.

ABB robots has a Robot Web Services. Which exposes RESTful APIs that leverages the HTTPS protocol and the messages are composed of XHTML and JSON. 
Robot Web Services facilitates platform independent and language independent communication with the robot controller. 

In REST, an URL identifies a resource. The representation of the application data sent from the robot controller can be either in XHTML or JSON format

ABB has two version of robot which has this feature. Irc5 with robotware (robot operating system) version 6.0 and Onmicore with robotware 7.0. 

If you connect to a robot Irc5, the connection is not secured with SSL, so only you use it on your program. 

But to connect to Abb Onmicore Controller You need install Openssl library version 1.1 to connect with Onmicore controllers. Because this version use SLL connection.
For windows aplications you must put in the same folder of the executable this files: 

* libssl-1_1-x64,dll 
* ibcrypto-1_1-x64.dll 

This files can be downloaded from here: https://wiki.overbyte.eu/wiki/index.php/ICS_Download.



