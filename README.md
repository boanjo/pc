pc
==

Pond Control is a garden automation project with sensors, feeder, water level control for my Koi pond. For low level io i use an Arduino UNO + 2 shields for all the connections of sensors and the few extra discrete components that are needed. For user interface and control logic a Raspberry PI is used running a nitrogen/CowBoy Erlang web server.

Features:
- Keeps a precise water level so you never risk overflowing the pond. 
- The water fill flow is monitored over time to detect leaks or suspiciously high water usage. 
- Monitors water temperature which controls the fish feeding frequency. 
- Monitors the PH value in realtime and signals if counter measures should be taken (too high/low). Good range is PH 7.8 - 8.2
- Watering plants and bushes near the pond with sprinkler system
- Fish feeder
- Webserver running a mobile friendly UI to monitor and control the system

![The pond](https://github.com/epkboan/epkboan.github.io/blob/master/pond_0.jpg?raw=true "The Pond under control")

All sensors are located close to the pond naturaly. The concrete box is partly placed above the water for the fish feeder and the sensors.
- Fish feeder for pellets (home made with a continous rotating servo like this http://www.adafruit.com/products/2442)
- The PH sensor (http://atlas-scientific.com/product_pages/kits/ph-kit.html?) and an insulation cricuit to eliminate motor noise in the ground plane (http://atlas-scientific.com/product_pages/circuits/pwr-iso.html)
- The liquid eTape ruler (http://www.adafruit.com/products/464) to measure the level (i keep a +/-5mm level) 
- The temperature sensor (http://www.adafruit.com/products/381)

![The pond](https://github.com/epkboan/epkboan.github.io/blob/master/pond_2.jpg?raw=true "Sensor location")


![Arduino ](https://github.com/epkboan/epkboan.github.io/blob/master/pc_1.jpg?raw=true "Arduino")

![Sensors](https://github.com/epkboan/epkboan.github.io/blob/master/pc_2.jpg?raw=true "Sensors")

![The pond](https://github.com/epkboan/epkboan.github.io/blob/master/pond_1.jpg?raw=true "The Pond at control")
