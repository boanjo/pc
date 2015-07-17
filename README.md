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

No, it's not the prettiest construction i've made but it does the job. The top shield contains 3 TIP120 power FETs to open the water flow to these solenoids (http://www.adafruit.com/products/997). One for the water fill and 2 spinkler systems (http://www.gardena.com/int/water-management/micro-drip-irrigation-system/)

![Arduino ](https://github.com/epkboan/epkboan.github.io/blob/master/pc_1.jpg?raw=true "Arduino")

The PH circuit i use required it's own breadboard so i placed that out near the water. Otherwise it is mainly a hub for all the sensors and fish feeder

![Sensors](https://github.com/epkboan/epkboan.github.io/blob/master/pc_2.jpg?raw=true "Sensors")

Instead of writing a specific android or iphone app i've chosen to make a web gui with the jQuery mobile look. Below is the simple web gui to interact and watch the sensor values. The web server runs on a raspberry PI which can be accessed from any device (LAN and WAN). All the sensor values are pushed to a mysql database once every 15 minutes to create statistics over time (ph, temp, flow etc) and can be followed here http://www.epkboan.net/pc/ph.php

![WEB gui](https://github.com/epkboan/epkboan.github.io/blob/master/pc_main.png?raw=true "The web gui")

The feeding time and sprinklers can be configured (drop down menu button). 

![Timers](https://github.com/epkboan/epkboan.github.io/blob/master/pc_zoom.png?raw=true "Programmable timers for feeding and sprinklers")


If you look really careful the hose that fills the water level is visible to the left of the waterfall. I.e. when draining the pond (back flushing the filter out in the garden) fresh water is filled from the opposite side of the pond.

Oh and YES! The Koi fishes loves the automation too :-)

![The pond](https://github.com/epkboan/epkboan.github.io/blob/master/pond_1.jpg?raw=true "The Pond at control")
