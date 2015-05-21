#include <OneWire.h>
#include <stdlib.h>
#include <SoftwareSerial.h>

SoftwareSerial mySerial(2, 3); // RX, TX
OneWire  ds(4); 
// the valuese of the 'other' resistor

#define SERIES_RESISTOR 560    

#define WATER_LEVEL_PIN   A1 
#define FLOW_SENSOR_PIN   9
#define FEED_PIN          8
#define WATER_CONTROL_PIN 5

#define STATE_TEMP_READ 1 
#define STATE_TEMP_MEAS 2 

#define STATE_WATER_ON 0
#define STATE_WATER_OFF 1

#define NO_OF_LEVEL_AVERAGE_ITEMS  8

#define MODE_WATER_MANUAL 0
#define MODE_WATER_AUTOMATIC 1

float levelArray[NO_OF_LEVEL_AVERAGE_ITEMS];
long levelAvgCnt = 0;
byte waterMode = MODE_WATER_AUTOMATIC;
byte waterState = STATE_WATER_OFF;
byte state = STATE_TEMP_MEAS;
byte addr[8];
long interval = 1000;           // interval at which to blink (milliseconds)
long previousMillis = 0;        // will store last time LED was updated
float celsius = 0.0;

String inputString = "";         // a string to hold incoming data
String phInput = ""; 
String phStr = ""; 
long waterLevelLow = 100;
long waterLevelHigh = 110;


volatile uint32_t flowPulses = 0;
volatile uint8_t lastFlowPinState = LOW;

// Interrupt is called once a millisecond, looks for any pulses from the sensor!
SIGNAL(TIMER0_COMPA_vect) {
	
  uint8_t x = digitalRead(FLOW_SENSOR_PIN);
	
  if((x == HIGH) && (lastFlowPinState == LOW)) {
    //low to high transition!
    flowPulses++;
  }
  lastFlowPinState = x;
}

void useInterrupt(boolean v) {
  if (v) {
    // Timer0 is already used for millis() - we'll just interrupt somewhere
    // in the middle and call the "Compare A" function above
    OCR0A = 0xAF;
    TIMSK0 |= _BV(OCIE0A);
  } else {
    // do not call the interrupt function COMPA anymore
    TIMSK0 &= ~_BV(OCIE0A);
  }
}

void setup(void) {
  Serial.begin(9600);
  mySerial.begin(9600);
  digitalWrite(WATER_CONTROL_PIN, LOW);
  pinMode(WATER_CONTROL_PIN, OUTPUT);
  digitalWrite(FEED_PIN, LOW);
  pinMode(FEED_PIN, OUTPUT);

  pinMode(WATER_LEVEL_PIN, INPUT);
  pinMode(FLOW_SENSOR_PIN, INPUT);
  digitalWrite(FLOW_SENSOR_PIN, HIGH);
  lastFlowPinState = digitalRead(FLOW_SENSOR_PIN);
	
  useInterrupt(true);

  Serial.println("Starting");

  if (!ds.search(addr)) 
  {
    Serial.println("No more addresses.");
    ds.reset_search();
    delay(250);
  }
	
}

int getSerialValue(String str) 
{
  String subStr = str.substring(str.indexOf(",")+1, str.length());
  return subStr.toInt();
}


void loop(void) {
  byte present = 0;
  byte type_s;
  char tempStr[8];
  char litersStr[8];
  char levelStr[8];
  float waterLevel;
  float waterLevelAvg = 0;
  byte badTempReading = false;
  char inChar;

  // HW SW serial
  if (mySerial.available())
  {
    char ch = mySerial.read();
    if(ch == '\r')
    {	  
      phStr = phInput;
      phInput = "";
    }
    else 
    {
      phInput += ch; 
    }
  }

  // USB HW serial
  if (Serial.available() > 0) 
  {
    inChar = Serial.read();
		
    if (inChar == '{') 
    {
      inputString = "";
    } 
    else if (inChar == '}') 
    {
      if(inputString.length() > 0) {

	if(inputString.indexOf("water_level_low") != -1)
      {
	waterLevelLow = getSerialValue(inputString);
      }
      else if(inputString.indexOf("water_level_high") != -1) 
      {
	waterLevelHigh = getSerialValue(inputString);
      }
      else if(inputString.indexOf("feed") != -1) 
      {
	int pulses = getSerialValue(inputString);
	feed(pulses);
      }
      else if(inputString.indexOf("water") != -1) 
      {
	String subCmd = inputString.substring(inputString.indexOf(",")+1, inputString.length());

	if(subCmd.indexOf("off") != -1)
	{
	  digitalWrite(WATER_CONTROL_PIN, LOW);
	  waterState = STATE_WATER_OFF;
	}
	else if(subCmd.indexOf("on") != -1)
	{
	  digitalWrite(WATER_CONTROL_PIN, HIGH);
	  waterState = STATE_WATER_ON;
	}
	else if(subCmd.indexOf("man") != -1)
	{
	  waterMode = MODE_WATER_MANUAL;
	}
	else if(subCmd.indexOf("auto") != -1)
	{
	  waterMode = MODE_WATER_AUTOMATIC;
	}
      }

      inputString = "";
      }
    } 
    else
    {
      // add it to the inputString:
      inputString += inChar;

    }
  }
	
	
  unsigned long currentMillis = millis();

  // Handle wrap around
  if(currentMillis < previousMillis) 
  {
    previousMillis = currentMillis;
  }
  if((currentMillis - previousMillis) > interval) 
  {
    // save the last time you blinked the LED 
    previousMillis = currentMillis;   
				
    // Temperature
    ReadTempSensor(); 
    dtostrf(celsius,5,1,tempStr);

    // Calculate amount of added water in liters  
    float liters = flowPulses;
    liters /= 8.1;
    liters -= 6;
    liters /= 60.0;

    if(liters < 0) liters = 0;
    dtostrf(liters,7,1,litersStr);

    // Calculate the Water level in pond
    waterLevel = analogRead(WATER_LEVEL_PIN);


    //    Serial.println(waterLevel, DEC);

    // convert the value to resistance
    waterLevel = (1023 / waterLevel)  - 1;
    waterLevel = SERIES_RESISTOR / waterLevel;
    waterLevel = 1483 - waterLevel;
    waterLevel = waterLevel / 6;
    waterLevel +=50;

    levelArray[levelAvgCnt%NO_OF_LEVEL_AVERAGE_ITEMS] = waterLevel;
    levelAvgCnt++;

    if(levelAvgCnt >= NO_OF_LEVEL_AVERAGE_ITEMS)
    {
      for(int i = 0; i < NO_OF_LEVEL_AVERAGE_ITEMS; i++)
      {
	waterLevelAvg += levelArray[i];
      }

      waterLevelAvg = waterLevelAvg / NO_OF_LEVEL_AVERAGE_ITEMS;

      if(waterMode == MODE_WATER_AUTOMATIC)
      {
	if((waterLevelAvg < waterLevelLow) && (waterState == STATE_WATER_OFF))
	{
	  digitalWrite(WATER_CONTROL_PIN, HIGH);
	  waterState = STATE_WATER_ON;
	}
	else if((waterLevelAvg > waterLevelHigh) && (waterState == STATE_WATER_ON))
        {
	  digitalWrite(WATER_CONTROL_PIN, LOW);
	  waterState = STATE_WATER_OFF;
	}
      }

    }

    dtostrf(waterLevelAvg,5,1,levelStr);

    long time = millis();

    //######### SERIAL SEND ################
    Serial.print("{ph,"); Serial.print(phStr); Serial.println("}.");
    Serial.print("{temp,"); Serial.print(tempStr); Serial.println("}.");
    Serial.print("{flow,"); Serial.print(litersStr); Serial.print(","); Serial.print(flowPulses, DEC); Serial.println("}.");
    Serial.print("{level,"); Serial.print(levelStr); Serial.println("}.");

    Serial.print("{water,"); 
    if (waterMode==MODE_WATER_AUTOMATIC) 
      Serial.print("auto");
    else 
      Serial.print("manual");
    Serial.print(",");
    if (waterState==STATE_WATER_ON) 
      Serial.print("on");
    else 
      Serial.print("off");
    Serial.println("}.");

    long diff = millis() - time;
    Serial.print("{cmd_time,");        
    Serial.print(diff);
    Serial.println("}.");
    
  }
}

void feed(int pulses)
{
  for(int i = 0; i < pulses; i++)
  {
    digitalWrite(FEED_PIN, LOW);
    delay(20);
    //   delayMicroseconds(10000);
    digitalWrite(FEED_PIN, HIGH);
    delay(1);
  }
  digitalWrite(FEED_PIN, LOW);
}


void ReadTempSensor() 
{
  byte data[12];
  byte i;

  if(state == STATE_TEMP_MEAS)
  {
    ds.reset();
    ds.select(addr);
    // start conversion, with parasite power on at the end
    ds.write(0x44, 1);        
    state = STATE_TEMP_READ;
  }

  else if(state == STATE_TEMP_READ)
  {  
    ds.reset();
    ds.select(addr);    
    // Read Scratchpad
    ds.write(0xBE);         
		
    for ( i = 0; i < 9; i++) {           // we need 9 bytes
      data[i] = ds.read();
    }
		
    // Convert the data to actual temperature
    // because the result is a 16 bit signed integer, it should
    // be stored to an "int16_t" type, which is always 16 bits
    // even when compiled on a 32 bit processor.
    int16_t raw = (data[1] << 8) | data[0];
    byte cfg = (data[4] & 0x60);
    // at lower res, the low bits are undefined, so let's zero them
    if (cfg == 0x00) raw = raw & ~7;  // 9 bit resolution, 93.75 ms
    else if (cfg == 0x20) raw = raw & ~3; // 10 bit res, 187.5 ms
    else if (cfg == 0x40) raw = raw & ~1; // 11 bit res, 375 ms
    //// default is 12 bit resolution, 750 ms conversion time   
    celsius = (float)raw / 16.0;
    state = STATE_TEMP_MEAS;
  }
}
