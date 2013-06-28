#include <OneWire.h>
#include <LiquidCrystal.h>
#include<stdlib.h>

LiquidCrystal lcd(8,9,10,11,12,13);
OneWire  ds(7); 
// the value of the 'other' resistor
#define SERIES_RESISTOR 560    

#define WATER_LEVEL_PIN A0 
#define FLOW_SENSOR_PIN 2
#define RAIN_GAUGE_PIN 6
#define WATER_CONTROL_PIN 4
#define FEED_PIN          3
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

long waterLevelLow = 100;
long waterLevelHigh = 120;


volatile uint32_t flowPulses = 0;
volatile uint8_t lastFlowPinState = LOW;

volatile uint32_t rainGaugePulses = 0;
volatile uint8_t lastRainGaugePinState = HIGH;

// Interrupt is called once a millisecond, looks for any pulses from the sensor!
SIGNAL(TIMER0_COMPA_vect) {
	
  uint8_t x  = digitalRead(RAIN_GAUGE_PIN);
  if((x == LOW) && (lastRainGaugePinState == HIGH))
  {
    rainGaugePulses++;
  }
  lastRainGaugePinState = x;
	
  x = digitalRead(FLOW_SENSOR_PIN);
	
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
  lcd.begin(16,4);
  digitalWrite(WATER_CONTROL_PIN, LOW);
  pinMode(WATER_CONTROL_PIN, OUTPUT);
  pinMode(FEED_PIN, OUTPUT);

  pinMode(RAIN_GAUGE_PIN, INPUT);
  pinMode(WATER_LEVEL_PIN, INPUT);
  pinMode(FLOW_SENSOR_PIN, INPUT);
  digitalWrite(FLOW_SENSOR_PIN, HIGH);
  lastFlowPinState = digitalRead(FLOW_SENSOR_PIN);
	
  useInterrupt(true);

  Serial.println("Starting");

  if (!ds.search(addr)) 
  {
    lcd.setCursor(0,0);
    Serial.println("No more addresses.");
    ds.reset_search();
    delay(250);
  }
	
}

int getSerialValue(String str) 
{
  String subStr = str.substring(str.indexOf("=")+1, str.length());
  return subStr.toInt();
}

void feed(int pulses)
{
  for(int i = 0; i < pulses; i++)
  {
    digitalWrite(FEED_PIN, LOW);
    delayMicroseconds(19150);
    digitalWrite(FEED_PIN, HIGH);
    delayMicroseconds(850);
  }
}

void loop(void) {
  byte present = 0;
  byte type_s;
  char tempStr[8];
  char rainStr[8];
  char litersStr[8];
  char levelStr[8];
  float waterLevel;
  float waterLevelAvg;
  byte badTempReading = false;
  char inChar;
  float rain = 0;
  if (Serial.available() > 0) 
  {
    // read the incoming byte:
    inChar = Serial.read();
		
    if (inChar == '\n') 
    {
      if(inputString.indexOf("waterLevelLow") != -1)
      {
	waterLevelLow = getSerialValue(inputString);
	Serial.print("waterLevelLow = "); Serial.println(waterLevelLow, DEC);
      }
      else if(inputString.indexOf("waterLevelHigh") != -1) 
      {
	waterLevelHigh = getSerialValue(inputString);
	Serial.print("waterLevelHigh = "); Serial.println(waterLevelHigh, DEC);
      }
      else if(inputString.indexOf("feed") != -1) 
      {
	int pulses = getSerialValue(inputString);
	feed(pulses);
      }
      else if(inputString.indexOf("waterOn") != -1) 
      {
	digitalWrite(WATER_CONTROL_PIN, HIGH);
	waterState = STATE_WATER_ON;
      }
      else if(inputString.indexOf("waterOff") != -1) 
      {
	digitalWrite(WATER_CONTROL_PIN, LOW);
	waterState = STATE_WATER_OFF;
      }
      else if(inputString.indexOf("waterManual") != -1) 
      {
	waterMode = MODE_WATER_MANUAL;
      }
      else if(inputString.indexOf("waterAutomatic") != -1) 
      {
	waterMode = MODE_WATER_AUTOMATIC;
      }

      Serial.print("Received = "); Serial.println(inputString);

      inputString = "";
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

    // Calculate rain in mm
    rain = rainGaugePulses * 0.2794;
    dtostrf(rain,6,2,rainStr);

    // Calculate amount of added water in liters  
    float liters = flowPulses;
    liters /= 8.1;
    liters -= 6;
    liters /= 60.0;
    dtostrf(liters,7,1,litersStr);

    // Calculate the Water level in pond
    waterLevel = analogRead(WATER_LEVEL_PIN);
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

    //########## LCD ######################
    lcd.clear();
    lcd.setCursor(0,0);
    lcd.print("Temp:"); lcd.print(tempStr);lcd.print((char)223);lcd.print("C");
		
    lcd.setCursor(0, 1);
    lcd.print("Rain:");lcd.print(rainStr);lcd.print("mm");
		
    lcd.setCursor(-4, 2);
    lcd.print("Liters:");lcd.print(litersStr);
		
    lcd.setCursor(-4, 3);
    lcd.print("Level:"); lcd.print(levelStr); lcd.print("mm");

    //######### SERIAL SEND ################
    Serial.print("Temp="); Serial.println(tempStr);
    Serial.print("Rain="); Serial.println(rainStr);
    Serial.print("RainPulses="); Serial.println(rainGaugePulses, DEC);
    Serial.print("Flow="); Serial.println(litersStr);
    Serial.print("FlowPulses=");Serial.println(flowPulses, DEC);
    Serial.print("Level="); Serial.println(levelStr);
    Serial.println("#");

  }
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
