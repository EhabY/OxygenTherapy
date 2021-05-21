# Introduction
As per specification, the system presented here is used in hospitals to track and regulate the amount of oxygen used for oxygen therapy. This specific system was designed to accommodate only two beds.

There are three main functionalities that the system performs. First, it interactively controls the oxygen flow for each bed, depending on the need, using control knobs (potentiometers) with the option to turn off the machine for either or both beds. The second functionality is interfacing the oxygen pump and producing the correct voltages for the pump to be able to deliver the total required oxygen flow. The final and third functionality is the tracking of the amount of oxygen in the tank and notifying the user when the amount falls beyond a specified volume.

The user can interact with the system either by using the potentiometers as described above to change the flow rate or by flipping the switches to toggle the oxygen for a certain bed. Alternatively, the user can also see the amount of oxygen left in the tank by switching the LCD View switch they can see whether the system is on or off and the individual flow rate for each bed.

The oxygen tracker can also turn on an alarm (modeled by a red LED) to notify the user that the oxygen volume is low and that the tank must be refilled.

# System Description
The system can be split into four different subsystems, each performing a specific functionality:
* **Rate Control Knobs**: Two different potentiometers are used to control the flow rate of the oxygen for each bed. The potentiometer for bed1 is connected to RA0/AN0, while the potentiometer for bed2 is connected to RA1/AN1. We have chosen Vs1 = 5V and V_BAT = 0V. We also connected them to RA3/Vref+ and RA2/Vref-, respectively, for more accurate conversion.
* **LCD**: This subsystem displays the volume of oxygen left in the tank. It can also display the flow rate and whether the system is turned on for each bed. The LCD (model LM016L) uses the Hitachi HD44780 controller. So, RC5 - RC7 were connected to RS, RW, and E respectively which can be used to configure the LCD mode of operation. While RD0 - RD7 were connected to the data pins of the LCD to send ASCII characters and the actual configuration commands.
* **Pump**: The pump is responsible for delivering the oxygen rate required from both beds. The pump is represented as a 20V DC motor that can only run at a constant speed. To control the speed of the pump, the pulse-width modulation technique is used where the pump is switched on and off periodically at a high frequency to achieve an average voltage that will run the pump at the required speed.
* **Oxygen Volume Tracker**: This subsystem keeps track of the oxygen volume available in the tank. That’s achieved by tracking the consumption rate of both beds and updating the oxygen volume periodically. This information is displayed on the LCD in Mode 1. It’s also used to decide when to trigger the alarm mode that turns on the alarm LED and displays a warning message on the LCD.

<br/>

![System Design](/Report/images/FullSystem.png "System Design")
