;*******************************************************************************
;			    OXYGEN THERAPY PROJECT
;*******************************************************************************
; * Program name: Oxygen Therapy Controller
; * Program description: a system that uses the PIC16F877A microcontroller to
;	mimic the process of delivering oxygen to patients and at the same time
;	monitor the available oxygen reservoir and notifying the user when it 
;	is too low
; *
; * Program version: 1.0
; * Created by Ehab Younes and Zeyad Al Najjar
; * Date Created: May 19th, 2021
; * Date Last Revised: May 20th, 2021
;*******************************************************************************
; * Inputs:
; *   Rate 1	(Potentiometer)	    RA0
; *   Rate 2	(Potentiometer)	    RA1
; *   VREF-	(Voltage)	    RA2
; *   VREF+	(Voltage)	    RA3
; *   Fill	(Button)	    RB0	(interrupt)
; *   Bed 1	(Switch)	    RB1
; *   Bed 2	(Switch)	    RB2
; *   LCD View	(Switch)	    RB3
; *
; * Outputs:
; *   Alarm	    (LED)	    RC0
; *   Pump	    (MOTOR)	    RC2
; *   LCD CONFIG    (LCD)	    RC5-RC7
; *   LCD DATA	    (LCD)	    RD0-RD7
;*******************************************************************************
    __CONFIG _DEBUG_OFF&_CP_OFF&_WRT_HALF&_CPD_OFF&_LVP_OFF&_BODEN_OFF&_PWRTE_OFF&_WDT_OFF&_XT_OSC
    
#include <P16F877A.INC> 
   
;*******************************************************************************
;* Performs 16-bit subtraction (DST = DST - SRC)
;* Inputs: DST, SRC
;* Outputs: DST
;*******************************************************************************
SUB16		macro   DST, SRC
    movf	(SRC), W         ; Get low byte of subtrahend
    subwf	(DST), F         ; Subtract DST(low) - SRC(low)
    movf	(SRC)+1, W       ; Now get high byte of subtrahend
    btfss	STATUS, C        ; If there was a borrow increment SRC(high)
    incfsz	(SRC)+1, W       
    subwf	(DST)+1, F       ; Subtract DST(high) - SRC(high)
    endm

    
;*******************************************************************************
;* Moves SRC_BIT in SRC_REG to DST_BIT in DST_REG
;* Inputs: SRC_REG, SRC_BIT, DST_REG, DST_BIT
;* Outputs: DST_REG
;*******************************************************************************
MOVE_BIT	macro	SRC_REG, SRC_BIT, DST_REG, DST_BIT 
    btfsc	SRC_REG, SRC_BIT
    bsf		DST_REG, DST_BIT
    btfss	SRC_REG, SRC_BIT
    bcf		DST_REG, DST_BIT
    endm
    
    
;*******************************************************************************
;* Adds NUM1 and NUM2 (RESULTS = NUM1 + NUM2)
;* Inputs: NUM1, NUM2
;* Outputs: RESULT
;*******************************************************************************
ADDFF		macro	RESULT, NUM1, NUM2
    movf	NUM1, W
    addwf	NUM2, W
    movwf	RESULT
    endm
    
    
;*******************************************************************************
;* Sends a 2 digit BCD number to the LCD   
;* Inputs: BCD
;*******************************************************************************
SEND_BCD	macro	BCD
    swapf	BCD, W		    ; Take the high nibble first (Left-to-right)
    andlw	0x0F
    iorlw	0x30
    call	SEND_CHAR
    
    movf	BCD, W		    ; Then take the low nibble
    andlw	0x0F
    iorlw	0x30
    call	SEND_CHAR
    endm
    
;*******************************************************************************
;* Determines the inputs to the subroutine DISPLAY_BED 
;* Inputs: BED_FLAG, RATE_IN
;* Outputs: BED_NUMBER, RATE, CURRENT_MODE
;*******************************************************************************
DBED_INPUTS	macro	BED_FLAG, RATE_IN
    movlw	BED_FLAG
    andlw	0x0F
    iorlw	0x30
    movwf	BED_NUMBER	    ; Convert number to BCD
    
    movf	RATE_IN, W
    movwf	RATE
    
    movlw	MODE_2_ON_INDEX	    ; Assume CURRENT_MODE = MODE_2_ON_INDEX
    btfss	FLAGS, BED_FLAG
    movlw	MODE_2_OFF_INDEX    ; Correct CURRENT_MODE if the switch is open
    movwf	CURRENT_MODE	    
    endm
    
    
;*******************************************************************************
;* CONSTANTS
;*******************************************************************************
OXYGEN_VOLUME_LO_MAX	    equ	    0xC4
OXYGEN_VOLUME_HI_MAX	    equ	    0x09
OXYGEN_VOLUME_ALARM_LO	    equ	    0xF3
OXYGEN_VOLUME_ALARM_HI	    equ	    0x01
ALARM_FLAG		    equ	    0
BED1_FLAG		    equ	    1
BED2_FLAG		    equ	    2
ALARM_BIN		    equ	    0
BED1_BIN		    equ	    1
BED2_BIN		    equ	    2
DISPLAY_SWITCH_BIN	    equ	    3
MODE_1_INDEX		    equ	    0
MODE_2_ON_INDEX		    equ	    1
MODE_2_OFF_INDEX	    equ	    2
MODE_ALARM_INDEX	    equ	    3
TIMER1_COUNTER		    equ	    2
    
    
;*******************************************************************************
;* VARIABLES
;******************************************************************************* 
    cblock	0x20		    ; Used in delay loop calculation
	LSD 
	MSD
	SEC_CALC
    endc

    cblock	0x25		    ; Printing on the LCD
	STR_INDEX
	FLAGS
	CURRENT_MODE
    endc
    
    cblock	0x30		    ; 16-bit Oxygen Volume register
	OXYGEN_VOLUME_LO
	OXYGEN_VOLUME_HI
    endc
    
    cblock	0x35		    ; 16-bit binary number to packed BCD
	NUMBER_LO
	NUMBER_HI
	BCD_LO
	BCD_HI
	BITS
    endc
    
    cblock	0x40		    ; Subtraction temporary registers 
	DST_LO
	DST_HI
	SRC_LO
	SRC_HI
	TEMP			    
    endc
    
    cblock	0x45		    ; ADC and Rate GPRs
	ADC_VALUE
	ADC_VALUE1
	ADC_VALUE2
	RATE1
	RATE2
	OXYGEN_RATE
	OXYGEN_RATE_HI
    endc
    
    cblock	0x55		    ; subroutine inputs for DISPLAY_BED
	RATE
	BED_NUMBER
    endc
    
    
;*******************************************************************************
;* Program starts here
;*******************************************************************************
MAIN_PROG
    org		0x0000
    goto	INITIAL             ; go to beginning of program

INT_SVC
    org		0x0004		    ; process ISR
    goto	ISR

    
;*******************************************************************************
;* System configuration and initialization
;*******************************************************************************
INITIAL
    banksel	TRISA
    movlw	b'00001111'	    
    movwf	TRISA		    ; RA0 to RA3 are inputs
    movwf	TRISB		    ; RB0 to RB3 are inputs
    clrf	TRISC		    ; Port C & Port D as outputs
    clrf	TRISD		    
    movlw	b'11000000'	    ; External interrupt is on the rising edge
    movwf	OPTION_REG	    
				    
    
    bsf		INTCON, GIE	   
    bsf		INTCON, PEIE
    bsf		INTCON, INTE
    bcf		INTCON, INTF
    bsf		PIE1, TMR1IE

    ; Timer2 setup
    movlw    b'01100011'	    ; PWM Period = (99 + 1) * 4 * 1us = 0.4ms
    movwf    PR2		    ; PWM Frequency = 2.5kHz
    banksel  T2CON
    movlw    b'00000101'	    ; Prescalar = 4
    movwf    T2CON	
    
    ; Timer1 setup
    banksel	PORTA
    call	SETUP_TIMER1
    
    ; LCD setup
    clrf	PORTC
    clrf	PORTD
    movlw	b'00111000'	    ; 8-bit mode, 2-line display, 5x7 dot format
    call	SEND_CMD
    movlw	b'00001100'	    ; Display on, Cursor Underline off, Blink off
    call	SEND_CMD
    call	RESET_DISPLAY
    
    ; ADC setup
    banksel	TRISA
    movlw	b'00001101'	    ; Left justified, AN0 and AN1 with Vref
    movwf	ADCON1
    
    banksel	PORTA
    movlw	b'01000001'	    ; Fosc/8, Channel0 selected
    movwf	ADCON0
    
    clrf	FLAGS
    clrf	OXYGEN_RATE
    clrf	OXYGEN_RATE_HI
    clrf	ADC_VALUE1
    clrf	ADC_VALUE2
    clrf	RATE1
    clrf	RATE2
    
   
;*******************************************************************************
;* Main Routine
;*******************************************************************************
MAIN
    call	FILL_TANK
    call	PROCESS
    goto	$    
    
    
;*******************************************************************************
;* Sets up Timer1 to generate 1 second delay 1,000,000 = 8 * 62500 * 2
;*******************************************************************************
SETUP_TIMER1
    movlw	0xDC
    movwf	TMR1L		    
    movlw	0x0B
    movwf	TMR1H		    ; <TMR1H:TMR1L> = 62500
    movlw	b'00110001'	    ; prescalar = 8, and turn on the timer
    movwf	T1CON		    ; Prescalar is reset when changing TMR1L/TMR1H
    
    return
    
;*******************************************************************************
;* This subroutine prints to the LCD based on the current display mode
;*******************************************************************************
DISPLAY_ON_LCD
    call	RESET_DISPLAY	    ; Start with a clear display w/ cursor home
    btfss	FLAGS, ALARM_FLAG
    goto	NON_ALARM_STATE	    ; Oxygen volume >= 500
    call	DISPLAY_ALARM_MODE  ; Oxygen volume < 500
    goto	FIHISH_DISPLAY_ON_LCD
    
NON_ALARM_STATE
    bcf		PORTC, ALARM_BIN    ; Turn off the LED
    btfss	PORTB, DISPLAY_SWITCH_BIN
    call	DISPLAY_MODE_1
    btfsc	PORTB, DISPLAY_SWITCH_BIN
    call	DISPLAY_MODE_2
    
FIHISH_DISPLAY_ON_LCD
    return
    
    
;*******************************************************************************
;* This subroutine displays mode 1
;*******************************************************************************
DISPLAY_MODE_1
    movlw	MODE_1_INDEX
    movwf	CURRENT_MODE
    call	DISPLAY_STR	    ; Display "Oxygen Volume"
    
    call	GO_TO_LINE2

    call	DISPLAY_OXYGEN_VOLUME	
				    ; Print the 16-bit volume
    call	DISPLAY_STR	    ; Display " Liter(s)"
    
    return
    
    
;*******************************************************************************
;* This subroutine displays mode 2
;*******************************************************************************
DISPLAY_MODE_2
    DBED_INPUTS	BED1_FLAG, RATE1    ; Calculate the inputs for DISPLAY_BED
    call	DISPLAY_BED
    
    call	GO_TO_LINE2
    
    DBED_INPUTS	BED2_FLAG, RATE2
    call	DISPLAY_BED
    
    return
    

;*******************************************************************************
;* Displays the information for a certain bed (ON/OFF and rate)
;* Inputs: BED_NUMBER, CURRENT_MODE, RATE
;*******************************************************************************
DISPLAY_BED
    clrf	STR_INDEX
    call	DISPLAY_STR	    ; Display "Bed"
    
    movf	BED_NUMBER, W
    call	SEND_CHAR	    ; Print bed number
    
    call	DISPLAY_STR	    ; Display "  ON" or " OFF --"

    movlw	MODE_2_ON_INDEX
    xorwf	CURRENT_MODE, W
    btfss	STATUS, Z	    ; Skip if a switch is turned on (CURRENT_MODE = MODE_2_ON_INDEX)
    goto	FINISH_DISPLAY_BED
    call	DISPLAY_RATE	    ; Print the rate
    call	DISPLAY_STR	    ; Display " L/s"
    
FINISH_DISPLAY_BED
    return   
 
    
;*******************************************************************************
;* This subroutine displays alarm mode
;*******************************************************************************
DISPLAY_ALARM_MODE
    movlw	MODE_ALARM_INDEX
    movwf	CURRENT_MODE
    
    bsf		PORTC, ALARM_BIN    ; Turn on the LED
    
    call	DISPLAY_STR	    ; Display "Oxygen Level Low"
    call	GO_TO_LINE2
    call	DISPLAY_STR	    ; Display "Fill Tank!"
    
    return
    
    
;*******************************************************************************
;* Clears the display and resets the cursor
;*******************************************************************************
RESET_DISPLAY
    clrf	STR_INDEX
    movlw	b'00000010'	    ; Display and cursor home
    call	SEND_CMD
    movlw	b'00000001'	    ; Clear display
    call	SEND_CMD
    
    return
    

;*******************************************************************************
;* Sets the cursor at line 2 in the LCD
;*******************************************************************************
GO_TO_LINE2
    movlw	b'11000000'	    ; go to the second line
    call	SEND_CMD
    
    return
    
    
;*******************************************************************************
;* Sends the string at STR_INDEX until a zero (0x00) is encountered
;* Inputs: STR_INDEX
;* Outputs: STR_INDEX
;*******************************************************************************
DISPLAY_STR
    call	STR_LOOKUP	    ; Get ASCII character
    incf	STR_INDEX, F	    ; Move to the next character (or 0x00)
    addlw	0
    btfsc	STATUS, Z	    ; If WREG == 0, then reached end of word/line
    goto	FINISH_DISPLAY_STR
    call	SEND_CHAR
    goto	DISPLAY_STR	    ; repeat until a 0x00 is encountered
    
FINISH_DISPLAY_STR
    return
    
    
;*******************************************************************************
;* String lookup table
;* Inputs: CURRENT_MODE, STR_INDEX
;* Outputs: WREG
;*******************************************************************************
STR_LOOKUP
    movf	CURRENT_MODE, W
    addwf	PCL, F		    ; Go to the correct lookup table based on LCD mode
    goto	MODE_1_LOOKUP
    goto	MODE_2_ON_LOOKUP
    goto	MODE_2_OFF_LOOKUP
    goto	MODE_ALARM_LOOKUP
    
    
MODE_1_LOOKUP
    movf	STR_INDEX, W
    addwf	PCL, F
    dt		"Oxygen Volume", 0x00
    dt		" Liter(s)", 0x00
    

MODE_2_ON_LOOKUP
    movf	STR_INDEX, W
    addwf	PCL, F
    dt		"Bed", 0x00
    dt		" ON  ", 0x00
    dt		" L/s", 0x00
    
MODE_2_OFF_LOOKUP
    movf	STR_INDEX, W
    addwf	PCL, F
    dt		"Bed", 0x00
    dt		" OFF --", 0x00
    
MODE_ALARM_LOOKUP
    movf	STR_INDEX, W
    addwf	PCL, F
    dt		"Oxygen Level Low", 0x00
    dt		"Fill Tank!", 0x00
    
    
;*******************************************************************************
;* Main Processing Routine: executes every one second 
;*******************************************************************************
PROCESS
    call	UPDATE_VOLUME
    call	READ_BED_SWITCHES
    call	READ_ADC_VALUES
    call	CALCULATE_RATES
    call	UPDATE_PUMP_PWM
    call	DISPLAY_ON_LCD

    return
    

;*******************************************************************************
;* Subtracts the rate of the last cycle from the volume 
;*  and tests if the alarm volume was reached
;* Outputs: ALARM_FLAG
;*******************************************************************************
UPDATE_VOLUME
    SUB16	OXYGEN_VOLUME_LO, OXYGEN_RATE
    btfss	STATUS, C		; If volume < 0 then volume = 0
    call	EMPTY_TANK
    
    movf	OXYGEN_VOLUME_LO, W	; We don't want to change the volume
    movwf	DST_LO
    movf	OXYGEN_VOLUME_HI, W
    movwf	DST_HI
    
    movlw	OXYGEN_VOLUME_ALARM_LO	; Move constants to temp registers
    movwf	SRC_LO
    movlw	OXYGEN_VOLUME_ALARM_HI
    movwf	SRC_HI
    
    SUB16	SRC_LO, DST_LO
    
    MOVE_BIT	STATUS, C, FLAGS, ALARM_FLAG
    
    return 
	
    
;*******************************************************************************
;* Sets the volume to zero   
;* Outputs: OXYGEN_VOLUME_LO, OXYGEN_VOLUME_HI
;*******************************************************************************
EMPTY_TANK
    clrf	OXYGEN_VOLUME_LO
    clrf	OXYGEN_VOLUME_HI
    
    return
    
    
;*******************************************************************************
;* sets the oxygen volume to the max value 
;* Outputs: OXYGEN_VOLUME_LO, OXYGEN_VOLUME_HI
;*******************************************************************************
FILL_TANK 
    movlw	OXYGEN_VOLUME_LO_MAX
    movwf	OXYGEN_VOLUME_LO
    
    movlw	OXYGEN_VOLUME_HI_MAX
    movwf	OXYGEN_VOLUME_HI
    
    return
    
    
;*******************************************************************************
;* Reads the beds switches and saves them to FLAGS   
;* Inputs: PORTB
;* Outputs: FLAGS
;*******************************************************************************
READ_BED_SWITCHES
    MOVE_BIT	PORTB, BED1_BIN, FLAGS, BED1_FLAG
    MOVE_BIT	PORTB, BED2_BIN, FLAGS, BED2_FLAG
    
    return    

    
;*******************************************************************************
;* Reads the potentiometers (the most significant 8 bits)   
;* Inputs: FLAGS
;* Outputs: ADC_VALUE1, ADC_VALUE2
;*******************************************************************************
READ_ADC_VALUES
    clrf	ADC_VALUE1
    btfss	FLAGS, BED1_FLAG    ; only read the ADC if the switch is on
    goto	BED2		    ; read the ADC for the 2nd bed
    movlw	b'01000001'	    ; Fosc/8, channel-0
    movwf	ADCON0
    call	READ_ADC
    movwf	ADC_VALUE1
   
BED2
    clrf	ADC_VALUE2
    btfss	FLAGS, BED2_FLAG
    goto	FINISH_READ_ADC
    movlw	b'01001001' 	    ; Fosc/8, channel-1
    movwf	ADCON0
    call	READ_ADC
    movwf	ADC_VALUE2
    
FINISH_READ_ADC
    return
    

;*******************************************************************************
;* Reads the selected ADC value (blocking)
;* Outputs: WREG
;*******************************************************************************
READ_ADC
    call	DELAY		    ; Wait for the previous change to ADCON0 to finish
    bsf		ADCON0, GO	    ; Start the ADC
WAIT_ADC
    btfss	PIR1, ADIF
    goto	WAIT_ADC	    ; Keep polling until it is done
    bcf		PIR1, ADIF
    movf	ADRESH, W	    ; Take the 8 most significant bits
    
    return
    
    
;*******************************************************************************
;* Calculate the rates from the ADC values   
;* Inputs: ADC_VALUE1, ADC_VALUE2
;* Outputs: RATE1, RATE2, OXYGEN_RATE
;*******************************************************************************
CALCULATE_RATES
    movf	ADC_VALUE1, W
    call	GET_RATE
    movwf	RATE1
    
    movf	ADC_VALUE2, W
    call	GET_RATE
    movwf	RATE2
    
    ; Total rate (RATE1 + RATE2)
    ADDFF	OXYGEN_RATE, RATE1, RATE2
    
    return
 
    
;*******************************************************************************
;* Sends a command to the LCD   
;* Inputs: WREG
;*******************************************************************************
SEND_CMD
    movwf	PORTD
    bcf		PORTC, 5
    bsf		PORTC, 7
    nop
    bcf		PORTC, 7
    bcf		PORTC, 6
    call	DELAY
    
    return
    
    
;*******************************************************************************
;* Sends an ASCII character to the LCD   
;* Inputs: WREG
;*******************************************************************************
SEND_CHAR
    movwf	PORTD
    bsf		PORTC, 5
    bsf		PORTC, 7
    nop
    bcf		PORTC, 7
    bcf		PORTC, 6
    call	DELAY
    
    return
    
    
;*******************************************************************************
;* A simple delay used for the LCD
;*******************************************************************************
DELAY
    movlw	0x03
    movwf	MSD
    clrf	LSD
LOOP_DELAY
    decfsz	LSD, F
    goto	LOOP_DELAY
    decfsz	MSD, F
    goto	LOOP_DELAY
    
    return
    
    
;*******************************************************************************
;* Converts the ADC value to the rate (see table in the report)
;* Inputs: WREG
;* Outputs: WREG
;*******************************************************************************
GET_RATE
    movwf	ADC_VALUE
    sublw	.127
    btfss	STATUS,	C
    retlw	.10		    ; ADC_VALUE >= 128
    movf	ADC_VALUE, W
    sublw	.63		    
    btfss	STATUS,	C
    retlw	5		    ; 128 > ADC_VALUE >= 64
    movf	ADC_VALUE, W
    btfss	STATUS,	Z
    retlw	1		    ; 64 > ADC_VALUE >= 1
    retlw	0		    ; ADC_VALUE = 0    
    
    
;*******************************************************************************
;* Converts the calculated rate binary to BCD then sends to the LCD
;* Inputs: WREG
;*******************************************************************************
DISPLAY_RATE
    movf	RATE, W
    movwf	NUMBER_LO	    ; input to GET_BCD_SMALL
    call	GET_BCD_SMALL
    
    SEND_BCD	BCD_LO
    
    return
    

;*******************************************************************************
;* Converts the oxygen volume binary to BCD then sends to the LCD
;* Inputs: OXYGEN_VOLUME_LO, OXYGEN_VOLUME_HI
;*******************************************************************************
DISPLAY_OXYGEN_VOLUME
    movf	OXYGEN_VOLUME_LO, W
    movwf	NUMBER_LO	    ; input to GET_BCD_BIG
    movf	OXYGEN_VOLUME_HI, W
    movwf	NUMBER_HI	    ; input to GET_BCD_BIG
    call	GET_BCD_BIG

    SEND_BCD	BCD_HI		    ; Send high first (displaying left to right)
    SEND_BCD	BCD_LO
    
    return
    
  
;*******************************************************************************
;* Converts a number in the range 0 - 31 to packed BCD
;* Inputs: NUMBER_LO
;* Outputs: BCD_LO
;*******************************************************************************
GET_BCD_SMALL
    clrf	BCD_LO		    ; Clear result
    movlw	D'5'		    
    movwf	BITS		    ; Set bit counter
    
    bcf		STATUS, C
    rlf		NUMBER_LO, F	    
    rlf		NUMBER_LO, F
    rlf		NUMBER_LO, F	    ; take only the first 5 bits (left-justify the bits)

CONVERT_BIT_SMALL
    movlw	.3		    ; Correct BCD value
    addwf	BCD_LO, F	    
    btfss	BCD_LO, .3	    
    subwf	BCD_LO, F	    ; Subtract the 3 because it was already correct

    rlf		NUMBER_LO, F	    ; Shift out a binary bit

    rlf		BCD_LO, F	    ; Shift that bit into the BCD value.

    decfsz	BITS, F		    ; Repeat for all bits
    goto	CONVERT_BIT_SMALL
    
    return
    

;*******************************************************************************
;* Converts a number in the range 0 - 4095 to packed BCD 
;* Inputs: NUMBER_LO, NUMBER_HI
;* Outputs: BCD_LO, BCD_HI
;*******************************************************************************
GET_BCD_BIG
    clrf	BCD_LO		    ; Clear result
    clrf	BCD_HI
    movlw	D'12'		    ; Set bit counter
    movwf	BITS
    
    bcf		STATUS, C
    rlf		NUMBER_LO, F
    rlf		NUMBER_HI, F
    rlf		NUMBER_LO, F
    rlf		NUMBER_HI, F
    rlf		NUMBER_LO, F
    rlf		NUMBER_HI, F
    rlf		NUMBER_LO, F
    rlf		NUMBER_HI, F	    ; take only the first 12 bits (left-justify the bits)

CONVERT_BIT_BIG
    movlw	0x33
    addwf	BCD_LO, F
    btfsc	BCD_LO, .3
    andlw	0xF0		    ; Remove the low 3 because it is correct
    btfsc	BCD_LO, .7
    andlw	0x0F		    ; Remove the high 3 because it is correct
    subwf	BCD_LO, F	    ; Correct the addition
    
    movlw	.3		    ; Correct BCD value
    addwf	BCD_HI, F	    
    btfss	BCD_HI, .3	    
    subwf	BCD_HI, F	    ; Subtract the 3 because it was already correct

    rlf		NUMBER_LO, F	    ; Shift out a binary bit
    rlf		NUMBER_HI, F

    rlf		BCD_LO, F	    ; Shift that bit into the BCD value.
    rlf		BCD_HI, F

    decfsz	BITS, F		    ; Repeat for all bits
    goto	CONVERT_BIT_BIG
    
    return
    
    
;*******************************************************************************
;* Pump Subsystem
;*******************************************************************************
;* Finds the appropriate PWM settings for the provided oxygen rate 
;*  according to the table in the report
;* Inputs: OXYGEN_RATE
;*******************************************************************************
UPDATE_PUMP_PWM
    movf	OXYGEN_RATE, w	    ; rate = 0
    btfsc	STATUS, Z
    goto	PUMP_0
    
    movlw	.6		    ; rate <= 5
    subwf	OXYGEN_RATE, w
    btfss	STATUS, C
    goto	PUMP_5
    
    movlw	.11		    ; rate <= 10
    subwf	OXYGEN_RATE, w
    btfss	STATUS, C
    goto	PUMP_10
    
    movlw	.16		    ; rate <= 15
    subwf	OXYGEN_RATE, w
    btfss	STATUS, C
    goto	PUMP_15

    goto	PUMP_20		    ; default case
    
    
PUMP_0				    ; set speed to 0 RPM
    movlw	b'00000000'	    ; MSBs = 0 (0% duty cycle)
    movwf	CCPR1L
    movlw	b'00001100'	    ; LSBs = 0, PWM mode
    movwf	CCP1CON
    goto	FINISH_PUMP
    
PUMP_5				    ; set speed to 10 RPM
    movlw	b'00001000'	    ; CCPR1 = 32 (8% duty cycle)
    movwf	CCPR1L		    
    movlw	b'00001100'	    ; PWM mode
    movwf	CCP1CON
    goto	FINISH_PUMP
    
PUMP_10				    ; set speed to 40 RPM
    movlw	b'00001111'	    ; CCPR1 = 63 (15.75% duty cycle)
    movwf	CCPR1L
    movlw	b'00111100'	    ; PWM mode
    movwf	CCP1CON
    goto	FINISH_PUMP
    
PUMP_15				    ; set speed to 70 RPM
    movlw	b'00010101'	    ; CCPR1 = 84 (21% duty cycle)
    movwf	CCPR1L
    movlw	b'00001100'	    ; PWM mode
    movwf	CCP1CON	
    goto	FINISH_PUMP
    
PUMP_20				    ; set speed to 100 RPM
    movlw	b'00011001'	    ; CCPR1 = 100 (25% duty cycle)
    movwf	CCPR1L 
    movlw	b'00001100'	    ; PWM mode
    movwf	CCP1CON
    goto    FINISH_PUMP
    
FINISH_PUMP
    return


;*******************************************************************************
;* ISR
;*******************************************************************************
ISR
    btfsc	INTCON, INTF
    goto	HANDLE_FILL_BUTTON
    btfsc	PIR1, TMR1IF
    goto	HANDLE_TIMER1
    
FINISH_ISR
    retfie
    
HANDLE_FILL_BUTTON
    bcf		INTCON, INTF	    ; Clear external interrupt flag
    call	FILL_TANK
    goto	FINISH_ISR
    
HANDLE_TIMER1
    bcf		PIR1, TMR1IF	    ; Clear TMR1 Flag
    call	SETUP_TIMER1
    incf	SEC_CALC, F
    movlw	TIMER1_COUNTER	    ; Fosc = 2 * 500,000 = 1,000,000
    subwf	SEC_CALC, W	    
    btfss	STATUS, Z	    ; Check if a second has passed
    goto	FINISH_HANDLE_TIMER1
    clrf	SEC_CALC	    ; reset the software counter
    call	PROCESS		    ; Reached 1 second
    
FINISH_HANDLE_TIMER1
    goto	FINISH_ISR    

    END
   