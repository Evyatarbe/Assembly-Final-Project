
LIST 	P=PIC16F877
		include	<P16f877.inc>
 __CONFIG _CP_OFF & _WDT_OFF & _BODEN_OFF & _PWRTE_OFF & _HS_OSC & _WRT_ENABLE_ON & _LVP_OFF & _DEBUG_OFF & _CPD_OFF
 ;---------------------------------------------------------------------------------------;


org		0x00
reset	goto	start

org		0x04
		goto	psika

		org	0x10
start	

		bcf		STATUS, RP0
		bcf		STATUS, RP1		;Bank 0

		clrf	PORTD	  ;output LCD
	clrf	PORTA	 ;output LCD
	 clrf	PORTE    ;input of the voltage
	clrf	INTCON	 ;for the interrupt
	clrf	PIR1	 ;for the psika
		
		
		
		
		
		bsf		STATUS, RP0		;Bank 1
		bcf		INTCON,GIE		;No interrupt
		
		clrf 	PIE1 ;clear the psika enabling that in low priorty

		bsf		PIE1, TMR1IE		; TIMER1 interrupt Enable

		bcf		PIE1, ADIE			;Enable ADC Interrupt


		movlw	0x02
		movwf	ADCON1;all A analog, all E digital
		movlw	0xff
		movwf	TRISA ; analog input port
		clrf	TRISD;	digital output port
		clrf	TRISE;	digital output port

		bcf		STATUS, RP0		;Bank 0

		movlw 0x81
		movwf ADCON0
		call d_20
		
		call	init ;initilaiz LCD

		;** TIMER 1 initialization **
		movlw	0x30                ;00110000
		movwf	T1CON				; internal clock source with 1:8 prescaler
		movlw	0xF0
		movwf	TMR1L
		movlw	0x0A
		movwf	TMR1H				;TMR1H:TMR1L = 0x0AF0 = 2800d
									;	Td = 200ns*(2^16-TMR1H:TMR1L)PS = 200ns(2^16-2800)*8 ~= 100ms

		bsf		ADCON0, GO			;Start conversion
		bsf 	INTCON, PEIE		;Enable Peripherals interrupts
		bsf		INTCON, GIE	;Enable global interrupts
		
		

clrf 0x78 ; voltage input register : bit '1' has input\dont have input
		  ; bit '0' counting up\counting down

clrf 0x79 ; display the number we currently at (counter)

clrf 0x77 ; timer delay reg
		
		bsf		T1CON, TMR1ON		; Timer 1 starts to increment

;---------- Main program area: ---------------------------------------------------------;
loop:
		bsf		ADCON0, GO
ad_loop:
		btfss 	PIR1, ADIF ;check AtD psika
		goto ad_loop
		movf 	ADRESH, w ;return the outpot to ADRESH AtD
		movwf 	0x45
		call	d_4
		BCF     PIR1, ADIF;clear AtD psika
		call 	checkRange
		
	
		btfss 0x78, 1
		goto displayNoInput

		btfss 0x78,0
		goto displayDown
		goto displayUp


displayDown:
		call printNumber
		call down
		goto loop

displayUp:
		call printNumber
		call up
		goto loop

displayNoInput:
		call printNumber
		call no_input
		goto loop

;---------- Functions area: -----------------------------------------------------------;


;---------- Interrupt program: ---------------------------------------------------------;

		
psika:	bcf		T1CON, TMR1ON		;stop timer1
		movwf	0x7A				;store W_reg --> 0x7A
		swapf	STATUS, w
		movwf	0x7B				;store STATUS --> 0x7B

		btfsc	PIR1, TMR1IF		;check timer1 int flag
		goto	Timer1
ERR:	goto	ERR

Timer1:	incf	0x77
		movf 	0x77, 0
		movwf	0x76
		movlw 	d'10'
		subwf 	0x76
		btfss 	STATUS, Z		
		goto continuPsik
		clrf	0x77
		goto subOrAdd

continuPsik:

		movlw	0xF0
		movwf	TMR1L
		movlw	0x0A
		movwf	TMR1H
		bcf		PIR1, TMR1IF

		swapf	0x7B, w
		movwf	STATUS				;restore STATUS <-- 0x7B
		swapf	0x7A, f
		swapf	0x7A, w				;restore W_reg <-- 0x7A
		bsf		T1CON, TMR1ON		;start timer1
		retfie
;---------------------------------------------------------------------------------------
subOrAdd:
		clrf 0x76
		btfss 0x78, 1
		goto continuPsik
		btfss 0x78, 0
		goto decDown
		goto incrementUp
		goto continuPsik
decDown:
		goto incrementDown
		goto continuPsik
;-------------------------------------------------------------------------------
incrementUp:
		incf 0x79
		movf 0x79, 0
		movwf 0x76
		movlw d'251'
		subwf 0x76
		btfsc STATUS, Z
		clrf 0x79
		goto continuPsik
incrementDown:
		decf 0x79
		movf 0x79, 0
		movwf 0x76
		movlw d'255'
		subwf 0x76
		btfss STATUS, Z
		goto continuPsik
		movlw d'250'
		movwf 0x79
		goto continuPsik
;-------------------------------------------------------------------------------
; if input reg is within 0.5 to 1.5 volt count up
; else if input is 1.8 to 2.3 volt count down
; else do nothing
checkRange: 
		movf 	0x45, 0
		movwf	0x46
		; check if within 50 to 150
		movlw d'25';
		subwf 0x46;
		btfss STATUS, C
		goto notInUpRange
				
		movf 	0x45, 0
		movwf	0x46
		
		movlw d'75';
		subwf 0x46;
		btfsc STATUS, C
		goto notInUpRange		

		; if passed the in range
		movlw d'3'
		movwf 0x78
		return

notInUpRange:
		
		movf 	0x45, 0
		movwf	0x46
		; check if within 180 to 230
		movlw d'90';
		subwf 0x46;
		btfss STATUS, C
		goto notInDownRange
				
		movf 	0x45, 0
		movwf	0x46
		movlw 	d'115';
		subwf 0x46;
		btfsc STATUS, C
		goto notInDownRange		

		; if passed the in range
		movlw d'2'
		movwf 0x78
		return

notInDownRange:
		clrf 0x78
		return
;---------------------------------------------------------------------------
;----------------------------*------------------------------	
; print no input second row
no_input:
        
		; print on lcd location 0xc0(start of second row)
		movlw 0xc0
		movwf 0x20
		call lcdc
		call	del_41
		
		; print "N"
		movlw 0x4E
		movwf 0x20
		call lcdd
		call	del_41

		; print "O"
		movlw 0x4F
		movwf 0x20
		call lcdd
		call	del_41

		; print "SPACE"
		movlw 0x20
		movwf 0x20
		call lcdd
		call	del_41

		; print "I"
		movlw 0x49
		movwf 0x20
		call lcdd
		call	del_41
		
; print "N"
		movlw 0x4E
		movwf 0x20
		call lcdd
		call	del_41
; print "P"
		movlw 0x50
		movwf 0x20
		call lcdd
		call	del_41
; print "U"
		movlw 0x55
		movwf 0x20
		call lcdd
		call	del_41
; print "T"
		movlw 0x54
		movwf 0x20
		call lcdd
		call	del_41

		
		return  

;----------------------------------------------------------
;print up second row
up:
        
		; print on lcd location 0xc0
		movlw 0xc0
		movwf 0x20
		call lcdc 
		call	del_41

		; print "U"
		movlw 0x55
		movwf 0x20
		call lcdd
		call	del_41

		; print "P"
		movlw 0x50
		movwf 0x20
		call lcdd
		call	del_41

	; print "SPACE"
		movlw 0x20
		movwf 0x20
		call lcdd
		call	del_41
	
	; print "SPACE"
		movlw 0x20
		movwf 0x20
		call lcdd
		call	del_41
	
	; print "SPACE"
		movlw 0x20
		movwf 0x20
		call lcdd
		call	del_41

	; print "SPACE"
		movlw 0x20
		movwf 0x20
		call lcdd
		call	del_41
		
		movlw 0x20
		movwf 0x20
		call lcdd
		call lcdd

		
		return
;----------------------------------------------------------

;down second row
down:
         
		; print on lcd location 0xc0
		movlw 0xc0
		movwf 0x20
		call lcdc 
		call	del_41		

		; print "D"
		movlw 0x44
		movwf 0x20
		call lcdd
		call	del_41

		; print "O"
		movlw 0x4f
		movwf 0x20
		call lcdd
		call	del_41

		; print "W"
		movlw 0x57
		movwf 0x20
		call lcdd
		call	del_41

		; print "N"
		movlw 0x4e
		movwf 0x20
		call lcdd
		call	del_41
		
	; print "SPACE"
		movlw 0x20
		movwf 0x20
		call lcdd
	; print "SPACE"
		movlw 0x20
		movwf 0x20
		call lcdd
	; print "SPACE"
		movlw 0x20
		movwf 0x20
		call lcdd
	; print "SPACE"
		movlw 0x20
		movwf 0x20
		call lcdd

		return  
;----------------------------------------------------------
printNumber:
		
		movf 0x79, 0

		movwf 0x34 ; temp
		clrf 0x56 ;1
		clrf 0x57 ;10
		clrf 0x58 ;100

		movlw d'100'
countHundreds:
		subwf 0x34, 1
		btfss STATUS, C
		goto continueToTens
		incf 0x58	
		goto countHundreds
continueToTens:
		addwf 0x34
		movlw d'10'
countTens:
		subwf 0x34, 1
		btfss STATUS, C
		goto continueToOnes
		incf 0x57	
		goto countTens
continueToOnes:
		addwf 0x34
		movlw d'1'
countOnes:
		subwf 0x34, 1
		btfss STATUS, C
		goto endcount
		incf 0x56	
		goto countOnes
endcount:

		; print on lcd location 0x80(start of first row)
		movlw 0x80
		movwf 0x20
		call lcdc 
		call	del_41

		movlw d'48'
        addwf 0x58,0 ; 0x61 is the argument
		movwf 0x20
		call lcdd
		call	del_41

		movlw d'48'
        addwf 0x57,0 ; 0x61 is the argument
		movwf 0x20
		call lcdd
		call	del_41

		movlw d'48'
        addwf 0x56,0 ; 0x61 is the argument
		movwf 0x20
		call lcdd
		call	del_41		
		
		
		return

;------------------------------------------------------------------------



;
;subroutine to initialize LCD
;
init	movlw	0x30
		movwf	0x20
		call 	lcdc
		call	del_41

		movlw	0x30
		movwf	0x20
		call 	lcdc
		call	del_01

		movlw	0x30
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x01		; display clear
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x06		; ID=1,S=0 increment,no  shift 000001 ID S
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x0c		; D=1,C=B=0 set display ,no cursor, no blinking
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x38		; dl=1 ( 8 bits interface,n=12 lines,f=05x8 dots)
		movwf	0x20
		call 	lcdc
		call	mdel
		return

;
;subroutine to write command to LCD
;

lcdc	movlw	0x00		; E=0,RS=0 
		movwf	PORTE
		movf	0x20,w
		movwf	PORTD
		movlw	0x01		; E=1,RS=0
		movwf	PORTE
        call	sdel
		movlw	0x00		; E=0,RS=0
		movwf	PORTE
		return

;
;subroutine to write data to LCD
;

lcdd	movlw		0x02		; E=0, RS=1
		movwf		PORTE
		movf		0x20,w
		movwf		PORTD
        movlw		0x03		; E=1, rs=1  
		movwf		PORTE
		call		sdel
		movlw		0x02		; E=0, rs=1  
		movwf		PORTE
		return

;----------------------------------------------------------

del_41	movlw		0xcd
		movwf		0x23
lulaa6	movlw		0x20
		movwf		0x22
lulaa7	decfsz		0x22,1
		goto		lulaa7
		decfsz		0x23,1
		goto 		lulaa6 
		return


del_01	movlw		0x20
		movwf		0x22
lulaa8	decfsz		0x22,1
		goto		lulaa8
		return


sdel	movlw		0x19		; movlw = 1 cycle
		movwf		0x23		; movwf	= 1 cycle
lulaa2	movlw		0xfa
		movwf		0x22
lulaa1	decfsz		0x22,1		; decfsz= 12 cycle
		goto		lulaa1		; goto	= 2 cycles
		decfsz		0x23,1
		goto 		lulaa2 
		return


mdel	movlw		0x0a
		movwf		0x24
lulaa5	movlw		0x19
		movwf		0x23
lulaa4	movlw		0xfa
		movwf		0x22
lulaa3	decfsz		0x22,1
		goto		lulaa3
		decfsz		0x23,1
		goto 		lulaa4 
		decfsz		0x24,1
		goto		lulaa5
		return


d_20:
	movlw 0x20
	movwf 0x22
lulaa10: 
	decfsz 0x22, 1
	goto lulaa10
	return

d_4:	
	movlw 0x06
	movwf 0x22
lulaa11:
	decfsz 0x22, 1
	goto lulaa11
	return
		
end