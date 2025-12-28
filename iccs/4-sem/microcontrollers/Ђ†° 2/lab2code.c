#include "stm32f2xx.h"             // Device header

//------------Delay function----------------------------------
void delay ()
  {
//------------Declaration of type of variables----------------

unsigned long i;                   // Counter for blinky delay

//------------Initialization of variables---------------------

i=0;
		
//------------Main cycle of algorithm-------------------------

  for(i=0; i<2000000; i++){}       // Delay

  }
/*------------------------------------------------------------
  Main function
 *----------------------------------------------------------*/
	int main ()
{
  RCC->AHB1ENR |= 1ul<<6;         // Enable port G clocking
	

  GPIOG->MODER = (GPIOG->MODER & ~1ul<<15) | 1ul<<14;//PG7

	for (;;)
	{	
  GPIOG->ODR |= 1ul<<7;	
		delay ();

  GPIOG->ODR &= ~1ul<<7;
	  delay ();	
		delay ();	
		delay ();	
	}	
}
