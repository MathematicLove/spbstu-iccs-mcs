#include "cmsis_os.h"
#include "stm32f2xx.h"                  // Device header


void delay ()
{
	unsigned int i;
	for (i = 0; i < 100000; i++) {}
}

union{
  uint32_t v;
  void *p;
  int32_t signals;
} value;

osMessageQId Q_LED;
osMessageQDef (Q_LED, 0x16, unsigned char);
osEvent result;

void LED_On (int ODRnum)
{
		GPIOG->ODR |= 1 << ODRnum;
		delay();
}

void LED_Off (int ODRnum)
{
		GPIOG->ODR &= ~1 << ODRnum;
		delay();
}


void thread1 (void const *argument)
{	
	while (1)
	{	
		osMessagePut(Q_LED, 6, osWaitForever);
	  delay();
		
		LED_Off(6);
		
		osMessagePut(Q_LED, 7, osWaitForever);
	  delay();
		
		LED_Off(7);
		
		osMessagePut(Q_LED, 8, osWaitForever);
	  delay();
		
		LED_Off(8);
	}
}

void thread2 (void const *argument)
{
	while (1)
	{	
		result = osMessageGet(Q_LED, osWaitForever);				//wait for a message to arrive
		LED_On(result.value.v);    
	}
}

osThreadDef (thread1, osPriorityNormal, 1, 0);
osThreadDef (thread2, osPriorityNormal, 1, 0);

int main () {
	
	osKernelInitialize ();
	
	// LEDs
	RCC->AHB1ENR |= RCC_AHB1ENR_GPIOGEN;
	GPIOG->MODER = 0;
	GPIOG->MODER |= GPIO_MODER_MODER6_0 | GPIO_MODER_MODER7_0 | GPIO_MODER_MODER8_0;
	
	osThreadCreate (osThread(thread1), NULL);
	osThreadCreate (osThread(thread2), NULL);
	
	Q_LED = osMessageCreate(osMessageQ(Q_LED),NULL);

	osKernelStart ();
}

