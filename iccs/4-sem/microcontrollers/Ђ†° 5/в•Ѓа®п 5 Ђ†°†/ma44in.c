#include "cmsis_os.h"
#include "stm32f2xx.h"                  // Device header


void delay ()
{
	unsigned int i;
	for (i = 0; i < 200000; i++) {}
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
	if(ODRnum==1){
		GPIOH->ODR |= 1 << 3;		
	}
	else if(ODRnum==2)
	{
		GPIOH->ODR |= 1 << 6;		
	}
	else if(ODRnum==3)
	{
		GPIOH->ODR |= 1 << 7;		
	}
	else if(ODRnum==4)
	{
		GPIOI->ODR |= 1 << 10;	
	}
	else if(ODRnum==5)
	{
		GPIOG->ODR |= 1 << 6;	
	}
	else if(ODRnum==6)
	{
		GPIOG->ODR |= 1 << 7;	
	}
	else if(ODRnum==7)
	{
		GPIOG->ODR |= 1 << 8;	
	}
	else if(ODRnum==8)
	{
		GPIOH->ODR |= 1 << 2;	
	}
	delay();
}

void LED_Off (int ODRnum)
{
	if(ODRnum==1){
		GPIOH->ODR &= ~1ul << 3;		
	}
	else if(ODRnum==2)
	{
		GPIOH->ODR &= ~1ul << 6;		
	}
	else if(ODRnum==3)
	{
		GPIOH->ODR &= ~1ul << 7;		
	}
	else if(ODRnum==4)
	{
		GPIOI->ODR &= ~1ul << 10;	
	}
	else if(ODRnum==5)
	{
		GPIOG->ODR &= ~1ul << 6;	
	}
	else if(ODRnum==6)
	{
		GPIOG->ODR &= ~1ul << 7;	
	}
	else if(ODRnum==7)
	{
		GPIOG->ODR &= ~1ul << 8;	
	}
	else if(ODRnum==8)
	{
		GPIOH->ODR &= ~1ul << 2;	
	}
		delay();
}


void thread3 (void const *argument)
{	
	while (1)
	{			
		osMessagePut(Q_LED, 5, osWaitForever);
	  delay();
		
		LED_Off(5);
		
		osMessagePut(Q_LED, 6, osWaitForever);
	  delay();
		
		LED_Off(6);
		
		osMessagePut(Q_LED, 7, osWaitForever);
	  delay();
		
		LED_Off(7);		
		
		osMessagePut(Q_LED, 8, osWaitForever);
	  delay();
		
		LED_Off(8);
		
		//osDelay(20);
	}
}

void thread4 (void const *argument)
{	
	while (1)
	{	
		result = osMessageGet(Q_LED, osWaitForever);				//wait for a message to arrive
		LED_On(result.value.v);    
	}
}

void thread1 (void const *argument)
{	
	while (1)
	{	
		osMessagePut(Q_LED, 1, osWaitForever);
	  delay();		
		LED_Off(1);
		osMessagePut(Q_LED, 2, osWaitForever);
	  delay();
		LED_Off(2);
		osMessagePut(Q_LED, 3, osWaitForever);
	  delay();		
		LED_Off(3);
		osMessagePut(Q_LED, 4, osWaitForever);
	  delay();
		
		LED_Off(4);
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
osThreadDef (thread3, osPriorityNormal, 1, 0);
osThreadDef (thread4, osPriorityNormal, 1, 0);

int main () {
	
	osKernelInitialize ();
	
	// LEDs
	RCC->AHB1ENR |= RCC_AHB1ENR_GPIOGEN;
	RCC->AHB1ENR |= RCC_AHB1ENR_GPIOHEN;
	RCC->AHB1ENR |= RCC_AHB1ENR_GPIOIEN;
	GPIOG->MODER = 0;
	GPIOH->MODER = 0;
	GPIOI->MODER = 0;
	GPIOG->MODER |= GPIO_MODER_MODER6_0 | GPIO_MODER_MODER7_0 | GPIO_MODER_MODER8_0;
	GPIOH->MODER |= GPIO_MODER_MODER3_0 | GPIO_MODER_MODER2_0 | GPIO_MODER_MODER6_0| GPIO_MODER_MODER7_0;
	GPIOI->MODER |= GPIO_MODER_MODER10_0;
	osThreadCreate (osThread(thread1), NULL);
	osThreadCreate (osThread(thread3), NULL);
	osThreadCreate (osThread(thread4), NULL);
	osThreadCreate (osThread(thread2), NULL);
	Q_LED = osMessageCreate(osMessageQ(Q_LED),NULL);

	osKernelStart ();
}
