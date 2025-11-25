#include "stm32f2xx.h"                  // Device header
#include "core_cm3.h"

void delay(){
unsigned int i;
	i = 0;
	for(i = 0;i < 2000000; i++){}
}

void EXTI0_IRQHandler(void){
  GPIOG->ODR |= 1ul<<6;
	delay();
	GPIOG->ODR &= ~1ul<<6;
	delay();
	
	EXTI->PR |=EXTI_PR_PR0;
}

void EXTI15_10_IRQHandler(void){
GPIOG->ODR |= 1ul<<8;
	delay();
	GPIOG->ODR &= ~1ul<<8;
	delay();
	
	EXTI->PR |=EXTI_PR_PR0;
}


int main ()
{
	RCC->AHB1ENR |= 1ul<<6;
	RCC->APB2ENR |= 1ul <<14;
	
	GPIOG->MODER = (GPIOG->MODER & ~(1ul<<13)) | 1ul<<12;
	GPIOG->MODER = (GPIOG->MODER & ~(1ul<<15)) | 1ul<<14;
	GPIOG->MODER = (GPIOG->MODER & ~(1ul<<17)) | 1ul<<16;
	GPIOG->MODER = (GPIOG->MODER & ~(1ul<<31)) & ~(1ul<<30);
	GPIOG->MODER = (GPIOG->MODER & ~(1ul<<1)) & ~(1ul);
	
	EXTI->IMR  |= EXTI_IMR_MR0 | EXTI_IMR_MR15;
	EXTI->RTSR |= EXTI_RTSR_TR0;
	EXTI->FTSR |= EXTI_FTSR_TR15;
	
	SYSCFG-> EXTICR[0]|= SYSCFG_EXTICR1_EXTI0_PA;
	SYSCFG-> EXTICR[3]|= SYSCFG_EXTICR4_EXTI15_PG;
	
	NVIC_SetPriority(6,5);
	NVIC_SetPriority(40,6);
	
	NVIC_EnableIRQ(6);
	NVIC_EnableIRQ(40);
	
	for(;;){
		GPIOG->ODR |= 1ul<<7;
		delay();
		GPIOG->ODR &= ~1ul<<7;
		delay();
	}
}
