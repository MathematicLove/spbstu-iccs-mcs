#include "stm32f2xx_hal.h"

DAC_HandleTypeDef DacHandle; 
DAC_ChannelConfTypeDef sConfig;

RCC_ClkInitTypeDef RCC_ClkInitStruct;
RCC_OscInitTypeDef RCC_OscInitStruct;

GPIO_InitTypeDef GPIO_InitStruct;

void SystemClock_Config(void)
{
   /* Enable HSE Oscillator and activate PLL with HSE as source */
  RCC_OscInitStruct.OscillatorType = RCC_OSCILLATORTYPE_HSE;
  RCC_OscInitStruct.HSEState = RCC_HSE_ON;
  RCC_OscInitStruct.PLL.PLLState = RCC_PLL_ON;
  RCC_OscInitStruct.PLL.PLLSource = RCC_PLLSOURCE_HSE;
  RCC_OscInitStruct.PLL.PLLM = 25;
  RCC_OscInitStruct.PLL.PLLN = 240;
  RCC_OscInitStruct.PLL.PLLP = RCC_PLLP_DIV2;
  RCC_OscInitStruct.PLL.PLLQ = 5;
  HAL_RCC_OscConfig(& RCC_OscInitStruct);
  
  /* Select PLL as system clock source and configure the HCLK, PCLK1 and PCLK2 
     clocks dividers */
  RCC_ClkInitStruct.ClockType = (RCC_CLOCKTYPE_SYSCLK | RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2);
  RCC_ClkInitStruct.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
  RCC_ClkInitStruct.AHBCLKDivider = RCC_SYSCLK_DIV1;
  RCC_ClkInitStruct.APB1CLKDivider = RCC_HCLK_DIV4;
  RCC_ClkInitStruct.APB2CLKDivider = RCC_HCLK_DIV2;
  HAL_RCC_ClockConfig(& RCC_ClkInitStruct, FLASH_LATENCY_3);
}

void HAL_DAC_MspInit(DAC_HandleTypeDef *hdac)
{
    /*##-1- Enable peripherals and GPIO Clocks #################################*/
  /* Enable GPIO clock ****************************************/
  __GPIOA_CLK_ENABLE();
  /* DAC Periph clock enable */
  __DAC_CLK_ENABLE();

  /*##-2- Configure peripheral GPIO ##########################################*/
  /* DAC Channel1 GPIO pin configuration */
  GPIO_InitStruct.Pin = GPIO_PIN_4;
  GPIO_InitStruct.Mode = GPIO_MODE_ANALOG;
  GPIO_InitStruct.Pull = GPIO_NOPULL;
  HAL_GPIO_Init(GPIOA, & GPIO_InitStruct);
}

int main(void)
{
  /* STM32F2xx HAL library initialization:
       - Configure the Flash prefetch, instruction and Data caches
       - Configure the Systick to generate an interrupt each 1 msec
       - Set NVIC Group Priority to 4
       - Global MSP (MCU Support Package) initialization
     */
  HAL_Init();

  /* Configure the system clock to have a system clock = 120 MHz */
  SystemClock_Config();

  /*##-1- Configure the DAC peripheral #######################################*/
  DacHandle.Instance = DAC;

  HAL_DAC_Init(& DacHandle);
 
  /*##-2- Configure DAC channel1 #############################################*/
  sConfig.DAC_Trigger = DAC_TRIGGER_NONE;
  sConfig.DAC_OutputBuffer = DAC_OUTPUTBUFFER_DISABLE;

  HAL_DAC_ConfigChannel(& DacHandle, & sConfig, DAC_CHANNEL_1);
 
  /*##-3- Set DAC Channel1 DHR register ######################################*/
  HAL_DAC_SetValue(& DacHandle, DAC_CHANNEL_1, DAC_ALIGN_12B_R, 4095);      //???? ?????????? ??????? ??????????? (???????? 4095)
  
  /*##-4- Enable DAC Channel1 ################################################*/
   HAL_DAC_Start(& DacHandle, DAC_CHANNEL_1);
 
  /* Infinite loop */
  while (1)
  {
  }
}
