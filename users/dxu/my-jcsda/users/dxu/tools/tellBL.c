// Date : 10/16/2014
// Author: Deyong Xu / RTi @ JCSDA
// Purpose: tell if a system is a big-endian or little-endian platform.
// Compile: $ gcc tellBL.c -o  tellBL.exe
// Run:     $ ./tellBL.exe
// 
int main()
{
  //big endian: 
  //             MOST significant Byte    LEAST significant Byte
  //             left      <===============  right 
  //             0000,00000 | 0000,0000 | 0000,00000 | 0000,0001 |
  //                      |
  //                      y
  //                      *y = 0
  //                      *y + 1 = 48  --> corresponding to char "0"

  //lit endian: 
  //             LEAST significant Byte    MOST significant Byte
  //             left      ===============>   right
  //             0000,00001 | 0000,0000 | 0000,00000 | 0000,0000 |
  //                      |
  //                      y
  //                      *y = 1
  //                      *y + 1 = 49  --> corresponding to char "1"

  // No matter it's big-end or little-end, bit order within a byte 
  // is always that MOST significant BIT is on the LEFT , similar to big endian. 
  //             MOST significant BIT    LEAST significant BIT
  //             left      <===============   right
   
  // No matter it's big-end or little-end, bytes in memory are stored from 
  // lower memory address to higher memory address, which is from left to right.

  int x = 1;

  // Align char variable with the start memory location (lower memory address) of int x. 
  char *y = (char*)&x;

  //===========================
  // Decimal  Hex   Char
  // 48       30    '0'
  // 49       31    '1'
  //===========================
  //
  // If big-endian, then lower memory address holds "00000000"
  // After converting integer to char, then it should be char '0'
  if ((*y)+48 == '0') printf("%s\n","big endian");

  // If little-endian, then lower memory address holds "00000001"
  // After converting integer to char, then it should be char '1'
  if ((*y)+48 == '1') printf("%s\n","little endian");

}
