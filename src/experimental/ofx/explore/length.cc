/* this little program calculate the content-length for a HTTP-POST
   Statement. Count begins not until the first OFX (uppercase) occures. 

   Copyright (c) 1998 by Ueli Rutishauser
   GPL'ed
  
 */

#include <iostream.h>
#include <fstream.h>

void main( int argc, char **argv)
{
   char character;
   int input_length=0, count=0, input_alphnum=0, O_yes=0, F_yes=0, X_yes=0;

   cout << "This program calculates the field content-length for a HTTP Header. Counting begins after the first occurens of the string OFX (uppercase)" << endl;

   if ( argc < 2)
        cout << "error - no command line argument given" << endl;

 
   cout << "use " << argv[1] << endl;
   ifstream input(argv[1],ios::in);

   if (!input)
      cout << "error open input file" << endl;
   else
   {
     while (input.get(character))
     {
        //cout << character;
        if (character == 'O')
          O_yes++;
        if (character == 'F' && O_yes > 0)
          F_yes++;
        if (character == 'X' && O_yes > 0 && F_yes)
          X_yes++;

        if (X_yes>0)
           input_length++;
     }

     input_length += 2;  //add OF

     input.close();

     cout << "content-length of this file is (dec)" << dec << input_length << endl;
   }

}
