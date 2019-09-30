   public static void del(int i)
   {
     int j;

     for (j = i; j <= (length - 1); j++)
     {
       s[j] = s[j+1];
     }

     length = length - 1;
   }