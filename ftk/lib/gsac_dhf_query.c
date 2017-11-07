/* ===========================================================================
 * Test read/write of .gz files
 */
#include <stdio.h>
#include "zlib.h"
#include <string.h>
#include <stdlib.h>

int main(){
    char *in="/home/tianyf/tmp/sopac.2008.200.full.dhf.Z"; 
    //char *buf;
    gzFile file;
    int len=1024; 
    char *buf,*buf1;

    printf("%s\n",in);
    file = gzopen(in, "rb");
    if (file == NULL) {
      fprintf(stderr, "gzopen error\n");
      exit(1);
    }
    
    printf(" reading data ...\n");
    buf1=gzgets(file, buf, len);
    printf("%s\n",buf);

    
    while(gzeof(file) != 1){
      buf1=gzgets(file, buf, len);
      if (gzeof(file)) {
	printf("ok 1\n");
	break;
      }
      printf("%s\n",buf);
    }   
  
    
    printf(" end of file\n");
    //return 1;
    len=gzclose(file);

    printf("file closed\n");

    return 1;

}
